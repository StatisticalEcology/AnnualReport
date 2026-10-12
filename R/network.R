suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(igraph)
})

source("R/people.R")

split_people <- function(x, sep) {
  x <- trimws(unlist(strsplit(coalesce(x, ""), sep)))
  x[x != "" & !grepl("^(ISEC-LOC|et al\\.?)$", x, ignore.case = TRUE)]
}

role_levels <- c("Leadership team", "Core", "Affiliate", "Postdoc",
                 "Current student", "Graduated student", "Former member",
                 "Other collaborator")

# Build people (nodes) and their links (edges) from the sheets.
seec_network <- function(d) {
  # Team members with no role recorded fall through to the postdoc/student lists
  team <- seec_team(d$coreteam) %>%
    filter(Role != "Other") %>%
    transmute(name = Name, role = ifelse(Role == "Core (postdoc)", "Core", Role),
              department = Department)
  postdocs <- d$postdocs %>% transmute(name = Name, role = "Postdoc", department = Department)
  alumni <- d$alumni %>% transmute(name = Name, role = "Former member", department = Department)
  students <- bind_rows(
    d$current_students %>% transmute(name = Name, role = "Current student", department = Department,
                                     degree = Degree, supervisors = Supervisors),
    d$past_students %>% transmute(name = Name, role = "Graduated student", department = Department,
                                  degree = Degree, supervisors = Supervisors)
  )

  # Priority order: a person's role comes from the first list they appear in
  registry <- bind_rows(team, postdocs,
                        students %>% select(name, role, department),
                        alumni) %>%
    filter(!is.na(name))

  # ---- Raw links, before name matching ----
  sup_sep <- "\\s*[,;/&]\\s*|\\s+and\\s+"
  supervision <- students %>%
    filter(!is.na(supervisors)) %>%
    mutate(supervisor = lapply(supervisors, split_people, sep = sup_sep)) %>%
    unnest(supervisor) %>%
    transmute(from = supervisor, to = name, degree)

  papers <- d$pubs %>%
    transmute(paper = row_number(), year = suppressWarnings(as.integer(Year)),
              author = lapply(`SEEC author(s)`, split_people, sep = "\\s*;\\s*")) %>%
    unnest(author)

  grants <- seec_grants(d$grantlist)$grants %>%
    transmute(grant = Funder,
              holder = lapply(`Grant holder`, split_people,
                              sep = "\\s*/\\s*|\\s*,\\s*|\\s+and\\s+")) %>%
    mutate(grant_id = row_number()) %>%
    unnest(holder)

  # ---- Match every spelling to one name ----
  lookup <- resolve_names(
    c(registry$name, supervision$from, supervision$to, papers$author, grants$holder),
    registry %>% select(name)
  )
  std <- function(x) lookup$name[match(x, lookup$raw)]

  registry <- registry %>% mutate(name = std(name)) %>% distinct(name, .keep_all = TRUE)
  supervision <- supervision %>% mutate(from = std(from), to = std(to)) %>%
    filter(from != to)
  papers <- papers %>% mutate(author = std(author)) %>% distinct(paper, author, .keep_all = TRUE)
  grants <- grants %>% mutate(holder = std(holder)) %>% distinct(grant_id, holder, .keep_all = TRUE)

  # ---- Edges ----
  pairs <- function(df, group, person) {
    df %>%
      group_by(.data[[group]]) %>%
      filter(n() > 1) %>%
      summarise(p = list(as.data.frame(t(combn(sort(.data[[person]]), 2)))), .groups = "drop") %>%
      unnest(p) %>%
      rename(from = V1, to = V2)
  }

  e_sup <- supervision %>%
    group_by(from, to) %>%
    summarise(weight = n(), detail = paste(sort(unique(na.omit(degree))), collapse = ", "),
              .groups = "drop") %>%
    mutate(type = "Supervision")
  e_pub <- pairs(papers, "paper", "author") %>%
    count(from, to, name = "weight") %>%
    mutate(type = "Co-authorship", detail = paste(weight, ifelse(weight == 1, "paper", "papers")))
  e_grant <- pairs(grants, "grant_id", "holder") %>%
    count(from, to, name = "weight") %>%
    mutate(type = "Shared grant", detail = paste(weight, ifelse(weight == 1, "grant", "grants")))
  edges <- bind_rows(e_sup, e_pub, e_grant)

  # ---- Nodes ----
  everyone <- unique(c(edges$from, edges$to, registry$name))
  nodes <- tibble(name = everyone) %>%
    left_join(registry, by = "name") %>%
    mutate(
      role = factor(coalesce(role, "Other collaborator"), levels = role_levels),
      department_raw = department,
      department = dept_group(department),
      department = ifelse(role == "Other collaborator", "Outside SEEC", department)
    )

  stud <- students %>% mutate(name = std(name))
  deg <- stud %>% group_by(name) %>%
    summarise(
      degrees = paste(unique(na.omit(degree)), collapse = ", "),
      # Highest degree, in the order Doctorate > Masters > ... > Unknown
      top_degree = degree_levels[min(as.integer(degree_group(degree)))],
      .groups = "drop"
    )

  counts <- list(
    students = supervision %>% distinct(from, to) %>% count(from, name = "n_students") %>% rename(name = from),
    papers = papers %>% count(author, name = "n_papers") %>% rename(name = author),
    grants = grants %>% count(holder, name = "n_grants") %>% rename(name = holder)
  )
  nodes <- nodes %>%
    left_join(deg, by = "name") %>%
    left_join(counts$students, by = "name") %>%
    left_join(counts$papers, by = "name") %>%
    left_join(counts$grants, by = "name") %>%
    mutate(across(c(n_students, n_papers, n_grants), ~ coalesce(.x, 0L)),
           degree_group = case_when(
             role %in% c("Current student", "Graduated student") ~ top_degree,
             TRUE ~ "Staff and collaborators"
           ))

  # ---- Communities (clusters of people who work together) ----
  g <- graph_from_data_frame(edges %>% select(from, to, weight), directed = FALSE,
                             vertices = nodes %>% select(name))
  g <- simplify(g, edge.attr.comb = list(weight = "sum"))
  set.seed(2026)
  comm <- cluster_louvain(g, weights = E(g)$weight)
  memb <- membership(comm)
  nodes$community_id <- as.integer(memb[nodes$name])
  nodes$links <- as.integer(degree(g)[nodes$name])

  # Name each community after its best-connected member; small ones -> "Other"
  csize <- table(nodes$community_id)
  big <- as.integer(names(sort(csize, decreasing = TRUE)))[1:min(7, length(csize))]
  big <- big[csize[as.character(big)] >= 4]
  leaders <- nodes %>% filter(community_id %in% big) %>%
    group_by(community_id) %>% slice_max(links, n = 1, with_ties = FALSE) %>%
    ungroup() %>% mutate(label = paste("Cluster around", name))
  nodes$community <- leaders$label[match(nodes$community_id, leaders$community_id)]
  nodes$community[is.na(nodes$community)] <- "Smaller clusters"

  list(nodes = nodes, edges = edges, lookup = lookup)
}
