suppressPackageStartupMessages({
  library(googlesheets4)
  library(dplyr)
})

# Drop personal identifiers (emails, student numbers) and unnamed spreadsheet
# columns so they never reach the published site.
drop_private_cols <- function(df) {
  df %>%
    select(
      -matches("e-?mail|(student|staff)[\\s_]*(number|no|nr|#)", ignore.case = TRUE),
      -starts_with("...")
    )
}

seec_sheet <- "https://docs.google.com/spreadsheets/d/1C5eBVgsEvAISlI_MDMJU2tRekhPIPGguKdRSaBTpuRw/edit?usp=sharing"

seec_read_data <- function() {
  gs4_deauth()
  
  coreteam <- read_sheet(seec_sheet, sheet = "Current core / affiliate team") %>%
    select(Name, Department, `core / researcher / student`, HRA) %>%
    arrange(Name)
  
  # One tab holds every student; a graduation year in `endyear` marks past students
  students <- read_sheet(seec_sheet, sheet = "Students", col_types = "c") %>%
    rename(Name = 1) %>%
    drop_private_cols() %>%
    filter(!is.na(Name), is.na(endyear) | endyear != "Deregistered") %>%
    mutate(
      startyear = suppressWarnings(as.integer(startyear)),
      endyear = suppressWarnings(as.integer(endyear))
    ) %>%
    arrange(Name)
  
  current_students <- students %>%
    filter(is.na(endyear)) %>%
    select(
      Name,
      Degree = degree,
      `First registered` = startyear,
      Status = status,
      Department = program,
      Supervisors = supervisors,
      Title = title
    )
  
  past_students <- students %>%
    filter(!is.na(endyear)) %>%
    select(
      Name,
      Degree = degree,
      `First Registered` = startyear,
      `Graduation Year` = endyear,
      Department = program,
      Supervisors = supervisors,
      Title = title
    )
  
  postdocs <- read_sheet(seec_sheet, sheet = "Current PostDocs") %>%
    drop_private_cols()

  alumni <- read_sheet(seec_sheet, sheet = "Alumni") %>%
    select(Name, `Former type of affiliation`, Department) %>%
    filter(!is.na(Name))
  
  # Grants 
  
  grantlist <- read_sheet('https://docs.google.com/spreadsheets/d/1055yLA_Wh7KN8OJfakazZTAyBguB9i_CqSzf0hmC5Pc/edit?usp=sharing', sheet='Projects')
  
  # Symposia 
  
  symposiums <- read_sheet('https://docs.google.com/spreadsheets/d/17IYEg1OGOv4pf7Q5wLuT8VDsJTo7UU14UjDBnxXi33w/edit?usp=sharing', sheet='Symposiums')
  
  # Publications
  
  pubs <- read_sheet(seec_sheet, sheet = "Publications")
  
  list(
    coreteam = coreteam,
    current_students = current_students,
    past_students = past_students,
    postdocs = postdocs,
    alumni = alumni,
    grantlist = grantlist,
    symposiums = symposiums,
    pubs = pubs
  )
}

# ---- Shared derived tables (used by more than one page) ---------------------

# Grants from 2019 on, with yearly amounts as numbers. Rows without an end
# year are the sheet's own "Total" rows and notes, so they are dropped.
seec_grants <- function(grantlist, from = 2019) {
  year_cols <- names(grantlist)[grepl("^20[0-9]{2}$", names(grantlist))]
  year_cols <- year_cols[as.integer(year_cols) >= from]
  grants <- grantlist %>%
    tidyr::drop_na(`End Year`) %>%
    mutate(
      `Start Year` = suppressWarnings(as.integer(`Start Year`)),
      `End Year`   = suppressWarnings(as.integer(`End Year`)),
      across(all_of(year_cols), ~ readr::parse_number(as.character(.x)))
    ) %>%
    filter(`End Year` >= from)
  long <- grants %>%
    select(Funder, `Grant holder`, all_of(year_cols)) %>%
    tidyr::pivot_longer(all_of(year_cols), names_to = "year", values_to = "amount") %>%
    mutate(year = as.integer(year)) %>%
    filter(!is.na(amount), amount > 0)
  yearly <- long %>%
    group_by(year) %>%
    summarise(total = sum(amount), active = n(), .groups = "drop") %>%
    arrange(year)
  list(grants = grants, long = long, yearly = yearly, year_cols = year_cols)
}

# Roles are free text (e.g. "affiliate (HRA exp 31 May 2029)"); group them
seec_team <- function(coreteam) {
  role <- coreteam$`core / researcher / student`
  coreteam %>%
    mutate(
      Role = case_when(
        grepl("leadership", role, ignore.case = TRUE) ~ "Leadership team",
        grepl("postdoc", role, ignore.case = TRUE) ~ "Core (postdoc)",
        grepl("core", role, ignore.case = TRUE) ~ "Core",
        grepl("affiliate", role, ignore.case = TRUE) ~ "Affiliate",
        TRUE ~ "Other"
      ),
      # The sheet's own HRA column is the source of truth (role text can be stale)
      `Honorary Research Associate` = ifelse(!is.na(HRA), "Yes", "")
    )
}
