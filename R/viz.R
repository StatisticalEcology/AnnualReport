suppressPackageStartupMessages({
  library(dplyr)
  library(plotly)
  library(reactable)
  library(htmltools)
})

# Shared look for every chart, tile and table on the site.
# Colours follow a fixed entity -> colour mapping so a degree or department
# keeps the same colour on every chart.

seec_col <- list(
  blue    = "#2a78d6",
  orange  = "#eb6834",
  aqua    = "#1baf7a",
  yellow  = "#eda100",
  muted   = "#898781",
  faint   = "#b7d3f6",  # lighter step of blue, used for a partial (in-progress) year
  ink     = "#0b0b0b",
  ink2    = "#52514e",
  grid    = "#e1e0d9",
  axis    = "#c3c2b7"
)

degree_levels <- c("Doctorate", "Masters", "Coursework Masters", "Honours", "Unknown")
degree_colors <- c(
  "Doctorate"          = seec_col$blue,
  "Masters"            = seec_col$orange,
  "Coursework Masters" = seec_col$aqua,
  "Honours"            = seec_col$yellow,
  "Unknown"            = seec_col$muted
)

# CB / AA / DS Masters are the coursework Masters programmes
degree_group <- function(x) {
  out <- case_when(
    is.na(x) ~ "Unknown",
    grepl("^doc|phd", x, ignore.case = TRUE) ~ "Doctorate",
    grepl("^(CB|AA|DS) ?masters", x, ignore.case = TRUE) ~ "Coursework Masters",
    grepl("masters|msc", x, ignore.case = TRUE) ~ "Masters",
    grepl("hon", x, ignore.case = TRUE) ~ "Honours",
    TRUE ~ "Unknown"
  )
  factor(out, levels = degree_levels)
}

dept_group <- function(x) {
  case_when(
    is.na(x) ~ "Not recorded",
    grepl("statistic", x, ignore.case = TRUE) ~ "Statistical Sciences",
    grepl("biolog", x, ignore.case = TRUE) ~ "Biological Sciences",
    TRUE ~ "Other"
  )
}
dept_colors <- c(
  "Statistical Sciences" = seec_col$blue,
  "Biological Sciences"  = seec_col$orange,
  "Other"                = seec_col$aqua,
  "Not recorded"         = seec_col$muted
)

# Journal names differ only in capitalisation in places ("Ecology and evolution"
# vs "Ecology and Evolution"); group case-insensitively and show the most
# common spelling.
normalise_journal <- function(x) {
  key <- tolower(gsub("\\s+", " ", trimws(x)))
  spelling <- tapply(x, key, function(v) names(sort(table(v), decreasing = TRUE))[1])
  ifelse(is.na(x), NA_character_, unname(spelling[key]))
}

# Author lists are comma-separated, except a few "Surname, First; ..." lists
count_authors <- function(x) {
  x <- coalesce(x, "")
  ifelse(x == "", 0L,
         ifelse(grepl(";", x), lengths(strsplit(x, "\\s*;\\s*")),
                lengths(strsplit(x, "\\s*,\\s*"))))
}

current_year <- as.integer(format(Sys.Date(), "%Y"))

fmt_num <- function(x, digits = 0) {
  formatC(x, format = "f", digits = digits, big.mark = ",")
}

fmt_money <- function(x) {
  ifelse(
    abs(x) >= 1e6, paste0("R", formatC(x / 1e6, format = "f", digits = 1), "M"),
    ifelse(abs(x) >= 1e3, paste0("R", formatC(x / 1e3, format = "f", digits = 0), "K"),
           paste0("R", fmt_num(x)))
  )
}

# ---- Stat tiles ------------------------------------------------------------

stat_tile <- function(label, value, note = NULL, href = NULL) {
  inner <- tagList(
    div(class = "stat-label", label),
    div(class = "stat-value", value),
    if (!is.null(note)) div(class = "stat-note", note)
  )
  if (!is.null(href)) {
    a(class = "stat-tile stat-link", href = href, inner)
  } else {
    div(class = "stat-tile", inner)
  }
}

stat_row <- function(...) div(class = "stat-row", ...)

# ---- Charts ----------------------------------------------------------------

seec_font <- list(
  family = "Inter, system-ui, -apple-system, 'Segoe UI', sans-serif",
  size = 13,
  color = seec_col$ink2
)

seec_layout <- function(p, title = NULL, subtitle = NULL, ytitle = NULL,
                        xtitle = NULL, legend = TRUE, yformat = ",d",
                        yprefix = "", height = NULL, horizontal = FALSE) {
  heading <- if (is.null(title)) NULL else list(
    text = paste0(
      "<b>", title, "</b>",
      if (!is.null(subtitle)) paste0("<br><span style='font-size:12px;color:", seec_col$muted, "'>", subtitle, "</span>") else ""
    ),
    x = 0, xanchor = "left", xref = "paper",
    y = 1, yanchor = "top", yref = "container", pad = list(t = 10),
    font = list(size = 15, color = seec_col$ink)
  )
  value_axis <- list(
    title = list(text = ytitle, font = list(size = 12, color = seec_col$muted)),
    gridcolor = seec_col$grid, gridwidth = 1, zeroline = TRUE,
    zerolinecolor = seec_col$axis, zerolinewidth = 1,
    tickformat = yformat, tickprefix = yprefix, rangemode = "tozero",
    tickfont = list(color = seec_col$muted), fixedrange = TRUE
  )
  category_axis <- list(
    title = list(text = xtitle, font = list(size = 12, color = seec_col$muted)),
    showgrid = FALSE, linecolor = seec_col$axis, linewidth = 1,
    tickfont = list(color = seec_col$ink2), fixedrange = TRUE,
    automargin = TRUE
  )
  p %>%
    layout(
      title = heading,
      font = seec_font,
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      margin = list(t = if (is.null(title)) 20 else 70, r = 20, b = 40, l = 10),
      xaxis = if (horizontal) value_axis else category_axis,
      yaxis = if (horizontal) category_axis else value_axis,
      showlegend = legend,
      legend = list(orientation = "h", x = 0, xanchor = "left", y = 1.02,
                    yanchor = "bottom", font = list(size = 12, color = seec_col$ink2),
                    traceorder = "normal"),
      hoverlabel = list(bgcolor = "#ffffff", bordercolor = seec_col$grid,
                        font = list(family = seec_font$family, size = 12, color = seec_col$ink)),
      bargap = 0.35
    ) %>%
    config(displayModeBar = FALSE, responsive = TRUE)
}

# Single-series column chart. `partial` marks bars (e.g. the current year)
# that are still accumulating, drawn in a lighter step of the same hue.
seec_columns <- function(df, x, y, title, subtitle = NULL, ytitle = NULL,
                         partial = NULL, hover_fmt = function(v) fmt_num(v),
                         unit = "", yprefix = "", yformat = ",d", height = NULL) {
  xv <- df[[x]]
  yv <- df[[y]]
  is_partial <- if (is.null(partial)) rep(FALSE, length(xv)) else xv %in% partial
  plot_ly(
    height = height,
    x = xv, y = yv, type = "bar",
    marker = list(color = ifelse(is_partial, seec_col$faint, seec_col$blue)),
    text = paste0("<b>", xv, ifelse(is_partial, " (to date)", ""), "</b><br>",
                  hover_fmt(yv), unit),
    hoverinfo = "text", textposition = "none"
  ) %>%
    seec_layout(title = title, subtitle = subtitle, ytitle = ytitle,
                legend = FALSE, yprefix = yprefix, yformat = yformat) %>%
    layout(xaxis = list(type = "category", categoryorder = "array",
                        categoryarray = sort(unique(xv))))
}

# Stacked columns, one colour per group (fixed by `colors`).
seec_stacked <- function(df, x, group, y, colors, title, subtitle = NULL,
                         ytitle = NULL, unit = "") {
  groups <- intersect(names(colors), unique(as.character(df[[group]])))
  p <- plot_ly()
  for (g in groups) {
    sub <- df[as.character(df[[group]]) == g, ]
    p <- p %>% add_trace(
      x = sub[[x]], y = sub[[y]], type = "bar", name = g,
      marker = list(color = colors[[g]], line = list(color = "#ffffff", width = 1)),
      text = paste0("<b>", sub[[x]], "</b><br>", g, ": ", fmt_num(sub[[y]]), unit),
      hoverinfo = "text", textposition = "none"
    )
  }
  p %>%
    seec_layout(title = title, subtitle = subtitle, ytitle = ytitle) %>%
    layout(barmode = "stack",
           xaxis = list(type = "category", categoryorder = "array",
                        categoryarray = sort(unique(df[[x]]))))
}

# Horizontal bars, largest at the top, value labelled at the bar tip.
seec_hbars <- function(df, label, y, title, subtitle = NULL, colors = NULL,
                       color_by = NULL, label_fmt = function(v) fmt_num(v),
                       hover_extra = NULL, height = 130 + 36 * nrow(df),
                       xformat = NULL, xprefix = "") {
  df <- df[order(df[[y]]), ]
  cols <- if (is.null(colors)) seec_col$blue else unname(colors[as.character(df[[color_by]])])
  # hover_extra: name of a column in df holding extra hover text
  hover <- paste0("<b>", df[[label]], "</b><br>", label_fmt(df[[y]]),
                  if (!is.null(hover_extra)) paste0("<br>", df[[hover_extra]]) else "")
  plot_ly(
    height = height,
    y = factor(df[[label]], levels = df[[label]]), x = df[[y]],
    type = "bar", orientation = "h",
    marker = list(color = cols),
    text = label_fmt(df[[y]]), textposition = "outside",
    textfont = list(color = seec_col$ink2, size = 12),
    hovertext = hover, hoverinfo = "text", cliponaxis = FALSE
  ) %>%
    seec_layout(title = title, subtitle = subtitle, legend = FALSE,
                horizontal = TRUE, height = height, yprefix = xprefix,
                yformat = if (!is.null(xformat)) xformat else if (all(df[[y]] == round(df[[y]]))) ",d" else ",.1f")
}

# ---- Tables ----------------------------------------------------------------

seec_table_theme <- reactableTheme(
  color = seec_col$ink,
  borderColor = seec_col$grid,
  headerStyle = list(
    color = seec_col$ink2, fontWeight = 600, fontSize = "0.8rem",
    textTransform = "uppercase", letterSpacing = "0.03em",
    borderBottom = paste("1px solid", seec_col$axis)
  ),
  cellPadding = "8px 10px",
  rowHighlightStyle = list(background = "#f4f8fd"),
  searchInputStyle = list(width = "260px", borderColor = seec_col$grid),
  filterInputStyle = list(borderColor = seec_col$grid),
  footerStyle = list(fontWeight = 700, borderTop = paste("1px solid", seec_col$axis))
)

# Searchable, sortable table with a CSV download button.
seec_table <- function(df, id, columns = NULL, page_size = 15, filterable = FALSE,
                       download = TRUE, ...) {
  tbl <- reactable(
    df,
    elementId = id,
    columns = columns,
    searchable = TRUE,
    filterable = filterable,
    sortable = TRUE,
    resizable = TRUE,
    highlight = TRUE,
    compact = TRUE,
    defaultPageSize = page_size,
    showPageSizeOptions = TRUE,
    pageSizeOptions = c(15, 25, 50, 100),
    paginationType = "jump",
    theme = seec_table_theme,
    defaultColDef = colDef(minWidth = 110, headerClass = "nowrap-header"),
    language = reactableLang(searchPlaceholder = "Search this table…"),
    ...
  )
  if (!download) return(tbl)
  tagList(
    div(class = "table-tools",
        tags$button(class = "btn btn-sm btn-outline-secondary",
                    onclick = sprintf("Reactable.downloadDataCSV('%s', '%s.csv')", id, id),
                    "Download CSV")),
    tbl
  )
}

# Capitalise the first letter only ("in progress" -> "In progress", "LoA" stays)
sentence_case <- function(x) sub("^(.)", "\\U\\1", x, perl = TRUE)

# Compact numbers table, used for the "Show the numbers" view under each chart.
seec_numbers <- function(df, ...) {
  reactable(
    df, compact = TRUE, sortable = TRUE, pagination = FALSE,
    theme = seec_table_theme,
    defaultColDef = colDef(align = "right", format = colFormat(separators = TRUE)),
    ...
  )
}

numbers_view <- function(tbl, summary = "Show the numbers") {
  tags$details(class = "numbers-view", tags$summary(summary), tbl)
}
