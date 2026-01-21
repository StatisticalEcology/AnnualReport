suppressPackageStartupMessages({
  library(googlesheets4)
  library(dplyr)
})

seec_read_data <- function() {
  gs4_deauth()
  
  coreteam <- read_sheet(
    "https://docs.google.com/spreadsheets/d/1CAFM0UHwShVwRUHQzJGa-gluk6qjIW4cP4Lo7q--TrY/edit?usp=sharing",
    sheet = "Current core / affiliate team"
  ) %>%
    select(Name, Department, `core / researcher / student`) %>%
    arrange(Name)
  
  current_students <- read_sheet(
    "https://docs.google.com/spreadsheets/d/1CAFM0UHwShVwRUHQzJGa-gluk6qjIW4cP4Lo7q--TrY/edit?usp=sharing",
    sheet = "Current students"
  ) %>%
    rename(
      Status = `Graduated (Date - leave open if ongoing)`,
      Title = `Title of thesis`,
      `First registered` = `First registered (Date)`
    ) %>%
    arrange(Name)
  
  past_students <- read_sheet(
    "https://docs.google.com/spreadsheets/d/1CAFM0UHwShVwRUHQzJGa-gluk6qjIW4cP4Lo7q--TrY/edit?usp=sharing",
    sheet = "Past students"
  ) %>%
    rename(
      Name = name,
      Degree = degree,
      `First Registered` = startyear,
      `Graduation Year` = endyear,
      Department = program,
      Supervisors = supervisors,
      Title = title
    ) %>%
    arrange(Name)
  
  list(
    coreteam = coreteam,
    current_students = current_students,
    past_students = past_students
  )
}
