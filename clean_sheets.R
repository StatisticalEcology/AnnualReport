# Clean student sheets from a copied Google Sheet

library(googlesheets4)
library(dplyr)
library(janitor)

sheet_url <- "https://docs.google.com/spreadsheets/d/1C5eBVgsEvAISlI_MDMJU2tRekhPIPGguKdRSaBTpuRw/edit?gid=1812245429#gid=1812245429"

current_raw <- read_sheet(sheet_url, sheet = "Current students")
past_raw <- read_sheet(sheet_url, sheet = "Past students")

current_clean <- current_raw %>%
  rename(startyear = `First registered (Date)`,
         status = `Graduated (Date - leave open if ongoing)`,
         program = Department,
         title = `Title of thesis`) %>%
  mutate(endyear = NA,
         across(everything(), as.character)) %>%
  clean_names()

past_clean <- past_raw %>%
  clean_names() %>%
  mutate(status = "Graduated",
         across(everything(), as.character))


glimpse(current_clean)
glimpse(past_clean)
# merge clean sheets 

all_students <- bind_rows(current_clean, past_clean)

# Write merged data back to Google Sheets (tab: "All students")
sheet_write(
  data = all_students,
  ss = sheet_url,
  sheet = "All students"
)
