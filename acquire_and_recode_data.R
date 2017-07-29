
library(RPostgreSQL)
library(tidyverse)
library(lubridate)

# connect to AACT to get the data
drv <- dbDriver('PostgreSQL')
con <- dbConnect(drv, dbname="aact",host="aact-prod.cr4nrslb1lw7.us-east-1.rds.amazonaws.com", port=5432, user="aact", password="aact")

# pass through a simple sql join of the main studies table and sponsors table
sponsors <- dbGetQuery(con, "select a.nct_id
                      , a.first_received_date
                      , b.agency_class
                      , b.lead_or_collaborator
                      , b.name
                      from studies a
                      left join sponsors b on a.nct_id=b.nct_id")

# each trial has 1 lead sponsor and potentially multiple collaborator
# lead and collaborators are classified in various ways
sponsors %>%
  group_by(lead_or_collaborator, agency_class) %>%
  summarise(n = n()) %>%
  arrange(lead_or_collaborator, agency_class) 

# Other is a big category, look at the actual names to see if we can recode some
other_names <- sponsors %>%
  filter(agency_class == 'Other') %>%
  group_by(name) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

# uncomment to look at some of them
# other_names
# many are hospitals, universities, so let's recode 

hospital <- c("Hospital","Clinic","Cancer Center","Medical Center","Hôpitaux")
university <- c("University","College")
foundation <- c("Foundation")
research_institute <- c("Research Institute")

sponsors_recode <- sponsors %>%
  mutate(class_recode = case_when(
    agency_class != 'Other' ~ agency_class,
    grepl(paste(hospital, collapse = "|"), name) ~ 'Hospital',
    grepl(paste(university, collapse = "|"), name) ~ 'University',
    grepl(paste(foundation, collapse = "|"), name) ~ 'Foundation',
    grepl(paste(research_institute, collapse = "|"), name) ~ 'Research Institute',
    TRUE ~ 'Other')
    , year = year(first_received_date)
  ) %>%
  select(nct_id, lead_or_collaborator, agency_class, class_recode, year) %>%
  as_data_frame()

sponsors_recode
# look at new breakdown
sponsors_recode %>%
  group_by(lead_or_collaborator, class_recode) %>%
  summarise(n = n()) %>%
  arrange(lead_or_collaborator, class_recode)

# i want to categorize overall sponsor following K. Chiswell
# http://aact.ctti-clinicaltrials.org/use_cases/2
# basically, if the lead is industry, it is industry
# otherwise, if nih is lead or collaborator, it is nih
# otherwise, if industry is a collaborator, it is industry
# Chiswell categorized the rest as other, but I will categorize
# according to the lead, based on my recoded classes

# to do this i need to group by nct_id and make a flag for any NIH
# and any Industry
# below works but is very, very slow
sponsors_any_nih_ind <- sponsors_recode %>%
  group_by(nct_id) %>%
  mutate(
    nih = case_when( 
      (any(agency_class == 'NIH')) ~ 'Y', 
      TRUE ~ 'N' 
    ),
    industry = case_when( 
      (any(agency_class == 'Industry')) ~ 'Y', 
      TRUE ~ 'N' 
    ),
    n_collab = n()
  )

sponsors_any_nih_ind
sponsors_any_nih_ind %>%
  filter(nih == "Y")

sponsors_any_nih_ind %>%
  group_by(nih) %>%
  summarise(n = n())

# nodup by filtering on the lead
# use the nih and industry indicators to assign overall based on above logic
# this also seems slow
sponsors_final <- sponsors_any_nih_ind %>%
  filter(lead_or_collaborator == 'lead') %>%
  mutate(class = case_when(
    agency_class == "Industry" ~ "Industry",
    nih == "Y" ~ "NIH",
    industry == "Y" ~ "Industry",
    TRUE ~ class_recode
  )) 

sponsors_final

sponsors_final %>%
  group_by(class) %>%
  summarise(n = n())

write_csv(sponsors_final, "sponsors.csv")

