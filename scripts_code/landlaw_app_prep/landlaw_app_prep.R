rm(list=ls(all=TRUE))

library(tidyverse)
library(magrittr)
library(rlang)
library(sf) # converting to sf
library(tigris) #geojoin
library(raster) # making bboxes

# clean bills df ####
load("landlaw_app_prep/d_bills.rdata")

d_bills %<>% subset(select = c(
        state, 
        number, 
        state_link, 
        status, 
        last_action_date, 
        last_action, 
        title, 
        description, 
        `PRC citizen`,
        `Resident status`,
        `PRC Companies, Orgs, Entities`,
        `PRC Govt, Entities`,
        `CCP Members, Companies`,
        `Foreign Govts, Entities, individuals`,
        `Foreign Adversaries (including PRC)`,
        Any,
        Residential,
        `Commercial or Not for Personal Use`,
        `Agricultural, Natural Resource`,
        `State Land`,
        `Near Federal Land/Critical Infrastructure`,
        `Near Military Facilities`,
        `Of a Certain Size`,
        `Existing Owners Must Sell Property`
))

# call US -> DC for merging with geometry df (will change back after merging with geometry df)
d_bills %<>% mutate(state = case_when(
        state == "US" ~ "DC",
        T ~ state))

# rename columns for convenience
d_bills %<>% rename("PRC_citizen" = `PRC citizen`,
                    "Resident_status" = `Resident status`,
                    "PRC_Companies_Orgs_Entities" = `PRC Companies, Orgs, Entities`,
                    "PRC_Govt_Entities" = `PRC Govt, Entities`,
                    "CCP_Members_Companies" = `CCP Members, Companies`,
                    "Foreign_Govts_Entities_Individuals" = `Foreign Govts, Entities, individuals`,
                    "Foreign_Adversaries_Including_PRC" = `Foreign Adversaries (including PRC)`,
                    "All_types" = "Any",
                    "Commercial_or_Not_for_Personal_Use" = `Commercial or Not for Personal Use`,
                    "Agricultural_Natural_Resource" = `Agricultural, Natural Resource`,
                    "State_Land" = `State Land`,
                    "Near_Federal_Land_Critical_Infrastructure" = `Near Federal Land/Critical Infrastructure`,
                    "Near_Military_Facilities" = `Near Military Facilities`,
                    "Of_a_Certain_Size" = `Of a Certain Size`,
                    "Existing_Owners_Must_Sell_Property" = `Existing Owners Must Sell Property`)

# recode bills status
d_bills %<>% mutate(status = case_when(
        status == 0 ~ "Pre-filed or pre-introduction",
        status == 1 ~ "Introduced",
        status == 2 ~ "Engrossed",
        status == 3 ~ "Enrolled",
        status == 4 ~ "Passed",
        status == 5 ~ "Vetoed",
        status == 6 ~ "Failed",
        TRUE ~ "Unknown"))

# create bills categories for maps based on bill status
d_bills_all <- d_bills
d_bills_passed <- d_bills %>% filter(status == "Passed") 
d_bills_considering <- d_bills %>% filter(status == "Pre-filed or pre-introduction" |
                                                  status == "Introduced" |
                                                  status == "Engrossed" |
                                                  status == "Enrolled") 
        
        
# ALL bills ####

# create bill-level summary variable
d_bills_all %<>% mutate(click_response = paste0("<u>", number, "</u>", ": ", title,  "<br/>",
                                            "<u>", "Description", "</u>", ": ",  description, "<br/>",
                                            "<u>", "Status", "</u>", ": ", status, "<br/>",
                                            "<u>", "Last action and date", "</u>", ": ", last_action, " (", last_action_date, ")", "<br/>",
                                            "<a href=", state_link, " target=\"_blank\" rel=\"noopener noreferrer\"", ">Full bill text</a>"))

# produce columns for identifying bill provisions in state-level summary
prohibit_provisions <- c("PRC_citizen", "Resident_status", "PRC_Companies_Orgs_Entities", "PRC_Govt_Entities", "CCP_Members_Companies", "Foreign_Govts_Entities_Individuals",
                         "Foreign_Adversaries_Including_PRC", "Existing_Owners_Must_Sell_Property", "All_types", "Residential", "Commercial_or_Not_for_Personal_Use", "Agricultural_Natural_Resource",
                         "State_Land", "Near_Federal_Land_Critical_Infrastructure", "Near_Military_Facilities", "Of_a_Certain_Size")

for (i in prohibit_provisions) {
        
        new_col <- paste0(i, "_2")
        
        old_col <- sym(i)
        
        d_bills_all %<>% mutate(!!new_col := case_when(
              !!old_col == 1 ~ number,
              TRUE ~ ""
        ))}

## make df state-level ####
# collapse df so that each state has its own row and concatenate bill names
d_bills_states_all <- d_bills_all %>%
        group_by(state) %>%
        summarise(Number_of_bills = n(),
                  click_response_state = str_c(click_response, collapse = "<br/><br/><br/>"),
                  PRC_citizen_2 = str_c(PRC_citizen_2, collapse = ", "),
                  Resident_status_2 = str_c(Resident_status_2, collapse = ", "),
                  PRC_Companies_Orgs_Entities_2 = str_c(PRC_Companies_Orgs_Entities_2, collapse = ", "), 
                  PRC_Govt_Entities_2 = str_c(PRC_Govt_Entities_2, collapse = ", "), 
                  CCP_Members_Companies_2 = str_c(CCP_Members_Companies_2, collapse = ", "), 
                  Foreign_Govts_Entities_Individuals_2 = str_c(Foreign_Govts_Entities_Individuals_2, collapse = ", "), 
                  Foreign_Adversaries_Including_PRC_2 = str_c(Foreign_Adversaries_Including_PRC_2, collapse = ", "),
                  All_types_2 = str_c(All_types_2, collapse = ", "),
                  Residential_2 = str_c(Residential_2, collapse = ", "),
                  Commercial_or_Not_for_Personal_Use_2 = str_c(Commercial_or_Not_for_Personal_Use_2, collapse = ", "),
                  Agricultural_Natural_Resource_2 = str_c(Agricultural_Natural_Resource_2, collapse = ", "),
                  State_Land_2 = str_c(State_Land_2, collapse = ", "),
                  Near_Federal_Land_Critical_Infrastructure_2 = str_c(Near_Federal_Land_Critical_Infrastructure_2, collapse = ", "),
                  Near_Military_Facilities_2 = str_c(Near_Military_Facilities_2, collapse = ", "),
                  Of_a_Certain_Size_2 = str_c(Of_a_Certain_Size_2, collapse = ", "),
                  Existing_Owners_Must_Sell_Property_2 = str_c(Existing_Owners_Must_Sell_Property_2, collapse = ", "),
                  PRC_citizen = max(PRC_citizen),
                  Resident_status = max(Resident_status),
                  PRC_Companies_Orgs_Entities = max(PRC_Companies_Orgs_Entities), 
                  PRC_Govt_Entities = max(PRC_Govt_Entities), 
                  CCP_Members_Companies = max(CCP_Members_Companies), 
                  Foreign_Govts_Entities_Individuals = max(Foreign_Govts_Entities_Individuals), 
                  Foreign_Adversaries_Including_PRC = max(Foreign_Adversaries_Including_PRC),
                  All_types = max(All_types),
                  Residential = max(Residential),
                  Commercial_or_Not_for_Personal_Use = max(Commercial_or_Not_for_Personal_Use),
                  Agricultural_Natural_Resource = max(Agricultural_Natural_Resource),
                  State_Land = max(State_Land),
                  Near_Federal_Land_Critical_Infrastructure = max(Near_Federal_Land_Critical_Infrastructure),
                  Near_Military_Facilities = max(Near_Military_Facilities),
                  Of_a_Certain_Size = max(Of_a_Certain_Size),
                  Existing_Owners_Must_Sell_Property = max(Existing_Owners_Must_Sell_Property))

# remove excess commas and spaces from summarization of columns
d_bills_states_all %<>% mutate(across(c(PRC_citizen_2, Resident_status_2, PRC_Companies_Orgs_Entities_2, PRC_Govt_Entities_2, CCP_Members_Companies_2, 
                                        Foreign_Govts_Entities_Individuals_2, Foreign_Adversaries_Including_PRC_2, Existing_Owners_Must_Sell_Property_2, 
                                        All_types_2, Residential_2, Commercial_or_Not_for_Personal_Use_2, Agricultural_Natural_Resource_2,
                                          State_Land_2, Near_Federal_Land_Critical_Infrastructure_2, Near_Military_Facilities_2, Of_a_Certain_Size_2), 
                                      ~ str_replace_all(., "^[, ]+|[, ]+$", ""))) %>% # removes leading and trailing commas and spaces
        mutate(across(c(PRC_citizen_2, Resident_status_2, PRC_Companies_Orgs_Entities_2, PRC_Govt_Entities_2, CCP_Members_Companies_2, 
                        Foreign_Govts_Entities_Individuals_2, Foreign_Adversaries_Including_PRC_2, Existing_Owners_Must_Sell_Property_2, 
                        All_types_2, Residential_2, Commercial_or_Not_for_Personal_Use_2, Agricultural_Natural_Resource_2,
                        State_Land_2, Near_Federal_Land_Critical_Infrastructure_2, Near_Military_Facilities_2, Of_a_Certain_Size_2), 
                      ~ str_replace_all(., ",[ ,]*", ", "))) # removes repeated comas between bills
                               
## merge with states df and make new variables ####
load("landlaw_app_prep/map_states.rdata")
d_bills_states_all %<>% full_join(., states, by = c("state" = "state_abb"))

# create state-level variables for map app
d_bills_states_all %<>% 
        ungroup() %>% 
        mutate(ID_1 = 1:51) %>% # ID variable for Shiny polygon identification
        mutate(any_entity = case_when(
                PRC_citizen == 1 |
                        Resident_status == 1 |
                        PRC_Companies_Orgs_Entities == 1 | 
                        PRC_Govt_Entities == 1 | 
                        CCP_Members_Companies == 1 |
                        Foreign_Govts_Entities_Individuals == 1 | 
                        Foreign_Adversaries_Including_PRC == 1 | 
                        Existing_Owners_Must_Sell_Property == 1 ~ 1,
                TRUE ~ 0)) %>% # any entities prohibited from property ownership
         mutate(any_land = case_when(
                All_types == 1  |
                        Commercial_or_Not_for_Personal_Use == 1 |
                        Agricultural_Natural_Resource == 1 |
                        State_Land == 1 |
                        Near_Federal_Land_Critical_Infrastructure == 1 |
                        Near_Military_Facilities == 1 |
                        Of_a_Certain_Size == 1 ~ 1, 
                TRUE ~ 0)) %>% # any property prohibited from ownership
        mutate(map_type = "all")

# Fill in 0s for states without bills and make integers for app
d_bills_states_all %<>% mutate(across(c(Number_of_bills, PRC_citizen, Resident_status, PRC_Companies_Orgs_Entities, PRC_Govt_Entities, CCP_Members_Companies, Foreign_Govts_Entities_Individuals,
                                        Foreign_Adversaries_Including_PRC, Existing_Owners_Must_Sell_Property, All_types, Residential, Commercial_or_Not_for_Personal_Use, Agricultural_Natural_Resource,
                                        State_Land, Near_Federal_Land_Critical_Infrastructure, Near_Military_Facilities, Of_a_Certain_Size, any_entity, any_land),
                                      ~ case_when(
                                              !is.na(.) ~ as.integer(.),
                                              TRUE ~ 0
                                      )))

# change DC -> US and District of Columbia -> US Congress
d_bills_states_all %<>% mutate(state = case_when(
        state == "DC" ~ "US",
        T ~ state)) %>%
        mutate(state_name = case_when(
                state_name == "District of Columbia" ~ "US Congress",
                T ~ state_name
        ))

# create state-level summary variable
d_bills_states_all %<>% mutate(Summary = case_when(
        Number_of_bills > 0 ~
                paste0("<strong>", state_name, "</strong>", "<br/><br/>",
                       "Number of bills considered in 2023 (as of November 29, 2023): ", Number_of_bills, "<br/><br/>",
                       "Entities prohibited from property ownership:", "<br/>",
                       "- PRC citizens: ", ifelse(PRC_citizen == 1, "Yes", "No"), " (", ifelse(PRC_citizen == 1, PRC_citizen_2, ""), ")",  "<br/>",
                       "- Permanent U.S. Residents: ", ifelse(Resident_status == 1, "Yes", "No"), " (", ifelse(Resident_status == 1, Resident_status_2, ""), ")",  "<br/>",
                       "- PRC companies and organizations: ", ifelse(PRC_Companies_Orgs_Entities == 1, "Yes", "No"), " (", ifelse(PRC_Companies_Orgs_Entities == 1, PRC_Companies_Orgs_Entities_2, ""), ")",  "<br/>",
                       "- PRC government and entities: ", ifelse(PRC_Govt_Entities == 1, "Yes", "No"), " (", ifelse(PRC_Govt_Entities == 1, PRC_Govt_Entities_2, ""), ")",  "<br/>",
                       "- CCP members and companies: ", ifelse(CCP_Members_Companies == 1, "Yes", "No"), " (", ifelse(CCP_Members_Companies == 1, CCP_Members_Companies_2, ""), ")",  "<br/>",
                       "- Foreign governments, entities, and individuals: ", ifelse(Foreign_Govts_Entities_Individuals == 1, "Yes", "No"), " (", ifelse(Foreign_Govts_Entities_Individuals == 1, Foreign_Govts_Entities_Individuals_2, ""), ")",  "<br/>",
                       "- Foreign adversaries: ", ifelse(Foreign_Adversaries_Including_PRC == 1, "Yes", "No"), " (", ifelse(Foreign_Adversaries_Including_PRC == 1, Foreign_Adversaries_Including_PRC_2, ""), ")",  "<br/>",
                       "- Existing owners must sell property: ", ifelse(Existing_Owners_Must_Sell_Property == 1, "Yes", "No"), " (", ifelse(Existing_Owners_Must_Sell_Property == 1, Existing_Owners_Must_Sell_Property_2, ""), ")",  "<br/><br/>",
                       "Properties prohibited from ownership:", "<br/>",
                       "- All property: ", ifelse(All_types == 1, "Yes", "No"), " (", ifelse(All_types == 1, All_types_2, ""), ")",  "<br/>",
                       "- Residential: ", ifelse(Residential == 1, "Yes", "No"), " (", ifelse(Residential == 1, Residential_2, ""), ")",  "<br/>",
                       "- Commercial or not for personal residence: ", ifelse(Commercial_or_Not_for_Personal_Use == 1, "Yes", "No"), " (", ifelse(Commercial_or_Not_for_Personal_Use == 1, Commercial_or_Not_for_Personal_Use_2, ""), ")",  "<br/>",
                       "- Agricultural or natural resources: ", ifelse(Agricultural_Natural_Resource == 1, "Yes", "No"), " (", ifelse(Agricultural_Natural_Resource == 1, Agricultural_Natural_Resource_2, ""), ")",  "<br/>",
                       "- State land: ", ifelse(State_Land == 1, "Yes", "No"), " (", ifelse(State_Land == 1, State_Land_2, ""), ")",  "<br/>",
                       "- Near federal land or near critical infrastructure: ", ifelse(Near_Federal_Land_Critical_Infrastructure == 1, "Yes", "No"), " (", ifelse(Near_Federal_Land_Critical_Infrastructure == 1, Near_Federal_Land_Critical_Infrastructure_2, ""), ")",  "<br/>",
                       "- Near military facilities: ", ifelse(Near_Military_Facilities == 1, "Yes", "No"), " (", ifelse(Near_Military_Facilities == 1, Near_Military_Facilities_2, ""), ")",  "<br/>",
                       "- Of a certain size: ", ifelse(Of_a_Certain_Size == 1, "Yes", "No"), " (", ifelse(Of_a_Certain_Size == 1, Of_a_Certain_Size_2, ""), ")",  "<br/><br/>",
                       click_response_state),
        Number_of_bills == 0 ~ paste0("<strong>", state_name, "</strong>", "<br/>", "<br/>", "No bills considered in 2023 (as of November 29, 2023)"))) %>% 
        mutate(across(Summary, ~str_replace_all(Summary, "\\(\\)", ""))) # clean () after "No"



# CONSIDERING bills ####
# create bill-level summary variable
d_bills_considering %<>% mutate(click_response = paste0("<u>", number, "</u>", ": ", title,  "<br/>",
                                                "<u>", "Description", "</u>", ": ",  description, "<br/>",
                                                "<u>", "Status", "</u>", ": ", status, "<br/>",
                                                "<u>", "Last action and date", "</u>", ": ", last_action, " (", last_action_date, ")", "<br/>",
                                                "<a href=", state_link, " target=\"_blank\" rel=\"noopener noreferrer\"", ">Full bill text</a>"))

# produce columns for identifying bill provisions in state-level summary
prohibit_provisions <- c("PRC_citizen", "Resident_status", "PRC_Companies_Orgs_Entities", "PRC_Govt_Entities", "CCP_Members_Companies", "Foreign_Govts_Entities_Individuals",
                         "Foreign_Adversaries_Including_PRC", "Existing_Owners_Must_Sell_Property", "All_types", "Residential", "Commercial_or_Not_for_Personal_Use", "Agricultural_Natural_Resource",
                         "State_Land", "Near_Federal_Land_Critical_Infrastructure", "Near_Military_Facilities", "Of_a_Certain_Size")

for (i in prohibit_provisions) {
        
        new_col <- paste0(i, "_2")
        
        old_col <- sym(i)
        
        d_bills_considering %<>% mutate(!!new_col := case_when(
                !!old_col == 1 ~ number,
                TRUE ~ ""
        ))}

## make df state-level ####
# collapse df so that each state has its own row and concatenate bill names
d_bills_states_considering <- d_bills_considering %>%
        group_by(state) %>%
        summarise(Number_of_bills = n(),
                  click_response_state = str_c(click_response, collapse = "<br/><br/><br/>"),
                  PRC_citizen_2 = str_c(PRC_citizen_2, collapse = ", "),
                  Resident_status_2 = str_c(Resident_status_2, collapse = ", "),
                  PRC_Companies_Orgs_Entities_2 = str_c(PRC_Companies_Orgs_Entities_2, collapse = ", "), 
                  PRC_Govt_Entities_2 = str_c(PRC_Govt_Entities_2, collapse = ", "), 
                  CCP_Members_Companies_2 = str_c(CCP_Members_Companies_2, collapse = ", "), 
                  Foreign_Govts_Entities_Individuals_2 = str_c(Foreign_Govts_Entities_Individuals_2, collapse = ", "), 
                  Foreign_Adversaries_Including_PRC_2 = str_c(Foreign_Adversaries_Including_PRC_2, collapse = ", "),
                  All_types_2 = str_c(All_types_2, collapse = ", "),
                  Residential_2 = str_c(Residential_2, collapse = ", "),
                  Commercial_or_Not_for_Personal_Use_2 = str_c(Commercial_or_Not_for_Personal_Use_2, collapse = ", "),
                  Agricultural_Natural_Resource_2 = str_c(Agricultural_Natural_Resource_2, collapse = ", "),
                  State_Land_2 = str_c(State_Land_2, collapse = ", "),
                  Near_Federal_Land_Critical_Infrastructure_2 = str_c(Near_Federal_Land_Critical_Infrastructure_2, collapse = ", "),
                  Near_Military_Facilities_2 = str_c(Near_Military_Facilities_2, collapse = ", "),
                  Of_a_Certain_Size_2 = str_c(Of_a_Certain_Size_2, collapse = ", "),
                  Existing_Owners_Must_Sell_Property_2 = str_c(Existing_Owners_Must_Sell_Property_2, collapse = ", "),
                  PRC_citizen = max(PRC_citizen),
                  Resident_status = max(Resident_status),
                  PRC_Companies_Orgs_Entities = max(PRC_Companies_Orgs_Entities), 
                  PRC_Govt_Entities = max(PRC_Govt_Entities), 
                  CCP_Members_Companies = max(CCP_Members_Companies), 
                  Foreign_Govts_Entities_Individuals = max(Foreign_Govts_Entities_Individuals), 
                  Foreign_Adversaries_Including_PRC = max(Foreign_Adversaries_Including_PRC),
                  All_types = max(All_types),
                  Residential = max(Residential),
                  Commercial_or_Not_for_Personal_Use = max(Commercial_or_Not_for_Personal_Use),
                  Agricultural_Natural_Resource = max(Agricultural_Natural_Resource),
                  State_Land = max(State_Land),
                  Near_Federal_Land_Critical_Infrastructure = max(Near_Federal_Land_Critical_Infrastructure),
                  Near_Military_Facilities = max(Near_Military_Facilities),
                  Of_a_Certain_Size = max(Of_a_Certain_Size),
                  Existing_Owners_Must_Sell_Property = max(Existing_Owners_Must_Sell_Property))

# remove excess commas and spaces from summarization of columns
d_bills_states_considering %<>% mutate(across(c(PRC_citizen_2, Resident_status_2, PRC_Companies_Orgs_Entities_2, PRC_Govt_Entities_2, CCP_Members_Companies_2, 
                                        Foreign_Govts_Entities_Individuals_2, Foreign_Adversaries_Including_PRC_2, Existing_Owners_Must_Sell_Property_2, 
                                        All_types_2, Residential_2, Commercial_or_Not_for_Personal_Use_2, Agricultural_Natural_Resource_2,
                                        State_Land_2, Near_Federal_Land_Critical_Infrastructure_2, Near_Military_Facilities_2, Of_a_Certain_Size_2), 
                                      ~ str_replace_all(., "^[, ]+|[, ]+$", ""))) %>% # removes leading and trailing commas and spaces
        mutate(across(c(PRC_citizen_2, Resident_status_2, PRC_Companies_Orgs_Entities_2, PRC_Govt_Entities_2, CCP_Members_Companies_2, 
                        Foreign_Govts_Entities_Individuals_2, Foreign_Adversaries_Including_PRC_2, Existing_Owners_Must_Sell_Property_2, 
                        All_types_2, Residential_2, Commercial_or_Not_for_Personal_Use_2, Agricultural_Natural_Resource_2,
                        State_Land_2, Near_Federal_Land_Critical_Infrastructure_2, Near_Military_Facilities_2, Of_a_Certain_Size_2), 
                      ~ str_replace_all(., ",[ ,]*", ", "))) # removes repeated commas between bills

## merge with states df and make new variables ####
d_bills_states_considering %<>% full_join(., states, by = c("state" = "state_abb"))

# create state-level variables for map app
d_bills_states_considering %<>% 
        ungroup() %>% 
        mutate(ID_1 = 1:51) %>% # ID variable for Shiny polygon identification
        mutate(any_entity = case_when(
                PRC_citizen == 1 |
                        Resident_status == 1 |
                        PRC_Companies_Orgs_Entities == 1 | 
                        PRC_Govt_Entities == 1 | 
                        CCP_Members_Companies == 1 |
                        Foreign_Govts_Entities_Individuals == 1 | 
                        Foreign_Adversaries_Including_PRC == 1 | 
                        Existing_Owners_Must_Sell_Property == 1 ~ 1,
                TRUE ~ 0)) %>% # any entities prohibited from property ownership
        mutate(any_land = case_when(
                All_types == 1  |
                        Commercial_or_Not_for_Personal_Use == 1 |
                        Agricultural_Natural_Resource == 1 |
                        State_Land == 1 |
                        Near_Federal_Land_Critical_Infrastructure == 1 |
                        Near_Military_Facilities == 1 |
                        Of_a_Certain_Size == 1 ~ 1, 
                TRUE ~ 0)) %>% # any property prohibited from ownership
        mutate(map_type = "considering")

# Fill in 0s for states without bills and make integers for app
d_bills_states_considering %<>% mutate(across(c(Number_of_bills, PRC_citizen, Resident_status, PRC_Companies_Orgs_Entities, PRC_Govt_Entities, CCP_Members_Companies, Foreign_Govts_Entities_Individuals,
                                        Foreign_Adversaries_Including_PRC, Existing_Owners_Must_Sell_Property, All_types, Residential, Commercial_or_Not_for_Personal_Use, Agricultural_Natural_Resource,
                                        State_Land, Near_Federal_Land_Critical_Infrastructure, Near_Military_Facilities, Of_a_Certain_Size, any_entity, any_land),
                                      ~ case_when(
                                              !is.na(.) ~ as.integer(.),
                                              TRUE ~ 0
                                      )))

# change DC -> US and District of Columbia -> US Congress
d_bills_states_considering %<>% mutate(state = case_when(
        state == "DC" ~ "US",
        T ~ state)) %>%
        mutate(state_name = case_when(
                state_name == "District of Columbia" ~ "US Congress",
                T ~ state_name
        ))

# create state-level summary variable #
d_bills_states_considering %<>% mutate(Summary = case_when(
        Number_of_bills > 0 ~
                paste0("<strong>", state_name, "</strong>", "<br/><br/>",
                       "Number of bills currently under consideration in 2023 (as of November 29, 2023): ", Number_of_bills, "<br/><br/>",
                       "Entities prohibited from property ownership:", "<br/>",
                       "- PRC citizens: ", ifelse(PRC_citizen == 1, "Yes", "No"), " (", ifelse(PRC_citizen == 1, PRC_citizen_2, ""), ")",  "<br/>",
                       "- Permanent U.S. Residents: ", ifelse(Resident_status == 1, "Yes", "No"), " (", ifelse(Resident_status == 1, Resident_status_2, ""), ")",  "<br/>",
                       "- PRC companies and organizations: ", ifelse(PRC_Companies_Orgs_Entities == 1, "Yes", "No"), " (", ifelse(PRC_Companies_Orgs_Entities == 1, PRC_Companies_Orgs_Entities_2, ""), ")",  "<br/>",
                       "- PRC government and entities: ", ifelse(PRC_Govt_Entities == 1, "Yes", "No"), " (", ifelse(PRC_Govt_Entities == 1, PRC_Govt_Entities_2, ""), ")",  "<br/>",
                       "- CCP members and companies: ", ifelse(CCP_Members_Companies == 1, "Yes", "No"), " (", ifelse(CCP_Members_Companies == 1, CCP_Members_Companies_2, ""), ")",  "<br/>",
                       "- Foreign governments, entities, and individuals: ", ifelse(Foreign_Govts_Entities_Individuals == 1, "Yes", "No"), " (", ifelse(Foreign_Govts_Entities_Individuals == 1, Foreign_Govts_Entities_Individuals_2, ""), ")",  "<br/>",
                       "- Foreign adversaries: ", ifelse(Foreign_Adversaries_Including_PRC == 1, "Yes", "No"), " (", ifelse(Foreign_Adversaries_Including_PRC == 1, Foreign_Adversaries_Including_PRC_2, ""), ")",  "<br/>",
                       "- Existing owners must sell property: ", ifelse(Existing_Owners_Must_Sell_Property == 1, "Yes", "No"), " (", ifelse(Existing_Owners_Must_Sell_Property == 1, Existing_Owners_Must_Sell_Property_2, ""), ")",  "<br/><br/>",
                       "Properties prohibited from ownership:", "<br/>",
                       "- All property: ", ifelse(All_types == 1, "Yes", "No"), " (", ifelse(All_types == 1, All_types_2, ""), ")",  "<br/>",
                       "- Residential: ", ifelse(Residential == 1, "Yes", "No"), " (", ifelse(Residential == 1, Residential_2, ""), ")",  "<br/>",
                       "- Commercial or not for personal residence: ", ifelse(Commercial_or_Not_for_Personal_Use == 1, "Yes", "No"), " (", ifelse(Commercial_or_Not_for_Personal_Use == 1, Commercial_or_Not_for_Personal_Use_2, ""), ")",  "<br/>",
                       "- Agricultural or natural resources: ", ifelse(Agricultural_Natural_Resource == 1, "Yes", "No"), " (", ifelse(Agricultural_Natural_Resource == 1, Agricultural_Natural_Resource_2, ""), ")",  "<br/>",
                       "- State land: ", ifelse(State_Land == 1, "Yes", "No"), " (", ifelse(State_Land == 1, State_Land_2, ""), ")",  "<br/>",
                       "- Near federal land or near critical infrastructure: ", ifelse(Near_Federal_Land_Critical_Infrastructure == 1, "Yes", "No"), " (", ifelse(Near_Federal_Land_Critical_Infrastructure == 1, Near_Federal_Land_Critical_Infrastructure_2, ""), ")",  "<br/>",
                       "- Near military facilities: ", ifelse(Near_Military_Facilities == 1, "Yes", "No"), " (", ifelse(Near_Military_Facilities == 1, Near_Military_Facilities_2, ""), ")",  "<br/>",
                       "- Of a certain size: ", ifelse(Of_a_Certain_Size == 1, "Yes", "No"), " (", ifelse(Of_a_Certain_Size == 1, Of_a_Certain_Size_2, ""), ")",  "<br/><br/>",
                       click_response_state),
        Number_of_bills == 0 ~ paste0("<strong>", state_name, "</strong>", "<br/>", "<br/>", "No bills currently under consideration in 2023 (as of November 29, 2023)"))) %>% 
        mutate(across(Summary, ~str_replace_all(Summary, "\\(\\)", ""))) # clean () after "No"

# PASSED bills ####
# create bill-level summary variable
d_bills_passed %<>% mutate(click_response = paste0("<u>", number, "</u>", ": ", title,  "<br/>",
                                                "<u>", "Description", "</u>", ": ",  description, "<br/>",
                                                "<u>", "Status", "</u>", ": ", status, "<br/>",
                                                "<u>", "Last action and date", "</u>", ": ", last_action, " (", last_action_date, ")", "<br/>",
                                                "<a href=", state_link, " target=\"_blank\" rel=\"noopener noreferrer\"", ">Full bill text</a>"))

# produce columns for identifying bill provisions in state-level summary
prohibit_provisions <- c("PRC_citizen", "Resident_status", "PRC_Companies_Orgs_Entities", "PRC_Govt_Entities", "CCP_Members_Companies", "Foreign_Govts_Entities_Individuals",
                         "Foreign_Adversaries_Including_PRC", "Existing_Owners_Must_Sell_Property", "All_types", "Residential", "Commercial_or_Not_for_Personal_Use", "Agricultural_Natural_Resource",
                         "State_Land", "Near_Federal_Land_Critical_Infrastructure", "Near_Military_Facilities", "Of_a_Certain_Size")

for (i in prohibit_provisions) {
        
        new_col <- paste0(i, "_2")
        
        old_col <- sym(i)
        
        d_bills_passed %<>% mutate(!!new_col := case_when(
                !!old_col == 1 ~ number,
                TRUE ~ ""
        ))}

## make df state-level ####
# collapse df so that each state has its own row and concatenate bill names
d_bills_states_passed <- d_bills_passed %>%
        group_by(state) %>%
        summarise(Number_of_bills = n(),
                  click_response_state = str_c(click_response, collapse = "<br/><br/><br/>"),
                  PRC_citizen_2 = str_c(PRC_citizen_2, collapse = ", "),
                  Resident_status_2 = str_c(Resident_status_2, collapse = ", "),
                  PRC_Companies_Orgs_Entities_2 = str_c(PRC_Companies_Orgs_Entities_2, collapse = ", "), 
                  PRC_Govt_Entities_2 = str_c(PRC_Govt_Entities_2, collapse = ", "), 
                  CCP_Members_Companies_2 = str_c(CCP_Members_Companies_2, collapse = ", "), 
                  Foreign_Govts_Entities_Individuals_2 = str_c(Foreign_Govts_Entities_Individuals_2, collapse = ", "), 
                  Foreign_Adversaries_Including_PRC_2 = str_c(Foreign_Adversaries_Including_PRC_2, collapse = ", "),
                  All_types_2 = str_c(All_types_2, collapse = ", "),
                  Residential_2 = str_c(Residential_2, collapse = ", "),
                  Commercial_or_Not_for_Personal_Use_2 = str_c(Commercial_or_Not_for_Personal_Use_2, collapse = ", "),
                  Agricultural_Natural_Resource_2 = str_c(Agricultural_Natural_Resource_2, collapse = ", "),
                  State_Land_2 = str_c(State_Land_2, collapse = ", "),
                  Near_Federal_Land_Critical_Infrastructure_2 = str_c(Near_Federal_Land_Critical_Infrastructure_2, collapse = ", "),
                  Near_Military_Facilities_2 = str_c(Near_Military_Facilities_2, collapse = ", "),
                  Of_a_Certain_Size_2 = str_c(Of_a_Certain_Size_2, collapse = ", "),
                  Existing_Owners_Must_Sell_Property_2 = str_c(Existing_Owners_Must_Sell_Property_2, collapse = ", "),
                  PRC_citizen = max(PRC_citizen),
                  Resident_status = max(Resident_status),
                  PRC_Companies_Orgs_Entities = max(PRC_Companies_Orgs_Entities), 
                  PRC_Govt_Entities = max(PRC_Govt_Entities), 
                  CCP_Members_Companies = max(CCP_Members_Companies), 
                  Foreign_Govts_Entities_Individuals = max(Foreign_Govts_Entities_Individuals), 
                  Foreign_Adversaries_Including_PRC = max(Foreign_Adversaries_Including_PRC),
                  All_types = max(All_types),
                  Residential = max(Residential),
                  Commercial_or_Not_for_Personal_Use = max(Commercial_or_Not_for_Personal_Use),
                  Agricultural_Natural_Resource = max(Agricultural_Natural_Resource),
                  State_Land = max(State_Land),
                  Near_Federal_Land_Critical_Infrastructure = max(Near_Federal_Land_Critical_Infrastructure),
                  Near_Military_Facilities = max(Near_Military_Facilities),
                  Of_a_Certain_Size = max(Of_a_Certain_Size),
                  Existing_Owners_Must_Sell_Property = max(Existing_Owners_Must_Sell_Property))

# remove excess commas and spaces from summarization of columns
d_bills_states_passed %<>% mutate(across(c(PRC_citizen_2, Resident_status_2, PRC_Companies_Orgs_Entities_2, PRC_Govt_Entities_2, CCP_Members_Companies_2, 
                                        Foreign_Govts_Entities_Individuals_2, Foreign_Adversaries_Including_PRC_2, Existing_Owners_Must_Sell_Property_2, 
                                        All_types_2, Residential_2, Commercial_or_Not_for_Personal_Use_2, Agricultural_Natural_Resource_2,
                                        State_Land_2, Near_Federal_Land_Critical_Infrastructure_2, Near_Military_Facilities_2, Of_a_Certain_Size_2), 
                                      ~ str_replace_all(., "^[, ]+|[, ]+$", ""))) %>% # removes leading and trailing commas and spaces
        mutate(across(c(PRC_citizen_2, Resident_status_2, PRC_Companies_Orgs_Entities_2, PRC_Govt_Entities_2, CCP_Members_Companies_2, 
                        Foreign_Govts_Entities_Individuals_2, Foreign_Adversaries_Including_PRC_2, Existing_Owners_Must_Sell_Property_2, 
                        All_types_2, Residential_2, Commercial_or_Not_for_Personal_Use_2, Agricultural_Natural_Resource_2,
                        State_Land_2, Near_Federal_Land_Critical_Infrastructure_2, Near_Military_Facilities_2, Of_a_Certain_Size_2), 
                      ~ str_replace_all(., ",[ ,]*", ", "))) # removes repeated commas between bills

## merge with states df and make new variables ####
d_bills_states_passed %<>% full_join(., states, by = c("state" = "state_abb"))

# create state-level variables for map app
d_bills_states_passed %<>% 
        ungroup() %>% 
        mutate(ID_1 = 1:51) %>% # ID variable for Shiny polygon identification
        mutate(any_entity = case_when(
                PRC_citizen == 1 |
                        Resident_status == 1 |
                        PRC_Companies_Orgs_Entities == 1 | 
                        PRC_Govt_Entities == 1 | 
                        CCP_Members_Companies == 1 |
                        Foreign_Govts_Entities_Individuals == 1 | 
                        Foreign_Adversaries_Including_PRC == 1 | 
                        Existing_Owners_Must_Sell_Property == 1 ~ 1,
                TRUE ~ 0)) %>% # any entities prohibited from property ownership
        mutate(any_land = case_when(
                All_types == 1  |
                        Commercial_or_Not_for_Personal_Use == 1 |
                        Agricultural_Natural_Resource == 1 |
                        State_Land == 1 |
                        Near_Federal_Land_Critical_Infrastructure == 1 |
                        Near_Military_Facilities == 1 |
                        Of_a_Certain_Size == 1 ~ 1, 
                TRUE ~ 0)) %>% # any property prohibited from ownership
        mutate(map_type = "passed")

# Fill in 0s for states without bills and make integers for app
d_bills_states_passed %<>% mutate(across(c(Number_of_bills, PRC_citizen, Resident_status, PRC_Companies_Orgs_Entities, PRC_Govt_Entities, CCP_Members_Companies, Foreign_Govts_Entities_Individuals,
                                        Foreign_Adversaries_Including_PRC, Existing_Owners_Must_Sell_Property, All_types, Residential, Commercial_or_Not_for_Personal_Use, Agricultural_Natural_Resource,
                                        State_Land, Near_Federal_Land_Critical_Infrastructure, Near_Military_Facilities, Of_a_Certain_Size, any_entity, any_land),
                                      ~ case_when(
                                              !is.na(.) ~ as.integer(.),
                                              TRUE ~ 0
                                      )))

# change DC -> US and District of Columbia -> US Congress
d_bills_states_passed %<>% mutate(state = case_when(
        state == "DC" ~ "US",
        T ~ state)) %>%
        mutate(state_name = case_when(
                state_name == "District of Columbia" ~ "US Congress",
                T ~ state_name
        ))

# create state-level summary variable
d_bills_states_passed %<>% mutate(Summary = case_when(
        Number_of_bills > 0 ~
                paste0("<strong>", state_name, "</strong>", "<br/><br/>",
                       "Number of bills passed in 2023 (as of November 29, 2023): ", Number_of_bills, "<br/><br/>",
                       "Entities prohibited from property ownership:", "<br/>",
                       "- PRC citizens: ", ifelse(PRC_citizen == 1, "Yes", "No"), " (", ifelse(PRC_citizen == 1, PRC_citizen_2, ""), ")",  "<br/>",
                       "- Permanent U.S. Residents: ", ifelse(Resident_status == 1, "Yes", "No"), " (", ifelse(Resident_status == 1, Resident_status_2, ""), ")",  "<br/>",
                       "- PRC companies and organizations: ", ifelse(PRC_Companies_Orgs_Entities == 1, "Yes", "No"), " (", ifelse(PRC_Companies_Orgs_Entities == 1, PRC_Companies_Orgs_Entities_2, ""), ")",  "<br/>",
                       "- PRC government and entities: ", ifelse(PRC_Govt_Entities == 1, "Yes", "No"), " (", ifelse(PRC_Govt_Entities == 1, PRC_Govt_Entities_2, ""), ")",  "<br/>",
                       "- CCP members and companies: ", ifelse(CCP_Members_Companies == 1, "Yes", "No"), " (", ifelse(CCP_Members_Companies == 1, CCP_Members_Companies_2, ""), ")",  "<br/>",
                       "- Foreign governments, entities, and individuals: ", ifelse(Foreign_Govts_Entities_Individuals == 1, "Yes", "No"), " (", ifelse(Foreign_Govts_Entities_Individuals == 1, Foreign_Govts_Entities_Individuals_2, ""), ")",  "<br/>",
                       "- Foreign adversaries: ", ifelse(Foreign_Adversaries_Including_PRC == 1, "Yes", "No"), " (", ifelse(Foreign_Adversaries_Including_PRC == 1, Foreign_Adversaries_Including_PRC_2, ""), ")",  "<br/>",
                       "- Existing owners must sell property: ", ifelse(Existing_Owners_Must_Sell_Property == 1, "Yes", "No"), " (", ifelse(Existing_Owners_Must_Sell_Property == 1, Existing_Owners_Must_Sell_Property_2, ""), ")",  "<br/><br/>",
                       "Properties prohibited from ownership:", "<br/>",
                       "- All property: ", ifelse(All_types == 1, "Yes", "No"), " (", ifelse(All_types == 1, All_types_2, ""), ")",  "<br/>",
                       "- Residential: ", ifelse(Residential == 1, "Yes", "No"), " (", ifelse(Residential == 1, Residential_2, ""), ")",  "<br/>",
                       "- Commercial or not for personal residence: ", ifelse(Commercial_or_Not_for_Personal_Use == 1, "Yes", "No"), " (", ifelse(Commercial_or_Not_for_Personal_Use == 1, Commercial_or_Not_for_Personal_Use_2, ""), ")",  "<br/>",
                       "- Agricultural or natural resources: ", ifelse(Agricultural_Natural_Resource == 1, "Yes", "No"), " (", ifelse(Agricultural_Natural_Resource == 1, Agricultural_Natural_Resource_2, ""), ")",  "<br/>",
                       "- State land: ", ifelse(State_Land == 1, "Yes", "No"), " (", ifelse(State_Land == 1, State_Land_2, ""), ")",  "<br/>",
                       "- Near federal land or near critical infrastructure: ", ifelse(Near_Federal_Land_Critical_Infrastructure == 1, "Yes", "No"), " (", ifelse(Near_Federal_Land_Critical_Infrastructure == 1, Near_Federal_Land_Critical_Infrastructure_2, ""), ")",  "<br/>",
                       "- Near military facilities: ", ifelse(Near_Military_Facilities == 1, "Yes", "No"), " (", ifelse(Near_Military_Facilities == 1, Near_Military_Facilities_2, ""), ")",  "<br/>",
                       "- Of a certain size: ", ifelse(Of_a_Certain_Size == 1, "Yes", "No"), " (", ifelse(Of_a_Certain_Size == 1, Of_a_Certain_Size_2, ""), ")",  "<br/><br/>",
                       click_response_state),
        Number_of_bills == 0 ~ paste0("<strong>", state_name, "</strong>", "<br/>", "<br/>", "No bills passed in 2023 (as of November 29, 2023)"))) %>% 
        mutate(across(Summary, ~str_replace_all(Summary, "\\(\\)", ""))) # clean () after "No"

# merge dfs, make into sf-df, and save ####
d_bills_states <- rbind(d_bills_states_all, d_bills_states_considering, d_bills_states_passed)

d_bills_states_sf <- d_bills_states %>% 
        st_as_sf(crs = 4326) %>% 
        shift_geometry() %>%
        st_transform('+proj=longlat +datum=WGS84')

save(d_bills_states_sf, file = "landlaw_app/d_bills_states_sf.rdata")
