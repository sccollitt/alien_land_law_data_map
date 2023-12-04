rm(list=ls(all=TRUE))

library(tidyverse)
library(jsonlite)
library(httr)
library(readxl)
library(magrittr)
library(htm2txt)
library(RCurl)
library(legiscanrr) # necessary for decoding bill text
library(uchardet)
library(pdftools)
library(readr)
library(vctrs)

# load data ####
# load coded bills
d_xl <- read_xlsx("legiscan_api/landlaw_bills_coded.xlsx")

# load legiscan api key
legiscan_key <- read_file("legiscan_api/legiscan_api_key.txt")

# set legiscan monitor list (only if have bill_ids) ####
# print list of bill ids to append to URL to set monitor list (only use if have removed relevant bills from monitor list and have bill IDs)
# cat(paste0(bill_id_vector), sep = ",")

# download monitor list ####
url_json <- paste0("https://api.legiscan.com/?key=", legiscan_key, "&op=getMonitorList") # defaults to current session monitored list
raw_json <- httr::GET(url_json) %>%
        httr::content()
bills <- pluck(raw_json, "monitorlist") %>%
        enframe() %>%
        unnest_wider(value)

# create column by which to merge monitor list and coded spreadsheet
bills %<>% mutate(merge_col = paste0(state, number))

# unlist nested items in monitor list
bills %<>% mutate(across(everything(), ~ unlist(.))) 

# download doc_id ####
# the text of bills can only be downloaded with doc_id, so need intermediary step to obtain doc_id using bill_id

# get doc_id for bills
bills_monitor1 <- pluck(bills, "bill_id")

# getBill is just needed to get the doc_id for each bill to run get_bill_text from legiscanrr

# perform download loop
bills_id <- data.frame()
for(i in bills_monitor1) {
        url_json <- paste0("https://api.legiscan.com/?key=", legiscan_key, "&op=getBill&id=",
                           as.character(i))
        raw_json <- httr::GET(url_json) %>% 
                httr::content()
        ex_output <- pluck(raw_json, "bill") 
        ex_output2 <- data.frame(bill_id = ex_output$bill_id, doc_id = ex_output$texts[[1]]$doc_id)
        bills_id <- rbind(bills_id, ex_output2)
} 

# remove any duplicates
bills_id %<>% distinct(.keep_all = T)

# add doc_id column to bills
bills <- left_join(bills, bills_id)

# use doc_id to get bill text
doc_id_vector <-  bills  %>%
        pluck("doc_id")

# download and decode bill text ####
# get bill text using doc_id

# perform download loop
bills_text <- data.frame()
bills_text_list <- list()
for(i in doc_id_vector) {
        downloaded <- get_bill_text(i, api_key = legiscan_key)
        bills_text_list[[i]] <- downloaded 
}

# remove rows with no bill text
bills_text_list %<>% list_drop_empty()

# decode bill text (imperfect - will get errors but still cleanest option)
bills_text <- lapply(bills_text_list, FUN = decode_bill_text)

# merge and clean dataframe ####
# turn into df
bt_df <- do.call(rbind.data.frame, bills_text)
bt_df %<>% mutate(across(everything(), ~ unlist(.))) 

# merge coded list, monitor list (with description), and bill text list (with bill text)
d <- bt_df %>% left_join(., bills, by = "bill_id")
d <- d %>% left_join(., d_xl, by = "merge_col")

# drop duplicate columns
d <- d[,!grepl("\\.y$", colnames(d))]
d <- d[,!grepl("\\.x$", colnames(d))]

# save file
d_bills <- d
save(d_bills, file = "landlaw_app_prep/d_bills.rdata")


