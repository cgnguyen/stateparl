#---------------------Comment and Introduction---------------------------------
#Code to Prepare StateParl data for analysis in Quanteda
#Requires considerable amount of RAM and Time, so transformed objects are saved for reanalysis
#Raw data found at https://search.gesis.org/research_data/SDN-10.7802-2854
#DOI: 10.7802/2854 | 10.7802/2744
--------------------------------------------------------------------------------

#Setup--------------------------------------------------------------------------
##Libraries----

 
library(tidyverse)
library(readr)
library(lubridate) # Dates
library(quanteda) #Data 
library(ggformula) #
library(data.table) #Faster Data Handling
library(dtplyr) #Tidyverse for Data Table
library(readxl)

##Read in GESIS dataset--------------------------------------------------------
#Adjust file path as necessary, 
corpus<-read_rds("paragraphs.rds")



##Turn into data table for faster speed in adjustments-------------------------
corpus_dt <- as.data.table(corpus)

#Prepare Quanteda Corpus------------
##Add and clean variables------------
corpus_dt[, `:=`(
  # Convert to factors 
  speaker = as.factor(speaker_extracted),
  affiliation = as.factor(affiliation),
  state = as.factor(state),
  # Extract year and month from date and add year_month variable
  year = year(date),
  month = month(date),
  # Clean content - replace patterns
  content_cleaned = str_replace_all(
    str_replace_all(content, "Artikel�(\\d+)", "Artikel§\\1"), 
    "�", 
    " "
  )
)]
##Add a year month variable for more fine-grained analysis----------------------
corpus_dt[, year_month := fifelse(is.na(year) | is.na(month), 
                                  NA_character_, 
                                  paste(year, sprintf("%02d", month), sep = "-"))]

##Clean superflous data and arrange by state,date,and sequence in protocol 
corpus_dt<-corpus_dt %>%
  arrange(state,date,sequence_number)%>%
  dplyr::select(-content) 

#Optional Additions-------------------------------------------------------------
#This provides code on how to merge in additional information.
#This can take a while, so it is commented out. Use only if necessary
#This is beta code/data. Please let me know if you wish to have access to the raw data

##Gov Speaker Affiliation---------------------------------------------------------
# reg_people<-read_excel("./aux_data/regierungsmitglieder_stateparl_2000-2023.xlsx")
# reg_people <- as.data.table(reg_people)
# 
# reg_people[, `:=`(
#   Land_lower = tolower(Land),
#   speaker_clean = tolower(trimws(speaker_extracted)),
#   Partei=tolower(Partei)
# )]
# 
### Clean corpus data for matching ----
# corpus_dt[, `:=`(
#   state_lower = tolower(state),
#   speaker_clean = tolower(trimws(speaker_extracted))
# )]
# 
### Set keys for efficient joining ----
# setkey(reg_people, Land_lower, Legislaturperiode, speaker_clean)
# setkey(corpus_dt, state_lower, period, speaker_clean)
# 
### Initialize party_affiliation with existing affiliation ----
# corpus_dt[, party_affiliation := as.character(affiliation)]
# 
# ###Update party_affiliation for government speakers ----
# # Only update when affiliation is "gov" and matched in reg_people
# corpus_dt[reg_people,
#           party_affiliation := fifelse(affiliation == "gov", i.Partei, party_affiliation),
#           on = .(state_lower == Land_lower,
#                  period == Legislaturperiode,
#                  speaker_extracted)]
# 
# ##Add Government/Opposition Party for each affiliation--------------------
# 
# ### Load and prepare government composition data ----
# reg_data <- read_excel("./aux_data/regierungsparteien_stateparl_2000-2023.xlsx")
# 
# # Convert to data.table and optimize date columns
# reg_dt <- as.data.table(reg_data)
# reg_dt[, `:=`(
#   start = as.Date(start),
#   end = as.Date(end),
#   state_lower = tolower(Land))]
# 
# 
# # Reshape to long format for coalition parties
# gov_composition_long <- melt(
#   reg_dt,
#   id.vars = c("Land", "start", "end"),
#   measure.vars = c("reg_1", "reg_2", "reg_3"),
#   variable.name = "coalition_position",
#   value.name = "party",
#   na.rm = TRUE
# )
# #Set to government variable
# gov_composition_long$gov<-1
# 
# # Prepare party affiliation for matching
# gov_composition_long[, party_affiliation := tolower(party)]
# 
# gov_composition_long<-
#   gov_composition_long%>%
#     dplyr::rename(state=Land)%>%
#     dplyr::select(state,start,end,party_affiliation,gov)
# 
# corpus_with_gov <- gov_composition_long[
#   corpus_dt,
#   on = .(state,
#          party_affiliation,
#          start <= date,
#          end >= date),
#   allow.cartesian = TRUE
# ]
# 
# #Set gov to 0 when not in gov. Should also add code to exclude smaller parties if necessary
# corpus_with_gov[is.na(gov), gov := 0L]
# 
# corpus_with_gov<-corpus_dt%>%
#   arrange(state,date,sequence_number)

#Create Quanteda Corpus---------------------------------------------------------


##Turn into quanteda corpus-----------------------------------------------------
corpus_quanteda<-corpus(corpus_dt, text_field="content_cleaned")

##Backup corpus for further analysis--------------------------------------------
write_rds(corpus_quanteda,file="stateparl_pvs.rds")

#CAREFUL: Remove data to clean memory------------------------------------------
corpus_dt<-NULL
corpus<-NULL
gc()






