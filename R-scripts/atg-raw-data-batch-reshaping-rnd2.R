library(fs)
library(optparse)
library(dplyr)
library(readr)
library(readxl)
library(stringr)
library(tidyr)

#### SCRIPT PARAMETERS ####

# define default parameters for use in defining script parameters with OPTPARSE
file_path <- "C:/Users/glandry/Desktop/Brierley Analytics/Ad Hoc/ATG/tier-analysis/output/batches"

output_file_path <- "C:/Users/glandry/Desktop/Brierley Analytics/Ad Hoc/ATG/tier-analysis/output/atg-rnd4-reshaped.csv"

# define parameters for reading in target file for batching, filename for writing batched files, and number of batches
script_parameters <- list(make_option(c("-f", "--filePath"), 
                                      type = "character", 
                                      default = file_path,
                                      help = "path to root folder where batch files reside"),
                          make_option(c("-b", "--outputPath"),
                                      type = "character",
                                      default = output_file_path,
                                      help = "full filepath/name for writing batched files (to single file) after processing"))

# instantiate OptionParser object with script_parameters object
param_parser <- OptionParser(option_list = script_parameters)

# parse command line arguments
params <- parse_args(param_parser)

# print to console if no arguments are provided by user and so default parameters used
if (params$filePath == file_path) {
  cat(paste("\n", "WARN ===> No filepath provided by user, running with default filepath:",
            params$filePath, "\n", sep = "\n"))
} else {
  cat(paste("\n", "INFO ===> Reading in batch files located at following root directory:",
            params$filePath, "\n", sep = "\n"))
}

if (params$outputPath == output_file_path) {
  cat(paste("\n", "WARN ===> No filepath for batch output provided by user, running with default filepath:",
            params$outputPath, "\n", sep = "\n"))
} else {
  cat(paste("\n", "INFO ===> Using filepath and name below for writing batch files back into single file (after processing):",
            params$outputPath, "\n", sep = "\n"))
}


#### FUNCTION DEFINITIONS ####

# define function which concats substrings to identify the year for that cell value
yearFunc <- function(x) {
  return(paste0("20", str_sub(x, -2, -1)))
}

# define function which strips away year suffix from field names in order to perform reshape (spread)
keyStrip <- function(x) {
  return(str_sub(x, 1, - 4))
}

# strip the suffix added to original ID in order to perform reshape (spread)
idStrip <- function(x) {
  return(str_sub(x, 1, -3))
}


#### DATA INPUT ####

# return list of batch files in target folder
batch_files <- as.character(dir_ls(params$filePath, type = "file"))

for (i in seq_along(batch_files)) {
  
  cat("\n", "INFO ===> PROCESSING BATCH FILE # ", i)
  
  # read in batch files
  atg_batch <- read_csv(batch_files[i],
                  col_types = list(UCID = col_character(), DISCOUNT_15 = col_character(), DISCOUNT_16 = col_character(), 
                                   DISCOUNT_17 = col_character(), SPEND_TAG_15 = col_character(), SPEND_TAG_16 = col_character(),
                                   SPEND_TAG_17 = col_character(), AMBER_SALES_15 = col_double(), AMBER_SALES_16 = col_double(),
                                   AMBER_SALES_17 = col_double(), QUANTITY_15 = col_integer(), QUANTITY_16 = col_integer(),
                                   QUANTITY_17 = col_integer(), AMBER_MEMBERSHIP_DATE = col_character(), AMBER_MEMBERSHIP_YEAR = col_character(),
                                   TIER_15 = col_character(), TIER_16 = col_character(), TIER_17 = col_character(), CURRENT_TIER = col_character(),
                                   TOTAL_POINTS_EARNED_15 = col_character(), TOTAL_POINTS_EARNED_16 = col_character(), 
                                   TOTAL_POINTS_EARNED_17 = col_character(), STANDARD_POINTS_15 = col_character(), STANDARD_POINTS_16 = col_character(),
                                   STANDARD_POINTS_17 = col_character(), CAMPAIGN_POINTS_15 = col_character(), CAMPAIGN_POINTS_16 = col_character(),
                                   CAMPAIGN_POINTS_17 = col_character(), BONUS_POINTS_15 = col_character(), BONUS_POINTS_16 = col_character(),
                                   BONUS_POINTS_17 = col_character(), OTHER_POINTS_15 = col_character(), OTHER_POINTS_16 = col_character(),
                                   OTHER_POINTS_17 = col_character()))
  
  # create subset of atg_batch which drops cols reshaping purposes,
  # and also filters out "CHECK" records
  atg_df <- atg_batch %>% 
    select(-c(AMBER_MEMBERSHIP_DATE, AMBER_MEMBERSHIP_YEAR, TIER_15, TIER_16, TIER_17, CURRENT_TIER)) %>%
    filter(!(DISCOUNT_17 %in% c("CHECK") | DISCOUNT_16 %in% c("CHECK") | DISCOUNT_15 %in% c("CHECK", "DISC_PROD"))) %>%
    mutate(DISCOUNT_15_FRMT = ifelse(DISCOUNT_15 == "DISC_PROD", "DISCOUNT", DISCOUNT_15)) %>%
    select(UCID, DISCOUNT_15 = DISCOUNT_15_FRMT, DISCOUNT_16:OTHER_POINTS_17)
  
  # create subset of atg_batch which contains columns dropped from atg_df for rejoining to atg_df after reshaping
  atg_membership <- atg_batch %>% 
    select(UCID, AMBER_MEMBERSHIP_DATE, AMBER_MEMBERSHIP_YEAR, TIER_15, TIER_16, TIER_17, CURRENT_TIER) %>% 
    distinct()
  
  # memory cleanup
  rm(atg_batch)
  
  
  #### DATA PRE-PROCESSING & RESHAPE (GATHER) ####
  
  # dynamically fill NA cells using mutate with case_when
  atg_df <- atg_df %>%
    mutate(
      DISCOUNT_17_FILL = case_when(
        is.na(DISCOUNT_17) & DISCOUNT_16 == "DISCOUNT" ~ "DISCOUNT",
        is.na(DISCOUNT_17) & DISCOUNT_16 == "FULL_PRICE" ~ "FULL_PRICE",
        is.na(DISCOUNT_17) & is.na(DISCOUNT_16) & DISCOUNT_15 == "FULL_PRICE" ~ "FULL_PRICE",
        TRUE ~ as.character(DISCOUNT_17)),
      DISCOUNT_16_FILL = case_when(
        DISCOUNT_17 == "DISCOUNT" & is.na(DISCOUNT_16) ~ "DISCOUNT",
        DISCOUNT_17 == "FULL_PRICE" & is.na(DISCOUNT_16) ~ "FULL_PRICE",
        is.na(DISCOUNT_17) & is.na(DISCOUNT_16) & DISCOUNT_15 == "FULL_PRICE" ~ "FULL_PRICE",
        TRUE ~ as.character(DISCOUNT_16))) %>%
    select(UCID, DISCOUNT_15, DISCOUNT_16 = DISCOUNT_16_FILL, DISCOUNT_17 = DISCOUNT_17_FILL, SPEND_TAG_15:OTHER_POINTS_17) %>%
    mutate(
      DISCOUNT_15_FILL = case_when(
        is.na(DISCOUNT_15) & DISCOUNT_16 == "FULL_PRICE" & DISCOUNT_17 == "FULL_PRICE" ~ "FULL_PRICE",
        is.na(DISCOUNT_15) & DISCOUNT_16 == "DISCOUNT" & DISCOUNT_17 == "DISCOUNT" ~ "DISCOUNT",
        DISCOUNT_15 == "FULL_PRICE" & DISCOUNT_16 == "DISCOUNT" & DISCOUNT_17 == "DISCOUNT" ~ "DISCOUNT-ERR",
        TRUE ~ as.character(DISCOUNT_15))) %>%
    select(UCID, DISCOUNT_15 = DISCOUNT_15_FILL, DISCOUNT_16:OTHER_POINTS_17)
  
  # filter out "CHECK" records (sanity check), add index over MEMBER window for spreading data later (distinct keys needed)
  atg_gather <- atg_df %>%
    group_by(UCID) %>% 
    mutate(ROW_ID = 1:n(), UNIQUE_UCID = paste(UCID, as.character(ROW_ID), sep="-")) %>%
    ungroup() %>%
    select(UNIQUE_UCID, DISCOUNT_15:OTHER_POINTS_17) %>%
    gather(key="KEY", value="VALUE", -UNIQUE_UCID)
  
  # memory cleanup
  rm(atg_df)
  
  
  #### DATA PROCESSING ####
  
  # lapply yearFunc function to key column in order to assign year to each row/record
  atg_gather["YEAR"] <- lapply(X=atg_gather["KEY"], FUN = yearFunc)
  
  # in order to eliminate redundant cols we're replacing the two-digit year suffix on each key with a "YEAR" column,
  # and so we need to "standardize" theese keys by removing the suffix
  atg_gather["KEY_STRIP"] <- lapply(X=atg_gather["KEY"], FUN = keyStrip)
  
  
  #### DATA RESHAPE (SPREAD) & RESULTS OUTPUT ####
  if (i == 1) {
    # create first of final dataframes and write to disk including col names
    left_join(
      atg_gather %>%
        select(UNIQUE_UCID, YEAR, KEY_STRIP, VALUE) %>%
        spread(key=KEY_STRIP, value=VALUE, fill = 0) %>%
        mutate(UCID = idStrip(UNIQUE_UCID)) %>%
        select(UCID, YEAR, PURCHASE_FLAG = DISCOUNT, SPEND_TAG, AMBER_SALES, QUANTITY,
               TOTAL_POINTS_EARNED, STANDARD_POINTS, CAMPAIGN_POINTS, BONUS_POINTS, OTHER_POINTS) %>%
        filter(!(PURCHASE_FLAG == "DISCOUNT-ERR")),
      atg_membership,
      by = "UCID") %>%
      write_csv(params$outputPath, col_names = TRUE)
  } else {
    # create first of final dataframes and write to disk including col names
    left_join(
      atg_gather %>%
        select(UNIQUE_UCID, YEAR, KEY_STRIP, VALUE) %>%
        spread(key=KEY_STRIP, value=VALUE, fill = 0) %>%
        mutate(UCID = idStrip(UNIQUE_UCID)) %>%
        select(UCID, YEAR, PURCHASE_FLAG = DISCOUNT, SPEND_TAG, AMBER_SALES, QUANTITY,
               TOTAL_POINTS_EARNED, STANDARD_POINTS, CAMPAIGN_POINTS, BONUS_POINTS, OTHER_POINTS) %>%
        filter(!(PURCHASE_FLAG == "DISCOUNT-ERR")),
      atg_membership,
      by = "UCID") %>%
      write_csv(params$outputPath, append = TRUE, col_names = FALSE)
  }

  # print number of UCIDs in each batch to console for QA
  cat("\n", paste0("BATCH ", i, ": ======> UCID Count: ", left_join(
    atg_gather %>%
      select(UNIQUE_UCID, YEAR, KEY_STRIP, VALUE) %>%
      spread(key=KEY_STRIP, value=VALUE, fill = 0) %>%
      mutate(UCID = idStrip(UNIQUE_UCID)) %>%
      select(UCID, YEAR, PURCHASE_FLAG = DISCOUNT, SPEND_TAG, AMBER_SALES),
    atg_membership,
    by = "UCID") %>% summarize(n_distinct(UCID))))
}

# memory cleanup
rm(list=ls())
gc(verbose = FALSE)