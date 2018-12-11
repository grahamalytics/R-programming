library(dplyr)
library(optparse)
library(readr)
library(readxl)
library(stringr)
library(tidyr)

#### SCRIPT PARAMETERS ####

# define default parameters for use in defining script parameters with OPTPARSE
file_path <- "C:/Users/glandry/Desktop/Brierley Analytics/Ad Hoc/ATG/tier-analysis/client-data-files/RAW_DATA_WITH_TIER_AND_EARNED_POINTS_v3.xlsx"

sheet_name <- "RAW_DATA_WITH_TIER_AND_EARNED_P"

batch_path <- "C:/Users/glandry/Desktop/Brierley Analytics/Ad Hoc/ATG/tier-analysis/output/batches/atg-rnd4-batch"

num_batches <- 8

# define parameters for reading in target file for batching, filename for writing batched files, and number of batches
script_parameters <- list(make_option(c("-f", "--filePath"), 
                                      type = "character", 
                                      default = file_path,
                                      help = "path to file for batch processing"),
                          make_option(c("-s", "--sheetName"),
                                      type = "character",
                                      default = sheet_name,
                                      help = "sheet name for reading XLSX or CSV files"),
                          make_option(c("-b", "--batchPath"),
                                      type = "character",
                                      default = batch_path,
                                      help = "full filepath/name for writing batched files"),
                          make_option(c("-n", "--numberBatches"),
                                      type = "integer",
                                      default = num_batches,
                                      help = "number of batch files to create"))

# instantiate OptionParser object with script_parameters object
param_parser <- OptionParser(option_list = script_parameters)

# parse command line arguments
params <- parse_args(param_parser)

# print to console if no arguments are provided by user and so default parameters used
if (params$filePath == file_path) {
  cat(paste("\n", "WARN ===> No filepath provided by user, running with default filepath:",
            params$filePath, "\n", sep = "\n"))
} else {
  cat(paste("\n", "INFO ===> Reading in file below for batch processing:",
            params$filePath, "\n", sep = "\n"))
}

if (params$sheetName == sheet_name) {
  cat(paste("\n", "WARN ===> No sheetname provided by user, running with default sheetname:",
            params$sheetName, "\n", sep = "\n"))
} else {
  cat(paste("\n", "INFO ===> Reading in sheet name below from file:",
            params$sheetName, "\n", sep = "\n"))
}

if (params$batchPath == batch_path) {
  cat(paste("\n", "WARN ===> No filepath for batch output provided by user, running with default filepath:",
            params$batchPath, "\n", sep = "\n"))
} else {
  cat(paste("\n", "INFO ===> Using filepath and name below for writing batch files:",
            params$batchPath, "\n", sep = "\n"))
}

if (params$numberBatches == num_batches) {
  cat(paste("\n", "WARN ===> Number of bathces not provided user, running with default number of batches:",
            params$numberBatches, "\n", sep = "\n"))
} else {
  cat(paste("\n", "INFO ===> Number of batches being returned:",
            params$numberBatches, "\n", sep = "\n"))
}


#### FUNCTION DEFINITIONS ####
defineBatches <- function(rootName, batches) {
  
  return(paste0(rootName, "-", seq(from = 1, to = batches, by = 1), ".csv"))
}


#### CREATE LIST OF FILENAMES FOR BATCH WRITES ####

# create list of file names for writing
file_names <- defineBatches(rootName = params$batchPath, batches = params$numberBatches)


#### DATA INPUT ####

cat("\n", "INFO ===> READING IN FILE FOR BATCHING", "\n")

# read in data file, dropping unwanted "CHECK" records and sorting by UCID
atg_raw <- read_excel(params$filePath, sheet=params$sheetName,
                      col_names = c("UCID", "DISCOUNT_15", "DISCOUNT_16", "DISCOUNT_17", "SPEND_TAG_15", "SPEND_TAG_16", "SPEND_TAG_17",
                                    "AMBER_SALES_15", "AMBER_SALES_16", "AMBER_SALES_17", "QUANTITY_15", "QUANTITY_16", "QUANTITY_17",
                                    "AMBER_MEMBERSHIP_DATE", "AMBER_MEMBERSHIP_YEAR", "TIER_15", "TIER_16", "TIER_17", "CURRENT_TIER",
                                    "TOTAL_POINTS_EARNED_15", "TOTAL_POINTS_EARNED_16", "TOTAL_POINTS_EARNED_17", "STANDARD_POINTS_15",
                                    "STANDARD_POINTS_16", "STANDARD_POINTS_17", "CAMPAIGN_POINTS_15", "CAMPAIGN_POINTS_16", "CAMPAIGN_POINTS_17",
                                    "BONUS_POINTS_15", "BONUS_POINTS_16", "BONUS_POINTS_17", "OTHER_POINTS_15", "OTHER_POINTS_16", "OTHER_POINTS_17"),
                      col_types = c("text", "text", "text", "text", "text", "text", "text",
                                    "text", "text", "text", "text", "text" , "text",
                                    "text", "text", "text", "text", "text", "text",
                                    "text", "text", "text", "text",
                                    "text", "text", "text", "text", "text",
                                    "text", "text", "text", "text", "text", "text"), 
                      skip=1) %>%
  arrange(as.numeric(UCID))


#### DATA BATCHING ####
## The method below ensures that all data associated with a member resides in one batch, and not split between batches

cat("\n", "INFO ===> CALCULATING FILE CUTOFF POINTS FOR BATCHING", "\n")

# create list of distinct UCIDs before creating batches (subsets) of members
dist_mbr_list <- unlist(unique(atg_raw["UCID"]), use.names = FALSE)

# calculate length of dist_mbr_list aka number of distinct members
dist_mbr <- length(dist_mbr_list)

# calculate mbr_batch_size as a function of # of distinct members and # of batches
mbr_batch_size <-  ceiling(dist_mbr / params$numberBatches)

# create a vector of indices which represent batch cutoffs
split_index <- seq(from = 1, to = dist_mbr, by = mbr_batch_size)

# instantiate a list of lists for holding batch cuoffs by UCID
mbr_filters <- vector("list", length = length(split_index))

# name list elements
names(mbr_filters) <- paste0("batch", "-", seq(from = 1, to = params$numberBatches, by = 1))

# create subsets of UCID to be used for constructing batches
for (i in seq_along(split_index)) {
  if (i == 1) {
    mbr_filters[i] <- list(dist_mbr_list[1:split_index[i + 1]])
  } else if (i < length(split_index)) {
    mbr_filters[i] <- list(dist_mbr_list[(split_index[i] + 1):split_index[i + 1]])
  } else {
    mbr_filters[i] <- list(dist_mbr_list[(split_index[i] + 1):dist_mbr])
  }
}

cat("\n", "INFO ===> WRITING BATCH FILES", "\n")

# filter entire atg_raw dataframe for members in each batch and write respective subset to disk
for (i in seq_along(mbr_filters)) {
  print(paste0("BATCH ", i, ": ======> UCID Count: ", atg_raw %>%
    filter(UCID %in% mbr_filters[[i]]) %>%
    summarize(n_distinct(UCID))))
  
  atg_raw %>%
    filter(UCID %in% mbr_filters[[i]]) %>%
    write_csv(path=file_names[i])
}

# memory cleanup
rm(list=ls())
gc(verbose = FALSE)
