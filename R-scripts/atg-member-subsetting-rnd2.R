library(dplyr)
library(readr)
library(readxl)
library(stringr)
library(tidyr)

#### SCRIPT PARAMETERS ####

# define file path for data input
file_path <- "C:/Users/glandry/Desktop/Brierley Analytics/Ad Hoc/ATG/tier-analysis/output/atg-rnd3-final.csv"

# define file path for data output
output_file_path <- "C:/Users/glandry/Desktop/Brierley Analytics/Ad Hoc/ATG/tier-analysis/output/atg-rnd3-model-input.csv"


#### DATA INPUT ####
atg_smpl <- read_csv(file_path,
                     col_types = cols(UCID = col_character(), YEAR = col_integer(), PURCHASE_FLAG = col_character(),
                                      SPEND_TAG = col_character(), AMBER_SALES = col_double(), QUANTITY = col_double(),
                                      TOTAL_POINTS_EARNED = col_double(), STANDARD_POINTS = col_double(),
                                      CAMPAIGN_POINTS = col_double(), BONUS_POINTS = col_double(), OTHER_POINTS = col_double(),
                                      AMBER_MEMBERSHIP_DATE = col_character(), AMBER_MEMBERSHIP_YEAR = col_character(),
                                      CURRENT_TIER = col_character(), TIER_16 = col_character(), TIER_17 = col_character()))

# return number of distinct UCIDs (members)
#length(unique(atg_smpl$UCID)) --> 573214


#### CREATE DATA SUBSET VIA MEMBER FILTERINGS####

# return list of members with spend < 0 in either 2016 or 2017
neg_spend <- atg_smpl %>%
  filter(AMBER_SALES < 0) %>%
  select(UCID) %>%
  unlist()

# return list of members with full_price_16 spend but no tier_16 assignment (bad data)
bad_tier <- atg_smpl %>%
  filter(!(UCID %in% neg_spend)) %>%
  filter(YEAR == "2016" & PURCHASE_FLAG == "FULL_PRICE") %>%
  filter(is.na(TIER_16) & AMBER_SALES > 0) %>%
  select(UCID) %>%
  unlist(use.names = FALSE)

# return list of members with no tier in either 2016 or 2017
no_tier <- atg_smpl %>%
  filter(is.na(TIER_16) & is.na(TIER_17)) %>%
  select(UCID) %>%
  unlist()


# filter sample to remove members in both member groups defined above
atg_filter <- atg_smpl %>%
  filter(!(UCID %in% neg_spend)) %>%
  filter(!(UCID %in% bad_tier)) %>%
  filter(!(UCID %in% no_tier))

# length(unique(atg_filter$UCID)) #--> 562,782

# memory cleanup
rm(atg_smpl, neg_spend, bad_tier)


#### DATA AGGREGATIONS & JOINS ####
atg_filter_join <- inner_join(atg_filter %>%
                            select(UCID, YEAR, PURCHASE_FLAG, AMBER_SALES) %>%
                            mutate(SALES_LABELS = paste0(PURCHASE_FLAG, "_", str_sub(YEAR, -2))) %>%
                            select(UCID, SALES_LABELS, AMBER_SALES) %>%
                            spread(key = SALES_LABELS, value = AMBER_SALES, fill = 0) %>%
                            mutate(TOTAL_SPEND_16 = FULL_PRICE_16 + DISCOUNT_16, TOTAL_SPEND_17 = FULL_PRICE_17 + DISCOUNT_17) %>%
                            select(UCID, FULL_PRICE_16, DISCOUNT_16, TOTAL_SPEND_16, FULL_PRICE_17, DISCOUNT_17, TOTAL_SPEND_17),
                          atg_filter %>%
                            filter(YEAR == "2017") %>%
                            select(UCID, TOTAL_POINTS_17 = TOTAL_POINTS_EARNED, STANDARD_POINTS_17 = STANDARD_POINTS, 
                            CAMPAIGN_POINTS_17 = CAMPAIGN_POINTS, BONUS_POINTS_17 = BONUS_POINTS, OTHER_POINTS_17 = OTHER_POINTS,
                            TIER_16, TIER_17, CURRENT_TIER, AMBER_MEMBERSHIP_YEAR) %>%
                            distinct(),
                          by="UCID") %>%
  select(UCID, FULL_PRICE_16, DISCOUNT_16, TOTAL_SPEND_16, TIER_16,  FULL_PRICE_17, DISCOUNT_17, TOTAL_SPEND_17, TIER_17,
         TOTAL_POINTS_17, STANDARD_POINTS_17, CAMPAIGN_POINTS_17, BONUS_POINTS_17, OTHER_POINTS_17, AMBER_MEMBERSHIP_YEAR)

# memory cleanup
rm(atg_filter)

# calculate YoY change in spend buckets across members before writing results to disk
atg_filter_join %>%
    mutate(FULL_PRICE_DELTA = FULL_PRICE_17 - FULL_PRICE_16,
           DISCOUNT_DELTA = DISCOUNT_17 - DISCOUNT_16,
           TOTAL_SPEND_DELTA = TOTAL_SPEND_17 - TOTAL_SPEND_16,
           FULL_PRICE_PCT_DELTA = ifelse(FULL_PRICE_16 > 0, FULL_PRICE_DELTA / FULL_PRICE_16, NA),
           DISCOUNT_PCT_DELTA = ifelse(DISCOUNT_16 > 0, DISCOUNT_DELTA / DISCOUNT_16, NA),
           TOTAL_SPEND_PCT_DELTA = ifelse(TOTAL_SPEND_16 > 0, TOTAL_SPEND_DELTA / TOTAL_SPEND_16, NA)
           ) %>%
  write_csv(output_file_path)


