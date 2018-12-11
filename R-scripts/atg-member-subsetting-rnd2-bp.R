library(dplyr)
library(optparse)
library(readr)
library(readxl)
library(stringr)
library(tidyr)

#### SCRIPT PARAMETERS ####

# define file path for data input
file_path <- "C:/Users/glandry/Desktop/Brierley Analytics/Ad Hoc/ATG/tier-analysis/output/atg-rnd4-reshaped.csv"

# define file path for data output
output_file_path <- "C:/Users/glandry/Desktop/Brierley Analytics/Ad Hoc/ATG/tier-analysis/output/atg-rnd4-model-input.csv"


#### DATA INPUT ####
atg_reshape <- read_csv(file_path,
                     col_types = cols(UCID = col_character(), YEAR = col_integer(), PURCHASE_FLAG = col_character(),
                                      SPEND_TAG = col_skip(), AMBER_SALES = col_double(), QUANTITY = col_skip(),
                                      TOTAL_POINTS_EARNED = col_double(), STANDARD_POINTS = col_double(),
                                      CAMPAIGN_POINTS = col_double(), BONUS_POINTS = col_double(), OTHER_POINTS = col_double(),
                                      AMBER_MEMBERSHIP_DATE = col_skip(), AMBER_MEMBERSHIP_YEAR = col_character(),
                                      TIER_15 = col_character(), TIER_16 = col_character(), TIER_17 = col_character(),
                                      CURRENT_TIER = col_character()))

# return number of distinct UCIDs (members)
# atg_reshape %>% summarize(n_distinct(UCID)) # 714,692


#### CREATE DATA SUBSET VIA MEMBER FILTERINGS####

# return list of members with spend < 0 in either 2016 or 2017
neg_spend <- atg_reshape %>%
  filter(AMBER_SALES < 0) %>%
  select(UCID) %>%
  unlist() # 1,503

# return list of members with full_price_16 spend but no tier_16 assignment (bad data)
bad_tier <- atg_reshape %>%
  filter(!(UCID %in% neg_spend)) %>%
  filter(YEAR == "2016" & PURCHASE_FLAG == "FULL_PRICE") %>%
  filter(is.na(TIER_16) & AMBER_SALES > 0) %>%
  select(UCID) %>%
  unlist(use.names = FALSE) # 8,0333

# return list of members with no tier in either 2016 or 2017
no_tier <- atg_reshape %>%
  filter(is.na(TIER_16) & is.na(TIER_17)) %>%
  select(UCID) %>%
  unlist() #21,126


# filter sample to remove members in both member groups defined above
atg_filter <- atg_smpl %>%
  filter(!(UCID %in% neg_spend)) %>%
  filter(!(UCID %in% bad_tier)) %>%
  filter(!(UCID %in% no_tier))

# length(unique(atg_filter$UCID)) #--> 562,782

# memory cleanup
rm(atg_smpl, neg_spend, bad_tier)


#### DATA AGGREGATIONS & JOINS ####

atg_filter_join <- inner_join(atg_reshape %>% # atg_filter %>%
                                select(UCID, YEAR, PURCHASE_FLAG, AMBER_SALES) %>%
                                mutate(SALES_LABELS = paste0(PURCHASE_FLAG, "_", str_sub(YEAR, -2))) %>%
                                select(UCID, SALES_LABELS, AMBER_SALES) %>%
                                spread(key = SALES_LABELS, value = AMBER_SALES, fill = 0),
                              atg_reshape %>% # atg_filter %>%
                                select(UCID, YEAR, TOTAL_POINTS_EARNED, STANDARD_POINTS, CAMPAIGN_POINTS, BONUS_POINTS, OTHER_POINTS) %>%
                                gather(key = "POINTS_FLAG", value = "POINTS_AMOUNT", -c(UCID, YEAR)) %>%
                                distinct() %>%
                                mutate(POINTS_LABELS = paste0(POINTS_FLAG, "_", str_sub(YEAR, -2))) %>%
                                select(UCID, POINTS_LABELS, POINTS_AMOUNT) %>%
                                spread(key = POINTS_LABELS, value = "POINTS_AMOUNT", fill = 0) %>%
                                select(UCID, STANDARD_POINTS_15, CAMPAIGN_POINTS_15, BONUS_POINTS_15, OTHER_POINTS_15, TOTAL_POINTS_15 = TOTAL_POINTS_EARNED_15,
                                       STANDARD_POINTS_16, CAMPAIGN_POINTS_16, BONUS_POINTS_16, OTHER_POINTS_16, TOTAL_POINTS_16 = TOTAL_POINTS_EARNED_16,
                                       STANDARD_POINTS_17, CAMPAIGN_POINTS_17, BONUS_POINTS_17, OTHER_POINTS_17, TOTAL_POINTS_17 = TOTAL_POINTS_EARNED_17),
                              atg_reshape %>%
                                select(UCID, AMBER_MEMBERSHIP_YEAR, TIER_15, TIER_16, TIER_17, CURRENT_TIER) %>%
                                distinct(),
                              by="UCID")

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


#### Ad Hoc ####

# calculate number of members who remained in the same tier YoY
  atg_filter_join %>%
    filter(!(is.na(TIER_16))) %>%
    mutate(TIER_CHANGE_FLAG = ifelse(TIER_16 == TIER_17, 1, 0)) %>%
    filter(TIER_CHANGE_FLAG == 0) %>%
    # group_by(TIER_16, TIER_17) %>%
    summarize(n())

# members without spend in 2016 (i.e. new in 2017)
atg_filter_join %>%
  filter(is.na(TIER_16)) %>%
  summarize(n())

# inspect tier labels for standardizing YoY movement
atg_reshape %>%
  select(UCID, TIER_15, TIER_16, TIER_17) %>%
  group_by(TIER_15, TIER_16, TIER_17) %>%
  summarize(MEMBER_COUNT = n_distinct(UCID))

# look into members with spend that qualifies for higher tier but member not properly tiered during 2015
View(atg_reshape %>%
       filter(YEAR == 2015 & PURCHASE_FLAG == "FULL_PRICE") %>%
       filter(AMBER_SALES > 6000)
     ) 

# look into members with no spend but points
View(atg_reshape %>%
       filter(PURCHASE_FLAG == "FULL_PRICE") %>%
       filter(AMBER_SALES < 1 & TOTAL_POINTS_EARNED > 10)
)

# count members with no spend in 2015
union(atg_reshape %>%
        group_by(YEAR) %>%
        filter(AMBER_SALES >+ 1) %>%
        summarize(MEMBER_COUNT = n_distinct(UCID), FLAG = "AED 1+"),
      atg_reshape %>%
        group_by(YEAR) %>%
        filter(AMBER_SALES < 1) %>%
        summarize(MEMBER_COUNT = n_distinct(UCID), FLAG = "AED < 1")
) %>%
  arrange(YEAR, FLAG)



  
  
  
  



