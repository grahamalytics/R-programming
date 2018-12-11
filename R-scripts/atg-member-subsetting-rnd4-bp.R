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
                                      AMBER_MEMBERSHIP_DATE = col_skip(), AMBER_MEMBERSHIP_YEAR = col_integer(),
                                      TIER_15 = col_character(), TIER_16 = col_character(), TIER_17 = col_character(),
                                      CURRENT_TIER = col_character()))

# return number of distinct UCIDs (members)
# atg_reshape %>% summarize(n_distinct(UCID)) # 714,692


#### DATA AGGREGATIONS & JOINS ####

atg_join <- inner_join(atg_reshape %>%
                                select(UCID, YEAR, PURCHASE_FLAG, AMBER_SALES) %>%
                                mutate(SALES_LABELS = paste0(PURCHASE_FLAG, "_", str_sub(YEAR, -2))) %>%
                                select(UCID, SALES_LABELS, AMBER_SALES) %>%
                                spread(key = SALES_LABELS, value = AMBER_SALES, fill = 0),
                              atg_reshape %>%
                                select(UCID, YEAR, TOTAL_POINTS_EARNED, STANDARD_POINTS, CAMPAIGN_POINTS, BONUS_POINTS, OTHER_POINTS) %>%
                                gather(key = "POINTS_FLAG", value = "POINTS_AMOUNT", -c(UCID, YEAR)) %>%
                                distinct() %>%
                                mutate(POINTS_LABELS = paste0(POINTS_FLAG, "_", str_sub(YEAR, -2))) %>%
                                select(UCID, POINTS_LABELS, POINTS_AMOUNT) %>%
                                spread(key = POINTS_LABELS, value = "POINTS_AMOUNT", fill = 0) %>%
                                select(UCID, STANDARD_POINTS_15, CAMPAIGN_POINTS_15, BONUS_POINTS_15, OTHER_POINTS_15, TOTAL_POINTS_15 = TOTAL_POINTS_EARNED_15,
                                       STANDARD_POINTS_16, CAMPAIGN_POINTS_16, BONUS_POINTS_16, OTHER_POINTS_16, TOTAL_POINTS_16 = TOTAL_POINTS_EARNED_16,
                                       STANDARD_POINTS_17, CAMPAIGN_POINTS_17, BONUS_POINTS_17, OTHER_POINTS_17, TOTAL_POINTS_17 = TOTAL_POINTS_EARNED_17),
                              by="UCID") %>%
                    inner_join(atg_reshape %>%
                                 select(UCID, AMBER_MEMBERSHIP_YEAR, TIER_15, TIER_16, TIER_17, CURRENT_TIER) %>%
                                 distinct(),
                              by="UCID") %>%
  mutate(TOTAL_SPEND_16 = DISCOUNT_16 + FULL_PRICE_16, TOTAL_SPEND_17 = DISCOUNT_17 + FULL_PRICE_17,
         BONUS_CAMPAIGN_POINTS_15 = CAMPAIGN_POINTS_15 + BONUS_POINTS_15 + OTHER_POINTS_15,
         BONUS_CAMPAIGN_POINTS_16 = CAMPAIGN_POINTS_16 + BONUS_POINTS_16 + OTHER_POINTS_16,
         BONUS_CAMPAIGN_POINTS_17 = CAMPAIGN_POINTS_17 + BONUS_POINTS_17 + OTHER_POINTS_17) %>%
  select(UCID, FULL_PRICE_15, FULL_PRICE_16, TOTAL_SPEND_16, FULL_PRICE_17, TOTAL_SPEND_17, STANDARD_POINTS_15, BONUS_CAMPAIGN_POINTS_15, 
         TOTAL_POINTS_15, STANDARD_POINTS_16, BONUS_CAMPAIGN_POINTS_16, TOTAL_POINTS_16, STANDARD_POINTS_17,
         BONUS_CAMPAIGN_POINTS_17, TOTAL_POINTS_17, AMBER_MEMBERSHIP_YEAR, TIER_15, TIER_16, TIER_17, CURRENT_TIER) %>%
  mutate(TIER_17_BP = case_when(
    is.na(TIER_17) & AMBER_MEMBERSHIP_YEAR <= 2017 ~ as.character(CURRENT_TIER),
    TRUE ~ as.character(TIER_17)),
    TIER_16_BP = case_when(
      is.na(TIER_16) & AMBER_MEMBERSHIP_YEAR <= 2016 ~ as.character(TIER_17_BP),
      TRUE ~ as.character(TIER_16)),
    TIER_15_BP = case_when(
      is.na(TIER_15) & AMBER_MEMBERSHIP_YEAR <= 2015 ~ as.character(TIER_16_BP),
      TRUE ~ as.character(TIER_15)))
    
    ## logic for proforma tier assignment
    # is.na(TIER_16) & TOTAL_POINTS_16 < 1 & AMBER_MEMBERSHIP_YEAR > 2016 ~ "NA",
    # is.na(TIER_16) & TOTAL_POINTS_16 < 1 & AMBER_MEMBERSHIP_YEAR < 2016 & TOTAL_POINTS_15 < 1 ~ "NA",
    # is.na(TIER_16) & TOTAL_POINTS_16 < 1 & AMBER_MEMBERSHIP_YEAR < 2016 & TOTAL_POINTS_15 < 20000 ~ "Classic",
    # is.na(TIER_16) & TOTAL_POINTS_16 < 1 & AMBER_MEMBERSHIP_YEAR < 2016 & TOTAL_POINTS_15 < 150000 ~ "Select",
    # is.na(TIER_16) & TOTAL_POINTS_16 < 1 & AMBER_MEMBERSHIP_YEAR < 2016 & TOTAL_POINTS_15 >= 150000 ~ "Plus",
    # is.na(TIER_16) & TOTAL_POINTS_16 > 1 & TOTAL_POINTS_16 < 20000 ~ "Classic",
    # is.na(TIER_16) & TOTAL_POINTS_16 > 1 & TOTAL_POINTS_16 < 150000 ~ "Select",
    # is.na(TIER_16) & TOTAL_POINTS_16 > 1 & TOTAL_POINTS_16 >= 150000 ~ "Plus",
    # TRUE ~ as.character(TIER_16)))

# memory cleanup
rm(atg_reshape)


#### CREATE DATA SUBSET VIA MEMBER FILTERINGS ####

# return list of members with full_price_16 spend but no tier_16 assignment (bad data)
bad_tier <- atg_join %>%
  filter(is.na(TIER_15_BP) & is.na(TIER_16_BP) & is.na(TIER_17_BP)) %>%
  select(UCID) %>%
  unlist(use.names = FALSE) # 1,062

# return list of members with amber_membership_year of 2018
members_2018 <- atg_join %>%
  filter(!(UCID %in% bad_tier)) %>%
  filter(AMBER_MEMBERSHIP_YEAR == 2018) %>%
  select(UCID) %>%
  unlist(use.names = FALSE)

# return list of PRIME members
members_prime <- atg_join %>%
  filter(!(UCID %in% bad_tier)) %>%
  filter(!(UCID %in% members_2018)) %>%
  filter(TIER_15_BP == "Prime" | TIER_16_BP == "Prime" | TIER_17_BP == "Prime") %>%
  select(UCID) %>%
  unlist(use.names = FALSE) # 461


# filter sample to remove members in both member groups defined above
atg_filter <- atg_join %>%
  filter(!(UCID %in% bad_tier)) %>%
  filter(!(UCID %in% members_2018)) %>%
  filter(!(UCID %in% members_prime))

# length(unique(atg_filter$UCID)) #--> 713,169

# memory cleanup
rm(bad_tier, members_2018, members_prime, atg_join)


#### RESULTS OUTPUT ####

atg_filter %>%
  mutate(TOTAL_SPEND_15 = FULL_PRICE_15 + 0) %>%
  select(UCID, AMBER_MEMBERSHIP_YEAR, FULL_PRICE_15, TOTAL_SPEND_15, FULL_PRICE_16, TOTAL_SPEND_16, FULL_PRICE_17, 
         TOTAL_SPEND_17, STANDARD_POINTS_15, BONUS_CAMPAIGN_POINTS_15, TOTAL_POINTS_15, STANDARD_POINTS_16, BONUS_CAMPAIGN_POINTS_16,
         TOTAL_POINTS_16, STANDARD_POINTS_17, BONUS_CAMPAIGN_POINTS_17, TOTAL_POINTS_17, TIER_15, TIER_16, TIER_17,
         TIER_15_BP, TIER_16_BP, TIER_17_BP, CURRENT_TIER) %>%
  write_csv(output_file_path)


# memory cleanup
rm(list=ls())
gc()



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



  
  
  
  



