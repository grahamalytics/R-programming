library(dplyr)
library(readr)
library(readxl)
library(stringr)
library(tidyr)

#### FUNCTION DEFINTIONS ####
qtileGroup <- function(x, q) {
  case_when(
    x < q[2] ~ "1",
    x < q[3] ~ "2",
    x < q[4] ~ "3",
    x < q[5] ~ "4",
    x < q[6] ~ "5",
    x < q[7] ~ "6",
    x < q[8] ~ "7",
    x < q[9] ~ "8",
    x < q[10] ~ "9",
    TRUE ~ "10"
  )
}

spendBandGroup <- function(x, bands) {
  case_when(
    x < bands[1] ~ "0 - 1",
    x < bands[2] ~ "1 - 5000",
    x < bands[3] ~ "5001 - 10000",
    x < bands[4] ~ "10001 - 15000",
    x < bands[5] ~ "15001 - 20000",
    x < bands[6] ~ "20001 - 25000",
    x < bands[7] ~ "25001 - 30000",
    x < bands[8] ~ "30001 - 35000",
    x < bands[9] ~ "35001 - 40000",
    x < bands[10] ~ "40001 - 45000",
    x < bands[11] ~ "45001 - 50000",
    TRUE ~ "50000 +"
  )
}

#### SCRIPT PARAMETERS ####

# define file path for data input
file_path <- "C:/Users/glandry/Desktop/Brierley Analytics/Ad Hoc/ATG/tier-analysis/output/atg-rnd3-model-input.csv"

# define file path for data output
output_file_path <- "C:/Users/glandry/Desktop/Brierley Analytics/Ad Hoc/ATG/tier-analysis/output/atg-deciles-migrations.csv"


#### DATA INPUT ####
atg_final <- read_csv(file_path,
                      col_types = cols(UCID = col_character(), FULL_PRICE_16 = col_double(), DISCOUNT_16 = col_double(),
                                       TOTAL_SPEND_16 = col_double(), TIER_16 = col_character(), FULL_PRICE_17 = col_double(),
                                       DISCOUNT_17 = col_double(), TOTAL_SPEND_17 = col_double(), TIER_17 = col_character(),
                                       TOTAL_POINTS_17 = col_double(), STANDARD_POINTS_17 = col_double(), CAMPAIGN_POINTS_17 = col_double(),
                                       BONUS_POINTS_17 = col_double(), OTHER_POINTS_17 = col_double(), AMBER_MEMBERSHIP_YEAR = col_integer(),
                                       FULL_PRICE_DELTA = col_double(), DISCOUNT_DELTA = col_double(), TOTAL_SPEND_DELTA = col_double(),
                                       FULL_PRICE_PCT_DELTA = col_double(), DISCOUNT_PCT_DELTA = col_double(), TOTAL_SPEND_PCT_DELTA = col_double()))


#### CALCULATE SIZE OF MEMBER SUBSETS ####

# how many members per tier in 2016
atg_final %>%
  group_by(TIER_16) %>%
  summarize(count = n_distinct(UCID))

# how many members in 2016 who were assigned to CLASSIC, SELECT, PLUS, or PRIME tiers
atg_final %>%
  filter(!is.na(TIER_16)) %>%
  summarize(count = n_distinct(UCID)) # 446,908

# how many members in 2016 who were in tiers CLASSIC, SELECT, or PLUS during 2016 and NOT PRIME in 2017
atg_final %>%
  filter(!is.na(TIER_16)) %>%
  filter(!(TIER_16 == "Prime" | TIER_17 == "Prime")) %>%
  summarize(count = n_distinct(UCID)) # 446,716 (difference of 192 btw this number and that on line 37)

atg_final %>%
  filter(!is.na(TIER_16)) %>%
  filter(TIER_16 == "Prime" | TIER_17 == "Prime") %>%
  summarize(count = n_distinct(UCID)) # 192

# how many distinct PRIME members in 2016
atg_final %>%
  filter(TIER_16 == "Prime") %>%
  summarize(count = n_distinct(UCID)) # 161

# how many distinct PRIME members in 2017
atg_final %>%
  filter(TIER_17 == "Prime") %>%
  summarize(count = n_distinct(UCID)) # 216

# how many distinct PRIME members in 2016 OR 2017
atg_final %>%
  filter(!is.na(TIER_16)) %>%
  filter(TIER_16 == "Prime" | TIER_17 == "Prime") %>%
  summarize(count = n_distinct(UCID)) # 192

# how many members with FULL_PRICE spend in 2016 > $0
atg_final %>%
  filter(FULL_PRICE_16 > 0) %>%
  summarize(count = n_distinct(UCID)) # 382,895

# how many members with FULL_PRICE spend in 2016 > $1
atg_final %>%
  filter(FULL_PRICE_16 > 1) %>%
  summarize(count = n_distinct(UCID)) # 382,881

# memory cleanup
rm(atg_final)


#### FILTER DATA TO EXCLUDE MEMBERS WITHOUT TIER IN 2016, AND PRIME MEMBERS IN 2016 OR 2017 ####

# fitler atg_final to exlude members without tier in 2016 and are are not PRIME in either year
atg_no_na_prime <- atg_final %>%
  filter(!is.na(TIER_16)) %>%
  filter(!(TIER_16 == "Prime" | TIER_17 == "Prime"))

# fitler atg_final to exlude members without tier in 2016 and are are not PRIME in either year & not CLASSIC in both years
atg_top2_tier <- atg_final %>%
  filter(!is.na(TIER_16)) %>%
  filter(!(TIER_16 == "Prime" | TIER_17 == "Prime")) %>%
  filter(!(TIER_16 == "Classic" & TIER_17 == "Classic"))


#### CALCULATE DECILE GROUPS (BASED ON SPEND) #### 

# calculate decile groups for FULL_PRICE_16 on all data in atg_no_na_prime
all_deciles_16 <- quantile(atg_no_na_prime$FULL_PRICE_16, probs = seq(from = 0, to = 1, by = 0.1))

# calculate decile groups for FULL_PRICE_17 on all data in atg_no_na_prime
all_deciles_17 <- quantile(atg_no_na_prime$FULL_PRICE_17, probs = seq(from = 0, to = 1, by = 0.1))

# calculate decile groups for FULL_PRICE_16 on all 2017 CLASSIC members
classic_deciles_16 <- quantile(atg_no_na_prime %>%
                                 filter(TIER_17 == "Classic") %>%
                                 select(FULL_PRICE_16) %>%
                                 unlist(), 
                               probs = seq(from = 0, to = 1, by = 0.1)
                               )

# calculate decile groups for FULL_PRICE_17 on all 2017 CLASSIC members
classic_deciles_17 <- quantile(atg_no_na_prime %>%
                                 filter(TIER_17 == "Classic") %>%
                                 select(FULL_PRICE_17) %>%
                                 unlist(), 
                               probs = seq(from = 0, to = 1, by = 0.1)
                               )

# calculate decile groups for FULL_PRICE_16 on all 2017 SELECT members
select_deciles_16 <- quantile(atg_no_na_prime %>%
                                 filter(TIER_17 == "Select") %>%
                                 select(FULL_PRICE_16) %>%
                                 unlist(), 
                               probs = seq(from = 0, to = 1, by = 0.1)
                              )

# calculate decile groups for FULL_PRICE_17 on all 2017 SELECT members
select_deciles_17 <- quantile(atg_no_na_prime %>%
                                 filter(TIER_17 == "Select") %>%
                                 select(FULL_PRICE_17) %>%
                                 unlist(), 
                               probs = seq(from = 0, to = 1, by = 0.1)
                              )

# calculate decile groups for FULL_PRICE_16 on all 2017 PLUS members
plus_deciles_16 <- quantile(atg_no_na_prime %>%
                                filter(TIER_17 == "Plus") %>%
                                select(FULL_PRICE_16) %>%
                                unlist(), 
                              probs = seq(from = 0, to = 1, by = 0.1)
                              )

# calculate decile groups for FULL_PRICE_17 on all 2017 PLUS members
plus_deciles_17 <- quantile(atg_no_na_prime %>%
                              filter(TIER_17 == "Plus") %>%
                              select(FULL_PRICE_17) %>%
                              unlist(), 
                            probs = seq(from = 0, to = 1, by = 0.1)
)


#### ASSIGN DECILE LABELS TO MEMBERS ####

# assign decile labels to customers for each decile scenario above
atg_no_na_prime %>% 
  mutate(DECILE_16_ALL = qtileGroup(FULL_PRICE_16, all_deciles_16),
         DECILE_17_ALL = qtileGroup(FULL_PRICE_17, all_deciles_17),
         DECILE_16_CLASSIC = ifelse(TIER_17 == "Classic", qtileGroup(FULL_PRICE_16, classic_deciles_16), "-99"),
         DECILE_17_CLASSIC = ifelse(TIER_17 == "Classic", qtileGroup(FULL_PRICE_17, classic_deciles_17), "-99"),
         DECILE_16_SELECT = ifelse(TIER_17 == "Select", qtileGroup(FULL_PRICE_16, select_deciles_16), "-99"),
         DECILE_17_SELECT = ifelse(TIER_17 == "Select", qtileGroup(FULL_PRICE_17, select_deciles_17), "-99"),
         DECILE_16_PLUS = ifelse(TIER_17 == "Plus", qtileGroup(FULL_PRICE_16, plus_deciles_16), "-99"),
         DECILE_17_PLUS = ifelse(TIER_17 == "Plus", qtileGroup(FULL_PRICE_17, plus_deciles_17), "-99")
         ) %>%
  select(UCID, FULL_PRICE_16, TIER_16, DECILE_16_ALL, DECILE_16_CLASSIC, DECILE_16_SELECT, DECILE_16_PLUS,
         FULL_PRICE_17, TIER_17, DECILE_17_ALL, DECILE_17_CLASSIC, DECILE_17_SELECT, DECILE_17_PLUS,
         TOTAL_POINTS_17, STANDARD_POINTS_17, CAMPAIGN_POINTS_17, BONUS_POINTS_17, OTHER_POINTS_17) %>%
  write_csv(output_file_path)





#### ASSIGN SPEND BANDS (IN 5K INCREMENTS) TO MEMBERS ####
spend_bands <- seq(from = 1, to = 50001, by = 5000)

atg_no_na_prime %>% 
  mutate(CLASSIC_17 = ifelse(TIER_17 == "Classic", 1, 0),
         SELECT_17 = ifelse(TIER_17 == "Select", 1, 0),
         PLUS_17 = ifelse(TIER_17 == "Plus", 1, 0),
         SPEND_BAND_16 = spendBandGroup(FULL_PRICE_16, spend_bands),
         SPEND_BAND_17 = spendBandGroup(FULL_PRICE_17, spend_bands)) %>%
  select(UCID, FULL_PRICE_16, SPEND_BAND_16, FULL_PRICE_17, SPEND_BAND_17, FULL_PRICE_DELTA,
         CLASSIC_17, SELECT_17, PLUS_17) %>%
  write_csv("C:/users/glandry/desktop/atg-spend-band-migrations.csv")

