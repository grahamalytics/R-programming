library(dplyr)
library(optparse)
library(readr)
library(readxl)
library(stringr)
library(tidyr)


#### SCRIPT PARAMETERS ####

# define file path for data input
file_path <- "C:/Users/glandry/Desktop/Brierley Analytics/Ad Hoc/ATG/tier-analysis/output/atg-rnd4-model-input.csv"

output_path <- "C:/Users/glandry/Desktop/Brierley Analytics/Ad Hoc/ATG/tier-analysis/output/atg-rnd4-proforma-input.csv"


#### FUNCTION DEFINITIONS ####
bpStndPoints <- function(x, year) {
  if (year == 2015) {
    if (x["ACTIVITY_15"] %in% c("Inactive", "Not a Member")) {
      return(0)
    }
    else if (x["ACTIVITY_15"] == "New") {
      if (as.numeric(x["FULL_PRICE_15"]) < 20000) {
        return(as.numeric(x["FULL_PRICE_15"]) * 1)
      } 
      else if (as.numeric(x["FULL_PRICE_15"]) < 85000) {
        return(((as.numeric(x["FULL_PRICE_15"]) - 20000) * 2) + 20000)
      } 
      else if (as.numeric(x["FULL_PRICE_15"]) >= 85000) {
        return(((as.numeric(x["FULL_PRICE_15"]) - 85000) * 3) + 150000)  
      }
    }
    else if (x["ACTIVITY_15"] == "Active" & x["TIER_15_BP"] == "Classic") {
      if (as.numeric(x["FULL_PRICE_15"]) < 20000) {
        return(as.numeric(x["FULL_PRICE_15"]) * 1)
      } 
      else if (as.numeric(x["FULL_PRICE_15"]) < 85000) {
        return(((as.numeric(x["FULL_PRICE_15"]) - 20000) * 2) + 20000)
      } 
      else if (as.numeric(x["FULL_PRICE_15"]) >= 85000) {
        return(((as.numeric(x["FULL_PRICE_15"]) - 85000) * 3) + 150000)  
      }
    }
    else if (x["ACTIVITY_15"] == "Active" & x["TIER_15_BP"] == "Select") {
      if (as.numeric(x["FULL_PRICE_15"]) < 75000) {
        return(as.numeric(x["FULL_PRICE_15"]) * 2)
      }
      else if (as.numeric(x["FULL_PRICE_15"]) >= 75000) {
        return(((as.numeric(x["FULL_PRICE_15"]) - 75000) * 3) + 150000)
      }
    }
    else if (x["ACTIVITY_15"] == "Active" &  x["TIER_15_BP"] == "Plus") {
      return(as.numeric(x["FULL_PRICE_15"]) * 3)
    }
    else { 
      return(-99.15)
    }
  }
  else if (year == 2016) {
    if (x["ACTIVITY_16"] %in% c("Inactive", "Not a Member", "Lapsed")) {
      return(0)
    }
    else if (x["ACTIVITY_16"] %in% c("New", "Reactivated")) {
      if (as.numeric(x["FULL_PRICE_16"]) < 20000) {
        return(as.numeric(x["FULL_PRICE_16"]) * 1)
      } 
      else if (as.numeric(x["FULL_PRICE_16"]) < 85000) {
        return(((as.numeric(x["FULL_PRICE_16"]) - 20000) * 2) + 20000)
      } 
      else if (as.numeric(x["FULL_PRICE_16"]) >= 85000) {
        return(((as.numeric(x["FULL_PRICE_16"]) - 85000) * 3) + 150000)  
      }
    }
    else if (x["ACTIVITY_16"] == "Active" & x["PF_TIER_15"] == "Classic") {
      if (as.numeric(x["FULL_PRICE_16"]) < 20000) {
        return(as.numeric(x["FULL_PRICE_16"]) * 1)
      } 
      else if (as.numeric(x["FULL_PRICE_16"]) < 85000) {
        return(((as.numeric(x["FULL_PRICE_16"]) - 20000) * 2) + 20000)
      } 
      else if (as.numeric(x["FULL_PRICE_16"]) >= 85000) {
        return(((as.numeric(x["FULL_PRICE_16"]) - 85000) * 3) + 150000)  
      }
    }
    else if (x["ACTIVITY_16"] == "Active" & x["PF_TIER_15"] == "Select") {
      if (as.numeric(x["FULL_PRICE_16"]) < 75000) {
        return(as.numeric(x["FULL_PRICE_16"]) * 2)
      }
      else if (as.numeric(x["FULL_PRICE_16"]) >= 75000) {
        return(((as.numeric(x["FULL_PRICE_16"]) - 75000) * 3) + 150000)
      }
    }
    else if (x["ACTIVITY_16"] == "Active" & x["PF_TIER_15"] == "Plus") {
      return(as.numeric(x["FULL_PRICE_16"]) * 3)
    }
    else { 
      return(-99.16)
    }
  }
  else if (year == 2017) {
    if (x["ACTIVITY_17"] %in% c("Lapsed", "Lasped (2yrs)")) {
      return(0)
    }
    else if (x["ACTIVITY_17"] %in% c("New", "Reactivated")) {
      if (as.numeric(x["FULL_PRICE_17"]) < 20000) {
        return(as.numeric(x["FULL_PRICE_17"]) * 1)
      } 
      else if (as.numeric(x["FULL_PRICE_17"]) < 85000) {
        return(((as.numeric(x["FULL_PRICE_17"]) - 20000) * 2) + 20000)
      } 
      else if (as.numeric(x["FULL_PRICE_17"]) >= 85000) {
        return(((as.numeric(x["FULL_PRICE_17"]) - 85000) * 3) + 150000)  
      }
    }
    else if (x["ACTIVITY_17"] == "Active" & x["PF_TIER_16"] == "Classic") {
      if (as.numeric(x["FULL_PRICE_17"]) < 20000) {
        return(as.numeric(x["FULL_PRICE_17"]) * 1)
      } 
      else if (as.numeric(x["FULL_PRICE_17"]) < 85000) {
        return(((as.numeric(x["FULL_PRICE_17"]) - 20000) * 2) + 20000)
      } 
      else if (as.numeric(x["FULL_PRICE_17"]) >= 85000) {
        return(((as.numeric(x["FULL_PRICE_17"]) - 85000) * 3) + 150000)  
      }
    }
    else if (x["ACTIVITY_17"] == "Active" & x["PF_TIER_16"] == "Select") {
      if (as.numeric(x["FULL_PRICE_17"]) < 75000) {
        return(as.numeric(x["FULL_PRICE_17"]) * 2)
      }
      else if (as.numeric(x["FULL_PRICE_17"]) >= 75000) {
        return(((as.numeric(x["FULL_PRICE_17"]) - 75000) * 3) + 150000)
      }
    }
    else if (x["ACTIVITY_17"] == "Active" & x["PF_TIER_16"] == "Plus") {
      return(as.numeric(x["FULL_PRICE_16"]) * 3)
    }
    else { 
      return(-99.17)
    }
  }
  else {
    return(-99)
  }
}

bpTierAssign <- function(x, year) {
  if (year == 2015) {
    if (as.numeric(x["AMBER_MEMBERSHIP_YEAR"]) > 2015) {
      return("NA")
    }
    else if (as.numeric(x["PF_TOTAL_POINTS_15"]) < 20000) {
      # return(paste0(x["ACTIVITY_15"], " - ", "Classic"))
      return("Classic")
    }
    else if (as.numeric(x["PF_TOTAL_POINTS_15"]) < 150000) {
      # return(paste0(x["ACTIVITY_15"], " - ", "Select"))
      return("Select")
    }
    else if (as.numeric(x["PF_TOTAL_POINTS_15"]) >= 150000) {
      # return(paste0(x["ACTIVITY_15"], " - ", "Plus"))
      return("Plus")
    }
    else {
      return(-99.15)
    }
  }
  else if (year == 2016) {
    if (as.numeric(x["AMBER_MEMBERSHIP_YEAR"]) > 2016) {
      return("NA")
    }
    else if (as.numeric(x["PF_TOTAL_POINTS_16"]) < 20000) {
      # return(paste0(x["ACTIVITY_16"], " - ", "Classic"))
      return("Classic")
    }
    else if (as.numeric(x["PF_TOTAL_POINTS_16"]) < 150000) {
      # return(paste0(x["ACTIVITY_16"], " - ", "Select"))
      return("Select")
    }
    else if (as.numeric(x["PF_TOTAL_POINTS_16"]) >= 150000) {
      # return(paste0(x["ACTIVITY_16"], " - ", "Plus"))
      return("Plus")
    }
    else {
      return(-99.16)
    }
  }
  else if (year == 2017) {
    if (as.numeric(x["AMBER_MEMBERSHIP_YEAR"]) > 2017) {
      return("NA")
    }
    else if (as.numeric(x["PF_TOTAL_POINTS_17"]) < 20000) {
      # return(paste0(x["ACTIVITY_17"], " - ", "Classic"))
      return("Classic")
    }
    else if (as.numeric(x["PF_TOTAL_POINTS_17"]) < 150000) {
      # return(paste0(x["ACTIVITY_17"], " - ", "Select"))
      return("Select")
    }
    else if (as.numeric(x["PF_TOTAL_POINTS_17"]) >= 150000) {
      # return(paste0(x["ACTIVITY_17"], " - ", "Plus"))
      return("Plus")
    }
    else {
      return(-99.17)
    }
  }
  else {
    return(-99)
  }
}

#### DATA INPUT ####

atg_tiers <- read_csv(file_path,
                      col_types = cols(UCID = col_character(), AMBER_MEMBERSHIP_YEAR = col_integer(), FULL_PRICE_15 = col_double(),
                                       TOTAL_SPEND_15 = col_double(), FULL_PRICE_16 = col_double(), TOTAL_SPEND_16 = col_double(),
                                       FULL_PRICE_17 = col_double(), TOTAL_SPEND_17 = col_double(), STANDARD_POINTS_15 = col_number(),
                                       BONUS_CAMPAIGN_POINTS_15 = col_number(), TOTAL_POINTS_15 = col_number(),
                                       STANDARD_POINTS_16 = col_number(), BONUS_CAMPAIGN_POINTS_16 = col_number(),
                                       TOTAL_POINTS_16 = col_number(), STANDARD_POINTS_17 = col_number(),
                                       BONUS_CAMPAIGN_POINTS_17 = col_number(), TOTAL_POINTS_17 = col_number(), TIER_15 = col_character(),
                                       TIER_16 = col_character(), TIER_17 = col_character(), TIER_15_BP = col_character(),
                                       TIER_16_BP = col_character(), TIER_17_BP = col_character(), CURRENT_TIER = col_character()))


#### ACTIVITY FLAG CALCULATION ####

# assign ACTIVITY_FLAG to members in each year with logic rules based upon spending behavior in each year and prior year tier
atg_tiers_activity <- atg_tiers %>%
  mutate(
    ACTIVITY_15 = case_when(
      AMBER_MEMBERSHIP_YEAR > 2015 ~ "Not a Member",
      AMBER_MEMBERSHIP_YEAR == 2015 ~ "New",
      AMBER_MEMBERSHIP_YEAR < 2015 & FULL_PRICE_15 > 1 ~ "Active",
      AMBER_MEMBERSHIP_YEAR < 2015 & FULL_PRICE_15 < 1 ~ "Inactive",
      TRUE ~ "NA"),
    ACTIVITY_16 = case_when(
      AMBER_MEMBERSHIP_YEAR > 2016 ~ "Not a Member",
      AMBER_MEMBERSHIP_YEAR == 2016 ~ "New",
      AMBER_MEMBERSHIP_YEAR < 2016 & ACTIVITY_15 %in% c("New", "Active") & FULL_PRICE_16 > 1 ~ "Active",
      AMBER_MEMBERSHIP_YEAR < 2016 & ACTIVITY_15 == "Inactive" & FULL_PRICE_16 < 1 ~ "Inactive",
      AMBER_MEMBERSHIP_YEAR < 2016 & ACTIVITY_15 == "Inactive" & FULL_PRICE_16 > 1 ~ "Reactivated",
      AMBER_MEMBERSHIP_YEAR < 2016 & ACTIVITY_15 %in% c("New", "Active") & FULL_PRICE_16 < 1 ~ "Lapsed",
      TRUE ~ "NA"),
    ACTIVITY_17 = case_when(
      AMBER_MEMBERSHIP_YEAR > 2017 ~ "Not a Member",
      AMBER_MEMBERSHIP_YEAR == 2017 ~ "New",
      AMBER_MEMBERSHIP_YEAR < 2017 & ACTIVITY_16 %in% c("New", "Active", "Reactivated") & FULL_PRICE_17 > 1 ~ "Active",
      AMBER_MEMBERSHIP_YEAR < 2017 & ACTIVITY_16 == "Inactive" & FULL_PRICE_17 < 1 ~ "Inactive",
      AMBER_MEMBERSHIP_YEAR < 2017 & ACTIVITY_16 %in% c("Inactive", "Lapsed") & FULL_PRICE_17 > 1 ~ "Reactivated",
      AMBER_MEMBERSHIP_YEAR < 2017 & ACTIVITY_16 %in% c("New", "Active", "Reactivated") & FULL_PRICE_17 < 1 ~ "Lapsed",
      AMBER_MEMBERSHIP_YEAR < 2017 & ACTIVITY_16 == "Lapsed" & FULL_PRICE_17 < 1 ~ "Lapsed (2yrs)",
      TRUE ~ "NA")
    ) %>% 
  filter(!(ACTIVITY_15 == "Inactive" & ACTIVITY_16 == "Inactive" & ACTIVITY_17 == "Inactive")) %>%
  filter(!(ACTIVITY_15 == "Active" & ACTIVITY_16 == "Active" & ACTIVITY_17 == "NA")) %>%
  filter(!(ACTIVITY_15 == "Active" & ACTIVITY_16 == "NA" & ACTIVITY_17 == "NA")) %>%
  filter(!(ACTIVITY_15 == "Inactive" & ACTIVITY_16 == "Inactive" & ACTIVITY_17 == "NA")) %>%
  filter(!(ACTIVITY_15 == "NA" & ACTIVITY_16 == "NA" & ACTIVITY_17 == "NA")) %>%
  filter(!(ACTIVITY_15 == "New" & ACTIVITY_16 == "Lapsed" & ACTIVITY_17 == "NA")) %>%
  filter(!(ACTIVITY_15 == "Not a Member" & ACTIVITY_16 == "New" & ACTIVITY_17 == "NA"))

# memory cleanup
rm(atg_tiers)
gc()


#### PRO-FORMA TIER ASSIGNMENT ####

# calculate STD_POINTS_15_BP using bpStandPoints function for year = 2015
atg_tiers_activity["PF_STND_POINTS_15"] <- apply(X = atg_tiers_activity, MARGIN = 1, FUN = bpStndPoints, year = 2015)

# create TOTAL_POINTS_15_BP defined as the sum of STD_POINTS_15_BP and BONUS_CAMPAIGN_POINTS_15
atg_tiers_activity["PF_TOTAL_POINTS_15"] <- atg_tiers_activity["PF_STND_POINTS_15"] + atg_tiers_activity["BONUS_CAMPAIGN_POINTS_15"]

# assign TIER_15_PROFORMA based upon TIER_15_BP, ACTIVITY_15, and TOTAL_POINTS_15_BP
atg_tiers_activity["PF_TIER_15"] <- apply(X = atg_tiers_activity, MARGIN = 1, FUN = bpTierAssign, year = 2015)


# calculate STD_POINTS_16_BP using bpStandPoints function for year = 2016
atg_tiers_activity["PF_STND_POINTS_16"] <- apply(X = atg_tiers_activity, MARGIN = 1, FUN = bpStndPoints, year = 2016)

# create TOTAL_POINTS_16_BP defined as the sum of STD_POINTS_16_BP and BONUS_CAMPAIGN_POINTS_16
atg_tiers_activity["PF_TOTAL_POINTS_16"] <- atg_tiers_activity["PF_STND_POINTS_16"] + atg_tiers_activity["BONUS_CAMPAIGN_POINTS_16"]

# assign TIER_16_PROFORMA based upon TIER_16_BP, ACTIVITY_16, and TOTAL_POINTS_16_BP
atg_tiers_activity["PF_TIER_16"] <- apply(X = atg_tiers_activity, MARGIN = 1, FUN = bpTierAssign, year = 2016)


# calculate STD_POINTS_17_BP using bpStandPoints function for year = 2017
atg_tiers_activity["PF_STND_POINTS_17"] <- apply(X = atg_tiers_activity, MARGIN = 1, FUN = bpStndPoints, year = 2017)

# create TOTAL_POINTS_16_BP defined as the sum of STD_POINTS_17_BP and BONUS_CAMPAIGN_POINTS_17
atg_tiers_activity["PF_TOTAL_POINTS_17"] <- atg_tiers_activity["PF_STND_POINTS_17"] + atg_tiers_activity["BONUS_CAMPAIGN_POINTS_17"]

# assign TIER_17_PROFORMA based upon TIER_16_BP, ACTIVITY_16, and TOTAL_POINTS_16_BP
atg_tiers_activity["PF_TIER_17"] <- apply(X = atg_tiers_activity, MARGIN = 1, FUN = bpTierAssign, year = 2017)


#### RESULTS POST-PROCESSING & OUTPUT####

# concatentate ACTIVITY in each year with TIER and select desired columns in "raw" data ouput
atg_tiers_proforma <-atg_tiers_activity %>%
  mutate(TIER_ACTIVITY_15 = paste0(ACTIVITY_15, " - ", TIER_15_BP),
         TIER_ACTIVITY_16 = paste0(ACTIVITY_16, " - ", TIER_16_BP),
         TIER_ACTIVITY_17 = paste0(ACTIVITY_17, " - ", TIER_17_BP),
         PF_TIER_ACTIVITY_15 = paste0(ACTIVITY_15, " - ", PF_TIER_15),
         PF_TIER_ACTIVITY_16 = paste0(ACTIVITY_16, " - ", PF_TIER_16),
         PF_TIER_ACTIVITY_17 = paste0(ACTIVITY_17, " - ", PF_TIER_17)) %>%
  select(UCID, AMBER_MEMBERSHIP_YEAR, FULL_PRICE_15, TOTAL_SPEND_15, FULL_PRICE_16, TOTAL_SPEND_16, FULL_PRICE_17, TOTAL_SPEND_17,
         STND_POINTS_15 = STANDARD_POINTS_15, BONUS_CAMPAIGN_POINTS_15, TOTAL_POINTS_15, TIER_15_BP, ACTIVITY_15, TIER_ACTIVITY_15, 
         STND_POINTS_16 = STANDARD_POINTS_16, BONUS_CAMPAIGN_POINTS_16, TOTAL_POINTS_16, TIER_16_BP, ACTIVITY_16, TIER_ACTIVITY_16,
         STND_POINTS_17 = STANDARD_POINTS_17, BONUS_CAMPAIGN_POINTS_17, TOTAL_POINTS_17, TIER_17_BP, ACTIVITY_17, TIER_ACTIVITY_17,
         PF_STND_POINTS_15, PF_TOTAL_POINTS_15, PF_TIER_15, PF_TIER_ACTIVITY_15, PF_STND_POINTS_16, PF_TOTAL_POINTS_16, PF_TIER_16, 
         PF_TIER_ACTIVITY_16, PF_STND_POINTS_17, PF_TOTAL_POINTS_17, PF_TIER_17, PF_TIER_ACTIVITY_17)

# write ATG_TIERS_PROFORMA to disk
atg_tiers_proforma %>%
  write_csv(output_path)


#### Migration Summarizations ####

# # summarize CURRENT STATE tier migration
# atg_tiers_proforma %>%
#   group_by(TIER_15_BP, TIER_16_BP, TIER_17_BP) %>%
#   summarize(MEMBER_COUNT = n_distinct(UCID), TOTAL_POINTS_15 = sum(TOTAL_POINTS_15), TOTAL_POINTS_16 = sum(TOTAL_POINTS_16), 
#             TOTAL_POINTS_17 = sum(TOTAL_POINTS_17), FULL_PRICE_15 = sum(FULL_PRICE_15), TOTAL_SPEND_15 = sum(TOTAL_SPEND_15), 
#             FULL_PRICE_16 = sum(FULL_PRICE_16), TOTAL_SPEND_16 = sum(TOTAL_SPEND_16), FULL_PRICE_17 = sum(FULL_PRICE_17),
#             TOTAL_SPEND_17 = sum(TOTAL_SPEND_17)) %>%
#   write_csv("./output/atg-tier-analysis-current-tiers.csv")
# 
# # summarize CURRENT STATE activity migration
# atg_tiers_proforma %>%
#   group_by(ACTIVITY_15, ACTIVITY_16, ACTIVITY_17) %>%
#   summarize(MEMBER_COUNT = n_distinct(UCID), TOTAL_POINTS_15 = sum(TOTAL_POINTS_15), TOTAL_POINTS_16 = sum(TOTAL_POINTS_16), 
#             TOTAL_POINTS_17 = sum(TOTAL_POINTS_17), FULL_PRICE_15 = sum(FULL_PRICE_15), TOTAL_SPEND_15 = sum(TOTAL_SPEND_15), 
#             FULL_PRICE_16 = sum(FULL_PRICE_16), TOTAL_SPEND_16 = sum(TOTAL_SPEND_16), FULL_PRICE_17 = sum(FULL_PRICE_17),
#             TOTAL_SPEND_17 = sum(TOTAL_SPEND_17)) %>%
#   write_csv("./output/atg-tier-analysis-current-activity.csv")

# summarize CURRENT STATE tier-activity migration
atg_tiers_proforma %>%
  group_by(ACTIVITY_15, TIER_15_BP, TIER_ACTIVITY_15, ACTIVITY_16, TIER_16_BP, 
           TIER_ACTIVITY_16, ACTIVITY_17, TIER_17_BP, TIER_ACTIVITY_17) %>%
  summarize(MEMBER_COUNT = n_distinct(UCID), TOTAL_POINTS_15 = sum(TOTAL_POINTS_15), BONUS_POINTS_15 = sum(BONUS_CAMPAIGN_POINTS_15),
            TOTAL_POINTS_16 = sum(TOTAL_POINTS_16), BONUS_POINTS_16 = sum(BONUS_CAMPAIGN_POINTS_16), TOTAL_POINTS_17 = sum(TOTAL_POINTS_17), 
            BONUS_POINTS_17 = sum(BONUS_CAMPAIGN_POINTS_17), FULL_PRICE_15 = sum(FULL_PRICE_15), TOTAL_SPEND_15 = sum(TOTAL_SPEND_15), 
            FULL_PRICE_16 = sum(FULL_PRICE_16), TOTAL_SPEND_16 = sum(TOTAL_SPEND_16), FULL_PRICE_17 = sum(FULL_PRICE_17),
            TOTAL_SPEND_17 = sum(TOTAL_SPEND_17)) %>%
  write_csv("./output/atg-tier-analysis-current-tieractivity-separate.csv")


# # summarize PROFORMA tier migration
# atg_tiers_proforma %>%
#   group_by(PF_TIER_15, PF_TIER_16, PF_TIER_17) %>%
#   summarize(MEMBER_COUNT = n_distinct(UCID), TOTAL_POINTS_15 = sum(PF_TOTAL_POINTS_15), TOTAL_POINTS_16 = sum(PF_TOTAL_POINTS_16), 
#             TOTAL_POINTS_17 = sum(PF_TOTAL_POINTS_17), FULL_PRICE_15 = sum(FULL_PRICE_15), TOTAL_SPEND_15 = sum(TOTAL_SPEND_15), 
#             FULL_PRICE_16 = sum(FULL_PRICE_16), TOTAL_SPEND_16 = sum(TOTAL_SPEND_16), FULL_PRICE_17 = sum(FULL_PRICE_17),
#             TOTAL_SPEND_17 = sum(TOTAL_SPEND_17)) %>%
#   write_csv("./output/atg-tier-analysis-proforma-tiers.csv")
# 
# # summarize PROFORMA activity migration
# atg_tiers_proforma %>%
#   group_by(ACTIVITY_15, ACTIVITY_16, ACTIVITY_17) %>%
#   summarize(MEMBER_COUNT = n_distinct(UCID), TOTAL_POINTS_15 = sum(PF_TOTAL_POINTS_15), TOTAL_POINTS_16 = sum(PF_TOTAL_POINTS_16), 
#             TOTAL_POINTS_17 = sum(PF_TOTAL_POINTS_17), FULL_PRICE_15 = sum(FULL_PRICE_15), TOTAL_SPEND_15 = sum(TOTAL_SPEND_15), 
#             FULL_PRICE_16 = sum(FULL_PRICE_16), TOTAL_SPEND_16 = sum(TOTAL_SPEND_16), FULL_PRICE_17 = sum(FULL_PRICE_17),
#             TOTAL_SPEND_17 = sum(TOTAL_SPEND_17)) %>%
#   write_csv("./output/atg-tier-analysis-proforma-activity.csv")
# 
# # summarize PROFORMA tier-activity migration
# atg_tiers_proforma %>%
#   group_by(PF_TIER_ACTIVITY_15, PF_TIER_ACTIVITY_16, PF_TIER_ACTIVITY_17) %>%
#   summarize(MEMBER_COUNT = n_distinct(UCID), TOTAL_POINTS_15 = sum(PF_TOTAL_POINTS_15), TOTAL_POINTS_16 = sum(PF_TOTAL_POINTS_16), 
#             TOTAL_POINTS_17 = sum(PF_TOTAL_POINTS_17), FULL_PRICE_15 = sum(FULL_PRICE_15), TOTAL_SPEND_15 = sum(TOTAL_SPEND_15), 
#             FULL_PRICE_16 = sum(FULL_PRICE_16), TOTAL_SPEND_16 = sum(TOTAL_SPEND_16), FULL_PRICE_17 = sum(FULL_PRICE_17),
#             TOTAL_SPEND_17 = sum(TOTAL_SPEND_17)) %>%
#   write_csv("./output/atg-tier-analysis-proforma-tieractivity.csv")

# summarize PROFORMA tier-activity migration with tier and activity in separate columns
atg_tiers_proforma %>%
  group_by(ACTIVITY_15, PF_TIER_15, PF_TIER_ACTIVITY_15, ACTIVITY_16, PF_TIER_16,
           PF_TIER_ACTIVITY_16, ACTIVITY_17, PF_TIER_17, PF_TIER_ACTIVITY_17) %>%
  summarize(MEMBER_COUNT = n_distinct(UCID), TOTAL_POINTS_15 = sum(PF_TOTAL_POINTS_15), BONUS_POINTS_15 = sum(BONUS_CAMPAIGN_POINTS_15),
            TOTAL_POINTS_16 = sum(PF_TOTAL_POINTS_16), BONUS_POINTS_16 = sum(BONUS_CAMPAIGN_POINTS_16), TOTAL_POINTS_17 = sum(PF_TOTAL_POINTS_17),
            BONUS_POINTS_17 = sum(BONUS_CAMPAIGN_POINTS_17), FULL_PRICE_15 = sum(FULL_PRICE_15), TOTAL_SPEND_15 = sum(TOTAL_SPEND_15), 
            FULL_PRICE_16 = sum(FULL_PRICE_16), TOTAL_SPEND_16 = sum(TOTAL_SPEND_16), FULL_PRICE_17 = sum(FULL_PRICE_17),
            TOTAL_SPEND_17 = sum(TOTAL_SPEND_17)) %>%
  write_csv("./output/atg-tier-analysis-proforma-tieractivity-separate.csv")
