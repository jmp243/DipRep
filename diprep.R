

library(dplyr)

##################################
##
##   READ IN THE DATA
##
##################################


full_data <- read.csv("data/Diplomatic_Exchange_2006v1.csv") %>%
    # relabel so that higher is better relations
    mutate(DR_at_1 = ifelse(DR_at_1 == 9,
                            0.5,
                            DR_at_1),
           DR_at_2 = ifelse(DR_at_2 == 9,
                            0.5,
                            DR_at_2))

contiguity <- read.csv("data/contdird.csv") %>%
    mutate(contiguity = case_when(conttype == 1 ~ "land_river",
                                  conttype == 2 ~ "water_12",
                                  conttype == 3 ~ "water_24",
                                  conttype == 4 ~ "water_150",
                                  conttype == 5 ~ "water_400")) %>%
    select(ccode1 = state1no, ccode2 = state2no, year, contiguity, conttype) 

regions <- read.csv("data/states_with_regions_March1_2020.csv")

# national material capability - we'll use the composite indicator cinc
nmc <- read.csv("data/NMC_5_0.csv")
   

##################################
##
##   PART 1: FORMATTING THE DATA
##
##################################


source("helper_functions.R")

d <- full_data %>%
    select(-version, -DE) %>%
    removeDuplicates() %>%
    labelBadYears() %>%
    mutate(period = case_when(year <= 1920 ~ 1,
                              year > 1920 & year <= 1950 ~ 2,
                              year > 1950 & year <= 1990 ~ 3,
                              year > 1990 ~ 4)) %>%
    left_join(contiguity) %>%
    mutate(is_close = !is.na(conttype) & conttype <= 3) %>%
    addTransition() %>%
    addTimeInCurrentState() %>%
    addPrevSymStatus() %>%
    addRegions(regions) %>%
    addNMC(nmc)


###################################################
##
##   PART 2: ANALYSIS
##
###################################################

# 1. P(not downgrade) higher within same region (after controlling for nmc, contiguity)?
#    highest in regions 1 and 5 without border violence?
# 2. P(not downgrade given started in sym) across periods to show it increases over time?
# 3. comparing P(not downgrade given start in sym) to P(not downgrade given start in asym).
#    similar for P(x, x).




