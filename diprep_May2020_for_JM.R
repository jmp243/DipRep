# Set it to GitHub
# https://jcoliver.github.io/learn-r/010-github.html

rm(list = ls(all = TRUE))

setwd("~/Documents/diplomatic representation readings/old csv files/Dip_Rep_2021")


##################################
##
##   READ IN THE DATA
##
##################################
library(rmarkdown)
library(dplyr)
library(statnet)
library(network)

full_data <- read.csv("Diplomatic_Exchange_2006v1.csv") %>%
  # relabel so that higher is better relations
  mutate(DR_at_1 = ifelse(DR_at_1 == 9,
                          0.5,
                          DR_at_1),
         DR_at_2 = ifelse(DR_at_2 == 9,
                          0.5,
                          DR_at_2))

contiguity <- read.csv("contdird.csv") %>%
  mutate(contiguity = case_when(conttype == 1 ~ "land_river",
                                conttype == 2 ~ "water_12",
                                conttype == 3 ~ "water_24",
                                conttype == 4 ~ "water_150",
                                conttype == 5 ~ "water_400")) %>%
  select(ccode1 = state1no, ccode2 = state2no, year, contiguity, conttype) 

regions <- read.csv("states_with_regions_March1_2020 copy.csv", stringsAsFactors = FALSE)

# national material capability - we'll use the composite indicator cinc
nmc <- read.csv("NMC_5_0.csv", stringsAsFactors = FALSE)


##################################
##
##   PART 1: FORMATTING THE DATA
##
##################################

source("helper_functions.R") 


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

fulldata = read.csv("Diplomatic_Exchange_2006v1.csv", header=T)


##################################
##
##   PART 1: FORMATTING THE DATA
##
##################################

###############################
##
##   remove pairs with no diplomatic relations
##
data = fulldata[which(fulldata$DR_at_1>0 | fulldata$DR_at_2>0), ]
###############################


###############################
##
##   remove version and DE columns
##
data = data[ ,-which(names(data)=="version" | names(data)=="DE")]
###############################


###############################
##
##   remove duplicates
##
allPairs = paste(data$ccode1,",",data$ccode2,sep="")
allPairsReversed = paste(data$ccode2,",",data$ccode1,sep="")
nInclDuplicates = length(unique(allPairs))
nCreatingDuplicates = length(unique(c(allPairs,allPairsReversed)))
if(nCreatingDuplicates > nInclDuplicates) {
  cat("uh oh, not every pairing has a duplicate")
} else {
  cat("good, it's easy to remove the duplicates \n")
}
data = data[which(data$ccode1<data$ccode2), ]


###############################
##
##   add period
##
nrows = nrow(data)
period = rep(0, nrows)
period[which(data$year<=1940)] = 			  "1"
period[which(data$year>1940 & data$year<=1965)] = "2"
period[which(data$year>1965 & data$year<=1990)] = "3"
period[which(data$year>1990 & data$year<=2005)] = "4" 
data$period = period                         #attach new column to dataset
###############################


###############################
##
##   add dipSym
##
dipSym = rep(0, nrows)
dipSym[which(data$DR_at_1 == data$DR_at_2)] = "sym"        #all cases with same
dipSym[which(data$DR_at_1 != data$DR_at_2)] = "asym"       #all cases with different
dipSym[which(xor(data$DR_at_1==0, data$DR_at_2==0))] = "x"  #all cases in which one but not both are 0
data$dipSym = dipSym     		                        #attach new column to dataset
###############################
##
##   add dipSymNext
##
pairCodes = paste(data$ccode1,",",data$ccode2,sep="")
uniqueCodes = unique(pairCodes)
dipSymNext = rep(NA, nrows)

for (i in 1:length(uniqueCodes)){
  inds = which(pairCodes==uniqueCodes[i])
  lenInds = length(inds)
  
  if (lenInds > 1) { 
    for (row in 1:(lenInds-1))
      dipSymNext[inds[row]] = data$dipSym[inds[row+1]]
  }
  
  if (data$year[inds[lenInds]]==2005) {
    dipSymNext[inds[lenInds]] = "limit"
  } else {
    dipSymNext[inds[lenInds]] = "end"
  }
}
data$dipSymNext = dipSymNext
data = data[which(data$dipSymNext != "limit"), ]
###############################

##
##   add time in current state
##
pairCodes = paste(data$ccode1,",",data$ccode2,sep="")
uniqueCodes = unique(pairCodes)
time.curr = rep(0, nrow(data))

for (i in 1:length(uniqueCodes)){
  inds = which(pairCodes==uniqueCodes[i])
  lenInds = length(inds)
  
  if (lenInds > 1) {
    for (row in 2:(length(inds))){			
      # There was not a state change, so increase time_curState
      if (data$dipSym[inds[row]] == data$dipSym[inds[row-1]]) { 
        time.curr[inds[row]] = time.curr[inds[row-1]] + data$year[inds[row]]- data$year[inds[row-1]]
        
        # or, there was a state change, so time.curr is zero
      } else { 
        time.curr[inds[row]] = 0
      }
    }
  }
}
data$time.curr = time.curr
###############################
##   merge regions
##
regionCodes = read.csv("states_with_regions_March1_2020.csv", header=T)

region = rep(0, nrow(data))
regionCodes = regionCodes[which(!duplicated(regionCodes$CCode)), ]  #remove duplicates
codeToRegMap = rep(0, length(regionCodes))
codeToRegMap[regionCodes$CCode] = regionCodes$Region
region1 = codeToRegMap[data$ccode1]
region2 = codeToRegMap[data$ccode2]
data$region1 = region1
data$region2 = region2
data$sameregion = ifelse(data$region1==data$region2, "yes", "no")
region[which(data$region1==1 & data$region2==1)] = 1
region[which(data$region1==2 & data$region2==2)] = 2
region[which(data$region1==3 & data$region2==3)] = 3
region[which(data$region1==4 & data$region2==4)] = 4
region[which(data$region1==5 & data$region2==5)] = 5
region[which((data$region1==1 & data$region2==2) | (data$region1==2 & data$region2==1))] = 6
region[which((data$region1==1 & data$region2==3) | (data$region1==3 & data$region2==1))] = 7
region[which((data$region1==1 & data$region2==4) | (data$region1==4 & data$region2==1))] = 8
region[which((data$region1==1 & data$region2==5) | (data$region1==5 & data$region2==1))] = 9
region[which((data$region1==2 & data$region2==3) | (data$region1==3 & data$region2==2))] = 10
region[which((data$region1==2 & data$region2==4) | (data$region1==4 & data$region2==2))] = 11
region[which((data$region1==2 & data$region2==5) | (data$region1==5 & data$region2==2))] = 12
region[which((data$region1==3 & data$region2==4) | (data$region1==4 & data$region2==3))] = 13
region[which((data$region1==3 & data$region2==5) | (data$region1==5 & data$region2==3))] = 14
region[which((data$region1==4 & data$region2==5) | (data$region1==5 & data$region2==4))] = 15
data$region = region
###############################

table(data$sameregion)

#### Jung Mee Park
#### June 29, 2021


library(ergm)
options(ergm.term=list(version="3.9.4"))
