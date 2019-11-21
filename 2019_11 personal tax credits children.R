#=============================================================================
#
# Script for formatting HMRC personal tax credit data for
# upload to statistics.gov.scot
#
# Source for data:
# https://www.gov.uk/government/statistics/personal-tax-credits-finalised-award-statistics-small-area-data-lsoa-and-data-zone-2017-to-2018
# https://www.gov.uk/government/statistics/personal-tax-credits-finalised-award-statistics-small-area-data-lsoa-and-data-zone-2016-to-2017
# https://www.gov.uk/government/statistics/personal-tax-credits-finalised-award-statistics-small-area-data-lsoa-and-data-zone-2015-to-2016
# https://www.gov.uk/government/statistics/personal-tax-credits-finalised-award-statistics-small-area-data-lsoa-and-data-zone-2014-to-2015
# https://www.gov.uk/government/statistics/personal-tax-credits-finalised-award-statistics-small-area-data-lsoa-and-data-zone-2013-to-2014
# 
# Data, Statistics and Outcomes
# Scottish Government
# 
# November 2019 Liam Cavin x44092
#
#=============================================================================
#*****************************************************************************
#=============================================================================


### 1 - Housekeeping ---------------------------------------------------------
#=============================================================================

# clear global environment
rm(list=ls())


# load in the approriate packages needed for this script
library("tidyverse")
library("dplyr")
library("stringr")
library("readxl")

# set the working directory
setwd("C:\\Users\\liam\\Desktop\\My files\\Data")
setwd("//scotland.gov.uk//dc1//fs3_home//u441625//Statistics.gov.uk//Data Loading//2019.11 Tax Credits")

list.files()


### 2 Load in the HMRC data (locally saved) ----------------------------------
#=============================================================================

# set vectors with which to populate column names
colnames_lachildren <- c("GeographyCode", "", "", "", "", "", "", "AllTaxCredit", "",
                       "InWorkWTCAndCTC", "CTC", "TotalInWork", "TotalLoneParent",
                       "", "AllOutOfWork", "OutOfWorkLoneParent", "OutOfWorkCouples")

colnames_dzchildren <- c("", "", "GeographyCode", "", "", "", "AllTaxCredit", "",
                         "InWorkWTCAndCTC", "CTC", "TotalInWork", "TotalLoneParent",  
                         "", "AllOutOfWork", "OutOfWorkLoneParent", "OutOfWorkCouples")

# specify the ones you'll want to keep
keepcols <- c("GeographyCode", "AllTaxCredit", "InWorkWTCAndCTC", 
              "CTC", "TotalInWork", "TotalLoneParent",  
              "AllOutOfWork", "OutOfWorkLoneParent", "OutOfWorkCouples")

# get la data, drop junk columns, and replace "-" with 0
la_children <- data.frame(read_xlsx("LA1718.xlsx", sheet = 2, range = "B470:r503", 
                             col_names = colnames_lachildren))
la_children <- la_children[-2, keepcols]
la_children[la_children=="-"] <- 0

# get dz data, drop junk columns , and replace "-" with 0
dz_children <- data.frame(read_xlsx("DZ1718.xlsx", sheet = 2, range = "c9:r6984", 
                                        col_names = colnames_dzchildren))
dz_children <- dz_children[, keepcols]
dz_children[dz_children=="-"] <- 0

# make all columns numeric
for (i in 2:9) {
  la_children[,i] <- as.numeric(la_children[,i]) 
}
sapply(la_children, class)

for (i in 2:9){
  dz_children[,i] <- as.numeric(dz_children[,i]) 
}
sapply(dz_children, class)


### 3 Create function to reformat raw data into tidy -------------------------
#=============================================================================

# where x is data frame, y is column to apply reformatting to, z is year, 
# and w is the multiplication factor (la figures are in thousands)
pipe.format <- function(x,y,z,w) {
  pipe <- data.frame(x[,1])  
  names(pipe) <- "GeographyCode"      
  pipe$DateCode <-  z             
  pipe$Measurement <- "Count"
  pipe$Units <- "Children"
  pipe$Value <- x[,y]*w
  pipe$WorkStatus <- colnames(x[y])                   
  pipe$TaxCreditType <- colnames(x[y])        
  pipe$FamilyType <- colnames(x[y])
  return(pipe)
}


### 4 Create function to recode text strings in dimensions -------------------
#=============================================================================

reformat.tax.credits <- function(x) {
  reformat <- x  
  reformat[,"WorkStatus"] <- str_replace_all(x[,"WorkStatus"], c("AllTaxCredit" = "All", "InWorkWTCAndCTC" = "In Work", "CTC" = "In Work", "WTC" = "In Work", "TotalInWork" = "In Work",
                                                                 "InWorkChildcare" = "In Work", "TotalLoneParent" = "In Work", "LoneParentChildcare" = "In Work", 
                                                                 "AllOutOfWork" = "Out of Work", "OutOfWorkLoneParent" = "Out of Work", "OutOfWorkCouples" = "Out of Work"))
  
  reformat[,"TaxCreditType"] <- str_replace_all(x[,"TaxCreditType"], c("AllTaxCredit" = "All", "InWorkWTCAndCTC" = "Working Tax Credit and Child Tax Credit", "CTC" = "Child Tax Credit Only", "WTC" = "Working Tax Credit Only", "TotalInWork" = "All",
                                                                       "InWorkChildcare" = "Working Tax Credit With Childcare Element", "TotalLoneParent" = "All", "LoneParentChildcare" = "Working Tax Credit With Childcare Element", 
                                                                       "AllOutOfWork" = "Child Tax Credit Only", "OutOfWorkLoneParent" = "Child Tax Credit Only", "OutOfWorkCouples" = "Child Tax Credit Only"))
  
  reformat[,"FamilyType"] <- str_replace_all(x[,"FamilyType"], c("AllTaxCredit" = "All", "InWorkWTCAndCTC" = "All", "CTC" = "All", "WTC" = "All", "TotalInWork" = "All",
                                                                 "InWorkChildcare" = "All", "TotalLoneParent" = "Lone Parent", "LoneParentChildcare" = "Lone Parent", 
                                                                 "AllOutOfWork" = "All", "OutOfWorkLoneParent" = "Lone Parent", "OutOfWorkCouples" = "Couples"))
  
  colnames(reformat) <- c("GeographyCode", "DateCode", "Measurement", "Units", "Value", "Work Status", "Tax Credit Type", "Family Type")
  return(reformat)
}


### 5 Reformat raw data into tidy           ----------------------------------
#=============================================================================

laf_tidy <- pipe.format(la_children, 2, "2016/17", 1000)
for (i in 3:12){
  laf_tidy <- rbind(laf_tidy, pipe.format(la_children, i, "2016/17", 1000))
}

dzf_tidy <- pipe.format(dz_children, 2, "2016/17", 1)
for (i in 3:12){
  dzf_tidy <- rbind(dzf_tidy, pipe.format(dz_children, i, "2016/17", 1))
}

laf_tidy <- reformat.tax.credits(laf_tidy)
dzf_tidy <- reformat.tax.credits(dzf_tidy)

children_forupload <- rbind(laf_tidy, dzf_tidy)

# Lastly, the HMRC data includes some obsolete geography codes. Replace them.
children_forupload[,1] <-  str_replace_all(children_forupload[,1], fixed("S12000015"), "S12000047")
children_forupload[,1] <-  str_replace_all(children_forupload[,1], fixed("S12000024"), "S12000048")
children_forupload[,1] <-  str_replace_all(children_forupload[,1], fixed("S12000044"), "S12000050")
children_forupload[,1] <-  str_replace_all(children_forupload[,1], fixed("S12000046"), "S12000049")

write.csv(children_forupload, "children1617_forupload.csv", row.names=FALSE)

# yaldi
