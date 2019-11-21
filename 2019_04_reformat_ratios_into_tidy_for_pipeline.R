#=============================================================================
#
# Script for formatting ratios data into statistics.gov.scot pipeline
# tidy data for upload
#
# Data, Statistics and Digital Outcomes
# Scottish Government
# April 2019 Liam Cavin x44092
#
#=============================================================================
#*****************************************************************************
#=============================================================================



# start with a clean slate
#=========================

rm(list=ls())


# load in the approriate package
# and any others that are needed 
# for this script.
#===============================

# the tidyverse
install.packages("tidyverse")


# set the working directory
#===========================

setwd("//scotland.gov.uk//dc1//fs3_home//u441625//Statistics.gov.uk//Data Loading//2019.04 dwellings by room spc fix")
list.files()


# Load in the data dwellings by type, as extracted from
# statistics.gov.scot using the data cart
#==================================================================


rawratio <- read.csv("dwellings by room ew spc ratios for munging.csv", stringsAsFactors = FALSE)

head(rawratio)
dim(rawratio)
colnames(rawratio)


# Create a function to reformat the data into tidy

tidymunge <- function(x,y){
  tidy <- data.frame(cbind(x[,1], 
          x[,y], 
          substring(colnames(x)[y], regexpr("Of.Rooms", colnames(x)[y]) + 11, regexpr("Ref", colnames(x)[y]) - 3)),
          substring(colnames(x)[y], regexpr("Period...", colnames(x)[y]) + 9, regexpr("..measure", colnames(x)[y]) -1),
          substring(colnames(x)[y], regexpr("type...", colnames(x)[y]) + 7),
          "Percent Of Dwellings")
  colnames(tidy) <- c("GeographyCode", "Value", "Number of Rooms", "DateCode", "Measurement", "Units")
  return(tidy)
}

# apply the function to the raw ratio data

tidy <- tidymunge(rawratio,3)

for(i in 4:46){
  tidy <- rbind(tidy, tidymunge(rawratio,i))
  
}

head(tidy)
unique(tidy[1])
unique(tidy[2])
unique(tidy[3])
unique(tidy[4])
unique(tidy[5])
unique(tidy[6])


# fix the dwelling type names
tidy[,3] <- str_replace_all(tidy[,3], fixed("10."), "10")

# remove the missing values
tidy <-tidy[!(tidy[2]=="#DIV/0!"),]


# Save the file, ready for upload
#================================

write.csv(tidy, "ratios for pipeline.csv", row.names=FALSE)


# yaldi
