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

setwd("//scotland.gov.uk//dc1//fs3_home//u441625//Statistics.gov.uk//Data Loading//2019.05 dwellings ct band")
list.files()


# Load in the data dwellings by type, as extracted from
# statistics.gov.scot using the data cart
#==================================================================


rawratio <- read.csv("spc ew ratios for munging.csv", stringsAsFactors = FALSE)

head(rawratio)
dim(rawratio)
colnames(rawratio)


# Create a function to reformat the data into tidy

tidymunge <- function(x,y){
  tidy <- data.frame(cbind(x[,1], 
          x[,y], 
          substring(colnames(x)[y], regexpr("Tax.Band...", colnames(x)[y]) + 29, regexpr("Ref", colnames(x)[y]) - 3)),
          substring(colnames(x)[y], regexpr("Period...", colnames(x)[y]) + 9, regexpr("..measure", colnames(x)[y]) -1),
          "Ratio",
          "Percent Of Dwellings")
  colnames(tidy) <- c("GeographyCode", "Value", "Council Tax Band", "DateCode", "Measurement", "Units")
  return(tidy)
}

# apply the function to the raw ratio data

tidy <- tidymunge(rawratio,2)

for(i in 3:45){
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
tidy[,3] <- str_replace_all(tidy[,3], fixed("."), " ")
tidy[,3] <- str_replace_all(tidy[,3], fixed("A C"), "A-C")
tidy[,3] <- str_replace_all(tidy[,3], fixed("D E"), "D-E")
tidy[,3] <- str_replace_all(tidy[,3], fixed("F H"), "F-H")




# Save the file, ready for upload
#================================

write.csv(tidy, "ratios for pipeline.csv", row.names=FALSE)


# yaldi
