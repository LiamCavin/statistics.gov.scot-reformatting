#===================================================================
#
# Script for reformatting household waste data from SEPA
# https://www.sepa.org.uk/environment/waste/waste-data/waste-data-reporting/household-waste-data/
#
# And putting to use some of the data wrangling tricks from 
# https://suzan.rbind.io/2018/01/dplyr-tutorial-1/
# 
# Data, Statistics and Outcomes
# Scottish Government
# April 2018 Liam Cavin x44092
#
#==================================================================
#******************************************************************
#==================================================================




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

setwd("")
list.files()


# Load in the data
#===================

# I've already unpicked some of the hand crafting of individual files for individual years, condensing it
# all to a simple csv

housewaste <- read.csv("Household waste.csv", stringsAsFactors = FALSE)

# take a peek
housewaste %>%
  glimpse %>%
  summary 
head(housewaste)

# a function to reformat selected columns from raw data into tidy format
waste.format <- function(x,y) {
  pipe <- data.frame(str_sub(x[,10]))
  names(pipe) <- "GeographyCode"
  pipe$DateCode <-  data.frame(str_sub(x[,9]))
  colnames(pipe[,2]) <- 'DateCode'
  pipe$Measurement <- "Count"
  pipe$Units <- "Tonnes"
  pipe$Value <- x[,y]
  pipe$householdwaste <- colnames(x)[y]
  return(pipe)
}

# apply function to 4 columns, results as tidy for upload
totalwaste <- waste.format(housewaste, 2)
recycledwaste <- waste.format(housewaste,3)
landfilledwaste <- waste.format(housewaste,7)
otherwaste <- waste.format(housewaste,5)

#calculate ratios, results as tidy for upload
recyledratio <- recycledwaste
recyledratio[,5] <- round(100*(recycledwaste[,5]/totalwaste[,5]),1)
recyledratio[,3] <- "Ratio"
recyledratio[,4] <- "Percent Of Waste"


landfillratio <- landfilledwaste
landfillratio[,5] <- round(100*(landfilledwaste[,5]/totalwaste[,5]),1)
landfillratio[,3] <- "Ratio"
landfillratio[,4] <- "Percent Of Waste"

otherratio <- otherwaste
otherratio[,5] <- round(100*(otherwaste[,5]/totalwaste[,5]),1)
otherratio[,3] <- "Ratio"
otherratio[,4] <- "Percent Of Waste"

# combine all the tidy data for upload
all.waste <- rbind(totalwaste, recycledwaste, recyledratio, landfilledwaste, landfillratio, otherwaste, otherratio)

# rename variables, and remove commas from values
all.waste[,5] <-  str_replace_all(all.waste[,5], fixed(","), "")
all.waste[,6] <-  str_replace_all(all.waste[,6], fixed("Generated..tonnes."), "Waste Generated")
all.waste[,6] <-  str_replace_all(all.waste[,6], fixed("Recycled..tonnes."), "Waste Recycled")
all.waste[,6] <-  str_replace_all(all.waste[,6], fixed("Landfilled..tonnes."), "Waste Landfilled")
all.waste[,6] <-  str_replace_all(all.waste[,6], fixed("Other.diversion.from.landfill...tonnes."), "Other Diversion From Landfill")
names(all.waste)[6]<-"Waste"

# check this looks ok
head(all.waste)
unique(all.waste[,6])

# and save the file, ready for upload
write.csv(all.waste, "household waste for upload.csv", row.names=FALSE)

# yaldi          
