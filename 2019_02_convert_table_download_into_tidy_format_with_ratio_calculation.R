#=============================================================================
#
# Script for formatting data extracted from statistics.gov.scot using 
# the data cart, calculating ratios and then
# manipulating into the correct format for upload using the 
# pipeline
# 
# Data, Statistics and Digital Outcomes
# Scottish Government
# February 2019 Liam Cavin x44092
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

setwd("//scotland.gov.uk//dc1//fs3_home//u441625//Statistics.gov.uk//Data Loading//2019.02 dwellings by type spc fix")
list.files()


# Load in the data dwellings by type, as extracted from
# statistics.gov.scot using the data cart
#==================================================================


cart <- read.csv("dwellings 2014 to 2017 counts cart extract.csv", stringsAsFactors = FALSE)

head(cart)
dim(cart)
colnames(cart)


# Calculate the ratio of the various dwellings types
#===================================================

# function which calculates ratios for any specified 'all' dwellings column
ratio.calc <- function(x,y) {
  ratios <-                data.frame(cbind(str_sub(x[,1],53), str_sub(colnames(x[y+1]),64,71), str_sub(colnames(x[y+1]),39,42), (x[,y+1]/x[,y])*100))
  ratios <-  rbind(ratios, data.frame(cbind(str_sub(x[,1],53), str_sub(colnames(x[y+2]),64,71), str_sub(colnames(x[y+1]),39,42), (x[,y+2]/x[,y])*100)))
  ratios <-  rbind(ratios, data.frame(cbind(str_sub(x[,1],53), str_sub(colnames(x[y+3]),64,71), str_sub(colnames(x[y+1]),39,42), (x[,y+3]/x[,y])*100)))
  ratios <-  rbind(ratios, data.frame(cbind(str_sub(x[,1],53), str_sub(colnames(x[y+4]),64,71), str_sub(colnames(x[y+1]),39,42), (x[,y+4]/x[,y])*100)))
  ratios <-  rbind(ratios, data.frame(cbind(str_sub(x[,1],53), str_sub(colnames(x[y+5]),64,71), str_sub(colnames(x[y+1]),39,42), (x[,y+5]/x[,y])*100)))
  return(ratios)
  }

head(ratios)
tail(ratios)
dim(ratios)
str(ratios)
summary(ratios)

# calculate ratios for the 4 years in the cart data
ratios <- rbind(ratio.calc(cart,3),
                ratio.calc(cart,9),
                ratio.calc(cart,15),
                ratio.calc(cart,21))

# add the other columns needed for the pipeline
ratios$Measurement <- "Ratio"
ratios$Units <- "Percent Of Dwellings"

# rename the columns
colnames(ratios) <- c("GeographyCode", "Type Of Dwelling", "DateCode", "Value", "Measurement", "Units")

# fix the dwelling type names
ratios[,2] <- str_replace_all(ratios[,2], fixed("Flats..m"), "Flats")
ratios[,2] <- str_replace_all(ratios[,2], fixed("Semi.Det"), "Semi-Detached")
ratios[,2] <- str_replace_all(ratios[,2], fixed("Unknown."), "Unknown")
unique(ratios[,2])

# round the decimal places of values
ratios[,4] <- as.numeric(as.character(ratios[,4]))
ratios[,4] <- round(ratios[,4],1)

head(ratios)
tail(ratios)
dim(ratios)
str(ratios)
summary(ratios)


# Save the file, ready for upload
#================================

write.csv(ratios, "dwellings by type ratios for pipeline.csv", row.names=FALSE)


# yaldi
