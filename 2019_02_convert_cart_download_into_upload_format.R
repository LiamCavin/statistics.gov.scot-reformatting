#=============================================================================
#
# Script for formatting data extracted from statistics.gov.scot using data 
# cart, into the correct format for upload using the pipeline
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


# Load in the data
# 
#======================

# you can access the cart file from https://statistics.gov.scot/carts/8ab85ac1-4745-4a53-96b9-5152a5c69629
# or load locally as below

cart <- read.csv("cart download 080319 ew counts 2015 and 2016.csv")
head(cart)
colnames(cart)
dim(cart)

# create function to reformat data into statistics.gov.scot upload format
#========================================================================

library("stringr", lib.loc="C:/Program Files/R/R-3.3.2/library")

pipe.format <- function(x,y) {
  pipe <- data.frame(str_sub(x[,1], start = -9))
  names(pipe) <- "GeographyCode"
  pipe$DateCode <- substring(colnames(x)[y], regexpr("Period...", colnames(x)[y]) + 9, regexpr("Type.Of", colnames(x)[y]) -3)
  pipe$Measurement <- "Count"
  pipe$Units <- "Dwellings"
  pipe$Value <- x[,y]
  pipe$Typeofdwelling <- substring(colnames(x)[y], regexpr("Of.Dwelling...", colnames(x)[y]) + 14, regexpr("..measure", colnames(x)[y]) -1)
  return(pipe)
}


# run reformating function on dataset
#=====================================

# apply function to first column of data in raw data object
pipe1 <- pipe.format(cart,3)
head(pipe1)
dim(pipe1)
dim(cart)

# apply function to all subsequent columns in object
for (i in 4:14){
  pipe1 <- rbind(pipe1, pipe.format(cart,i))
}


# Reformat the text in the Time_Period column
#============================================

colnames(pipe1)
colnames(pipe1)[6] <- "Type Of Dwelling"
head(pipe1)

# Reformat one of the the dwelling type names
#============================================

pipe1[,6] <-  str_replace_all(pipe1[,6], fixed("Semi.Detached"), "Semi-Detached")
unique(pipe1[,6])


# Output a file, ready for the pipeline
#======================================

write.csv(pipe1, "dwellings ew 2015 2016 pipeline.csv", row.names=FALSE)

# yaldi
