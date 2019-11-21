#=============================================================================
#
# Script for formatting Scottish Fire and Rescue Service data for
# upload to statistics.gov.scot
#
# raw data from https://www.firescotland.gov.uk/about-us/fire-and-rescue-statistics.aspx
# 
# Data, Statistics and Outcomes
# Scottish Government
# 
# November 2019 Liam Cavin x44092
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
library("dplyr")
library("stringr")

# set the working directory
#===========================

setwd("")

list.files()

# Load in the SFRS data
#======================

rawfire<- read.csv("firesbydatazone.csv")

# extract data for most recent 2 years, as previous years data is revised
fire<- subset(rawfire, FiscalYear == '2017-18'| FiscalYear == '2018-19')

# in accident column, 1 = accidental fire, 0 = not accidental fire
# in data zone column, some fires recorded as unknown data zone but assigned to council area

# Calculate aggregate figures for all fires of any type and accident status,
# for any datazone in each year.
# Then format for statistics.gov.scot
#===========================================================================

library("dplyr")
datazones.agg <- fire %>% group_by(DataZone, FiscalYear) %>% summarize(count=n()) %>% data.frame    # count the number of fires for each datazone in each fiscal year
datazones.agg <- datazones.agg[ grep(":unknown", datazones.agg$DataZone, invert = TRUE) , ]         # drop the rows where datazone is unknown, and make the results into a standard data frame
names(datazones.agg)  <- c("GeographyCode", "DateCode", "Value")                                    # renames some columns
library("stringr")
datazones.agg$DateCode <-  str_replace_all(datazones.agg$DateCode, "-", "/")                        # change date notation to correct format
datazones.agg$Measurement <- "Count"                                                                # create additional columns in correct format
datazones.agg$Units <- "Fires"                                                            #
datazones.agg$"Type of Fire" <- "All"                                                               #
datazones.agg$"Accident Status" <- "All"                                                            #

# Calculate aggregate figures for each type of fire and only 'all' accident status,
# for any datazone in each year.
#===========================================================================

datazones.allaccident <- fire %>% group_by(DataZone, FiscalYear, IncidentType) %>% summarize(count=n()) %>% data.frame  # count the number of fires for each datazone for each incident type in each fiscal year
datazones.allaccident <- datazones.allaccident[ grep(":unknown", datazones.allaccident$DataZone, invert = TRUE) , ]     # drop the rows where datazone is unknown, and make the results into a standard data frame
names(datazones.allaccident)  <- c("GeographyCode", "DateCode", "Type of Fire", "Value")                                # renames some columns
datazones.allaccident$DateCode <-  str_replace_all(datazones.allaccident$DateCode, "-", "/")                            # change date notation to correct format
datazones.allaccident$Measurement <- "Count"                                                                            # create additional columns in correct format
datazones.allaccident$Units <- "Fires"                                                                        #
datazones.allaccident$"Accident Status" <- "All"                                                                        #

# Calculate aggregate figures for each accident status and only 'all' type of fire,
# for any datazone in each year.
#===========================================================================

datazones.alltype <- fire %>% group_by(DataZone, FiscalYear, Accidental) %>% summarize(count=n()) %>% data.frame        # count the number of fires for each datazone for each accident status in each fiscal year
datazones.alltype <- datazones.alltype[ grep(":unknown", datazones.alltype$DataZone, invert = TRUE) , ]                 # drop the rows where datazone is unknown, and make the results into a standard data frame
names(datazones.alltype)  <- c("GeographyCode", "DateCode", "Accident Status", "Value")                                 # renames some columns
datazones.alltype$DateCode <-  str_replace_all(datazones.alltype$DateCode, "-", "/")                                    # change date notation to correct format
datazones.alltype$Measurement <- "Count"                                                                                # create additional columns in correct format
datazones.alltype$Units <- "Fires"                                                                            #
datazones.alltype$"Type of Fire" <- "All"                                                                               #
datazones.alltype$"Accident Status" <-  str_replace_all(datazones.alltype$"Accident Status",                            # replace accident codes with text
                                                        c("1" = "Accidental", "0" = "Not Accidental"))

# Calculate aggregate figures for all fires for any datazone in each year, 
# broken down by accident status and type of fire.
# Then format for statistics.gov.scot
#===========================================================================

datazones.breakdown <- fire %>% group_by(DataZone, FiscalYear, Accidental, IncidentType) %>%                     # count the number of fires for each datazone in each fiscal year
                                summarize(count=n()) %>% data.frame                                              #
datazones.breakdown <- datazones.breakdown[ grep(":unknown", datazones.breakdown$DataZone, invert = TRUE) , ]    # drop the rows where datazone is unknown, and make the results into a standard data frame
names(datazones.breakdown)  <- c("GeographyCode", "DateCode", "Accident Status", "Type of Fire", "Value")        # rename some columns
datazones.breakdown$DateCode <-  str_replace_all(datazones.breakdown$DateCode, "-", "/")                         # change date notation to correct format
datazones.breakdown$"Accident Status" <-  str_replace_all(datazones.breakdown$"Accident Status",                 # change accident status to words rather than codes  
                                              c("1" = "Accidental", "0" = "Not Accidental"))                     #
datazones.breakdown$Measurement <- "Count"                                                                       # create additional columns in correct format
datazones.breakdown$Units <- "Fires"                                                                   #

# Reorder the columns in the dataframes, and merge them
#======================================================

datazones.breakdown <- datazones.breakdown[,c('GeographyCode', 'DateCode', 
                         'Measurement', 'Units', 'Value', 'Accident Status', 'Type of Fire')]
datazones.agg <- datazones.agg[,c('GeographyCode', 'DateCode', 
                         'Measurement', 'Units', 'Value', 'Accident Status', 'Type of Fire')]                        
datazones.alltype <- datazones.alltype[,c('GeographyCode', 'DateCode', 
                                  'Measurement', 'Units', 'Value', 'Accident Status', 'Type of Fire')] 
datazones.allaccident <- datazones.allaccident[,c('GeographyCode', 'DateCode', 
                                  'Measurement', 'Units', 'Value', 'Accident Status', 'Type of Fire')] 

datazones.fire.counts <- rbind(datazones.agg, datazones.breakdown, datazones.alltype, datazones.allaccident)          # concatenate the dataframes into final object

# but but but, most datazones have 0 fires for most combinations of variables, and these aren't reflected in the counts
# create a dataframe of all potential combinations of datazones, dates, accident status and type of fire

list.datazones <- sprintf("S0%d", seq(1006506,1013481))                                                    # all datazones
list.datecode <- unique(datazones.fire.counts[,2])                                                                     # all dates
list.accident <- unique(datazones.fire.counts[,6])                                                                     # all accident statuses
list.type     <- unique(datazones.fire.counts[,7])                                                                     # all types of fire
all.possible.observations <- expand.grid(list.datazones, list.datecode, list.accident, list.type)          # create table of all combinations   
names(all.possible.observations)  <- c("GeographyCode", "DateCode", "Accident Status", "Type of Fire")     # name the columns
all.possible.observations$Measurement <- "Count"                                                           # add measurement variable
all.possible.observations$"Units" <- "Fires"                                                     # add units variable

datazones.inc.nulls <- merge(all.possible.observations, datazones.fire.counts, all = TRUE)                 # merge the counts of fires with the potential combinations
datazones.inc.nulls[is.na(datazones.inc.nulls)] <- 0                                                       # replace NA values with 0

write.csv(datazones.inc.nulls, "datazone 2018_19 fire data for upload to odpp.csv", row.names=FALSE)               # save as csv

# running a very big dataset through the pipeline could be tricky, so save yearly slices of the data
write.csv(subset(datazones.inc.nulls, datazones.inc.nulls$DateCode == "2017/18"), "datazone fire data 2017-18.csv", row.names=FALSE)
write.csv(subset(datazones.inc.nulls, datazones.inc.nulls$DateCode == "2018/19"), "datazone fire data 2018-19.csv", row.names=FALSE)


# ***********************************************************************************************
#
# Some fires couldn't be attributed to datazone, but could be attributed to council area.
# Thus, geo-aggregation pipeline should be used to upload datazone level info (from above file)
# to aggregate to higher level geographies. Then, council and scotland figures should be 
# deleted, and the files generated below uploaded
#
# ***********************************************************************************************

# clear the global environment and reload the raw data
rm(list=ls())
rawfire<- read.csv("FiresByDatazone.csv")
fire<- subset(rawfire, FiscalYear == '2017-18'| FiscalYear == '2018-19')

# Calculate aggregate figures for all fires of any type and accident status,
# for any local authority in each year. Same for Scotland.
# Then format for statistics.gov.scot
#===========================================================================

la.agg <- fire %>% group_by(LocalAuthority, FiscalYear) %>% summarize(count=n()) %>% data.frame   # count the number of fires for each local authority in each fiscal year
scot.agg <- fire %>% group_by(FiscalYear) %>% summarize(count=n()) %>% data.frame              # do the same for scotland as a whole 
scot.agg$LocalAuthority <- 'Scotland'                                                             # create the geographic unit Scotland                       
scot.agg <- scot.agg[,c(3, 1, 2)]                                                              # reorder columns so the la and Scotland objects match
scot.la.agg <- rbind(la.agg, scot.agg)                                                         # concatenate them
names(scot.la.agg)  <- c("GeographyCode", "DateCode", "Value")                                 # renames some columns
scot.la.agg$DateCode <-  str_replace_all(scot.la.agg$DateCode, "-", "/")                       # change date notation to correct format
scot.la.agg$Measurement <- "Count"                                                             # create additional columns in correct format
scot.la.agg$Units <- "Fires"                                                         # 
scot.la.agg$"Type of Fire" <- "All"                                                            #
scot.la.agg$"Accident Status" <- "All"                                                         #  

# Calculate aggregate figures for each type of fire and only all accident status,
# for any council/scotland in each year.
#===========================================================================

la.allaccident <- fire %>% group_by(LocalAuthority, FiscalYear, IncidentType) %>% summarize(count=n()) %>% data.frame    # count the number of fires for each local authority in each fiscal year for each type of fire
scot.allaccident <- fire %>% group_by(FiscalYear, IncidentType) %>% summarize(count=n()) %>% data.frame               # do the same for scotland as a whole
scot.allaccident$LocalAuthority <- 'Scotland'                                                                            # create the geographic unit Scotland
scot.allaccident <- scot.allaccident[,c(4, 1, 2, 3)]                                                                  # reorder columns so the la and Scotland objects match
scot.la.allaccident <- rbind(la.allaccident, scot.allaccident)                                                        # concatenate them
names(scot.la.allaccident)  <- c("GeographyCode", "DateCode", "Type of Fire", "Value")                                # renames some columns
scot.la.allaccident$DateCode <-  str_replace_all(scot.la.allaccident$DateCode, "-", "/")                              # change date notation to correct format
scot.la.allaccident$Measurement <- "Count"                                                                            # create additional columns in correct format
scot.la.allaccident$Units <- "Fires"                                                                        #
scot.la.allaccident$"Accident Status" <- "All"                                                                        #

# Calculate aggregate figures for each accident status and only all type of fire,
# for any council/scotland in each year.
#===========================================================================

la.alltype <- fire %>% group_by(LocalAuthority, FiscalYear, Accidental) %>% summarize(count=n()) %>% data.frame    # count the number of fires for each local authority in each fiscal year for each accident status         
scot.alltype <- fire %>% group_by(FiscalYear, Accidental) %>% summarize(count=n()) %>% data.frame               # do the same for scotland as a whole
scot.alltype$LocalAuthority <- 'Scotland'                                                                          # create the geographic unit Scotland
scot.alltype <- scot.alltype[,c(4, 1, 2, 3)]                                                                    # reorder columns so the la and Scotland objects match
scot.la.alltype <- rbind(la.alltype, scot.alltype)                                                              # concatenate them
names(scot.la.alltype)  <- c("GeographyCode", "DateCode", "Accident Status", "Value")                           # renames some columns
scot.la.alltype$DateCode <-  str_replace_all(scot.la.alltype$DateCode, "-", "/")                                # change date notation to correct format
scot.la.alltype$Measurement <- "Count"                                                                          # create additional columns in correct format
scot.la.alltype$Units <- "Fires"                                                                      #
scot.la.alltype$"Type of Fire" <- "All"                                                                         #
scot.la.alltype$"Accident Status" <-  str_replace_all(scot.la.alltype$"Accident Status",                        # replace accident codes with sensible text
                                                        c("1" = "Accidental", "0" = "Not Accidental"))          #

# Calculate aggregate figures for all fires for any local authority in each year, 
# broken down by accident status and type of fire. Same for Scotland
# Then format for statistics.gov.scot
#===========================================================================

la.breakdown <- fire %>% group_by(LocalAuthority, FiscalYear, Accidental, IncidentType) %>% summarize(count=n()) %>% data.frame   # count the number of fires for each council in each fiscal year for all types of fire and all accident statuses
scot.breakdown <- fire %>% group_by(FiscalYear, Accidental, IncidentType) %>% summarize(count=n()) %>% data.frame              # same for Scotland
scot.breakdown$LocalAuthority <- 'Scotland'                                                                     # create the geograpical unit Scotland
scot.breakdown <- scot.breakdown[,c(5, 1, 2, 3, 4)]                                                          # reorder columns
scot.la.breakdown <- rbind(la.breakdown, scot.breakdown)                                                     # concatenate the dataframes
names(scot.la.breakdown)  <- c("GeographyCode", "DateCode", "Accident Status", "Type of Fire", "Value")      # rename some columns
scot.la.breakdown$DateCode <-  str_replace_all(scot.la.breakdown$DateCode, "-", "/")                         # change date notation to correct format
scot.la.breakdown$"Accident Status" <-  str_replace_all(scot.la.breakdown$"Accident Status",                 # change accident status to words rather than codes 
                                                          c("1" = "Accidental", "0" = "Not Accidental"))     #
scot.la.breakdown$Measurement <- "Count"                                                                     # create additional columns in correct format
scot.la.breakdown$Units <- "Fires"                                                                 #
          
# Reorder the columns in the dataframes, and merge them
#======================================================

scot.la.breakdown <- scot.la.breakdown[,c('GeographyCode', 'DateCode', 
                                              'Measurement', 'Units', 'Value', 'Accident Status', 'Type of Fire')]
scot.la.agg <- scot.la.agg[,c('GeographyCode', 'DateCode', 
                                  'Measurement', 'Units', 'Value', 'Accident Status', 'Type of Fire')] 
scot.la.alltype <- scot.la.alltype[,c('GeographyCode', 'DateCode', 
                                          'Measurement', 'Units', 'Value', 'Accident Status', 'Type of Fire')]
scot.la.allaccident <- scot.la.allaccident[,c('GeographyCode', 'DateCode', 
                              'Measurement', 'Units', 'Value', 'Accident Status', 'Type of Fire')] 


scotland.la.fire.counts <- rbind(scot.la.agg, scot.la.breakdown, scot.la.allaccident, scot.la.alltype)

# a few councils have 0 fires for some combinations of variables, and these aren't reflected in the counts
# create a dataframe of all potential combinations of councils/scotland, dates, accident status and type of fire

list.geography <- unique(scotland.la.fire.counts[,1])                                                      # all datazones
list.datecode <- unique(scotland.la.fire.counts[,2])                                                       # all dates
list.accident <- unique(scotland.la.fire.counts[,6])                                                       # all accident statuses
list.type     <- unique(scotland.la.fire.counts[,7])                                                       # all types of fire
all.possible.observations <- expand.grid(list.geography, list.datecode, list.accident, list.type)          # create table of all combinations   
names(all.possible.observations)  <- c("GeographyCode", "DateCode", "Accident Status", "Type of Fire")     # name the columns
all.possible.observations$Measurement <- "Count"                                                           # add measurement variable
all.possible.observations$"Units" <- "Fires"                                                     # add units variable

scotland.la.inc.nulls <- merge(all.possible.observations, scotland.la.fire.counts, all = TRUE)                 # merge the counts of fires with the potential combinations
scotland.la.inc.nulls[is.na(scotland.la.inc.nulls)] <- 0                                                       # replace NA values with 0

#replace geography names with GSS codes from register

scotland.la.inc.nulls$GeographyCode <-  str_replace_all(scotland.la.inc.nulls$GeographyCode, c(
  "Fife" = "S12000047",
  "Perth & Kinross" =	"S12000048",
  "Clackmannanshire" =	"S12000005",
  "Dumfries & Galloway"	= "S12000006",
  "East Ayrshire"	= "S12000008",
  "East Lothian" =	"S12000010",
  "East Renfrewshire"	= "S12000011",
  "Na h-Eileanan Siar"	= "S12000013",
  "Falkirk"	= "S12000014",
  "Highland" = "S12000017",
  "Inverclyde" =	"S12000018",
  "Midlothian" =	"S12000019",
  "Moray"	= "S12000020",
  "North Ayrshire" =	"S12000021",
  "Orkney Islands" =	"S12000023",
  "Scottish Borders" =	"S12000026",
  "Shetland Islands" =	"S12000027",
  "South Ayrshire" =	"S12000028",
  "South Lanarkshire" =	"S12000029",
  "Stirling" =	"S12000030",
  "Aberdeen City" =	"S12000033",
  "Aberdeenshire" =	"S12000034",
  "Argyll & Bute" =	"S12000035",
  "Edinburgh City"	= "S12000036",
  "Renfrewshire" =	"S12000038",
  "West Dunbartonshire"	= "S12000039",
  "West Lothian" =	"S12000040",
  "Angus"	= "S12000041",
  "Dundee City" =	"S12000042",
  "North Lanarkshire" =	"S12000050",
  "East Dunbartonshire"	= "S12000045",
  "Glasgow City" = "S12000049",
  "Scotland" = "S92000003"))	

write.csv(scotland.la.inc.nulls, "scotland and la fire data for upload to odpp.csv", row.names=FALSE)

# yaldi
