# Purpose: Urban scaling analysis for residential water and electricity use
# Author: Renee Obringer & Vijay Chiluveru
# Created: 09 August 2023
# Last Run: 17 October 2024

# ORGANIZATION: 
# This code is organized into sections, the start of each is denoted by multiple #
# The sections can be run independently by after loading the data
# Each section is described below
#
# LOAD DATA: load data
# SCALING ANALYSIS - TOTALS: Scaling with total annual data for each year (2007-2018), includes figures
# SCALING ANALYSIS - SUMMER: Scaling with total summer data for each year (2007-2018), including figures
# SCALING ANALYSIS - WINTER: Scaling with total winter data for each year (2007-2018), including figures

rm(list=ls())
options(scipen = 999)

# Libraries
library(dplyr)
library(ggplot2)
library(ggrepel)
library(stringr)
library(data.table)
library(rlist)
library(tidyverse)

# set file path
# NOTE: set this path to the folder on your personal machine which contains the downloaded data/cloned repo
# for example: path <- '/Users/rqo5125/Downloads/ClimateAnalogs_WEN'

path <- '    ' # main file path

# Directories
datadir1 <- paste(path, '/UtilityData/rawdata', sep = '') 
datadir2 <- paste(path, '/ClimateData', sep = '')

# OPTIONAL: create an output directory for any any non-rdata output files (e.g., csv, pdf, etc.)
outputdir <- paste(path, '/output', sep = '')
#dir.create(outputdir)

########### LOAD DATA #############
setwd(datadir1)

filelist <- list.files(pattern = "*.csv", full.names = TRUE)

# load cities with both water and electricity data
cities <- list()
for (filename in filelist) {
  raw_df <- fread(filename)
  raw_df <- na.omit(raw_df)
  cities <- list.append(cities, raw_df)
}

############ SCALING ANALYSIS - TOTALS #################################

# initialize variables
column_name <- "year"      # Column name to subset by
years_list <- c(2007:2018) # Condition to subset rows

beta_wateruse <- numeric(length(years_list))
intercept_wateruse <- numeric(length(years_list))
R2_wateruse <- numeric(length(years_list))

beta_elecuse <- numeric(length(years_list))
R2_elecuse <- numeric(length(years_list))
intercept_elecuse <- numeric(length(years_list))
output_df <- data.frame()

output_vars <- c("year", "beta_wateruse", "beta_elecuse",
                 "intercept_wateruse", "intercept_elecuse",
                 "r2_wateruse", "r2_elecuse",
                 "rmse_wateruse", "rmse_elecuse",
                 "nrmse_wateruse", "nrmse_elecuse")

# Loop through the cities
my_df_list <- list()
for (condition in years_list) {
  # Create an empty list to store the subset data frames
  ann_list <- list()
  # Loop through each data frame in the list and subset rows
  for (i in 1:length(cities)) {
    df <- cities[[i]]
    subset_df <- df[df[[column_name]] == condition, ]
    ann_list[[i]] <- subset_df
  }
  # Resulting list of subset data frames
  ann_list_avg <- list()
  my_df <- data.frame()
  for (i in 1:length(ann_list)) {
    if (nrow(ann_list[[i]]) > 0) {
      ann_avg <- ann_list[[i]]%>%
        group_by(year) %>%
        summarize(wateruse = mean(wateruse),
                  elecuse = mean(elecuse),
                  wateruse_population = mean(wateruse_population),
                  elecuse_population = mean(elecuse_population))
      cityname <- strsplit(filelist[i], "./")[[1]][[2]]
      cityname <- strsplit(cityname, ".csv")[[1]]
      ann_avg[, "cityname"] = cityname
      my_df[nrow(my_df) + 1,names(ann_avg)] = ann_avg
    }
  }
  my_df_list <- list.append(my_df_list, my_df)
}

for (i in 1:length(years_list)) {
  condition <- years_list[i]
  
  # Calculate natural logarithms
  my_df <- my_df_list[[i]]
  ln_wateruse_population <- log(my_df$wateruse_population)
  ln_elecuse <- log(my_df$elecuse)
  ln_wateruse <- log(my_df$wateruse)
  ln_elecuse_population <- log(my_df$elecuse_population)
  
  # Perform linear regression
  scaling_model_wateruse <- lm(ln_wateruse ~ ln_wateruse_population)
  scaling_model_elecuse <- lm(ln_elecuse ~ ln_elecuse_population)
  
  # Extract beta values (coefficients)
  beta_wateruse <- coef(scaling_model_wateruse)
  beta_elecuse <- coef(scaling_model_elecuse)
  
  # The first element is the intercept, and the second is the slope (scaling exponent)
  intercept_wateruse <- beta_wateruse[1]
  scaling_exponent_wateruse <- beta_wateruse[2]
  intercept_elecuse <- beta_elecuse[1]
  scaling_exponent_elecuse <- beta_elecuse[2]
  
  # Extract R-squared value
  r_squared_wateruse <- summary(scaling_model_wateruse)$r.squared
  r_squared_elecuse <- summary(scaling_model_elecuse)$r.squared

  # calculate rmse
  rmse_wateruse <- sqrt(mean(scaling_model_wateruse$residuals^2))
  rmse_elecuse <- sqrt(mean(scaling_model_elecuse$residuals^2))
  nrmse_wateruse <- rmse_wateruse / sd(ln_wateruse)
  nrmse_elecuse <- rmse_elecuse / sd(ln_elecuse)
  
  # store as a data frame
  output_df[nrow(
    output_df) + 1,output_vars] = c(condition, scaling_exponent_wateruse,
                                    scaling_exponent_elecuse, intercept_wateruse,
                                    intercept_elecuse, r_squared_wateruse,
                                    r_squared_elecuse, rmse_wateruse, rmse_elecuse,
                                    nrmse_wateruse, nrmse_elecuse)
  
}

# store as dataframe
out_df_filename <- paste("results_yrly_all_w_p_e_ep_",
                         as.character(years_list[1]), "_",
                         as.character(years_list[length(years_list)]),
                         ".csv", sep="")

# write as csv
setwd(outputdir)
write.csv(output_df, out_df_filename, row.names = FALSE)


# READ IN CLIMATE DATA 
# code copied from `climateanalogs.R` in GitHub repo

setwd(datadir2)

# function to read text files
readTextFilesIn <- function(dir_path = datadir2){
  myfun <- function(fl=paste(datadir2,"/city_id_001", sep="")){
    dfnarr <- read.table(fl, header=T, fill = TRUE, stringsAsFactors = F)
    cityN <- sub( ".txt", "", sub(".*/","",fl) ) # at first, remove left of /  and then *.txt
    dfnarr <- cbind(dfnarr, seq(as.Date("1979-01-01"), as.Date("2019-12-31"), by="days"))
    return(dfnarr)
  }
  filelist <- list.files(path = dir_path, pattern = ".*.txt", full.names = TRUE)
  datalist <- lapply(filelist, FUN =  myfun)
  return(datalist)
}

# call function 
narrdata <- readTextFilesIn(datadir2)

# read in city names from separate text file
analognames <- read.table(paste(path, 'MiscFiles/cities_loc.txt', sep = ''), header=T, fill = TRUE, stringsAsFactors = F, sep = ',')

oids <- c(2,3,4,8,12,14:16,19:22,24,25,31,32,38,42,47,51:53,56,58,63,66:68,70,71,74,76,78:80,82:85,92,94,93,97:99,102) # original cities
originalcities <- list()
for (i in 1:46) {
  originalcities[[i]] <- narrdata[[oids[i]]]
  originalcities[[i]] <- originalcities[[i]][which(originalcities[[i]][,8] > "2006-12-31" & originalcities[[i]][,8] < "2018-12-31"),]
}

originalmeans <- list(); originalmins <- list(); originalmaxs <- list(); originalsums <- list()

for (i in 1:46) {
  # convert dates
  originalcities[[i]][,8] <- format(originalcities[[i]][,8],'%Y-%m')
  
  # convert u and v wind to overall wind speed
  originalcities[[i]][,9] <- sqrt(originalcities[[i]][,6]^2+originalcities[[i]][,7]^2)
  
  # calculate precipitation count
  originalcities[[i]][,10] <- originalcities[[i]][,1]; originalcities[[i]][which(originalcities[[i]][,10] > 0),10] <- 1 
  
  # aggregate data from daily to monthly
  originalmeans[[i]] <- aggregate(originalcities[[i]][,c(2:5,9)], by = list(originalcities[[i]][,8]), FUN = mean) 
  originalmins[[i]] <- aggregate(originalcities[[i]][,c(2:5,9)], by = list(originalcities[[i]][,8]), FUN = min) 
  originalmaxs[[i]] <- aggregate(originalcities[[i]][,c(2:5,9)], by = list(originalcities[[i]][,8]), FUN = max) 
  originalsums[[i]] <- aggregate(originalcities[[i]][,c(1,10)], by = list(originalcities[[i]][,8]), FUN = sum)  
}

# convert temperature data
for (i in 1:46) {
  originalmeans[[i]][c(2,4)] <- originalmeans[[i]][c(2,4)] - 273.15
  originalmins[[i]][c(2,4)] <- originalmins[[i]][c(2,4)] - 273.15
  originalmaxs[[i]][c(2,4)] <- originalmaxs[[i]][c(2,4)] - 273.15
}

citynames <- analognames[oids,4]

# combine data
obsdata <- list()
for (i in 1:46) {
  # merge data
  obsdata[[i]] <- cbind(originalmeans[[i]][,1:6], originalmins[[i]][,2:6], originalmaxs[[i]][,2:6], originalsums[[i]][,2:3])
  names(obsdata[[i]]) <- c('date', 'avgdbt','avgwbt','avgdp','avgrh','avgws','mindbt','minwbt','mindp','minrh',
                           'minws','maxdbt','maxwbt','maxdp','maxrh','maxws','accpr','prcount')
  obsdata[[i]]$year <- as.numeric(sapply(strsplit(obsdata[[i]]$date,  '-'), '[', 1))
}

# CREATE FIGURES

# extract climate variables of interest
annualclimate <- list()
for (i in 1:46) {
  annualclimate[[i]] <- obsdata[[i]]%>%
    group_by(year) %>%
    summarize(precip = mean(accpr), temp = mean(avgdbt))
}

# Build Up Plot Data - Water

plotdata_water <- data.frame(population = c(), wateruse = c(), precip = c(), year = c(), cities = c())

for (i in 1:12){
  # extract each year
  mydf_annual <- my_df_list[[i]]
  
  # calculate logs
  ln_wateruse_population <- log(mydf_annual$wateruse_population)
  ln_wateruse <- log(mydf_annual$wateruse)
  
  # extract precipitation for each city
  precip_annual <- c()
  for (j in 1:46) {
    precip_annual <- append(precip_annual, annualclimate[[j]]$precip[i])
  }
  
  # remove points from precipitation with no data in a given year
  if (i == 1) {
    # remove Raleigh and Seattle values (no 2007 data)
    precip_annual <- precip_annual[-c(which(citynames == 'Raleigh'), which(citynames == 'Seattle'))]
  } else if (i == 2 | i == 3) {
    # remove Seattle values (no 2008 or 2009 data)
    precip_annual <- precip_annual[-c(which(citynames == 'Seattle'))]
  } else if (i == 11) {
    # remove Chicago, Cleveland, Columbus, Indianapolis, Madison, and Minneapolis values (no 2018 data)
    precip_annual <- precip_annual[-c(which(citynames == 'Chicago'), which(citynames == 'Cleveland'), which(citynames == 'Columbus'), which(citynames == 'Indianapolis'), which(citynames == 'Madison'), which(citynames == 'Minneapolis'))]
  } else if (i == 12) {
    # remove Albuquerque, Chicago, Cleveland, Columbus, Indianapolis, Madison, and Minneapolis values (no 2018 data)
    precip_annual <- precip_annual[-c(which(citynames == 'Albuquerque'), which(citynames == 'Chicago'), which(citynames == 'Cleveland'), which(citynames == 'Columbus'), which(citynames == 'Indianapolis'), which(citynames == 'Madison'), which(citynames == 'Minneapolis'))]
  }
  
  # combine plotdata
  plotdata_annual <- data.frame(population = ln_wateruse_population, wateruse = ln_wateruse, precip = precip_annual, year = mydf_annual$year, cities = mydf_annual$cityname)
  
  # append to complete plot data
  plotdata_water <- rbind(plotdata_water, plotdata_annual)
}

# create plots

# get labels
water_label_city_list <- c('chulavista', 'stpetersburg', 'austin', 'lasvegas', 'losangeles', 'nyc')

# 2007 & 2018
plotdata2007_18 <- plotdata_water[which(plotdata_water$year == 2007 | plotdata_water$year == 2018),]

setwd(outputdir)
pdf('annualwater2007-18.pdf', width = 11, height = 6)
ggplot(plotdata2007_18, aes(x = population, y = wateruse)) +
  geom_point(aes(color = precip), size = 2.5) +
  scale_x_continuous(limits=c(11.5,16.1)) +
  scale_y_continuous(limits=c(5,11)) +
  geom_smooth(method = "lm", se = FALSE, color = 'black', linetype = 'dashed', size = 0.5) +
  xlab("Log(Service Population)") +
  ylab("Log(Water Use in Mgal)") +
  theme_light() + facet_wrap(~year) + theme(text = element_text(size = 20), legend.position="bottom", legend.key.width = unit(3, "cm")) +
  geom_text_repel(data = plotdata2007_18[which(plotdata2007_18$cities %in% water_label_city_list),], aes(label = cities), nudge_y = 0.3, nudge_x = -0.25, 
                  box.padding = 1.95, point.padding = 0.85, segment.color = 'grey50') +
  labs(color = "Avg Precipitation (mm)")
dev.off()

# all years

setwd(outputdir)
pdf('annualwater_allyears.pdf', width = 10.5, height = 9)
ggplot(plotdata_water, aes(x = population, y = wateruse)) +
  geom_point(aes(color = precip), size = 2.5) +
  scale_x_continuous(limits=c(11.5,16.1)) +
  scale_y_continuous(limits=c(5,11)) +
  geom_smooth(method = "lm", se = FALSE, color = 'black', linetype = 'dashed', size = 0.5) +
  xlab("Log(Service Population)") +
  ylab("Log(Water Use in Mgal)") +
  theme_light() + facet_wrap(~year) + theme(text = element_text(size = 20), legend.position="bottom", legend.key.width = unit(3, "cm")) +
  geom_text_repel(data = plotdata_water[which(plotdata_water$cities %in% water_label_city_list),], aes(label = cities), nudge_y = 0.3, nudge_x = -0.25, 
                  box.padding = 1.95, point.padding = 0.85, segment.color = 'grey50') +
  labs(color = "Avg Precipitation (mm)")
dev.off()

# Build Up Plot Data - Electricity

plotdata_elec <- data.frame(population = c(), elecuse = c(), temp = c(), year = c(), cities = c())

for (i in 1:12){
  # extract each year
  mydf_annual <- my_df_list[[i]]
  
  # calculate logs
  ln_elecuse_population <- log(mydf_annual$elecuse_population)
  ln_elecuse <- log(mydf_annual$elecuse)
  
  # extract precipitation for each city
  temp_annual <- c()
  for (j in 1:46) {
    temp_annual <- append(temp_annual, annualclimate[[j]]$temp[i])
  }
  
  # remove points from precipitation with no data in a given year
  if (i == 1) {
    # remove Raleigh and Seattle values (no 2007 data)
    temp_annual <- temp_annual[-c(which(citynames == 'Raleigh'), which(citynames == 'Seattle'))]
  } else if (i == 2 | i == 3) {
    # remove Seattle values (no 2008 or 2009 data)
    temp_annual <- temp_annual[-c(which(citynames == 'Seattle'))]
  } else if (i == 11) {
    # remove Chicago, Cleveland, Columbus, Indianapolis, Madison, and Minneapolis values (no 2018 data)
    temp_annual <- temp_annual[-c(which(citynames == 'Chicago'), which(citynames == 'Cleveland'), which(citynames == 'Columbus'), which(citynames == 'Indianapolis'), which(citynames == 'Madison'), which(citynames == 'Minneapolis'))]
  } else if (i == 12) {
    # remove Albuquerque, Chicago, Cleveland, Columbus, Indianapolis, Madison, and Minneapolis values (no 2018 data)
    temp_annual <- temp_annual[-c(which(citynames == 'Albuquerque'), which(citynames == 'Chicago'), which(citynames == 'Cleveland'), which(citynames == 'Columbus'), which(citynames == 'Indianapolis'), which(citynames == 'Madison'), which(citynames == 'Minneapolis'))]
  }
  
  # combine plotdata
  plotdata_annual <- data.frame(population = ln_elecuse_population, elecuse = ln_elecuse, temp = temp_annual, year = mydf_annual$year, cities = mydf_annual$cityname)
  
  # append to complete plot data
  plotdata_elec <- rbind(plotdata_elec, plotdata_annual)
}

# create plots

# get labels
elec_label_city_list <- c('madison', 'coloradosprings', 'phoenix', 'losangeles', 'nyc', 'sanfrancisco', 'sandiego', 'miami')

# 2007 & 2018
plotdata2007_18 <- plotdata_elec[which(plotdata_elec$year == 2007 | plotdata_elec$year == 2018),]

setwd(outputdir)
pdf('annualelectricity2007-18.pdf', width = 11, height = 6)
ggplot(plotdata2007_18, aes(x = population, y = elecuse)) +
  geom_point(aes(color = temp), size = 2.5) +
  scale_x_continuous(limits=c(11.5,15.5)) +
  scale_y_continuous(limits=c(11.5,15.5)) +
  geom_smooth(method = "lm", se = FALSE, color = 'black', linetype = 'dashed', size = 0.5) +
  xlab("Log(Service Population)") +
  ylab("Log(Electricity Use in MWh)") +
  scale_colour_gradient(low = '#fdd0a2', high = '#7f2704') +
  theme_light() + facet_wrap(~year) + theme(text = element_text(size = 20), legend.position="bottom", legend.key.width = unit(3, "cm")) +
  geom_text_repel(data = plotdata2007_18[which(plotdata2007_18$cities %in% elec_label_city_list),], aes(label = cities),
                  box.padding = 1.95, point.padding = 0.85, segment.color = 'grey50', min.segment.length = 0.01) +
  labs(color = "Avg Temperature (degC)")
dev.off()

# all years

setwd(outputdir)
pdf('annualelectricity_allyears.pdf', width = 10.5, height = 9)
ggplot(plotdata_elec, aes(x = population, y = elecuse)) +
  geom_point(aes(color = temp), size = 2.5) +
  scale_x_continuous(limits=c(11.5,15.5)) +
  scale_y_continuous(limits=c(11.5,15.5)) +
  geom_smooth(method = "lm", se = FALSE, color = 'black', linetype = 'dashed', size = 0.5) +
  xlab("Log(Service Population)") +
  ylab("Log(Electricity Use in MWh)") +
  scale_colour_gradient(low = '#fdd0a2', high = '#7f2704') +
  theme_light() + facet_wrap(~year) + theme(text = element_text(size = 20), legend.position="bottom", legend.key.width = unit(3, "cm")) +
  geom_text_repel(data = plotdata_elec[which(plotdata_elec$cities %in% elec_label_city_list),], aes(label = cities),
                  box.padding = 1.95, point.padding = 0.85, segment.color = 'grey50', min.segment.length = 0.01) +
  labs(color = "Avg Temperature (degC)")
dev.off()

# All Years - Best Fit Lines

waterdata <- plotdata_water[,-3]; names(waterdata)[2] <- c('consumption'); waterdata$utility <- rep('Water', nrow(waterdata))
elecdata <- plotdata_elec[,-3]; names(elecdata)[2] <- c('consumption'); elecdata$utility <- rep('Electricity', nrow(elecdata))

plotdata <- rbind(waterdata, elecdata)

plotdata$year <- as.factor(plotdata$year)

setwd(outputdir)
pdf('allyears_bestfitlines.pdf', width = 11, height = 5)
ggplot(plotdata) + 
  geom_smooth(aes(x = population, y = consumption, color = year, group = year), method = 'lm', se = F) +
  facet_wrap(~utility, scales = 'free') + theme_light() + xlab('Log(Population)') + ylab('Log(Consumption)') +
  theme(text = element_text(size = 16)) + 
  scale_color_manual(name = 'Year', values = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928'))
dev.off()

############ SCALING ANALYSIS - SUMMER #################################

# Initialize Variables
column_name <- "year"
month_column_name <- "month"
years_list <- c(2007:2018)
summer_month_list <- c('6','7','8','9')

beta_wateruse <- numeric(length(years_list))
intercept_wateruse <- numeric(length(years_list))
R2_wateruse <- numeric(length(years_list))

beta_elecuse <- numeric(length(years_list))
R2_elecuse <- numeric(length(years_list))
intercept_elecuse <- numeric(length(years_list))
output_df <- data.frame()

output_vars <- c("year", "beta_wateruse", "beta_elecuse",
                 "intercept_wateruse", "intercept_elecuse",
                 "r2_wateruse", "r2_elecuse",
                 "rmse_wateruse", "rmse_elecuse",
                 "nrmse_wateruse", "nrmse_elecuse")

my_df_list <- list()
for (condition in years_list) {
  # Create an empty list to store the subset data frames
  ann_list <- list()
  # Loop through each data frame in the list and subset rows
  for (i in 1:length(cities)) {
    df <- cities[[i]]
    subset_df <- df[df[[column_name]] == condition, ]
    subset_df <- subset_df[subset_df[[month_column_name]] %in% summer_month_list, ]
    ann_list[[i]] <- subset_df
  }
  # Resulting list of subset data frames
  ann_list_avg <- list()
  my_df <- data.frame()
  for (i in 1:length(ann_list)) {
    if (nrow(ann_list[[i]]) > 0) {
      ann_avg <- ann_list[[i]]%>%
        group_by(year) %>%
        summarize(wateruse = mean(wateruse),
                  elecuse = mean(elecuse),
                  wateruse_population = mean(wateruse_population),
                  elecuse_population = mean(elecuse_population))
      cityname <- strsplit(filelist[i], "./")[[1]][[2]]
      cityname <- strsplit(cityname, ".csv")[[1]]
      ann_avg[, "cityname"] = cityname
      my_df[nrow(my_df) + 1,names(ann_avg)] = ann_avg
    }
  }
  my_df_list <- list.append(my_df_list, my_df)
}

for (i in 1:length(years_list)) {
  condition <- years_list[i]
  # Calculate natural logarithms
  my_df <- my_df_list[[i]]
  ln_wateruse_population <- log(my_df$wateruse_population)
  ln_elecuse <- log(my_df$elecuse)
  ln_wateruse <- log(my_df$wateruse)
  ln_elecuse_population <- log(my_df$elecuse_population)
  
  # Perform linear regression
  scaling_model_wateruse <- lm(ln_wateruse ~ ln_wateruse_population)
  scaling_model_elecuse <- lm(ln_elecuse ~ ln_elecuse_population)
  
  # Extract beta values (coefficients)
  beta_wateruse <- coef(scaling_model_wateruse)
  beta_elecuse <- coef(scaling_model_elecuse)
  
  # The first element is the intercept, and the second is the slope (scaling exponent)
  intercept_wateruse <- beta_wateruse[1]
  scaling_exponent_wateruse <- beta_wateruse[2]
  intercept_elecuse <- beta_elecuse[1]
  scaling_exponent_elecuse <- beta_elecuse[2]
  
  # Extract R-squared value
  r_squared_wateruse <- summary(scaling_model_wateruse)$r.squared
  r_squared_elecuse <- summary(scaling_model_elecuse)$r.squared
  
  # Calculate RMSE
  rmse_wateruse <- sqrt(mean(scaling_model_wateruse$residuals^2))
  rmse_elecuse <- sqrt(mean(scaling_model_elecuse$residuals^2))
  nrmse_wateruse <- rmse_wateruse / sd(ln_wateruse)
  nrmse_elecuse <- rmse_elecuse / sd(ln_elecuse)
  
  # store as a dataframe
  output_df[nrow(
    output_df) + 1,output_vars] = c(condition, scaling_exponent_wateruse,
                                    scaling_exponent_elecuse, intercept_wateruse,
                                    intercept_elecuse, r_squared_wateruse,
                                    r_squared_elecuse, rmse_wateruse, rmse_elecuse,
                                    nrmse_wateruse, nrmse_elecuse)
}

# write to csv
setwd(outputdir)
out_df_filename <- paste("results_summer_all_w_p_e_ep_",
                         as.character(years_list[1]), "_",
                         as.character(years_list[length(years_list)]),
                         ".csv", sep="")
write.csv(output_df, out_df_filename, row.names = FALSE)

# READ IN CLIMATE DATA 
# code copied from `climateanalogs.R` in GitHub repo

setwd(datadir2)

# function to read text files
readTextFilesIn <- function(dir_path = datadir2){
  myfun <- function(fl=paste(datadir2,"/city_id_001", sep="")){
    dfnarr <- read.table(fl, header=T, fill = TRUE, stringsAsFactors = F)
    cityN <- sub( ".txt", "", sub(".*/","",fl) ) # at first, remove left of /  and then *.txt
    dfnarr <- cbind(dfnarr, seq(as.Date("1979-01-01"), as.Date("2019-12-31"), by="days"))
    return(dfnarr)
  }
  filelist <- list.files(path = dir_path, pattern = ".*.txt", full.names = TRUE)
  datalist <- lapply(filelist, FUN =  myfun)
  return(datalist)
}

# call function 
narrdata <- readTextFilesIn(datadir2)

# read in city names from separate text file
analognames <- read.table(paste(path, 'MiscFiles/cities_loc.txt', sep = ''), header=T, fill = TRUE, stringsAsFactors = F, sep = ',')

oids <- c(2,3,4,8,12,14:16,19:22,24,25,31,32,38,42,47,51:53,56,58,63,66:68,70,71,74,76,78:80,82:85,92,94,93,97:99,102) # original cities
originalcities <- list()
for (i in 1:46) {
  originalcities[[i]] <- narrdata[[oids[i]]]
  originalcities[[i]] <- originalcities[[i]][which(originalcities[[i]][,8] > "2006-12-31" & originalcities[[i]][,8] < "2018-12-31"),]
}

originalmeans <- list(); originalmins <- list(); originalmaxs <- list(); originalsums <- list()

for (i in 1:46) {
  # convert dates
  originalcities[[i]][,8] <- format(originalcities[[i]][,8],'%Y-%m')
  
  # convert u and v wind to overall wind speed
  originalcities[[i]][,9] <- sqrt(originalcities[[i]][,6]^2+originalcities[[i]][,7]^2)
  
  # calculate precipitation count
  originalcities[[i]][,10] <- originalcities[[i]][,1]; originalcities[[i]][which(originalcities[[i]][,10] > 0),10] <- 1 
  
  # aggregate data from daily to monthly
  originalmeans[[i]] <- aggregate(originalcities[[i]][,c(2:5,9)], by = list(originalcities[[i]][,8]), FUN = mean) 
  originalmins[[i]] <- aggregate(originalcities[[i]][,c(2:5,9)], by = list(originalcities[[i]][,8]), FUN = min) 
  originalmaxs[[i]] <- aggregate(originalcities[[i]][,c(2:5,9)], by = list(originalcities[[i]][,8]), FUN = max) 
  originalsums[[i]] <- aggregate(originalcities[[i]][,c(1,10)], by = list(originalcities[[i]][,8]), FUN = sum)  
}

# convert temperature data
for (i in 1:46) {
  originalmeans[[i]][c(2,4)] <- originalmeans[[i]][c(2,4)] - 273.15
  originalmins[[i]][c(2,4)] <- originalmins[[i]][c(2,4)] - 273.15
  originalmaxs[[i]][c(2,4)] <- originalmaxs[[i]][c(2,4)] - 273.15
}

citynames <- analognames[oids,4]

# combine data
obsdata <- list()
for (i in 1:46) {
  # merge data
  obsdata[[i]] <- cbind(originalmeans[[i]][,1:6], originalmins[[i]][,2:6], originalmaxs[[i]][,2:6], originalsums[[i]][,2:3])
  names(obsdata[[i]]) <- c('date', 'avgdbt','avgwbt','avgdp','avgrh','avgws','mindbt','minwbt','mindp','minrh',
                           'minws','maxdbt','maxwbt','maxdp','maxrh','maxws','accpr','prcount')
  obsdata[[i]]$year <- as.numeric(sapply(strsplit(obsdata[[i]]$date,  '-'), '[', 1))
  obsdata[[i]]$month <- as.numeric(sapply(strsplit(obsdata[[i]]$date,  '-'), '[', 2))
}

# extract summer data
summerobsdata <- list()
for (i in 1:46) {
  summerobsdata[[i]] <- obsdata[[i]][which(obsdata[[i]]$month >= 6 & obsdata[[i]]$month <= 9),]
}

# CREATE FIGURES

# extract climate variables of interest
annualclimate <- list()
for (i in 1:46) {
  annualclimate[[i]] <- summerobsdata[[i]]%>%
    group_by(year) %>%
    summarize(precip = mean(accpr), temp = mean(avgdbt))
}

# Build Up Plot Data - Water

plotdata_water <- data.frame(population = c(), wateruse = c(), precip = c(), year = c(), cities = c())

for (i in 1:12){
  # extract each year
  mydf_annual <- my_df_list[[i]]
  
  # calculate logs
  ln_wateruse_population <- log(mydf_annual$wateruse_population)
  ln_wateruse <- log(mydf_annual$wateruse)
  
  # extract precipitation for each city
  precip_annual <- c()
  for (j in 1:46) {
    precip_annual <- append(precip_annual, annualclimate[[j]]$precip[i])
  }
  
  # remove points from precipitation with no data in a given year
  if (i == 1) {
    # remove Raleigh and Seattle values (no 2007 data)
    precip_annual <- precip_annual[-c(which(citynames == 'Raleigh'), which(citynames == 'Seattle'))]
  } else if (i == 2 | i == 3) {
    # remove Seattle values (no 2008 or 2009 data)
    precip_annual <- precip_annual[-c(which(citynames == 'Seattle'))]
  } else if (i == 11) {
    # remove Chicago, Cleveland, Columbus, Indianapolis, Madison, and Minneapolis values (no 2018 data)
    precip_annual <- precip_annual[-c(which(citynames == 'Chicago'), which(citynames == 'Cleveland'), which(citynames == 'Columbus'), which(citynames == 'Indianapolis'), which(citynames == 'Madison'), which(citynames == 'Minneapolis'))]
  } else if (i == 12) {
    # remove Albuquerque, Chicago, Cleveland, Columbus, Indianapolis, Madison, and Minneapolis values (no 2018 data)
    precip_annual <- precip_annual[-c(which(citynames == 'Albuquerque'), which(citynames == 'Chicago'), which(citynames == 'Cleveland'), which(citynames == 'Columbus'), which(citynames == 'Indianapolis'), which(citynames == 'Madison'), which(citynames == 'Minneapolis'))]
  }
  
  # combine plotdata
  plotdata_annual <- data.frame(population = ln_wateruse_population, wateruse = ln_wateruse, precip = precip_annual, year = mydf_annual$year, cities = mydf_annual$cityname)
  
  # append to complete plot data
  plotdata_water <- rbind(plotdata_water, plotdata_annual)
}

# create plots

# get labels
water_label_city_list <- c('phoenix', 'reno', 'stpetersburg', 'austin', 'lasvegas', 'losangeles', 'nyc')

# 2007 & 2018
plotdata2007_18 <- plotdata_water[which(plotdata_water$year == 2007 | plotdata_water$year == 2018),]

setwd(outputdir)
pdf('summerwater2007-18.pdf', width = 11, height = 6)
ggplot(plotdata2007_18, aes(x = population, y = wateruse)) +
  geom_point(aes(color = precip), size = 2.5) +
  scale_x_continuous(limits=c(11.5,16.1)) +
  scale_y_continuous(limits=c(5,11)) +
  geom_smooth(method = "lm", se = FALSE, color = 'black', linetype = 'dashed', size = 0.5) +
  xlab("Log(Service Population)") +
  ylab("Log(Water Use in Mgal)") +
  theme_light() + facet_wrap(~year) + theme(text = element_text(size = 20), legend.position="bottom", legend.key.width = unit(3, "cm")) +
  geom_text_repel(data = plotdata2007_18[which(plotdata2007_18$cities %in% water_label_city_list),], aes(label = cities), nudge_y = 0.3, nudge_x = -0.25, 
                  box.padding = 1.95, point.padding = 0.85, segment.color = 'grey50') +
  labs(color = "Avg Precipitation (mm)")
dev.off()

# all years

setwd(outputdir)
pdf('summerwater_allyears.pdf', width = 10.5, height = 9)
ggplot(plotdata_water, aes(x = population, y = wateruse)) +
  geom_point(aes(color = precip), size = 2.5) +
  scale_x_continuous(limits=c(11.5,16.1)) +
  scale_y_continuous(limits=c(5,11)) +
  geom_smooth(method = "lm", se = FALSE, color = 'black', linetype = 'dashed', size = 0.5) +
  xlab("Log(Service Population)") +
  ylab("Log(Water Use in Mgal)") +
  theme_light() + facet_wrap(~year) + theme(text = element_text(size = 20), legend.position="bottom", legend.key.width = unit(3, "cm")) +
  geom_text_repel(data = plotdata_water[which(plotdata_water$cities %in% water_label_city_list),], aes(label = cities), nudge_y = 0.3, nudge_x = -0.25, 
                  box.padding = 1.95, point.padding = 0.85, segment.color = 'grey50') +
  labs(color = "Avg Precipitation (mm)")
dev.off()

# Build Up Plot Data - Electricity

plotdata_elec <- data.frame(population = c(), elecuse = c(), temp = c(), year = c(), cities = c())

for (i in 1:12){
  # extract each year
  mydf_annual <- my_df_list[[i]]
  
  # calculate logs
  ln_elecuse_population <- log(mydf_annual$elecuse_population)
  ln_elecuse <- log(mydf_annual$elecuse)
  
  # extract temperature for each city
  temp_annual <- c()
  for (j in 1:46) {
    temp_annual <- append(temp_annual, annualclimate[[j]]$temp[i])
  }
  
  # remove points from precipitation with no data in a given year
  if (i == 1) {
    # remove Raleigh and Seattle values (no 2007 data)
    temp_annual <- temp_annual[-c(which(citynames == 'Raleigh'), which(citynames == 'Seattle'))]
  } else if (i == 2 | i == 3) {
    # remove Seattle values (no 2008 or 2009 data)
    temp_annual <- temp_annual[-c(which(citynames == 'Seattle'))]
  } else if (i == 11) {
    # remove Chicago, Cleveland, Columbus, Indianapolis, Madison, and Minneapolis values (no 2018 data)
    temp_annual <- temp_annual[-c(which(citynames == 'Chicago'), which(citynames == 'Cleveland'), which(citynames == 'Columbus'), which(citynames == 'Indianapolis'), which(citynames == 'Madison'), which(citynames == 'Minneapolis'))]
  } else if (i == 12) {
    # remove Albuquerque, Chicago, Cleveland, Columbus, Indianapolis, Madison, and Minneapolis values (no 2018 data)
    temp_annual <- temp_annual[-c(which(citynames == 'Albuquerque'), which(citynames == 'Chicago'), which(citynames == 'Cleveland'), which(citynames == 'Columbus'), which(citynames == 'Indianapolis'), which(citynames == 'Madison'), which(citynames == 'Minneapolis'))]
  }
  
  # combine plotdata
  plotdata_annual <- data.frame(population = ln_elecuse_population, elecuse = ln_elecuse, temp = temp_annual, year = mydf_annual$year, cities = mydf_annual$cityname)
  
  # append to complete plot data
  plotdata_elec <- rbind(plotdata_elec, plotdata_annual)
}

# create plots

# get labels
elec_label_city_list <- c('seattle', 'coloradosprings', 'phoenix', 'losangeles', 'nyc', 'sanfrancisco', 'sandiego', 'miami')

# 2007 & 2018
plotdata2007_18 <- plotdata_elec[which(plotdata_elec$year == 2007 | plotdata_elec$year == 2018),]

setwd(outputdir)
pdf('summerelectricity2007-18.pdf', width = 11, height = 6)
ggplot(plotdata2007_18, aes(x = population, y = elecuse)) +
  geom_point(aes(color = temp), size = 2.5) +
  scale_x_continuous(limits=c(11.9,16)) +
  scale_y_continuous(limits=c(11.5,16)) +
  geom_smooth(method = "lm", se = FALSE, color = 'black', linetype = 'dashed', size = 0.5) +
  xlab("Log(Service Population)") +
  ylab("Log(Electricity Use in MWh)") +
  scale_colour_gradient(low = '#fdd0a2', high = '#7f2704') +
  theme_light() + facet_wrap(~year) + theme(text = element_text(size = 20), legend.position="bottom", legend.key.width = unit(3, "cm")) +
  geom_text_repel(data = plotdata2007_18[which(plotdata2007_18$cities %in% elec_label_city_list),], aes(label = cities),
                  box.padding = 1.95, point.padding = 0.85, segment.color = 'grey50', min.segment.length = 0.01) +
  labs(color = "Avg Temperature (degC)")
dev.off()

# all years

setwd(outputdir)
pdf('summerelectricity_allyears.pdf', width = 10.5, height = 9)
ggplot(plotdata_elec, aes(x = population, y = elecuse)) +
  geom_point(aes(color = temp), size = 2.5) +
  scale_x_continuous(limits=c(11.5,15.5)) +
  scale_y_continuous(limits=c(11.5,15.5)) +
  geom_smooth(method = "lm", se = FALSE, color = 'black', linetype = 'dashed', size = 0.5) +
  xlab("Log(Service Population)") +
  ylab("Log(Electricity Use in MWh)") +
  scale_colour_gradient(low = '#fdd0a2', high = '#7f2704') +
  theme_light() + facet_wrap(~year) + theme(text = element_text(size = 20), legend.position="bottom", legend.key.width = unit(3, "cm")) +
  geom_text_repel(data = plotdata_elec[which(plotdata_elec$cities %in% elec_label_city_list),], aes(label = cities),
                  box.padding = 1.95, point.padding = 0.85, segment.color = 'grey50', min.segment.length = 0.01) +
  labs(color = "Avg Temperature (degC)")
dev.off()

# summer 2018 electricity vs temperature "back of the envelope" analysis for discussion section
temp2018 <- c()
for (j in 1:46) {
  temp2018 <- append(temp2018, annualclimate[[j]]$temp[12])
}

temp2018 <- temp2018[-c(which(citynames == 'Albuquerque'), which(citynames == 'Chicago'), which(citynames == 'Cleveland'), which(citynames == 'Columbus'), which(citynames == 'Indianapolis'), which(citynames == 'Madison'), which(citynames == 'Minneapolis'))]

ln_elecuse2018 <- log(my_df_list[[12]]$elecuse)
ln_temp2018 <- log(temp2018)

lm(ln_elecuse2018 ~ ln_temp2018)

############ SCALING ANALYSIS - WINTER #################################

# initialize variables
column_name <- "year"
month_column_name <- "month"

years_list <- c(2007:2018)
winter_month_list <- c('12','1','2','3')

beta_wateruse <- numeric(length(years_list))
intercept_wateruse <- numeric(length(years_list))
R2_wateruse <- numeric(length(years_list))

beta_elecuse <- numeric(length(years_list))
R2_elecuse <- numeric(length(years_list))
intercept_elecuse <- numeric(length(years_list))
output_df <- data.frame()

output_vars <- c("year", "beta_wateruse", "beta_elecuse",
                 "intercept_wateruse", "intercept_elecuse",
                 "r2_wateruse", "r2_elecuse",
                 "rmse_wateruse", "rmse_elecuse",
                 "nrmse_wateruse", "nrmse_elecuse")

my_df_list <- list()
for (condition in years_list) {
  # Create an empty list to store the subset data frames
  ann_list <- list()
  # Loop through each data frame in the list and subset rows
  for (i in 1:length(cities)) {
    df <- cities[[i]]
    subset_df <- df[df[[column_name]] == condition, ]
    subset_df <- subset_df[subset_df[[month_column_name]] %in% winter_month_list, ]
    ann_list[[i]] <- subset_df
  }
  # Resulting list of subset data frames
  ann_list_avg <- list()
  my_df <- data.frame()
  for (i in 1:length(ann_list)) {
    if (nrow(ann_list[[i]]) > 0) {
      ann_avg <- ann_list[[i]]%>%
        group_by(year) %>%
        summarize(wateruse = mean(wateruse),
                  elecuse = mean(elecuse),
                  wateruse_population = mean(wateruse_population),
                  elecuse_population = mean(elecuse_population))
      cityname <- strsplit(filelist[i], "./")[[1]][[2]]
      cityname <- strsplit(cityname, ".csv")[[1]]
      ann_avg[, "cityname"] = cityname
      my_df[nrow(my_df) + 1,names(ann_avg)] = ann_avg
    }
  }
  my_df_list <- list.append(my_df_list, my_df)
}

for (i in 1:length(years_list)) {
  condition <- years_list[i]
  
  # Calculate natural logarithms
  my_df <- my_df_list[[i]]
  ln_wateruse_population <- log(my_df$wateruse_population)
  ln_elecuse <- log(my_df$elecuse)
  ln_wateruse <- log(my_df$wateruse)
  ln_elecuse_population <- log(my_df$elecuse_population)
  
  # Perform linear regression
  scaling_model_wateruse <- lm(ln_wateruse ~ ln_wateruse_population)
  scaling_model_elecuse <- lm(ln_elecuse ~ ln_elecuse_population)
  
  # Extract beta values (coefficients)
  beta_wateruse <- coef(scaling_model_wateruse)
  beta_elecuse <- coef(scaling_model_elecuse)
  
  # The first element is the intercept, and the second is the slope (scaling exponent)
  intercept_wateruse <- beta_wateruse[1]
  scaling_exponent_wateruse <- beta_wateruse[2]
  intercept_elecuse <- beta_elecuse[1]
  scaling_exponent_elecuse <- beta_elecuse[2]
  
  # Extract R-squared value
  r_squared_wateruse <- summary(scaling_model_wateruse)$r.squared
  r_squared_elecuse <- summary(scaling_model_elecuse)$r.squared

  # calculate RMSE
  rmse_wateruse <- sqrt(mean(scaling_model_wateruse$residuals^2))
  rmse_elecuse <- sqrt(mean(scaling_model_elecuse$residuals^2))
  nrmse_wateruse <- rmse_wateruse / sd(ln_wateruse)
  nrmse_elecuse <- rmse_elecuse / sd(ln_elecuse)
  
  # store as dataframe
  output_df[nrow(
    output_df) + 1,output_vars] = c(condition, scaling_exponent_wateruse,
                                    scaling_exponent_elecuse, intercept_wateruse,
                                    intercept_elecuse, r_squared_wateruse,
                                    r_squared_elecuse, rmse_wateruse, rmse_elecuse,
                                    nrmse_wateruse, nrmse_elecuse)
}

# write to csv
out_df_filename <- paste("results_winter_all_w_p_e_ep_",
                         as.character(years_list[1]), "_",
                         as.character(years_list[length(years_list)]),
                         ".csv", sep="")
setwd(outputdir)
write.csv(output_df, out_df_filename, row.names = FALSE)

# READ IN CLIMATE DATA 
# code copied from `climateanalogs.R` in GitHub repo

setwd(datadir2)

# function to read text files
readTextFilesIn <- function(dir_path = datadir2){
  myfun <- function(fl=paste(datadir2,"/city_id_001", sep="")){
    dfnarr <- read.table(fl, header=T, fill = TRUE, stringsAsFactors = F)
    cityN <- sub( ".txt", "", sub(".*/","",fl) ) # at first, remove left of /  and then *.txt
    dfnarr <- cbind(dfnarr, seq(as.Date("1979-01-01"), as.Date("2019-12-31"), by="days"))
    return(dfnarr)
  }
  filelist <- list.files(path = dir_path, pattern = ".*.txt", full.names = TRUE)
  datalist <- lapply(filelist, FUN =  myfun)
  return(datalist)
}

# call function 
narrdata <- readTextFilesIn(datadir2)

# read in city names from separate text file
analognames <- read.table(paste(path, 'MiscFiles/cities_loc.txt', sep = ''), header=T, fill = TRUE, stringsAsFactors = F, sep = ',')

oids <- c(2,3,4,8,12,14:16,19:22,24,25,31,32,38,42,47,51:53,56,58,63,66:68,70,71,74,76,78:80,82:85,92,94,93,97:99,102) # original cities
originalcities <- list()
for (i in 1:46) {
  originalcities[[i]] <- narrdata[[oids[i]]]
  originalcities[[i]] <- originalcities[[i]][which(originalcities[[i]][,8] > "2006-12-31" & originalcities[[i]][,8] < "2018-12-31"),]
}

originalmeans <- list(); originalmins <- list(); originalmaxs <- list(); originalsums <- list()

for (i in 1:46) {
  # convert dates
  originalcities[[i]][,8] <- format(originalcities[[i]][,8],'%Y-%m')
  
  # convert u and v wind to overall wind speed
  originalcities[[i]][,9] <- sqrt(originalcities[[i]][,6]^2+originalcities[[i]][,7]^2)
  
  # calculate precipitation count
  originalcities[[i]][,10] <- originalcities[[i]][,1]; originalcities[[i]][which(originalcities[[i]][,10] > 0),10] <- 1 
  
  # aggregate data from daily to monthly
  originalmeans[[i]] <- aggregate(originalcities[[i]][,c(2:5,9)], by = list(originalcities[[i]][,8]), FUN = mean) 
  originalmins[[i]] <- aggregate(originalcities[[i]][,c(2:5,9)], by = list(originalcities[[i]][,8]), FUN = min) 
  originalmaxs[[i]] <- aggregate(originalcities[[i]][,c(2:5,9)], by = list(originalcities[[i]][,8]), FUN = max) 
  originalsums[[i]] <- aggregate(originalcities[[i]][,c(1,10)], by = list(originalcities[[i]][,8]), FUN = sum)  
}

# convert temperature data
for (i in 1:46) {
  originalmeans[[i]][c(2,4)] <- originalmeans[[i]][c(2,4)] - 273.15
  originalmins[[i]][c(2,4)] <- originalmins[[i]][c(2,4)] - 273.15
  originalmaxs[[i]][c(2,4)] <- originalmaxs[[i]][c(2,4)] - 273.15
}

citynames <- analognames[oids,4]

# combine data
obsdata <- list()
for (i in 1:46) {
  # merge data
  obsdata[[i]] <- cbind(originalmeans[[i]][,1:6], originalmins[[i]][,2:6], originalmaxs[[i]][,2:6], originalsums[[i]][,2:3])
  names(obsdata[[i]]) <- c('date', 'avgdbt','avgwbt','avgdp','avgrh','avgws','mindbt','minwbt','mindp','minrh',
                           'minws','maxdbt','maxwbt','maxdp','maxrh','maxws','accpr','prcount')
  obsdata[[i]]$year <- as.numeric(sapply(strsplit(obsdata[[i]]$date,  '-'), '[', 1))
  obsdata[[i]]$month <- as.numeric(sapply(strsplit(obsdata[[i]]$date,  '-'), '[', 2))
}

# extract winter data
winterobsdata <- list()
for (i in 1:46) {
  winterobsdata[[i]] <- obsdata[[i]][which(obsdata[[i]]$month >= 12 | obsdata[[i]]$month <= 3),]
}

# CREATE FIGURES

# extract climate variables of interest
annualclimate <- list()
for (i in 1:46) {
  annualclimate[[i]] <- winterobsdata[[i]]%>%
    group_by(year) %>%
    summarize(precip = mean(accpr), temp = mean(avgdbt))
}

# Build Up Plot Data - Water

plotdata_water <- data.frame(population = c(), wateruse = c(), precip = c(), year = c(), cities = c())

for (i in 1:12){
  # extract each year
  mydf_annual <- my_df_list[[i]]
  
  # calculate logs
  ln_wateruse_population <- log(mydf_annual$wateruse_population)
  ln_wateruse <- log(mydf_annual$wateruse)
  
  # extract precipitation for each city
  precip_annual <- c()
  for (j in 1:46) {
    precip_annual <- append(precip_annual, annualclimate[[j]]$precip[i])
  }
  
  # remove points from precipitation with no data in a given year
  if (i == 1) {
    # remove Raleigh and Seattle values (no 2007 data)
    precip_annual <- precip_annual[-c(which(citynames == 'Raleigh'), which(citynames == 'Seattle'))]
  } else if (i == 2 | i == 3) {
    # remove Seattle values (no 2008 or 2009 data)
    precip_annual <- precip_annual[-c(which(citynames == 'Seattle'))]
  } else if (i == 11) {
    # remove Chicago, Cleveland, Columbus, Indianapolis, Madison, and Minneapolis values (no 2018 data)
    precip_annual <- precip_annual[-c(which(citynames == 'Chicago'), which(citynames == 'Cleveland'), which(citynames == 'Columbus'), which(citynames == 'Indianapolis'), which(citynames == 'Madison'), which(citynames == 'Minneapolis'))]
  } else if (i == 12) {
    # remove Albuquerque, Chicago, Cleveland, Columbus, Indianapolis, Madison, and Minneapolis values (no 2018 data)
    precip_annual <- precip_annual[-c(which(citynames == 'Albuquerque'), which(citynames == 'Chicago'), which(citynames == 'Cleveland'), which(citynames == 'Columbus'), which(citynames == 'Indianapolis'), which(citynames == 'Madison'), which(citynames == 'Minneapolis'))]
  }
  
  # combine plotdata
  plotdata_annual <- data.frame(population = ln_wateruse_population, wateruse = ln_wateruse, precip = precip_annual, year = mydf_annual$year, cities = mydf_annual$cityname)
  
  # append to complete plot data
  plotdata_water <- rbind(plotdata_water, plotdata_annual)
}

# create plots

# get labels
water_label_city_list <- c('phoenix', 'stpetersburg', 'austin', 'lasvegas', 'losangeles', 'nyc', 'sandiego')

# 2007 & 2018
plotdata2007_18 <- plotdata_water[which(plotdata_water$year == 2007 | plotdata_water$year == 2018),]

setwd(outputdir)
pdf('winterwater2007-18.pdf', width = 11, height = 6)
ggplot(plotdata2007_18, aes(x = population, y = wateruse)) +
  geom_point(aes(color = precip), size = 2.5) +
  scale_x_continuous(limits=c(11.5,16.1)) +
  scale_y_continuous(limits=c(5,11)) +
  geom_smooth(method = "lm", se = FALSE, color = 'black', linetype = 'dashed', size = 0.5) +
  xlab("Log(Service Population)") +
  ylab("Log(Water Use in Mgal)") +
  theme_light() + facet_wrap(~year) + theme(text = element_text(size = 20), legend.position="bottom", legend.key.width = unit(3, "cm")) +
  geom_text_repel(data = plotdata2007_18[which(plotdata2007_18$cities %in% water_label_city_list),], aes(label = cities), nudge_y = 0.3, nudge_x = -0.25, 
                  box.padding = 1.95, point.padding = 0.85, segment.color = 'grey50') +
  labs(color = "Avg Precipitation (mm)")
dev.off()

# all years

setwd(outputdir)
pdf('winterwater_allyears.pdf', width = 10.5, height = 9)
ggplot(plotdata_water, aes(x = population, y = wateruse)) +
  geom_point(aes(color = precip), size = 2.5) +
  scale_x_continuous(limits=c(11.5,16.1)) +
  scale_y_continuous(limits=c(5,11)) +
  geom_smooth(method = "lm", se = FALSE, color = 'black', linetype = 'dashed', size = 0.5) +
  xlab("Log(Service Population)") +
  ylab("Log(Water Use in Mgal)") +
  theme_light() + facet_wrap(~year) + theme(text = element_text(size = 20), legend.position="bottom", legend.key.width = unit(3, "cm")) +
  geom_text_repel(data = plotdata_water[which(plotdata_water$cities %in% water_label_city_list),], aes(label = cities), nudge_y = 0.3, nudge_x = -0.25, 
                  box.padding = 1.95, point.padding = 0.85, segment.color = 'grey50') +
  labs(color = "Avg Precipitation (mm)")
dev.off()

# Build Up Plot Data - Electricity

plotdata_elec <- data.frame(population = c(), elecuse = c(), temp = c(), year = c(), cities = c())

for (i in 1:12){
  # extract each year
  mydf_annual <- my_df_list[[i]]
  
  # calculate logs
  ln_elecuse_population <- log(mydf_annual$elecuse_population)
  ln_elecuse <- log(mydf_annual$elecuse)
  
  # extract precipitation for each city
  temp_annual <- c()
  for (j in 1:46) {
    temp_annual <- append(temp_annual, annualclimate[[j]]$temp[i])
  }
  
  # remove points from precipitation with no data in a given year
  if (i == 1) {
    # remove Raleigh and Seattle values (no 2007 data)
    temp_annual <- temp_annual[-c(which(citynames == 'Raleigh'), which(citynames == 'Seattle'))]
  } else if (i == 2 | i == 3) {
    # remove Seattle values (no 2008 or 2009 data)
    temp_annual <- temp_annual[-c(which(citynames == 'Seattle'))]
  } else if (i == 11) {
    # remove Chicago, Cleveland, Columbus, Indianapolis, Madison, and Minneapolis values (no 2018 data)
    temp_annual <- temp_annual[-c(which(citynames == 'Chicago'), which(citynames == 'Cleveland'), which(citynames == 'Columbus'), which(citynames == 'Indianapolis'), which(citynames == 'Madison'), which(citynames == 'Minneapolis'))]
  } else if (i == 12) {
    # remove Albuquerque, Chicago, Cleveland, Columbus, Indianapolis, Madison, and Minneapolis values (no 2018 data)
    temp_annual <- temp_annual[-c(which(citynames == 'Albuquerque'), which(citynames == 'Chicago'), which(citynames == 'Cleveland'), which(citynames == 'Columbus'), which(citynames == 'Indianapolis'), which(citynames == 'Madison'), which(citynames == 'Minneapolis'))]
  }
  
  # combine plotdata
  plotdata_annual <- data.frame(population = ln_elecuse_population, elecuse = ln_elecuse, temp = temp_annual, year = mydf_annual$year, cities = mydf_annual$cityname)
  
  # append to complete plot data
  plotdata_elec <- rbind(plotdata_elec, plotdata_annual)
}

# create plots

# get labels
elec_label_city_list <- c('seattle', 'coloradosprings', 'raleigh', 'losangeles', 'nyc', 'sanfrancisco', 'sandiego', 'miami', 'charlotte')

# 2007 & 2018
plotdata2007_18 <- plotdata_elec[which(plotdata_elec$year == 2007 | plotdata_elec$year == 2018),]

setwd(outputdir)
pdf('winterelectricity2007-18.pdf', width = 11, height = 6)
ggplot(plotdata2007_18, aes(x = population, y = elecuse)) +
  geom_point(aes(color = temp), size = 2.5) +
  scale_x_continuous(limits=c(11.9,15.5)) +
  scale_y_continuous(limits=c(11.5,15.5)) +
  geom_smooth(method = "lm", se = FALSE, color = 'black', linetype = 'dashed', size = 0.5) +
  xlab("Log(Service Population)") +
  ylab("Log(Electricity Use in MWh)") +
  scale_colour_gradient(low = '#fdd0a2', high = '#7f2704') +
  theme_light() + facet_wrap(~year) + theme(text = element_text(size = 20), legend.position="bottom", legend.key.width = unit(3, "cm")) +
  geom_text_repel(data = plotdata2007_18[which(plotdata2007_18$cities %in% elec_label_city_list),], aes(label = cities),
                  box.padding = 1.95, point.padding = 0.85, segment.color = 'grey50', min.segment.length = 0.01) +
  labs(color = "Avg Temperature (degC)")
dev.off()

# all years

setwd(outputdir)
pdf('winterelectricity_allyears.pdf', width = 10.5, height = 9)
ggplot(plotdata_elec, aes(x = population, y = elecuse)) +
  geom_point(aes(color = temp), size = 2.5) +
  scale_x_continuous(limits=c(11.5,15.5)) +
  scale_y_continuous(limits=c(11.5,15.5)) +
  geom_smooth(method = "lm", se = FALSE, color = 'black', linetype = 'dashed', size = 0.5) +
  xlab("Log(Service Population)") +
  ylab("Log(Electricity Use in MWh)") +
  scale_colour_gradient(low = '#fdd0a2', high = '#7f2704') +
  theme_light() + facet_wrap(~year) + theme(text = element_text(size = 20), legend.position="bottom", legend.key.width = unit(3, "cm")) +
  geom_text_repel(data = plotdata_elec[which(plotdata_elec$cities %in% elec_label_city_list),], aes(label = cities),
                  box.padding = 1.95, point.padding = 0.85, segment.color = 'grey50', min.segment.length = 0.01) +
  labs(color = "Avg Temperature (degC)")
dev.off()

