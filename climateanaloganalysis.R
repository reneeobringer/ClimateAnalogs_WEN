# Project: Projecting the Water-Energy Demand Nexus using Climate Analogs
# Code By: Renee Obringer
# Last Updated: 26 April 2022

# ORGANIZATION: 
# This code is organized into sections, the start of each is denoted by multiple #
# The sections can be run independently by loading the rdata files at the beginning of each section
# Each section is described below
#
# LOAD DATA: load the data, including NARR climate data, analog information, and water/electricity consumption data
# DATA PRE_PROCESSING: de-trend water/electricity consumption data, convert units, reformat analog information, separate into NOAA climate regions
# VARIABLE SELECTION (BY REGION): determine the important variables for the region
# MODEL RUN (BY CITY): run model for each city, using variables selected for the region
# PERCENT CHANGE ANALYSIS: calculate the percent change for each of the cities of interest, based on the predicted water/electricity demand for the analogs
# FIGURES: code for plotting figures included in manuscript

rm(list=ls())

# libraries
devtools::install_github("patr1ckm/mvtboost") # NOTE: MVTboost is no longer available on CRAN

library(mvtboost)           # for modeling
library(stringr)            # for data loading
library(measurements)       # for converting units
library(gbm)                # for modeling
library(ggplot2)            # for plotting
library(cowplot)            # for plotting
library(scatterpie)         # for plotting
library(rnaturalearth)      # for plotting
library(dplyr)              # for data formatting
library(sf)                 # for plotting

# set file path
# NOTE: set this path to the folder on your personal machine which contains the downloaded data
# for example: path <- '/Users/rqo5125/Downloads/ClimateAnalogs_WEN-main'

path <- '    ' # main file path

# set directories

datadir1 <- paste(path,'/ClimateData/', sep = '')   # contains the climate data obtained from NARR
datadir2 <- paste(path,'/UtilityData/', sep = '')   # contains the water/electricity consumption data
rdatadir <- paste(path,'/rdatafiles/', sep = '')    # contains the rdatafiles
miscdir <- paste(path,'/MiscFiles/', sep = '')      # contains miscellaneous files used for plotting

# OPTIONAL: create an output directory
outputdir <- paste(path,'/output/', sep = '')      # The output directory will be where you send any non-rdata output files (e.g., csv, pdf, etc.)
dir.create(outputdir)

############# LOAD DATA ######################

#### NARR data

# function to read text files
readTextFilesIn <- function(dir_path = datadir1){
  myfun <- function(fl=paste(datadir1,"/city_id_001", sep="")){
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
narrdata <- readTextFilesIn(datadir1)

# read in city names from separate text file
analognames <- read.table(paste(path,'/cities_loc.txt',sep=''), header=T, fill = TRUE, stringsAsFactors = F, sep = ',')

#### water and electricity use data
setwd(datadir2)
filenames <- list.files(pattern="*.csv", full.names=F)
ldf <- lapply(filenames, read.csv)
citynames <- str_remove(filenames,'.csv')

############# DATA PRE_PROCESSING ######################

# water and electricity use data pre-processing: de-trending and converting units

for (i in 1:46) {
  city <- ldf[[i]]
  
  # convert data types
  if (typeof(city[,4]) != "numeric") {
    city[,4] <- as.numeric(as.character(city[,4]))
  }
  
  if (typeof(city[,3]) != "numeric") {
    city[,3] <- as.numeric(as.character(city[,3]))
  }
  
  # Convert to metric units 
  city[,3] <- conv_unit(city[,3],"us_gal","l")  # gal/cap --> L/cap
  
  # detrend data (Sailor and Munoz 1997: https://doi.org/10.1016/S0360-5442(97)00034-0)
  
  fullmean <- colMeans(city[,3:4],na.rm=T) # get mean over entire period
  yearlymeans <- aggregate(x = city[,3:4], by = list(city[,1]), function(x) mean(x, na.rm=TRUE)) # get yearly means
  yearlymeans <- yearlymeans[,2:3]
  
  nyear <- floor(nrow(city)/12)
  
  # get adjustment term for each year and each variable
  fadj <- matrix(numeric(0), nrow = nyear, ncol = 2)
  for (y in 1:nyear) {
    for (v in 1:2) {
      fadj[y,v] <- yearlymeans[y,v]/fullmean[v]
    }
  }
  
  # convert from yearly to monthly matrix
  monthlyfadj <- fadj[rep(1:nrow(fadj), c(rep(12,nyear))), ]
  
  # remove trend
  detrendedwat <- city[,3]/monthlyfadj[,1]
  detrendedele <- city[,4]/monthlyfadj[,2]
  
  ldf[[i]][,3] <- detrendedwat
  ldf[[i]][,4] <- detrendedele
  
  ldf[[i]] <- ldf[[i]][,-c(5:21)]
  
}

# NARR data pre-processing: separating into analogs, converting units, and aggregating 

# hard code in the ids for each city and analog
oids <- c(2,3,4,8,12,14:16,19:22,24,25,31,32,38,42,47,51:53,56,58,63,66:68,70,71,74,76,78:80,82:85,92,94,93,97:99,102) # original cities
ca1ids <- c(37,23,64,1,59,86,89,87,13,73,28,23,100,54,9,6,28,26,55,60,41,75,103,40,44,29,34,11,9,57,62,90,50,40,64,
            89,69,60,71,61,5,39,39,9,95,26) # low warming analogs
ca2ids <- c(88,7,64,77,96,45,101,18,43,48,41,7,96,54,27,62,41,30,10,46,6,45,81,45,41,91,26,17,27,50,96,37,65,45,64,101,
            69,46,57,72,33,17,17,35,36,49) # high warming analogs

# extract NARR data for each city
originalcities <- list()
analog1cities <- list()
analog2cities <- list()
for (i in 1:46) {
  originalcities[[i]] <- narrdata[[oids[i]]]
  analog1cities[[i]] <- narrdata[[ca1ids[i]]]
  analog2cities[[i]] <- narrdata[[ca2ids[i]]]
}

# extract dates of interest
for (i in 1:46) {
  originalcities[[i]] <- originalcities[[i]][which(originalcities[[i]][,8] > "2006-12-31" & originalcities[[i]][,8] < "2018-12-31"),]
  analog1cities[[i]] <- analog1cities[[i]][which(analog1cities[[i]][,8] > "2006-12-31" & analog1cities[[i]][,8] < "2018-12-31"),]
  analog2cities[[i]] <- analog2cities[[i]][which(analog2cities[[i]][,8] > "2006-12-31" & analog2cities[[i]][,8] < "2018-12-31"),]
}

# convert and aggregate data
originalmeans <- list(); originalmins <- list(); originalmaxs <- list(); originalsums <- list()
analog1means <- list(); analog1mins <- list(); analog1maxs <- list(); analog1sums <- list()
analog2means <- list(); analog2mins <- list(); analog2maxs <- list(); analog2sums <- list()
for (i in 1:46) {
  # convert dates
  originalcities[[i]][,8] <- format(originalcities[[i]][,8],'%Y-%m')
  analog1cities[[i]][,8] <- format(analog1cities[[i]][,8],'%Y-%m')
  analog2cities[[i]][,8] <- format(analog2cities[[i]][,8],'%Y-%m')
  
  # convert u and v wind to overall wind speed
  originalcities[[i]][,9] <- sqrt(originalcities[[i]][,6]^2+originalcities[[i]][,7]^2)
  analog1cities[[i]][,9] <- sqrt(analog1cities[[i]][,6]^2+analog1cities[[i]][,7]^2)
  analog2cities[[i]][,9] <- sqrt(analog2cities[[i]][,6]^2+analog2cities[[i]][,7]^2)
  
  # calculate precipitation count
  originalcities[[i]][,10] <- originalcities[[i]][,1]; originalcities[[i]][which(originalcities[[i]][,10] > 0),10] <- 1 
  analog1cities[[i]][,10] <- analog1cities[[i]][,1]; analog1cities[[i]][which(analog1cities[[i]][,10] > 0),10] <- 1 
  analog2cities[[i]][,10] <- analog2cities[[i]][,1]; analog2cities[[i]][which(analog2cities[[i]][,10] > 0),10] <- 1 
  
  # aggregate data from daily to monthly
  originalmeans[[i]] <- aggregate(originalcities[[i]][,c(2:5,9)], by = list(originalcities[[i]][,8]), FUN = mean) 
  originalmins[[i]] <- aggregate(originalcities[[i]][,c(2:5,9)], by = list(originalcities[[i]][,8]), FUN = min) 
  originalmaxs[[i]] <- aggregate(originalcities[[i]][,c(2:5,9)], by = list(originalcities[[i]][,8]), FUN = max) 
  originalsums[[i]] <- aggregate(originalcities[[i]][,c(1,10)], by = list(originalcities[[i]][,8]), FUN = sum)  

  analog1means[[i]] <- aggregate(analog1cities[[i]][,c(2:5,9)], by = list(analog1cities[[i]][,8]), FUN = mean) 
  analog1mins[[i]] <- aggregate(analog1cities[[i]][,c(2:5,9)], by = list(analog1cities[[i]][,8]), FUN = min) 
  analog1maxs[[i]] <- aggregate(analog1cities[[i]][,c(2:5,9)], by = list(analog1cities[[i]][,8]), FUN = max) 
  analog1sums[[i]] <- aggregate(analog1cities[[i]][,c(1,10)], by = list(analog1cities[[i]][,8]), FUN = sum) 
  
  analog2means[[i]] <- aggregate(analog2cities[[i]][,c(2:5,9)], by = list(analog2cities[[i]][,8]), FUN = mean) 
  analog2mins[[i]] <- aggregate(analog2cities[[i]][,c(2:5,9)], by = list(analog2cities[[i]][,8]), FUN = min) 
  analog2maxs[[i]] <- aggregate(analog2cities[[i]][,c(2:5,9)], by = list(analog2cities[[i]][,8]), FUN = max) 
  analog2sums[[i]] <- aggregate(analog2cities[[i]][,c(1,10)], by = list(analog2cities[[i]][,8]), FUN = sum) 
}

# convert temperature data
for (i in 1:46) {
  originalmeans[[i]][c(2,4)] <- originalmeans[[i]][c(2,4)] - 273.15
  originalmins[[i]][c(2,4)] <- originalmins[[i]][c(2,4)] - 273.15
  originalmaxs[[i]][c(2,4)] <- originalmaxs[[i]][c(2,4)] - 273.15
  
  analog1means[[i]][c(2,4)] <- analog1means[[i]][c(2,4)] - 273.15
  analog1mins[[i]][c(2,4)] <- analog1mins[[i]][c(2,4)] - 273.15
  analog1maxs[[i]][c(2,4)] <- analog1maxs[[i]][c(2,4)] - 273.15
  
  analog2means[[i]][c(2,4)] <- analog2means[[i]][c(2,4)] - 273.15
  analog2mins[[i]][c(2,4)] <- analog2mins[[i]][c(2,4)] - 273.15
  analog2maxs[[i]][c(2,4)] <- analog2maxs[[i]][c(2,4)] - 273.15
}

# combine utility data with NARR data in original cities
obsdata <- list()
for (i in 1:46) {
  # get water and electricity data
  obsdata[[i]] <- ldf[[i]][1:144,]
  
  # merge data
  obsdata[[i]][,5:9] <- originalmeans[[i]][,2:6]
  obsdata[[i]][,10:14] <- originalmins[[i]][,2:6]
  obsdata[[i]][,15:19] <- originalmaxs[[i]][,2:6]
  obsdata[[i]][,20:21] <- originalsums[[i]][,2:3]
  names(obsdata[[i]]) <- c('year','month','wateruse','elecuse', 'avgdbt','avgwbt','avgdp','avgrh','avgws','mindbt','minwbt','mindp','minrh',
                      'minws','maxdbt','maxwbt','maxdp','maxrh','maxws','accpr','prcount')
}

# separate into NOAA regions
northwest <- rbind(obsdata[[30]], obsdata[[39]]) # Portland, Seattle
west <- rbind(obsdata[[33]],obsdata[[41]],obsdata[[20]],obsdata[[38]],obsdata[[36]],obsdata[[7]], obsdata[[37]], obsdata[[32]],obsdata[[19]]) # Sacramento, Stockton, Los Angeles, Santa Ana, San Diego, Chula Vista, San Francisco, Reno, Las Vegas 
southwest <- rbind(obsdata[[12]],obsdata[[2]],obsdata[[10]], obsdata[[29]],obsdata[[15]],obsdata[[44]],obsdata[[1]],obsdata[[14]]) # Denver, Aurora, Colorado Springs, Phoenix, Glendale, Tucson, Albuquerque, El Paso
south <- rbind(obsdata[[26]],obsdata[[45]],obsdata[[46]],obsdata[[3]],obsdata[[35]]) # Oklahoma City, Tulsa, Wichita, Austin, San Antonio
eastnorthcentral <- rbind(obsdata[[27]],obsdata[[22]],obsdata[[24]],obsdata[[34]]) # Omaha, Madison, Minneapolis, Saint Paul
central <- rbind(obsdata[[6]],obsdata[[17]],obsdata[[9]],obsdata[[11]],obsdata[[8]],obsdata[[40]], obsdata[[18]], obsdata[[21]]) # Chicago, Indianapolis, Cleveland, Columbus, Cincinnati, St Louis, Kansas City, Louisville
southeast <- rbind(obsdata[[23]],obsdata[[28]],obsdata[[43]],obsdata[[42]],obsdata[[5]],obsdata[[13]],obsdata[[31]],obsdata[[16]]) # Miami, Orlando, Tampa, St Petersburg,Charlotte, Durham, Raleigh, Greensboro
northeast <- rbind(obsdata[[4]],obsdata[[25]]) # Boston, NYC

regions <- list(northwest,west,southwest,south,eastnorthcentral,central,southeast,northeast)
names(regions) <- c('Northwest','West','Southwest','South','East North Central','Central','Southeast','Northeast')

# combine NARR data in analog cities
analog1data <- list()
analog2data <- list()
for (i in 1:46) {
  # get year/month data
  analog1data[[i]] <- ldf[[i]][c(1:144),c(1:2)]
  analog2data[[i]] <- ldf[[i]][c(1:144),c(1:2)]
  
  # merge data
  analog1data[[i]][,3:7] <- analog1means[[i]][,2:6]
  analog1data[[i]][,8:12] <- analog1mins[[i]][,2:6]
  analog1data[[i]][,13:17] <- analog1maxs[[i]][,2:6]
  analog1data[[i]][,18:19] <- analog1sums[[i]][,2:3]
  names(analog1data[[i]]) <- c('year','month','avgdbt','avgwbt','avgdp','avgrh','avgws','mindbt','minwbt','mindp','minrh',
                           'minws','maxdbt','maxwbt','maxdp','maxrh','maxws','accpr','prcount')
  
  analog2data[[i]][,3:7] <- analog2means[[i]][,2:6]
  analog2data[[i]][,8:12] <- analog2mins[[i]][,2:6]
  analog2data[[i]][,13:17] <- analog2maxs[[i]][,2:6]
  analog2data[[i]][,18:19] <- analog2sums[[i]][,2:3]
  names(analog2data[[i]]) <- c('year','month','avgdbt','avgwbt','avgdp','avgrh','avgws','mindbt','minwbt','mindp','minrh',
                               'minws','maxdbt','maxwbt','maxdp','maxrh','maxws','accpr','prcount')
}

# Save data
setwd(rdatadir)
save(list=c("citynames","obsdata","analog1data","analog2data","regions"), file="preprocessedinputdata.RDATA")


############# VARIABLE SELECTION (BY REGION) ######################

# load rdata file from previous section, if necessary
setwd(rdatadir)
load("preprocessedinputdata.RDATA")

# initialize some variables and parameters for the model
newmods <- list()
allnewri <- list()
'%notin%' <- Negate('%in%') # define custom operator
numformat <- function(val){sub("^(-?)0.", "\\1.", sprintf("%5.2f", val))} # set number format
blues <- c('#deebf7','#c6dbef','#9ecae1','#6baed6','#4292c6','#2171b5','#08519c','#08306b') # create color ramp
bgr <- colorRampPalette(blues,space = "Lab",bias = 7)
regionnames <- names(regions)

# loop through each region
for (i in 1:8){
  
  # set region
  region <- regions[[i]]
  
  # create a new dataframe for summer
  summer <- rbind(region[which(region[,2] >= 6 & region[,2] <= 9),])
  
  # remove NA values
  summer <- na.omit(summer)
  
  # set up 5-fold cross validation
  k <- 5                                               # number of folds
  n <- nrow(summer)                                    # number of data points
  set.seed(11)                                         
  summer <- summer[sample(n),]                         # randomize the data
  folds <- cut(seq(1,n),breaks = k, labels = FALSE)    # split the randomized data into k folds
  
  # initialize measures of error
  Rallnrmsew <- c()
  Rallnrmsee <- c()
  
  # initial model run 
  for (j in 1:k) {
    
    # separate training and test data
    testIndex <- which(folds == j, arr.ind = TRUE)
    testData <- summer[testIndex,]
    trainData <- summer[-testIndex,]
    
    Y <- trainData[,3:4] 
    X <- trainData[,c(5:21)] 
    
    # run model
    out <- mvtb(Y=Y,X=X, n.trees=1000, shrinkage=.01, interaction.depth=3)
    
    # fit model
    yhat <- predict(out,newdata=testData[,c(5:21)])
    
    # calculate measures of error
    Rallnrmsew[j] <- sqrt(sum((yhat[,1]-testData[,3])^2)/nrow(testData))/(range(testData[,3])[2]-range(testData[,3])[1])
    Rallnrmsee[j] <- sqrt(sum((yhat[,2]-testData[,4])^2)/nrow(testData))/(range(testData[,4])[2]-range(testData[,4])[1])
  }
  
  # VARIABLE SELECTION:
  
  # set relative importance based on initial model run
  Rrelinf <- mvtb.ri(out, relative = 'col')
  
  # initialize variables
  newnrmsew <- mean(Rallnrmsew)
  newnrmsee <- mean(Rallnrmsee)
  newrelinf <- Rrelinf
  newvars <- summer
  Rnrmsew <- c()
  Rnrmsee <- c()
  
  # iteratively remove least important variables while there is no significant change in predictive accuracy
  flag <- 1
  while (flag == 1) {
    # store old values
    oldnrmsew <- newnrmsew
    oldnrmsee <- newnrmsee
    oldrelinf <- newrelinf
    oldmod <- out
    
    # sort important variables
    impw <- sort(oldrelinf[,1])
    impe <- sort(oldrelinf[,2])
    
    # remove 2 lowest RI if not in the other variables top 4
    if (names(impe[1]) %notin% names(tail(impw, n=4)) & names(impw[1]) %notin% names(tail(impe, n=4))) {
      drops <- c(names(impw[1]),names(impe[1]))
      newvars <- newvars[,!(names(newvars) %in% drops)]
    } else {
      if (names(impe[1]) %in% names(tail(impw, n=4))) {
        drops <- c(names(impw[1]),names(impe[2]))
        newvars <- newvars[,!(names(newvars) %in% drops)]
      } else {
        drops <- c(names(impw[2]),names(impe[1]))
        newvars <- newvars[,!(names(newvars) %in% drops)]
      }
    }
    
    # rurun model w/ updated variables
    for (j in 1:k) {
      
      # set training and test data
      testIndex <- which(folds == j, arr.ind = TRUE)
      testData <- newvars[testIndex,]
      trainData <- newvars[-testIndex,]
      
      Y <- trainData[,3:4] 
      X <- trainData[,c(5:ncol(newvars))] 
      
      # run model
      out <- mvtb(Y=Y,X=X, n.trees=1000, shrinkage=.01, interaction.depth=3)
      
      # fit model
      yhat <- predict(out,newdata=testData[,c(5:ncol(newvars))])
      
      # calculate measures of error
      Rnrmsew[j] <- sqrt(sum((yhat[,1]-testData[,3])^2)/nrow(testData))/(range(newvars[,3])[2]-range(newvars[,3])[1])
      Rnrmsee[j] <- sqrt(sum((yhat[,2]-testData[,4])^2)/nrow(testData))/(range(newvars[,4])[2]-range(newvars[,4])[1])
    }
    
    # average measures of error from cross validation folds
    newnrmsew <- mean(Rnrmsew)
    newnrmsee <- mean(Rnrmsee)
    newrelinf <- mvtb.ri(out, relative = 'col')
    
    # check predictive accuracy, if it hasn't changed significantly, continue with loop, else stop
    if (newnrmsew <= oldnrmsew + 0.01 & newnrmsee <= oldnrmsee + 0.01) {
      relinf <- newrelinf
      flag <- 1
    } else {
      relinf <- oldrelinf
      finalmod <- oldmod
      flag <- 0
    }
    
    # if fewer than 7 variables remain, stop
    if (length(relinf) == 6) {
      relinf <- oldrelinf
      finalmod <- oldmod
      flag <- 0
    }
  }
  
  # store data
  newmods[[i]] <- finalmod
  allnewri[[i]] <- relinf
  
  # save images of heat maps showing relative importance
  setwd(outputdir)
  filename = paste(regionnames[i],'.png',sep='')
  png(file=filename, width = 1000,height = 500)
  par(mar=c(15,8,1,1),mfrow=c(1,1),cex=1.5)
  mvtb.heat(t(relinf),clust.method = NULL,cexRow=1,cexCol=1,numformat=numformat, col = bgr(500))
  dev.off()
}

# save data
setwd(rdatadir)
save(list=c("regions","regionnames","newmods","allnewri"),file="variableselection.RDATA")



############# MODEL RUN (BY CITY) ######################

# load rdata files, if necessary
setwd(rdatadir)
load("preprocessedinputdata.RDATA")
load("variableselection.RDATA")


# initialize variables
lowwarm <- list()
highwarm <- list()
relinf <- list()
avgrsq <- list()
avgnrmsew <- list()
avgnrmsee <- list()
cityyhat <- list()
citytest <- list()
citymods <- list()

regionlist <- c('Southwest','Southwest','South','Northeast','Southeast','Central','West','Central','Central','Southwest',
                'Central','Southwest','Southeast','Southwest','Southwest','Southeast','Central','Central','West','West',
                'Central','East North Central','Southeast','East North Central','Northeast','South','East North Central',
                'Southeast','Southwest','Northwest','Southeast','West','West','East North Central','South','West','West',
                'West','Northwest','Central','West','Southeast','Southeast','Southwest','South','South')

# loop through each city
for (i in 1:46) {
  
  # get final variable names from variable selection
  varnames <- names(allnewri[[which(regionnames == regionlist[i])]][,1])
  
  # select subset using only final variables from variable selection
  ndata <- cbind(obsdata[[i]][which(obsdata[[i]]$month >= 6 & obsdata[[i]]$month <= 9),c(3:4)],obsdata[[i]][which(obsdata[[i]]$month >= 6 & obsdata[[i]]$month <= 9),varnames])
  projdata1 <- analog1data[[i]][which(analog1data[[i]]$month >= 6 & analog1data[[i]]$month <= 9),varnames]
  projdata2 <- analog2data[[i]][which(analog2data[[i]]$month >= 6 & analog2data[[i]]$month <= 9),varnames]
  
  ndata <- na.omit(ndata)

  # set up cross validation
  n <- nrow(ndata)
  
  # initialize variables
  rsqs <- list()
  nrmsew <- c()
  nrmsee <- c()
  testlength <- c()
  allytestw <- c()
  allyteste <- c()
  allyhatsw <- c()
  allyhatse <- c()
  
  # LOOCV due to small sample size
  for (j in 1:n) {
    
    # separate out training and test data
    testData <- ndata[j,]
    trainData <- ndata[-j,]
  
    Y <- trainData[,1:2] 
    X <- trainData[,c(3:ncol(ndata))] 
    
    # run model
    out <- mvtb(Y=Y,X=X, n.trees=1000, shrinkage=.01, interaction.depth=3)
    
    # fit model
    yhat <- predict(out,newdata=testData[c(3:ncol(testData))])
    
    # calculate measures of error
    nrmsew[j] <- sqrt(sum((yhat[1]-testData[1])^2)/nrow(testData))/(range(ndata[,1])[2]-range(ndata[,1])[1])
    nrmsee[j] <- sqrt(sum((yhat[2]-testData[2])^2)/nrow(testData))/(range(ndata[,2])[2]-range(ndata[,2])[1])
    
    allytestw[j] <- testData[,1]
    allyteste[j] <- testData[,2]
    allyhatsw[j] <- yhat[1]
    allyhatse[j] <- yhat[2]
  }
  
  # projections
  lowwarm[[i]] <- predict(out,newdata=projdata1)
  highwarm[[i]] <- predict(out,newdata=projdata2)
  
  # relative influence of predictors
  relinf[[i]] <- mvtb.ri(out, relative = 'col')
  
  # store important values
  cityyhat[[i]] <- cbind(allyhatsw, allyhatse)
  citytest[[i]] <- cbind(allytestw, allyteste)
  citymods[[i]] <- out
  
  # average model performance
  avgrsq[[i]] <- diag(var(cityyhat[[i]])/var(citytest[[i]]))
  avgnrmsew[[i]] <- mean(nrmsew)
  avgnrmsee[[i]] <- mean(nrmsee)
}

# save data
save(list=c("lowwarm","highwarm","relinf","avgrsq","avgnrmsew","avgnrmsee","cityyhat","citytest", "citymods"),file="modelrunresults.RDATA")

############# PERCENT CHANGE ANALYSIS ######################

# load rdata files, if necessary
setwd(rdatadir)
load("preprocessedinputdata.RDATA")
load("variableselection.RDATA")
load("modelrunresults.RDATA")

# percent change under different warming scenarios (pos == % increase) [new-old/old]

# initialize variables
pcw_low <- list()
pcw_high <- list()
pce_low <- list()
pce_high <- list()

# calculate percent change for each city
for (i in 1:46) {
  
  # extract data from lists
  currentw <- mean(cityyhat[[i]][,1])
  futurew1 <- mean(lowwarm[[i]][,1], na.rm=T)
  futurew2 <- mean(highwarm[[i]][,1], na.rm=T)
  currente <- mean(cityyhat[[i]][,2])
  futuree1 <- mean(lowwarm[[i]][,2], na.rm=T)
  futuree2 <- mean(highwarm[[i]][,2], na.rm=T)
  
  # calculate percent change
  pcw_low[[i]] <- (futurew1 - currentw)/currentw*100
  pcw_high[[i]] <- (futurew2 - currentw)/currentw*100
  
  pce_low[[i]] <- (futuree1 - currente)/currente*100
  pce_high[[i]] <- (futuree2 - currente)/currente*100
}

# save data
save(pcw_low, pcw_high, pce_low, pce_high, file='percentchange.rdata')

############# FIGURES ######################

# load rdata files, if necessary
setwd(rdatadir)
load("preprocessedinputdata.RDATA")
load("variableselection.RDATA")
load("modelrunresults.RDATA")
load("percentchange.RDATA")

# set up data for labels in plots
cities <- c('Albuquerque', 'Aurora', 'Austin', 'Boston', 'Charlotte', 'Chicago', 'Chula Vista', 'Cincinnati', 'Cleveland', 'Colorado Springs',
            'Columbus', 'Denver', 'Durham', 'El Paso', 'Glendale', 'Greensboro', 'Indianapolis', 'Kansas City', 'Las Vegas', 'Los Angeles', 
            'Louisville', 'Madison', 'Miami', 'Minneapolis', 'New York City', 'Oklahoma City', 'Omaha', 'Orlando', 'Phoenix', 'Portland', 
            'Raleigh', 'Reno', 'Sacramento', 'Saint Paul', 'San Antonio', 'San Diego', 'San Francisco', 'Santa Ana', 'Seattle', 'St. Louis',
            'Stockton', 'St. Petersburg', 'Tampa', 'Tucson', 'Tulsa', 'Wichita')

states <- c('NM', 'CO','TX', 'MA', 'NC', 'IL', 'CA', 'OH', 'OH', 'CO', 'OH', 'CO', 'NC', 'TX', 'AZ', 'NC', 'IN', 'MO', 'NV', 'CA', 'KY', 'WI', 'FL', 
            'MN', 'NY', 'OK', 'NE', 'FL', 'AZ', 'OR', 'NC', 'NV', 'CA', 'MN', 'TX', 'CA', 'CA', 'CA', 'WA', 'MO', 'CA', 'FL', 'FL', 'AZ', 'OK', 'KS')

regions <- c('Southwest','Southwest','South','Northeast','Southeast','Central','West','Central','Central','Southwest',
             'Central','Southwest','Southeast','Southwest','Southwest','Southeast','Central','Central','West','West',
             'Central','East North Central','Southeast','East North Central','Northeast','South','East North Central',
             'Southeast','Southwest','Northwest','Southeast','West','West','East North Central','South','West','West',
             'West','Northwest','Central','West','Southeast','Southeast','Southwest','South','South')

# read csv with high warming data + population
setwd(miscdir)
highwarmdata <- read.csv('highwarmingdata.csv')

# FIGURE 1a

# extract total and summer-only water and electricity consumption
totalelec <- c()
totalwater <- c()
summerelec <- c()
summerwater <- c()
for (i in 1:46) {
  totalelec[i] <- sum(obsdata[[i]][,4], na.rm = T)
  totalwater[i] <- sum(obsdata[[i]][,3], na.rm = T)
  summerelec[i] <- sum(obsdata[[i]][which(obsdata[[i]][,2] >= 6 & obsdata[[i]][,2] <= 9),4], na.rm = T)
  summerwater[i]<- sum(obsdata[[i]][which(obsdata[[i]][,2] >= 6 & obsdata[[i]][,2] <= 9),3], na.rm = T)
}

# convert water from L to m^3
Twater_cm <- totalwater*0.001
Summer <- summerwater*0.001

# create data frames
waterdata <- data.frame(cities, states, Twater_cm, Summer)
elecdata <- data.frame(cities, states, totalelec, summerelec)

# add addittional variables from plotting
waterdata['xloc'] <- (1:46)
waterdata['yloc'] <- rep(1,46)
waterdata['Other'] <- waterdata$Twater_cm - waterdata$Summer
waterdata['Norm_Total'] <- waterdata$Twater_cm/max(waterdata$Twater_cm)

elecdata['xloc'] <- (1:46)
elecdata['yloc'] <- rep(1,46)
elecdata['Other'] <- elecdata$totalelec - elecdata$summerelec
elecdata['Norm_Total'] <- elecdata$totalelec/max(elecdata$totalelec)
elecdata['Summer'] <- summerelec

p1 <- ggplot() + geom_scatterpie(aes(x = xloc, y = yloc, r = Norm_Total), data = waterdata, cols = c('Other', 'Summer'), color=NA) + 
  coord_equal() + theme_light() + theme(axis.text = element_blank(), axis.ticks.y = element_blank(), text = element_text(size = 16),
                                        plot.margin = unit(c(0,0,0,0), 'cm'), panel.grid = element_blank(),
                                        panel.border = element_blank()) + ylim(0,2.25) + xlim(0,54) +
  ylab('') + xlab('') + geom_scatterpie_legend(waterdata$Norm_Total, x=49, y=1,n=2, labeller=function(x) round(max(waterdata$Twater_cm)*x, -2)) +
  scale_fill_manual(name = "Water", values = c('darkblue', 'lightblue')) + scale_x_continuous(breaks = c(1:46))

p2 <- ggplot() + geom_scatterpie(aes(x = xloc, y = yloc, r = Norm_Total), data = elecdata, cols = c('Other', 'Summer'), color=NA) + 
  coord_equal() + theme_light() + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), text = element_text(size = 16),
                                        plot.margin = unit(c(0,0,0,0), 'cm'), panel.grid = element_blank(),
                                        panel.border = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) + ylim(0,2.25) + xlim(0,54) +
  ylab('') + xlab('Cities') + geom_scatterpie_legend(elecdata$Norm_Total, x=49, y=1,n=2, labeller=function(x) round(max(elecdata$totalelec)*x, -1)) +
  scale_fill_manual(name = "Electricity", values = c('darkorange', '#fed8b1')) + scale_x_continuous(breaks = c(1:46), labels = elecdata$cities)

# save figure as pdf
setwd(outputdir)
pdf('figure1a.pdf', width = 15, height = 3.5)
plot_grid(p1,p2,nrow = 2, rel_heights = c(1/3, 2/3))
dev.off()

# FIGURE 1b

# load map data
states <- ne_states(country = 'united states of america', returnclass = "sf")

sm <- matrix(c(2,1,0,1), 2, 2)
states$geometry <- states$geometry * sm

# initialize variables
drybulbtemp <- c()
wetbulbtemp <- c()
dewpointtemp <- c()
relhumidity <- c()
windspeed <- c()
accprecip <- c()

# extract average of each input variable
for (i in 1:46) {
  drybulbtemp[i] <- mean(obsdata[[i]][which(obsdata[[i]][,2] >= 6 & obsdata[[i]][,2] <= 9), 5])
  wetbulbtemp[i] <- mean(obsdata[[i]][which(obsdata[[i]][,2] >= 6 & obsdata[[i]][,2] <= 9), 6])
  dewpointtemp[i] <- mean(obsdata[[i]][which(obsdata[[i]][,2] >= 6 & obsdata[[i]][,2] <= 9), 7])
  relhumidity[i] <- mean(obsdata[[i]][which(obsdata[[i]][,2] >= 6 & obsdata[[i]][,2] <= 9), 8])
  windspeed[i] <- mean(obsdata[[i]][which(obsdata[[i]][,2] >= 6 & obsdata[[i]][,2] <= 9), 9])
  accprecip[i] <- mean(obsdata[[i]][which(obsdata[[i]][,2] >= 6 & obsdata[[i]][,2] <= 9), 20])
}

# create plotting data
inputdata <- data.frame(highwarmdata[,c(2:6)],drybulbtemp, wetbulbtemp, dewpointtemp, relhumidity, windspeed, accprecip)
xy <- as.matrix(inputdata[,c(4,3)]) %*% sm
inputdata$x <- xy[,1]
inputdata$y <- xy[,2]

# create plots
p1 <- ggplot(inputdata) + theme_light() + geom_sf(data = states, color = "black", fill = "#F0F0F0") +
  coord_sf(xlim = c(-220, -80), ylim = c(20, 54), expand = FALSE) +
  geom_point(aes(x = x, y = y, fill = drybulbtemp), pch = 21, size = 3) +
  scale_fill_gradient(high = '#bd0026', low = '#fee5d9', guide = 'none') +
  scale_size_continuous(range = c(2,5), guide = 'none') + xlab('') + ylab('') +
  ggtitle('Dry Bulb Temperature') + 
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.border = element_blank(), panel.grid = element_blank())

p2 <- ggplot(inputdata) + theme_light() + geom_sf(data = states, color = "black", fill = "#F0F0F0") +
  coord_sf(xlim = c(-220, -80), ylim = c(20, 54), expand = FALSE) +
  geom_point(aes(x = x, y = y, fill = wetbulbtemp), pch = 21, size = 3) +
  scale_fill_gradient(high = '#7fcdbb', low = '#ffffcc', guide = 'none') +
  scale_size_continuous(range = c(2,5), guide = 'none') + xlab('') + ylab('') +
  ggtitle('Wet Bulb Temperature') + 
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.border = element_blank(), panel.grid = element_blank())

p3 <- ggplot(inputdata) + theme_light() + geom_sf(data = states, color = "black", fill = "#F0F0F0") +
  coord_sf(xlim = c(-220, -80), ylim = c(20, 54), expand = FALSE) +
  geom_point(aes(x = x, y = y, fill = dewpointtemp), pch = 21, size = 3) +
  scale_fill_gradient(high = '#9ebcda', low = '#edf8fb', guide = 'none') +
  scale_size_continuous(range = c(2,5), guide = 'none') + xlab('') + ylab('') +
  ggtitle('Dew Point Temperature') + 
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.border = element_blank(), panel.grid = element_blank())

p4 <- ggplot(inputdata) + theme_light() + geom_sf(data = states, color = "black", fill = "#F0F0F0") +
  coord_sf(xlim = c(-220, -80), ylim = c(20, 54), expand = FALSE) +
  geom_point(aes(x = x, y = y, fill = relhumidity), pch = 21, size = 3) +
  scale_fill_gradient(high = '#54278f', low = '#f2f0f7', guide = 'none') +
  scale_size_continuous(range = c(2,5), guide = 'none') + xlab('') + ylab('') +
  ggtitle('Relative Humidity') + 
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.border = element_blank(), panel.grid = element_blank())

p5 <- ggplot(inputdata) + theme_light() + geom_sf(data = states, color = "black", fill = "#F0F0F0") +
  coord_sf(xlim = c(-220, -80), ylim = c(20, 54), expand = FALSE) +
  geom_point(aes(x = x, y = y,fill = windspeed), pch = 21, size = 3) +
  scale_fill_gradient(high = '#006d2c', low = '#edf8e9', guide = 'none') +
  scale_size_continuous(range = c(2,5), guide = 'none') + xlab('') + ylab('') +
  ggtitle('Wind Speed') + 
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.border = element_blank(), panel.grid = element_blank())

p6 <- ggplot(inputdata) + theme_light() + geom_sf(data = states, color = "black", fill = "#F0F0F0") +
  coord_sf(xlim = c(-220, -80), ylim = c(20, 54), expand = FALSE) +
  geom_point(aes(x = x, y = y, fill = accprecip), pch = 21, size = 3) +
  scale_fill_gradient(high = '#08519c', low = '#eff3ff', guide = 'none') +
  scale_size_continuous(range = c(2,5), guide = 'none') + xlab('') + ylab('') +
  ggtitle('Precipitation') + 
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.border = element_blank(), panel.grid = element_blank())

# save as pdf
setwd(outputdir)
pdf('figure1b.pdf', width = 5, height = 7.5)
plot_grid(p1,p2,p3,p4,p5,p6,align = 'v',nrow = 6)
dev.off()

# FIGURE 1c

# initialize variables
actualwater <- c()
actualelec <- c()
predwater <- c()
predelec <- c()
reglist <- c()

# extract data
for (i in 1:46) {
  actualwater <- append(actualwater, citytest[[i]][,1])
  actualelec <- append(actualelec, citytest[[i]][,2])
  predwater <- append(predwater, cityyhat[[i]][,1])
  predelec <- append(predelec, cityyhat[[i]][,2])
  reglist <- append(reglist, rep(regions[i], nrow(citytest[[i]])))
}

# create plotting data
waterdata <- data.frame(reglist, actualwater, predwater)
elecdata <- data.frame(reglist, actualelec, predelec)

# create plots
p1 <- ggplot(waterdata) + geom_point(aes(x = actualwater, y = predwater, fill = reglist), size = 3, pch=21) + theme_light() +
  xlab('Actual Water Use (L/capita/month)') + ylab('Predicted Water Use\n(L/capita/month)') +
  scale_fill_manual(name = 'Region', values = c('#7fc97f','#beaed4','#fdc086','#ffff99','#386cb0','#f0027f','#bf5b17','#666666')) +
  guides(fill = guide_legend(override.aes = list(size=3))) + theme(text = element_text(size = 16)) +
  geom_abline(slope = 1,intercept = 0) +
  theme(legend.position = "none")

p2 <- ggplot(elecdata) + geom_point(aes(x = actualelec, y = predelec, fill = reglist), size = 3, pch=21) + theme_light() +
  xlab('Actual Electricity Use (MWh/capita/month)') + ylab('Predicted Electricity Use\n(MWh/capita/month)\n') +
  scale_fill_manual(name = 'Region', values = c('#7fc97f','#beaed4','#fdc086','#ffff99','#386cb0','#f0027f','#bf5b17','#666666')) +
  guides(fill = guide_legend(override.aes = list(size=3))) + theme(text = element_text(size = 16))+
  geom_abline(slope = 1,intercept = 0) + theme(legend.position = "bottom")

# save as pdf
setwd(outputdir)
pdf('figure1c.pdf', width = 8, height = 10)
plot_grid(p1,p2,nrow=2, rel_heights = c(3/7,4/7))
dev.off()

# FIGURE 1d

# create plotting data
maxs <- c(max(unlist(avgnrmsew)), max(unlist(avgnrmsee)), max(unlist(lapply(avgrsq, '[[', 1))), max(unlist(lapply(avgrsq, '[[', 2))))
mins <- c(min(unlist(avgnrmsew)), min(unlist(avgnrmsee)), min(unlist(lapply(avgrsq, '[[', 1))), min(unlist(lapply(avgrsq, '[[', 2))))
medians <- c(median(unlist(avgnrmsew)), median(unlist(avgnrmsee)), median(unlist(lapply(avgrsq, '[[', 1))), median(unlist(lapply(avgrsq, '[[', 2))))
Utility <- c('Water','Electricity','Water','Electricity')
Measure <- c('NRMSE','NRMSE','R-squared','R-squared')

errdata <- data.frame(Utility, Measure, maxs, mins, medians)

# save plot
setwd(outputdir)
pdf('figure1d.pdf', width = 6, height = 6)
ggplot(errdata, aes(x = Measure, ymin = mins, ymax = maxs, lower= mins, upper = maxs, middle = medians)) + 
  geom_boxplot(aes(fill = Utility), stat = 'identity') + theme_light() + 
  xlab('Measure of Error') + scale_fill_manual(values = c('#fdae61','#74add1')) +
  theme(legend.position = 'bottom', text = element_text(size = 14)) + ylim(0,1)
dev.off()

# FIGURE 2a

# read in location data
setwd(miscdir)
locations <- read.csv('analoglocations.csv', na = '') %>% arrange(desc(type))

# extract map data
northamerica <- ne_states(country = c('united states of america', 'mexico', 'cuba'), returnclass = "sf")

# filter high warming analogs
highlocations <- locations[which(locations$warming == "target" | locations$warming == "high"),1:6]

# save plot as pdf
setwd(outputdir)
pdf('figure2a.pdf', width = 10, height = 9)
ggplot(highlocations) + theme_light() + geom_sf(data = northamerica, color = "#8a8a8a", fill = "#f8f8f8") +
  coord_sf(xlim = c(-130, -60), ylim = c(14, 50), expand = FALSE) +
  geom_point(aes(x = longitude, y = latitude, fill = warming, size = warming), pch = 21) +
  geom_path(aes(x = longitude, y = latitude, group = pair2), size = .5, arrow = arrow(length = unit(0.25,"cm"))) +
  scale_size_manual(values = c(6,8), guide = 'none') + xlab('') + ylab('')+
  scale_fill_manual(values = c('#da3434','#92c5de'), name = '', labels = c('High Warming Analog','City of Interest')) +
  theme(text = element_text(size = 20), legend.position = 'bottom') + 
  guides(fill = guide_legend(override.aes = list(size=10)))
dev.off()

# FIGURE 2b

# extract map data 
states <- ne_states(country = 'united states of america', returnclass = "sf")
states_crop <- st_crop(states, xmin = -130, xmax = -60, ymin = 20, ymax = 50)

# save plot to pdf
setwd(outputdir)
pdf('figure2b.pdf', width = 10, height = 9)
ggplot(highwarmdata) + theme_light() + geom_sf(data = states_crop, color = "#8a8a8a", fill = "#f8f8f8") +
  geom_point(aes(x = longitude, y = latitude, size = population, fill = pcelec), pch = 21) +
  scale_fill_gradient2(high = '#053061', mid = 'white', low = '#67001f', name = '%') +
  scale_size_continuous(range = c(3,15), guide = 'none') + xlab('') + ylab('') +
  ggtitle('Percent Change in Electricity Demand') + theme(legend.position = 'right', text = element_text(size = 26))
dev.off()

# FIGURE 2c

# read in ssp population data
setwd(miscdir)
sspdata_csv <- read.csv('sspscenarios.csv')

# calculate mean electricity consumption based on ssp population estimates 
chicagossp <- sspdata_csv[,2] * mean(highwarm[[which(citynames == 'chicago')]][,2])
lassp <- sspdata_csv[,3] * mean(highwarm[[which(citynames == 'losangeles')]][,2])
nycssp <- sspdata_csv[,4] * mean(highwarm[[which(citynames == 'newyorkcity')]][,2])

# calculate standard deviation of electricity consumption based on ssp population estimates 
chicagossp_sd <- sspdata_csv[,2] * sd(highwarm[[which(citynames == 'chicago')]][,2])
lassp_sd <- sspdata_csv[,3] * sd(highwarm[[which(citynames == 'losangeles')]][,2])
nycssp_sd <- sspdata_csv[,4] * sd(highwarm[[which(citynames == 'newyorkcity')]][,2])

# create plotting data
ssp <- c(chicagossp/1000000, lassp/1000000, nycssp/1000000)
City <- c(rep('Chicago',5), rep('Los Angeles',5), rep('New York City',5))
Scenario <- rep(c('SSP 1', 'SSP 2', 'SSP 3', 'SSP 4', 'SSP 5'),3)
sds <- c(chicagossp_sd/1000000, lassp_sd/1000000, nycssp_sd/1000000)

plotdata <- data.frame(Scenario, City, ssp, sds)

# save as a pdf
setwd(outputdir)
pdf('figure2c.pdf', width = 14, height = 6)
ggplot(plotdata) + geom_bar(aes(x = City, y = ssp, fill = Scenario),color = 'black', stat = 'identity', position = 'dodge') +
  geom_errorbar(aes(x = City, ymin = ssp-sds, ymax = ssp+sds, fill = Scenario), width = 0.5, position = position_dodge(0.9)) +
  ylab('Total Projected Electricity\nDemand (mill. MWh/month)') + xlab('') +
  scale_fill_manual(values = c('#fef0d9','#fdcc8a','#fc8d59','#e34a33','#b30000')) + theme_light() +
  theme(text = element_text(size = 26))
dev.off()

# FIGURE 2d

# extract map data 
states <- ne_states(country = 'united states of america', returnclass = "sf")
states_crop <- st_crop(states, xmin = -130, xmax = -60, ymin = 20, ymax = 50)

# save plot to pdf
setwd(outputdir)
pdf('figure2d.pdf', width = 10, height = 9)
ggplot(highwarmdata) + theme_light() + geom_sf(data = states_crop, color = "#8a8a8a", fill = "#f8f8f8") +
  geom_point(aes(x = longitude, y = latitude, size = population, fill = pcwater), pch = 21) +
  scale_fill_gradient2(high = '#053061', mid = 'white', low = '#67001f', name = '%', breaks = c(-10,0,10)) +
  scale_size_continuous(range = c(3,15), guide = 'none') + xlab('') + ylab('') +
  ggtitle('Percent Change in Water Demand') + theme(legend.position = 'right', text = element_text(size = 26))
dev.off()







