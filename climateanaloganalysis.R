# Project: Water-Energy Demand Nexus using Climate Analogs
# Code By: Renee Obringer
# Last Updated: 8 March 2021

rm(list=ls())

# libraries
library(mvtboost) # note: This package is no longer available on CRAN, find it at https://github.com/patr1ckm/mvtboost
library(stringr)
library(measurements)
library(gbm)

############# load data ######################

#### NARR data

# function to read text files
readTextFilesIn <- function(dir_path = datadir1){
  myfun <- function(fl=paste(datadir1,"/city_id_1", sep="")){
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
analognames <- read.table(paste(maindir,"/analoginfo/cities_loc.txt",sep=""), header=T, fill = TRUE, stringsAsFactors = F, sep = ',')

#### water and electricity use data
setwd(datadir2)
filenames <- list.files(pattern="*.csv", full.names=F)
ldf <- lapply(filenames, read.csv)
citynames <- str_remove(filenames,'data.csv')

############# preprocess data ######################

# water and electricity use data
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

# NARR data

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
save(list=c("citynames","obsdata","analog1data","analog2data","regions"), file="preprocessedinputdata.RDATA")


############# variable selection (by region) ######################

# load rdata file from previous section, if necessary
load("preprocessedinputdata.RDATA")

# initialize
s <- 1 # to consider only the summer season
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
  
  # create a new dataframe per season
  summer <- rbind(region[which(region[,2] >= 6 & region[,2] <= 9),])
  winter <- rbind(region[which(region[,2] >= 12 | region[,2] <= 3),])
  interm <- rbind(region[which(region[,2] == 4 | region[,2] == 5 | region[,2] == 10 | region[,2] == 11),])
  
  # store in a single list
  seasons <- (list(summer,winter,interm))
  
  # remove NA values
  seasons[[s]] <- na.omit(seasons[[s]])
  
  # set up 5-fold cross validation
  k <- 5
  n <- nrow(seasons[[s]])
  set.seed(11)
  seasons[[s]] <- seasons[[s]][sample(n),]
  folds <- cut(seq(1,n),breaks = k, labels = FALSE)
  
  # initialize
  Rallnrmsew <- c()
  Rallnrmsee <- c()
  
  # initial model run
  for (j in 1:k) {
    testIndex <- which(folds == j, arr.ind = TRUE)
    testData <- seasons[[s]][testIndex,]
    trainData <- seasons[[s]][-testIndex,]
    
    # run model
    Y <- trainData[,3:4] 
    X <- trainData[,c(5:21)] 
    
    out <- mvtb(Y=Y,X=X, n.trees=1000, shrinkage=.01, interaction.depth=3)
    
    # fit model
    yhat <- predict(out,newdata=testData[,c(5:21)])
    
    # measures of error
    Rallnrmsew[j] <- sqrt(sum((yhat[,1]-testData[,3])^2)/nrow(testData))/(range(testData[,3])[2]-range(testData[,3])[1])
    Rallnrmsee[j] <- sqrt(sum((yhat[,2]-testData[,4])^2)/nrow(testData))/(range(testData[,4])[2]-range(testData[,4])[1])
  }
  
  # set realtive importance based on initial model run
  Rrelinf <- mvtb.ri(out, relative = 'col')
  
  # initialize
  newnrmsew <- mean(Rallnrmsew)
  newnrmsee <- mean(Rallnrmsee)
  newrelinf <- Rrelinf
  newvars <- seasons[[s]]
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
    
    # 5-fold CV
    for (j in 1:k) {
      testIndex <- which(folds == j, arr.ind = TRUE)
      testData <- newvars[testIndex,]
      trainData <- newvars[-testIndex,]
      
      # run model
      Y <- trainData[,3:4] 
      X <- trainData[,c(5:ncol(newvars))] 
      
      out <- mvtb(Y=Y,X=X, n.trees=1000, shrinkage=.01, interaction.depth=3)
      
      # fit model
      yhat <- predict(out,newdata=testData[,c(5:ncol(newvars))])
      
      # measures of error
      Rnrmsew[j] <- sqrt(sum((yhat[,1]-testData[,3])^2)/nrow(testData))/(range(newvars[,3])[2]-range(newvars[,3])[1])
      Rnrmsee[j] <- sqrt(sum((yhat[,2]-testData[,4])^2)/nrow(testData))/(range(newvars[,4])[2]-range(newvars[,4])[1])
    }
    
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
    print(oldnrmsew)
    print(newnrmsew)
    
    # if fewer than 7 variables remain, stop
    if (length(relinf) == 6) {
      relinf <- oldrelinf
      finalmod <- oldmod
      flag <- 0
    }
  }
  
  # store data
  newmods[[i]] <- finalmod
  print(relinf)
  allnewri[[i]] <- relinf
  
  # save images of heat maps showing relative importance
  filename = paste(regionnames[i],'.png',sep='')
  png(file=filename, width = 1000,height = 500)
  par(mar=c(15,8,1,1),mfrow=c(1,1),cex=1.5)
  mvtb.heat(t(relinf),clust.method = NULL,cexRow=1,cexCol=1,numformat=numformat, col = bgr(500))
  dev.off()
}

# save data
save(list=c("regions","regionnames","newmods","allnewri"),file="variableselection.RDATA")



############# model run (by city) ######################

# load rdata files, if necessary
load("preprocessedinputdata.RDATA")
load("variableselection.RDATA")


# intialize variables
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
  
  # LOOCV
  for (j in 1:n) {
    
    testData <- ndata[j,]
    trainData <- ndata[-j,]
    
    # run model
    Y <- trainData[,1:2] 
    X <- trainData[,c(3:ncol(ndata))] 
    
    out <- mvtb(Y=Y,X=X, n.trees=1000, shrinkage=.01, interaction.depth=3)
    
    # fit model
    yhat <- predict(out,newdata=testData[c(3:ncol(testData))])
    
    # measures of error
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

############# percent change analysis ######################

# load rdata files, if necessary
load("preprocessedinputdata.RDATA")
load("variableselection.RDATA")
load("modelrunresults.RDATA")

# percent change under different warming scenarios (pos == % increase) [new-old/old]

pcw_low <- list()
pcw_high <- list()
pce_low <- list()
pce_high <- list()

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


