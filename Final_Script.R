setwd("E:/ST/AmmmMagistras/Modelling Rstudio/courseMarineEcologicalModelling-master/courseMarineEcologicalModelling-master")
source("E:/ST/AmmmMagistras/Modelling Rstudio/courseMarineEcologicalModelling-master/courseMarineEcologicalModelling-master/SessionsP/sourceFunctions.R")
library(tidyverse)
MytilusOccurrence <- read.csv("MytilusOccurence.csv")
MytilusOccurrence <- select(MytilusOccurrence,"Lon","Lat")
environmentalConditions <- load_layers(c("BO2_templtmax_bdmax","BO2_templtmax_ss","BO2_salinityltmin_bdmax","BO2_icethickrange_ss")) 

occurrenceRecords<- MytilusOccurrence
rasterLayers<-environmentalConditions
relocateSpeciesDistance<- 9

occurrenceRecords <- relocateNACoords(occurrenceRecords,rasterLayers,relocateSpeciesDistance)

MytilusOccurrence<-occurrenceRecords

#Cleaning the data
nrow(MytilusOccurrence)
MytilusOccurrence <- removeNA(MytilusOccurrence,"Lon","Lat") #removing NA 
MytilusOccurrence <- removeDuplicated(MytilusOccurrence,"Lon","Lat") #removing duplicates #world shape file 
MytilusOccurrence <- removeOverLand(MytilusOccurrence,"Lon","Lat") #remove records on land
#how much records now?
nrow(MytilusOccurrence)

#ploting occurrences in the world
Dataworld<-"SessionsP/Data/vectorShapefiles/globalLandmass/world.shp"
world<-shapefile(Dataworld)
crs(world)
plot(world,border="slategrey",col="darkolivegreen3",main="Mytilus edulis/trossulus occurrence records", axes=TRUE)
points(MytilusOccurrence[,c("Lon","Lat")], pch=1, col="red4")
north_world <- crop(world, extent(-150, 50, 25, 85))
plot(north_world,border="slategrey",col="darkolivegreen3",main="Mytilus edulis/trossulus occurrence records", axes=TRUE)
points(MytilusOccurrence[,c("Lon","Lat")], pch=1, col="red4")

#Speecies occurence according depth and assesing the adequacy of biodiversity data. 
# M. edulis distribution is 100-499 m, however its unlikely (in Mestre et al., 2009). M. edulis distribution was set to 100 m according to data
# occurences (only few points goes deeper) and literature  
bathymetry <- raster("Data/rasterLayers/BathymetryDepthMean.tif")
depthUse <- extract(bathymetry, MytilusOccurrence[,c("Lon","Lat")])
#if I have problems with extract function I want to know which extract method is being loaded > extract
#unload the offending package >.rs.unloadPackage("tidyr")
.rs.unloadPackage("tidyr")
depthUse <- extract(bathymetry, MytilusOccurrence[,c("Lon","Lat")])
head(depthUse)
hist(depthUse,breaks=50, na.rm=TRUE, main="Histogram of Mytilus sp. depth distribution", col="mediumseagreen", xlab="Mean_Depth, m")

#Regions where species distribution is not likely due to depth according to literature
bathymetry[ Which(bathymetry >= -100, cells= TRUE) ] <- 0 
bathymetry[ Which(bathymetry < -100, cells= TRUE) ] <- 1 
plot(bathymetry, col=c("Gray","Red") , main="Regions with mean depth deeper than 100 m")
north <- crop(bathymetry, extent(-150, 50, 25, 85))
plot(north, col=c("Gray","Red") , main="Regions with mean depth deeper than 100 m")
MytilusOccurrence <- MytilusOccurrence[ which(depthUse > -100) ,] 
points(MytilusOccurrence[,c("Lon","Lat")], pch=1, col="green")

bathymetry <- raster("Data/rasterLayers/BathymetryDepthMean.tif")
depthUse <- extract(bathymetry, MytilusOccurrence[,c("Lon","Lat")])
head(depthUse)
hist(depthUse,breaks=50, na.rm=TRUE, main="Histogram of Mytilus sp. depth distribution", col="mediumseagreen", xlab="Mean_Depth, m")
min(depthUse)

#Mytilus lethal uper temperature limit is +25, however I want to check if my biodiversity data aligns with the literature 
MaxSST_Present <- load_layers("BO2_templtmax_ss")
temperatMAX <- extract(MaxSST_Present, MytilusOccurrence[,c("Lon","Lat")])
head(temperatMAX)
hist(temperatMAX,breaks=50, na.rm=T, main="Histogram of Mytilus sp. thermal distribution", col="mediumseagreen", xlab="Maximum_temperature")
max(temperatMAX)
min(temperatMAX)
mean(temperatMAX)
MytilusOccurrence <- MytilusOccurrence[ which(temperatMAX < 25) ,] #only few temperature values is higher than 25 where Mytilus occurs and lethal temp is 25, so I keep 25 and confirm that distribution data according to temp aligns with literature 
temperatMAX <- extract(MaxSST_Present, MytilusOccurrence[,c("Lon","Lat")])
hist(temperatMAX,breaks=50, na.rm=T, main="Histogram of Mytilus sp. thermal distribution", col="mediumseagreen", xlab="Maximum_temperature")

#Regions where species distribution are limited due to temperature
MaxSST_Present[ Which(MaxSST_Present < 25, cells= TRUE) ] <- 0
MaxSST_Present[ Which(MaxSST_Present >= 25, cells= TRUE) ] <- 1
plot(MaxSST_Present, col=c("Gray","Red") , main="Regions with maximum SST > 25ºC")
points(MytilusOccurrence[,c("Lon","Lat")], pch=1, col="green")
north <- crop(MaxSST_Present, extent(-150, 50, 25, 85))
plot(north, col=c("Gray","Red") , main="Regions with maximum SST > 25ºC")
points(MytilusOccurrence[,c("Lon","Lat")], pch=1, col="green")

#ploting occurences
north <- crop(world, extent(-150, 50, 25, 85))
plot(north,border="slategrey",col="darkolivegreen3",main="Mytilus edulis/trossulus occurrence records", axes=TRUE)
points(MytilusOccurrence[,c("Lon","Lat")], pch=1, col="red4")
nrow(MytilusOccurrence)
library(ggplot2)
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  geom_point(data = MytilusOccurrence, aes(x = Lon, y = Lat), color = "red4") +
  scale_y_continuous(breaks = seq(-90,90, by=20)) +
  scale_x_continuous(breaks = seq(-180,180,by=20)) +
  coord_map("orthographic", orientation=c(90, 0, 0))

# save data frame to external file
write.table(MytilusOccurrence, file="MytilusOccurrence_clean.csv", row.names = F, sep=",")
MytilusOccurrence <- read.csv("MytilusOccurrence_clean.csv")

#Gathering relevant environmental data 
#BO loading layers
BOLayers <- list_layers(datasets = "Bio-ORACLE")
View(BOLayers)
BOLayersFuture <- list_layers_future(datasets = "Bio-ORACLE")
View(BOLayersFuture)
layerCodes <- list_layers( datasets = "Bio-ORACLE")
View(layerCodes)

environmentalConditions <- crop(environmentalConditions, north)
# extract values from layers
environmentUse <- extract(environmentalConditions, MytilusOccurrence[,c("Lon","Lat")])
head(environmentUse)

# Looking at species environmental data that has been extracted from present layers and will be used for modeling 
hist(environmentUse[,"BO2_templtmax_bdmax"],breaks=25, main="Histogram of longterm maximum sea water temperature \n at the bottom at maximum bottom depth", col="mediumseagreen", xlab="Temperature")
hist(environmentUse[,"BO2_templtmax_ss"],breaks=25, main="Histogram of longterm maximum sea surface temperature", col="mediumseagreen", xlab="Temperature")

hist(environmentUse[,"BO2_salinityltmin_bdmax"],breaks=25, main="Histogram of longterm minimum sea water salinity \n at the bottom at maximum bottom depth", col="mediumseagreen", xlab="Salinity")
hist(environmentUse[,"BO2_icethickrange_ss"],breaks=25, main="Histogram of sea ice thickness range", col="mediumseagreen", xlab="Meters")

# test for correlations
environmentalConditions_North <- crop(environmentalConditions, extent(-150, 50, 25, 85))
names(environmentalConditions_North) <- c("templtmax_bdmax","templtmax_ss","salinityltmin_bdmax","icethickrange_ss")
pairs(environmentalConditions_North)

# load a presence dataset for an intertidal species
MytilusOccurrence$PA <- 1
write.table(MytilusOccurrence, file="MytilusOccurrence_clean_PA.csv", row.names = F, sep=",")
records <- read.csv("MytilusOccurrence_clean_PA.csv")
presences <- records[records[,"PA"] == 1 , c("Lon","Lat") ]

# load present layers
plot(environmentalConditions)
SvalbardExtent <- extent(5,35,76,81)
Svalbard <- crop(environmentalConditions,SvalbardExtent)
plot(Svalbard)

# crop layers to the coastline region to match intertidal distributions
coastline <- raster("Data/rasterLayers/CoastLine.tif")
environmentalConditions <- load_layers(c("BO2_templtmax_bdmax","BO2_templtmax_ss","BO2_salinityltmin_bdmax","BO2_icethickrange_ss"))
environmentalConditions_coastline <- mask(environmentalConditions,coastline)
environmentalConditions_coastline <- crop(environmentalConditions_coastline,north)
plot(environmentalConditions_coastline)
environmentalConditions <- crop(environmentalConditions,north)

#Generating Pseudo-absence data
absences <- pseudoAbsences(environmentalConditions_coastline,presences,n=1000)

# plot presence and pseudo-absences records
North_Part <- crop(world,north)
plot(North_Part , col="gray")
points(absences,col="Red",pch=16)
points(presences,col="Black",pch=16)

# extract environmental values and make an object with all information to model
modelData <- prepareModelData(presences,absences,environmentalConditions) 

# fit a binomial GLM to the dataset
model <- trainGLM(modelData)

# predict the distribution with the GLM over a raster stack
map <- predict(environmentalConditions,model,type='response')

# plot glm and records
plot(map , col = brewer.pal(n = 7, name = "YlOrRd") )
SvalbardExtent <- extent(5,35,76,81)
Model_Svalbard<- crop(map,SvalbardExtent)
plot(Model_Svalbard,  col = brewer.pal(n = 7, name = "YlOrRd"))

#With Maxent
# load a presence dataset for a Mytilus with PA column
presences <- read.csv("MytilusOccurrence_clean.csv")
presences <- presences[,c("Lon","Lat")]

environmentalConditionsRCP45 <- load_layers(c("BO2_RCP45_2100_templtmax_bdmax","BO2_RCP45_2100_templtmax_ss","BO2_RCP45_2100_salinityltmin_bdmax","BO2_RCP45_2100_icethickrange_ss")) 
environmentalConditionsRCP85 <- load_layers(c("BO2_RCP85_2100_templtmax_bdmax","BO2_RCP85_2100_templtmax_ss","BO2_RCP85_2100_salinityltmin_bdmax","BO2_RCP85_2100_icethickrange_ss")) 

# compare layer names
names(environmentalConditions)
names(environmentalConditionsRCP45)
names(environmentalConditionsRCP45) <- c("BO2_templtmax_bdmax","BO2_templtmax_ss","BO2_salinityltmin_bdmax","BO2_icethickrange_ss")
names(environmentalConditionsRCP85) <- c("BO2_templtmax_bdmax","BO2_templtmax_ss","BO2_salinityltmin_bdmax","BO2_icethickrange_ss")

environmentalConditionsRCP45 <- crop(environmentalConditionsRCP45,north)
environmentalConditionsRCP85 <- crop(environmentalConditionsRCP85,north)

background <- backgroundInformation(environmentalConditions,n=1000)

# extract environmental values and make a data.frame with PA information
modelData <- prepareModelData(presences,background,environmentalConditions) 

# fit a Maxent model to the dataset
model <- train("Maxnet", modelData, folds = NULL )

# predict with Maxent to raster stack
map <- predict(model, environmentalConditions, type="logistic")
plot(map, main="Predicted distribution of Mytilus edulis/trossulus for the Present" , col = brewer.pal(n = 11, name = "YlOrRd") )

Model_Svalbard<- crop(map,SvalbardExtent)

plot(Model_Svalbard, main="Predicted distribution of Mytilus edulis/trossulus for the Present" , col = brewer.pal(n = 11, name = "YlOrRd"))

mapRCP45 <- predict(model, environmentalConditionsRCP45, type=c("logistic"))
plot(mapRCP45, main="Predicted distribution of Mytilus edulis/trossulus \n for the Future (RCP45)" , col = brewer.pal(n = 11, name = "YlOrRd") )

Model_Svalbard<- crop(mapRCP45,SvalbardExtent)
plot(Model_Svalbard, main="Predicted distribution of Mytilus edulis/trossulus \n for the Future (RCP45)" , col = brewer.pal(n = 11, name = "YlOrRd"))

mapRCP85 <- predict(model, environmentalConditionsRCP85, type=c("logistic"))
plot(mapRCP85, main="Predicted distribution of Mytilus edulis/trossulus \n for the Future (RCP85)" , col = brewer.pal(n = 11, name = "YlOrRd") )

Model_Svalbard<- crop(mapRCP85,SvalbardExtent)
plot(Model_Svalbard, main="Predicted distribution of Mytilus edulis/trossulus \n for the Future (RCP85)" , col = brewer.pal(n = 11, name = "YlOrRd"))

differenceRCP45 <- mapRCP45 - map
plot(differenceRCP45, main="Difference in habitat suitablity (RCP45)" , col = brewer.pal(n = 11, name = "RdBu") )

Model_Svalbard<- crop(differenceRCP45,SvalbardExtent)
plot(Model_Svalbard, main="Difference in habitat suitablity (RCP45)" , col = brewer.pal(n = 11, name = "RdBu") )

differenceRCP85 <- mapRCP85 - map
plot(differenceRCP85, main="Difference in habitat suitablity (RCP85)" , col = brewer.pal(n = 11, name = "RdBu") )

Model_Svalbard<- crop(differenceRCP85,SvalbardExtent)
plot(Model_Svalbard, main="Difference in habitat suitablity (RCP85)" , col = brewer.pal(n = 11, name = "RdBu") )


# determine threshold dependent indices
getAccuracy(model)
getAccuracy(model,threshold = 0.1)
getAccuracy(model,threshold = 0.9)

# plot the threshold-independent Receiver operating characteristic 
plotROC(model)

# determine the threshold-independent AUC
getAUC(model)

# Model evaluation with cross-validation
# Cross validation with random partitioning of 10 folds
folds <- randomFolds(modelData, k = 10)

# fit a Maxent model to the dataset
model <- train("Maxnet", modelData, folds = folds )

# determine AUC
getAUC(model, test = TRUE)

# Cross validation with 4 blocks partitioning
folds2 <- get.block(presences, background)

# fit a Maxent model to the dataset
model <- train("Maxnet", modelData, folds = folds2 )

# determine AUC
getAUC(model, test = TRUE)

# determine the threshold maximazing the sum of sensitivity and specificity
threshold <- thresholdMaxTSS(model)
threshold

# determine model performance
getAccuracy(model,threshold = threshold)

getAUC(model, test = TRUE)

# generate a reclassification table
thresholdConditions <- data.frame(from = c(0,threshold) , to=c(threshold,1) , reclassValue=c(0,1))
thresholdConditions

# apply threshold to reclassify the predictive surface
map <- reclassify(map, rcl = thresholdConditions)
plot(map)

Model_Svalbard<- crop(map,SvalbardExtent)
plot(Model_Svalbard)

# predict with Maxent to raster stack
mapRCP45 <- predict(model, environmentalConditionsRCP45, type=c("logistic"))
plot(mapRCP45)

# apply threshold to reclassify the future predictive surface RCP45
mapRCP45 <- reclassify(mapRCP45, rcl = thresholdConditions)
plot(mapRCP45)

Model_Svalbard<- crop(mapRCP45,SvalbardExtent)
plot(Model_Svalbard)

# predict with Maxent to raster stack
mapRCP85 <- predict(model, environmentalConditionsRCP85, type=c("logistic"))
plot(mapRCP85)

# apply threshold to reclassify the future predictive surface RCP85
mapRCP85 <- reclassify(mapRCP85, rcl = thresholdConditions)
plot(mapRCP85)

Model_Svalbard<- crop(mapRCP85,SvalbardExtent)
plot(Model_Svalbard)

#Variable relative importance and response curves
# generate pseudo-absences
background <- backgroundInformation(environmentalConditions,n=1000)

# extract environmental values and make a data.frame with PA information
modelData <- prepareModelData(presences,background,environmentalConditions) 

# fit a Maxent model to the dataset
model <- train("Maxnet", modelData, folds = NULL )

# calculate the variable relative importance with permutations
viModel <- varImp(model, permut = 5)
viModel

plotVarImp(viModel, col="mediumseagreen")

# inspecting response curves

plotResponse(model, var = "BO2_templtmax_bdmax", type = "logistic", only_presence = FALSE, marginal = FALSE, rug = FALSE, color="mediumseagreen")
plotResponse(model, var = "BO2_templtmax_ss", type = "logistic", only_presence = FALSE, marginal = FALSE, rug = FALSE, color="mediumseagreen")
plotResponse(model, var = "BO2_salinityltmin_bdmax", type = "logistic", only_presence = FALSE, marginal = FALSE, rug = FALSE, color="mediumseagreen")
plotResponse(model, var = "BO2_icethickrange_ss", type = "logistic", only_presence = FALSE, marginal = FALSE, rug = FALSE, color="mediumseagreen")

# train a model with threshold feature
folds <- get.block(presences, background)
model <- train("Maxnet", modelData, folds = folds , fc="tp" )

# given a set of possible hyperparameter values (regularization)
h <- list(reg = seq(1, 10, 1))

# train models with all the possible combinations of hyperparameters
exp1 <- gridSearch(model, hypers = h, metric = "auc")
plot(exp1)

exp1@results

#seems that regularisation 9 is the most apropriate value
model <- train("Maxnet", modelData, folds = folds , fc="tp" , reg=9)
getAUC(model, test = TRUE)

plotResponse(model, var = "BO2_templtmax_bdmax", type = "logistic", only_presence = FALSE, marginal = FALSE, rug = FALSE, color="mediumseagreen")
plotResponse(model, var = "BO2_templtmax_ss", type = "logistic", only_presence = FALSE, marginal = FALSE, rug = FALSE, color="mediumseagreen")
plotResponse(model, var = "BO2_salinityltmin_bdmax", type = "logistic", only_presence = FALSE, marginal = FALSE, rug = FALSE, color="mediumseagreen")
plotResponse(model, var = "BO2_icethickrange_ss", type = "logistic", only_presence = FALSE, marginal = FALSE, rug = FALSE, color="mediumseagreen")

# determine relative variable contribution and performance as AUC
viModel <- varImp(model, permut = 5)
plotVarImp(viModel, col="mediumseagreen")
viModel

getAUC(model, test = TRUE)

# reduce model complexity by dropping variabale at a time and remove all variables that have a permutation importance lower than 5%.
reducedModel <- reduceVar(model, th = 5, metric = "auc", permut = 5)

# determine relative variable contribution and performance as AUC of the reduced model
viModel <- varImp(reducedModel, permut = 5)
plotVarImp(viModel, col="mediumseagreen")
viModel
getAUC(reducedModel, test = TRUE)

getAccuracy(reducedModel)
getAccuracy(reducedModel,threshold = threshold)

# predict with Maxent to raster stack
map <- predict(reducedModel, environmentalConditions, type=c("logistic"))
plot(map, main="Predicted distribution of Mytilus edulis/trossulus for the Present" , col = brewer.pal(n = 11, name = "YlOrRd") )

Model_Svalbard<- crop(map,SvalbardExtent)
plot(Model_Svalbard, main="Predicted distribution of Mytilus edulis/trossulus for the Present" , col = brewer.pal(n = 11, name = "YlOrRd"))

mapRCP45 <- predict(reducedModel, environmentalConditionsRCP45, type=c("logistic"))
plot(mapRCP45, main="Predicted distribution of Mytilus edulis/trossulus \n for the Future (RCP45)" , col = brewer.pal(n = 11, name = "YlOrRd") )

Model_Svalbard<- crop(mapRCP45,SvalbardExtent)
plot(Model_Svalbard, main="Predicted distribution of Mytilus edulis/trossulus \n for the Future (RCP45)" , col = brewer.pal(n = 11, name = "YlOrRd"))

mapRCP85 <- predict(reducedModel, environmentalConditionsRCP85, type=c("logistic"))
plot(mapRCP85, main="Predicted distribution of Mytilus edulis/trossulus \n for the Future (RCP85)" , col = brewer.pal(n = 11, name = "YlOrRd") )

Model_Svalbard<- crop(mapRCP85,SvalbardExtent)
plot(Model_Svalbard, main="Predicted distribution of Mytilus edulis/trossulus \n for the Future (RCP85)" , col = brewer.pal(n = 11, name = "YlOrRd"))

differenceRCP45 <- mapRCP45 - map
plot(differenceRCP45, main="Difference in habitat suitablity (RCP45)" , col = brewer.pal(n = 11, name = "RdBu") )

Model_Svalbard<- crop(differenceRCP45,SvalbardExtent)
plot(Model_Svalbard, main="Difference in habitat suitablity (RCP45)" , col = brewer.pal(n = 11, name = "RdBu") )

differenceRCP85 <- mapRCP85 - map
plot(differenceRCP85, main="Difference in habitat suitablity (RCP85)" , col = brewer.pal(n = 11, name = "RdBu") )

Model_Svalbard<- crop(differenceRCP85,SvalbardExtent)
plot(Model_Svalbard, main="Difference in habitat suitablity (RCP85)" , col = brewer.pal(n = 11, name = "RdBu") )

threshold <- thresholdMaxTSS(reducedModel)
threshold

# determine model performance
getAccuracy(reducedModel,threshold = threshold)
getAUC(reducedModel, test = TRUE)

# generate a reclassification table
thresholdConditions <- data.frame(from = c(0,threshold) , to=c(threshold,1) , reclassValue=c(0,1))
thresholdConditions

# apply threshold to reclassify the predictive surface
map <- reclassify(map, rcl = thresholdConditions)
plot(map, main="Predicted distribution of Mytilus edulis/trossulus for the Present")

Model_Svalbard<- crop(map,SvalbardExtent)
plot(Model_Svalbard)

mapRCP45 <- reclassify(mapRCP45, rcl = thresholdConditions)
plot(mapRCP45, main="Predicted distribution of Mytilus edulis/trossulus \n for the Future (RCP45)")

Model_Svalbard<- crop(mapRCP45,SvalbardExtent)
plot(Model_Svalbard)

mapRCP85 <- reclassify(mapRCP85, rcl = thresholdConditions)
plot(mapRCP85, main="Predicted distribution of Mytilus edulis/trossulus \n for the Future (RCP85)")

Model_Svalbard<- crop(mapRCP85,SvalbardExtent)
plot(Model_Svalbard)


difference <- mapRCP45 - map
plot(difference, main="Difference in habitat suitablity (RCP45)", col = brewer.pal(n = 11, name = "RdBu"))
Svalbard_mapRCP45<- crop(difference,SvalbardExtent)
plot(Svalbard_mapRCP45, main="Difference in habitat suitablity (RCP85)", col = brewer.pal(n = 11, name = "RdBu"))

difference_mapRCP85 <- mapRCP85 - map
plot(difference_mapRCP85, main="Difference in habitat suitablity (RCP85)", col = brewer.pal(n = 11, name = "RdBu"))
Svalbard_RCP85<- crop(difference_mapRCP85,SvalbardExtent)
plot(Svalbard_RCP85, main="Difference in habitat suitablity (RCP85)", col = brewer.pal(n = 11, name = "RdBu"))


writeRaster(mapRCP45,'RCP45.tif',options=c('TFW=YES'), overwrite = TRUE)
writeRaster(map,'Present.tif',options=c('TFW=YES'),overwrite = TRUE)
writeRaster(mapRCP85,'RCP85.tif',options=c('TFW=YES'),overwrite = TRUE)
writeRaster(difference,'Difference_RCP45.tif',options=c('TFW=YES'),overwrite = TRUE)
writeRaster(difference_mapRCP85,'Difference_RCP85.tif',options=c('TFW=YES'),overwrite = TRUE)
writeRaster(Svalbard_mapRCP45,'S_Difference_RCP45.tif',options=c('TFW=YES'),overwrite = TRUE)
writeRaster(Svalbard_RCP85,'S_Difference_RCP85.tif',options=c('TFW=YES'),overwrite = TRUE)


crs(map)
crs(mapRCP45)

RStudio.Version()








