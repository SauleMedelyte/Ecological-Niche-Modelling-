## -----------------------------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------
##
## Pack of Functions
##
## -----------------------------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------

packages.to.use <- c("ENMeval","plotROC","RColorBrewer","devtools","shiny","robis","mapproj","knitr","sf","worms","RCurl","RJSONIO","sp","rgdal","rgeos","raster","geosphere","ggplot2","gridExtra","rnaturalearth","rnaturalearthdata","leaflet","leaflet.extras","rgbif","dismo","SDMTools","SDMtune","sdmpredictors")

options(warn=-1)

for(package in packages.to.use) {
  
  if( ! "rnaturalearthhires" %in% rownames(installed.packages()) ) { devtools::install_github("ropensci/rnaturalearthhires")  }
  if( ! "SDMTools" %in% rownames(installed.packages()) ) { devtools::install_github('dbahrdt/SDMTools@ignore_invalid')  }
  
  if( ! package %in% rownames(installed.packages()) ) { install.packages( package ) }
  if( ! package %in% rownames(installed.packages()) ) { install.packages( package , type = "source" ) }

  if( ! package %in% rownames(installed.packages()) ) { stop("Error on package instalation") }
  
  library(package, character.only = TRUE)

}

options(warn=0)

## -----------------------------------------------------------------------------------------------

defineRegion <- function(records,lonName,latName) {
  
  records <- records[which(!is.na(records[,lonName])),c(lonName,latName)] 
  
  ui <- fluidPage(leafletOutput("mymap",height=500))
  
  server <- function(input, output) {
    
    output$mymap <- renderLeaflet(
      leaflet() %>%
        addProviderTiles("Esri.OceanBasemap",group = "Ocean Basemap") %>%
        
        addCircles(lng=records[,lonName], lat=records[,latName] , weight = 3,color="Black",radius = 6) %>%
        
        addDrawToolbar(
          targetGroup='draw')  
      
    )
    
    observeEvent(input$mymap_draw_new_feature,{
      feature <<- input$mymap_draw_new_feature
      
      print(feature)
      
    })
    
  }
  
  return(shinyApp(ui = ui, server = server))
  
}
  
## -----------------------------------------------------------------------------------------------

getAccuracy <- function(model,threshold=0.5) {
  
  acc <- accuracy(model@data@pa,  predict(model, model@data@data, type=c("logistic")) , threshold = threshold)
  acc <- acc[,c("threshold","omission.rate","sensitivity","specificity","prop.correct","Kappa")]
return(acc)

}

## -----------------------------------------------------------------------------------------------

getAUC <- function(model, test = NULL) {
  
  return(SDMtune::auc(model, test = test))
  
}

## -----------------------------------------------------------------------------------------------

thresholdMaxTSS <- function(model) {
  
  r <- data.frame(t(sapply(seq(0,1,by=0.01),function(x) { getAccuracy(model,threshold = x) })))

  plot(r$threshold,r$sensitivity,type="l", xlab="Threshold",ylab="Performance" , lty=1, lwd=1 )
  lines(r$threshold,r$specificity , lty=1, lwd=4 , col= "gray")
  
  val <- unlist(r$threshold)[which.max( unlist(r$sensitivity) + unlist(r$specificity) )]
  
  abline(v=val , lty=3, lwd=0.7 )
  legend(0.7, 0.9, legend=c("Sensitivity", "Specificity"),col=c("black", "gray"), lty=1, lwd=c(1,4) , cex=1)
  
  return(val)
  
}


## -----------------------------------------------------------------------------------------------

trainGLM <- function(modelData) {
  
  data <- modelData@data
  data <- data.frame(PA=modelData@pa,modelData@data)
  model <- glm( paste0("PA ~ ",paste(colnames(modelData@data),collapse = " + ")) , family="binomial", data=data)

return(model)

}

## -----------------------------------------------------------------------------------------------

prepareModelData <- function(p,a,env) {
    
    return(prepareSWD(species = "Model species", p = p, a = a, env = env))
    
  }

## -----------------------------------------------------------------------------------------------

backgroundInformation <- function(rasters,n) {
  
  shape <- subset(rasters,1)
  nonNACells <- Which(!is.na(shape), cells=TRUE) 
  sink.points <- xyFromCell(shape, nonNACells)
  
  absences <- sample( 1:nrow(sink.points) , min(n,nrow(sink.points)) , replace=FALSE)
  absences <- sink.points[absences,]
  colnames(absences) <- c("Lon","Lat")
  
  return(absences)
  
}


## -----------------------------------------------------------------------------------------------



pseudoAbsences <- function(rasters,records,n) {
  
  shape <- subset(rasters,1)
  nonNACells <- Which(!is.na(shape), cells=TRUE) 
  sink.points <- xyFromCell(shape, nonNACells)
  
  absences <- sample( 1:nrow(sink.points) , min(n,nrow(sink.points)) , replace=FALSE)
  absences <- sink.points[absences,]
  colnames(absences) <- c("Lon","Lat")
  
    # Removes those closer than paDist
    
    sink.points.poly <- as.data.frame(records)
    coordinates( sink.points.poly ) <- c( "Lon", "Lat" )
    proj4string( sink.points.poly ) <- CRS( "+proj=longlat +datum=WGS84" )
    
    sink.points.poly <- gBuffer( sink.points.poly, width=25 / 111.699, byid=TRUE )
    # plot(sink.points.poly)
    
    sink.points.pts <- as.data.frame(absences)
    colnames( sink.points.pts ) <- c( "Lon", "Lat" )
    coordinates( sink.points.pts ) <- c( "Lon", "Lat" )
    proj4string( sink.points.pts ) <- CRS( "+proj=longlat +datum=WGS84" )
    
    to.remove.id <- sp::over(sink.points.pts,sink.points.poly)
    to.keep <- which(is.na(to.remove.id))
    absences <- absences[to.keep,]
    
  return(absences)
    
}

## -----------------------------------------------------------------------------------------------

selectRecords <- function(records,lonName,latName) {
  
  records <- records[which(!is.na(records[,lonName])),] 
  
  nPoints <- length(feature$geometry$coordinates[[1]])
  sapply(1:nPoints,function(x) { unlist(feature$geometry$coordinates[[1]][[x]]) })
  poly <- spPolygons(t(sapply(1:nPoints,function(x) { unlist(feature$geometry$coordinates[[1]][[x]]) })))
  
  spobj1 <- SpatialPointsDataFrame(records[,c(lonName,latName)], data=records)
  crs(spobj1) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  crs(poly) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  records <- records[as.numeric(which( ! is.na(over(spobj1,poly) ))),]
  
  return(records)
}

## -----------------------------------------------------------------------------------------------
  
removeOverLand <- function(spobj1,lonName,latName) {
  
  spobj1 <- spobj1[which(!is.na(spobj1[,lonName])),] 
  spobj2 <- ne_countries(scale = 110)
  
  if(class(spobj1) == "data.frame" ) {
    
    spobj1 <- SpatialPointsDataFrame(spobj1[,c(lonName,latName)], data=spobj1)
    
  }

  crs(spobj1) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  crs(spobj2) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  overLand <- which( is.na(over(spobj1,spobj2)[,1] ))
  cat("Removing",length(overLand),"records over Land")
  overLand <- which( ! is.na(over(spobj1,spobj2)[,1] ))
  spobj1 <- spobj1[-overLand,]
  spobj1 <- as.data.frame(spobj1)[,c(lonName,latName)]
  return(spobj1)
  
}


## -----------------------------------------------------------------------------------------------

whichOverPolygon <- function(spobj1,spobj2) {
  
  crs(spobj1) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  crs(spobj2) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  which( ! is.na(over(spobj2,spobj1)[,1] ))
  
}

## -----------------------------------------------------------------------------------------------

removeNA <- function(records,lonName,latName) {
  
  cat("Removing",sum( is.na( records[,lonName] ) ),"NA records")
  records <- records[which( ! is.na(records[,lonName]) ), ]
  return(records)
  
}

## -----------------------------------------------------------------------------------------------

removeDuplicated <- function(records,lonName,latName) {
  
  cat("Removing",length(which( duplicated( records[,c(lonName,latName)] ) )),"NA records")
  records <- records[ which( ! duplicated( records[,c(lonName,latName)] ) ), ]
  
  return(records)
  
}
## -----------------------------------------------------------------------------------------------

getOccurrencesObis <- function(taxa) {
  
  result <- occurrence(taxa)
  result <- data.frame(Name=result$scientificName,Locality=result$locality,Lon=as.numeric(result$decimalLongitude),Lat=as.numeric(result$decimalLatitude),Depth=as.numeric(result$depth),dateYear=as.numeric(result$year),dateMonth=as.numeric(result$month),dateDay=as.numeric(result$day),stringsAsFactors = FALSE)
  return(result)
}

## -----------------------------------------------------------------------------------------------

getOccurrencesGBIF <- function(taxa) {
  
  taxa <- unlist(strsplit(taxa, " "))
  
  if( length(length(taxa)) == 1) { result <- gbif(taxa[1],taxa[2]) }
  if( length(length(taxa)) == 2) { result <- gbif(taxa[1],taxa[2]) }
  result <- data.frame(Name=result$scientificName,Locality=result$locality,Lon=as.numeric(result$lon),Lat=as.numeric(result$lat),Depth=as.numeric(result$depth),dateYear=as.numeric(result$year),dateMonth=as.numeric(result$month),dateDay=as.numeric(result$day),stringsAsFactors = FALSE)
  return(result)
}

## -----------------------------------------------------------------------------------------------

decimals <- function(x) {
  if ((x %% 1) != 0) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}

## -----------------------------------------------------------------------------------------------

getCoordinates <- function(address) {
  
  construct.geocode.url <- function(x, return.call = "json", sensor = "false") {
    
    root <- "http://www.mapquestapi.com/geocoding/v1/"
    u <- paste(root, "address?key=mpIx2AWq4Lj9R0mDbW1hNWrPe1Jju4X9&location=", x, sep = "")
    return(URLencode(u))
  }
  
  coords <- data.frame()
  
  options(warn=-1)
  
  for(address.i in address) {
  
    x <- NA
    u <- construct.geocode.url(address.i)
    doc <- getURL(u)
    
    tryCatch({
      x <- fromJSON(doc,simplify = FALSE)
    }, error=function(e){ error <- TRUE })
    
    if( is.na(x) | is.na(address.i) ) { lat <- NA ; lng <- NA }
    
    if(!  is.na(x) ) { 
      
      lat <- x$results[[1]]$locations[[1]]$latLng$lat
      lng <- x$results[[1]]$locations[[1]]$latLng$lng
      
      }

    coords <- rbind(coords,data.frame(Lon=lng,Lat=lat))
    
  }

  options(warn=0)
  
  return(coords)
  
}

## -----------------------------------------------------------------------------------------------

getLocation <- function(coordLon,coordLat) {
  
  construct.geocode.url <- function(coordLon,coordLat, return.call = "json", sensor = "false") {
    
    root <- "http://www.mapquestapi.com/geocoding/v1/reverse?key=mpIx2AWq4Lj9R0mDbW1hNWrPe1Jju4X9&"
    u <- paste(root, "location=", coordLat,",",coordLon,"", sep = "") #&includeRoadMetadata=true&includeNearestIntersection=true
    return(URLencode(u))
  }
  
  u <- construct.geocode.url(coordLon,coordLat)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  
  
  return(x$results[[1]]$locations[[1]]$adminArea1)
  
}

## -----------------------------------------------------------------------------------------------
relocateNACoords <- function(occurrenceRecords,rasterLayers,relocateSpeciesDistance) {
  
  shape <- subset(rasterLayers,1)
  
  to.relocate <- unique(which(is.na(raster::extract(shape,occurrenceRecords))))
  coordinates.to.relocate <- occurrenceRecords[to.relocate,]
  
  if( nrow(coordinates.to.relocate) > 0 ) {
    
    old.presences <- occurrenceRecords[ (1:nrow(occurrenceRecords))[! 1:nrow(occurrenceRecords) %in% to.relocate] ,]
    
    result <- Which(!is.na(shape),cells=TRUE)
    correct.points <- xyFromCell(shape, result)
    
    cat( paste0("Relocating ",length(to.relocate)," Points that were falling out of range"))
    cat( paste0("\n"))
    
    near.cells <- numeric(nrow(coordinates.to.relocate))
    
    for(p in 1:nrow(coordinates.to.relocate)) {
      
      near.cell.p <- spDistsN1( as.matrix(correct.points), as.matrix(coordinates.to.relocate[p,]),longlat=TRUE)
      
      if( near.cell.p[which.min(near.cell.p)] <= sqrt(sum(relocateSpeciesDistance^2,relocateSpeciesDistance^2)) ) {
        
        near.cell.p <- which.min(near.cell.p)
        
      } else { near.cell.p <- NA }
      
      near.cells[p] <- near.cell.p
      
    }
    
    relocated <- which(!is.na(near.cells))
    
    if( length(relocated) > 0) {
      
      near.cells <- correct.points[near.cells[relocated],]
      
      colnames(near.cells) <- c("Lon","Lat")
      
      occurrenceRecords <- rbind(old.presences,near.cells)
      
    }
    
  }
  
  ## -----------------------
  
  if( nrow(coordinates.to.relocate) == 0) {
    
    cat( paste0("None to Relocate"))
    cat( paste0("\n"))
    
  }
  
  ## -----------------------
  
  toKeep <- raster::extract(shape,occurrenceRecords)
  toKeep <- which(!is.na(toKeep))
  occurrenceRecords <- occurrenceRecords[toKeep,]
  
  return( occurrenceRecords )
  
}
  ## --------------------------
