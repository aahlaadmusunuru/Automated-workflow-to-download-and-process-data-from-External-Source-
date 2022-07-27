library(rgdal)
library(raster)
library(httr)
library(XML)
library(RCurl)
library(dplyr)
library(MASS)
library(reshape2)
library(reshape)
library(yaml)
library(spatialEco)
library(sf)
library(sp)
library(maptools)
library(config)
# Pull the list of urels in from the link 
rm(list = ls())
# working Directer 

config <- read_yaml("Data.yaml")








Dounwload_Data<-function(URl,MainDir,subDir,Latest_Download){
  resource<-GET(URl)
  parse <-htmlParse(resource)
  links<-xpathSApply(parse, path="//a", xmlGetAttr, "href")
  lapply(links,function(x){rt<-paste0(config$url,x)})->linkslist
  Organise_linkslist <- data.frame(matrix(unlist(linkslist), nrow=length(linkslist), byrow=TRUE))
  names(Organise_linkslist)<-"Datasets"
  data.frame(Organise_linkslist[-1:-5,])->Datasets
  data.frame(Organise_linkslist[-1:-5,])->Datasets
  
  Linkes <- Datasets[-nrow(Datasets),]
  
  c(Linkes)->Linkes_datasets
  
  # Removing all the preacious Years appart filtering from the year 2010
  Linkes_datasets[-1:-348]->Linkes_datasets
  
  options(timeout=10000000000)
  
  
  # Chech the Folder is existing in the directoriey or not if existed 
  
  
  setwd(subDir)
  
  
  
  
  
  
  ReferenceFile <- Latest_Download
  
  
  if (file.exists(ReferenceFile)) {
    
    
    
  } else {
    
    LastImage<-data.frame(first(basename(Linkes_datasets))) 
    names(LastImage)<-"Latest_download"
    
    write.csv(LastImage,ReferenceFile)
    
  }
  
  Result<- read.csv(ReferenceFile)
  
  
  Result$Latest_download
  
  data.frame(Linkes_datasets,
             basename(Linkes_datasets))->result
  
  
  names(result)<-c("Links","Image_ID")
  result$ID <- seq.int(nrow(result))
  result$ID<-as.numeric(result$ID)
  result[result$Image_ID== Result$Latest_download[1],]->result_Filter
  result_Filter$ID<-as.numeric( result_Filter$ID)
  # result[result$ID >= result_Filter$ID[[1]]]->Filter_Images_To_Download
  result[result$ID>= result_Filter$ID,]->result_Filter_links
  data.frame(result_Filter_links)%>%dplyr::select(Links)->result_Filter_linksData
  
  for (i in result_Filter_linksData) {
    q<-paste0(destfile = basename(i),".tiff")
    
    download.file(i,q, mode = 'wb',  overwrite = TRUE)
  }
  
  list.files(path = getwd(),pattern = ".tiff$")->LastImage
  
  
  
  LastImage<-data.frame(last(LastImage)) 
  names(LastImage)<-"Latest_download"
  
  write.csv(LastImage,ReferenceFile)
  
}




# maske data Function 

Admin_Maks<-function(MainDir,Adminboundaries,subDirMask,Row_Images){
  
  setwd(MainDir)
  Aruba_Country<-readOGR(Adminboundaries)
  Row_Images_path<-list.files(path = Row_Images,pattern = ".tiff$")
  
  
  setwd(Row_Images)
  lapply(Row_Images_path,raster)->raster_image_data
  
  
  setwd(subDirMask)
  
  
  
  
  
  
  for (i in raster_image_data) {
    q<-paste0(destfile = names(i))
    
    masked <- mask(x = i, mask = Aruba_Country)  
    
    
    writeRaster(masked,q, bylayer=TRUE)
    
  }
  
  setwd(MainDir)
  
}



Resample_Raster<-function(MainDir,Sample_Raster,subDirResample,Image_to_resample,grid){
  
  
  setwd(MainDir)
  Sample_Raster_data<-raster(Sample_Raster)
  Arebcountries_Grid_Data<-readOGR(grid)
  
  setwd(Image_to_resample)
  Row_Images_path<-list.files(path = Image_to_resample,pattern = ".tiff$")
  
  
  lapply(Row_Images_path,raster)->Mask_Arub_image_data
  
  
  
  
  
  
  
  data.frame(Row_Images_path)->data_names
  
  
  
  
  
  
  data_names$Year<-substr(data_names$Row_Images_path,7,10)
  data_names$ID<-substr(data_names$Row_Images_path,12,13)
  
  data_names
  ID<-c(1,2,3,4,5,6,7,8,9,10,11,12)
  
  Month<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dece")
  
  
  Month_ID<-data.frame(ID,Month)
  
  data_names$ID<-as.numeric(data_names$ID)
  
  joined_tibble <- left_join(data_names, Month_ID, 
                             by = c("ID" = "ID"))
  joined_tibble$Data<-paste0(joined_tibble$Year,"_",joined_tibble$Month)
  
  names(Mask_Arub_image_data)<-joined_tibble$Data
  
  
  
  setwd(subDirResample)
  
  
  for (i in Mask_Arub_image_data) {
    q<-paste0(destfile = names(i),".tif")
    
    fg<-resample(i,Sample_Raster_data,"ngb")
    writeRaster(fg,q,bylayer=TRUE)
  }
  
  
  
  
  
  
  
  
  
}




Mesh_gneration<-function(MainDir,Image_to_resample,subDirResample,grid,Final_Mesh_Directerey){
  
  
  setwd(MainDir)
  Arebcountries_Grid_Data<-readOGR(grid)
  setwd(subDirResample)
  raster_Images_maks_Resample<-list.files(path = getwd(),pattern = ".tif$")
  
  
  
  
  
  
  
  lapply(raster_Images_maks_Resample,raster)->raster_image_data_Resample
  
  data.frame(raster_Images_maks_Resample)->data_names
  data_names$Year<-substr(data_names$raster_Images_maks_Resample,7,10)
  data_names$ID<-substr(data_names$raster_Images_maks_Resample,12,13)
  
  
  ID<-c(1,2,3,4,5,6,7,8,9,10,11,12)
  
  Month<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dece")
  
  
  Month_ID<-data.frame(ID,Month)
  
  data_names$ID<-as.numeric(data_names$ID)
  
  joined_tibble <- left_join(data_names, Month_ID, 
                             by = c("ID" = "ID"))
  joined_tibble$Data<-paste0(joined_tibble$Year,"_",joined_tibble$Month)
  
  band_stack<-stack(raster_image_data_Resample)
  
  as.data.frame(band_stack,xy=TRUE)%>%na.omit()->frame 
  
  molten.data <- melt(frame, id = c("x","y"))
  
  
  molten.data$Year<-substr(molten.data$variable,7,10)
  molten.data$ID<-substr(molten.data$variable,12,13)
  molten.data$ID<-as.numeric(molten.data$ID)
  ID<-c(01,02,03,04,05,06,07,08,09,10,11,12)
  
  Month<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dece")
  
  
  Month_ID2<-data.frame(ID,Month)
  Month_ID2$ID<-as.numeric(Month_ID2$ID)
  
  joined_tibble <- left_join(molten.data, Month_ID2, 
                             by = c("ID" = "ID"))
  joined_tibble_selection<- joined_tibble%>%dplyr::select(x,y,Year,Month,value)
  
  joined_tibble_selection
  
  
  
  
  
  filter(joined_tibble_selection, Month == "Jan")->Jan
  split(Jan,Jan$Year)->JanSplit
  JanSplit[1]->firstJan
  lapply(JanSplit,function(x){x[5]})->JanSplitData
  do.call(cbind.data.frame, JanSplitData)->JanSplitDataTranspose
  
  Data_namesJan<-unique(Jan$Year)
  names(JanSplitDataTranspose)<-Data_namesJan
  
  
  firstJan[1:2]->Lat_long_Jan
  Lat_long_Jan[1]->Lat_long_Jan
  Lat_long_Jan$`2010`[1:2]->Lat_long_Jan
  
  
  # data.frame(Lat_long,do.call(cbind.data.frame, f))
  data.frame(Lat_long_Jan,JanSplitDataTranspose)->Lat_long_data_Jan
  
  
  result_Jan<-lapply(JanSplit,function(x){
    
    left_join(x,Lat_long_data_Jan, by = c("x" = "x", "y" = "y"))
  })
  
  Res_Jan<-list()
  
  for(i in names(result_Jan)){
    
    
    w<-c("X", "y","Year","Month","value", names(result_Jan))
    
    data.frame(w) ->Y
    data.frame(result_Jan[i])->j1dataframe
    names(j1dataframe)<-Y$w
    
    j1dataframe%>%dplyr::select(-i)->gh
    gh[-1:-5]->d
    
    Result<-data.frame(gh[1:5],rowMeans(d))
    # Result$anomalies<-c(rowMeans.d.-value )
    # Result%>%mutate(anomalies=(rowMeans.d.-value))
    
    names(Result )<-c("X","Y","Year","Month","Rainfall","Mean")
    
    Result$Anomalies<-c(Result$Rainfall-Result$Mean)
    Result$Date<-paste0(Result$Year,"",Result$Month)
    
    Result%>%dplyr::select(X,Y,Year,Month,Date,Rainfall,Anomalies)->ResultData
    # ResultDataSets<-melt(ResultData, id = c("X","Y","Year","Month","Rainfall","Anomalies"))
    
    Res_Jan[[i]] <-ResultData
    
  }
  Res_Jan
  lapply(Res_Jan,function(x){
    
    
    points <- x%>%st_as_sf(coords=c('X','Y'), crs=4326)
    
    poly <- point.in.poly(points, Arebcountries_Grid_Data)
    philly_sf_merged <- sp::merge(Arebcountries_Grid_Data,poly, by = c("X" = "x", "Y" = "y"))
  })->Jan_Result
  
  
  Jan_Result_Shapfile <- do.call(rbind, Jan_Result)
  
  
  data.frame(names(Jan_Result_Shapfile)[1:5])->Data1
  
  Jan_Result_Shapfile[-1:-5]->Jan_Result_Shapfile
  Jan_Result_Shapfile[-8:-9]->Jan_Result_Shapfile
  Jan_Result_Shapfile
  
  
  
  
  
  
  
  filter(joined_tibble_selection, Month == "Feb")->Feb
  
  
  
  
  
  
  
  
  split(Feb,Feb$Year)->FebSplit
  FebSplit[1]->firstFeb
  lapply(FebSplit,function(x){x[5]})->firstFebData
  do.call(cbind.data.frame, firstFebData)->firstFebDataTranspose
  
  
  
  Data_names_Feb<-unique(Feb$Year)
  
  names(firstFebDataTranspose)<-Data_names_Feb
  
  
  
  firstFeb[1:2]->Lat_long_Feb
  Lat_long_Feb[1]->Lat_long_Feb
  Lat_long_Feb$`2010`[1:2]->Lat_long_Feb
  
  
  
  # data.frame(Lat_long,do.call(cbind.data.frame, f))
  data.frame(Lat_long_Feb,firstFebDataTranspose)->Lat_long_data_Feb
  
  
  
  result_Feb<-lapply(FebSplit,function(x){
    
    left_join(x,Lat_long_data_Feb, by = c("x" = "x", "y" = "y"))
  })
  
  
  Res_Feb<-list()
  for(i in names(result_Feb)){
    
    w<-c("X", "y","Year","Month","value", names(result_Feb))
    
    
    data.frame(w) ->Y
    data.frame(result_Feb[i])->j1dataframe
    names(j1dataframe)<-Y$w
    
    j1dataframe%>%dplyr::select(-i)->gh
    gh[-1:-5]->d
    
    Result<-data.frame(gh[1:5],rowMeans(d))
    # Result$anomalies<-c(rowMeans.d.-value )
    # Result%>%mutate(anomalies=(rowMeans.d.-value))
    
    names(Result )<-c("X","Y","Year","Month","Rainfall","Mean")
    
    Result$Anomalies<-c(Result$Rainfall-Result$Mean)
    Result$Date<-paste0(Result$Year,"",Result$Month)
    
    Result%>%dplyr::select(X,Y,Year,Month,Date,Rainfall,Anomalies)->ResultData
    # ResultDataSets<-melt(ResultData, id = c("X","Y","Year","Month","Rainfall","Anomalies"))
    
    Res_Feb[[i]] <-ResultData
    
  }
  
  
  Res_Feb
  
  
  
  lapply(Res_Feb,function(x){
    
    
    points <- x%>%st_as_sf(coords=c('X','Y'), crs=4326)
    
    poly <- point.in.poly(points, Arebcountries_Grid_Data)
    philly_sf_merged <- sp::merge(Arebcountries_Grid_Data,poly, by = c("X" = "x", "Y" = "y"))
  })->Feb_Result
  
  
  Feb_Result_Shapfile <- do.call(rbind, Feb_Result)
  
  
  data.frame(names(Feb_Result_Shapfile)[1:5])->Data1
  
  Feb_Result_Shapfile[-1:-5]->Feb_Result_Shapfile
  Feb_Result_Shapfile[-8:-9]->Feb_Result_Shapfile
  Feb_Result_Shapfile
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  filter(joined_tibble_selection, Month == "Mar")->Mar
  
  
  
  
  split(Mar,Mar$Year)->MarSplit
  MarSplit[1]->firstMar
  lapply(MarSplit,function(x){x[5]})->MarSplitData
  do.call(cbind.data.frame, MarSplitData)->MarSplitDataTranspose
  
  Data_namesMar<-unique(Mar$Year)
  names(MarSplitDataTranspose)<-Data_namesMar
  
  
  firstMar[1:2]->Lat_long_Mar
  Lat_long_Mar[1]->Lat_long_Mar
  Lat_long_Mar$`2010`[1:2]->Lat_long_Mar
  
  
  # data.frame(Lat_long,do.call(cbind.data.frame, f))
  data.frame(Lat_long_Mar,MarSplitDataTranspose)->Lat_long_data_Mar
  
  
  result_Mar<-lapply(MarSplit,function(x){
    
    left_join(x,Lat_long_data_Mar, by = c("x" = "x", "y" = "y"))
  })
  
  Res_Mar<-list()
  
  for(i in names(result_Mar)){
    
    
    w<-c("X", "y","Year","Month","value", names(result_Mar))
    
    data.frame(w) ->Y
    data.frame(result_Mar[i])->j1dataframe
    names(j1dataframe)<-Y$w
    
    j1dataframe%>%dplyr::select(-i)->gh
    gh[-1:-5]->d
    
    Result<-data.frame(gh[1:5],rowMeans(d))
    # Result$anomalies<-c(rowMeans.d.-value )
    # Result%>%mutate(anomalies=(rowMeans.d.-value))
    
    names(Result )<-c("X","Y","Year","Month","Rainfall","Mean")
    
    Result$Anomalies<-c(Result$Rainfall-Result$Mean)
    Result$Date<-paste0(Result$Year,"",Result$Month)
    
    Result%>%dplyr::select(X,Y,Year,Month,Date,Rainfall,Anomalies)->ResultData
    # ResultDataSets<-melt(ResultData, id = c("X","Y","Year","Month","Rainfall","Anomalies"))
    
    Res_Mar[[i]] <-ResultData
    
  }
  lapply(Res_Mar,function(x){
    
    
    points <- x%>%st_as_sf(coords=c('X','Y'), crs=4326)
    
    poly <- point.in.poly(points, Arebcountries_Grid_Data)
    philly_sf_merged <- sp::merge(Arebcountries_Grid_Data,poly, by = c("X" = "x", "Y" = "y"))
  })->Mar_Result
  
  
  Mar_Result_Shapfile <- do.call(rbind, Mar_Result)
  
  
  data.frame(names(Mar_Result_Shapfile)[1:5])->Data1
  
  Mar_Result_Shapfile[-1:-5]->Mar_Result_Shapfile
  Mar_Result_Shapfile[-8:-9]->Mar_Result_Shapfile
  Mar_Result_Shapfile
  
  
  
  
  
  
  
  
  
  
  
  
  
  filter(joined_tibble_selection, Month == "Apr")->Apr
  
  
  
  
  
  
  
  
  
  
  split(Apr,Apr$Year)->AprSplit
  AprSplit[1]->firstApr
  lapply(AprSplit,function(x){x[5]})->AprSplitData
  do.call(cbind.data.frame, AprSplitData)->AprSplitDataTranspose
  
  Data_namesApr<-unique(Apr$Year)
  names(AprSplitDataTranspose)<-Data_namesApr
  
  
  firstApr[1:2]->Lat_long_Apr
  Lat_long_Apr[1]->Lat_long_Apr
  Lat_long_Apr$`2010`[1:2]->Lat_long_Apr
  
  
  # data.frame(Lat_long,do.call(cbind.data.frame, f))
  data.frame(Lat_long_Apr,AprSplitDataTranspose)->Lat_long_data_Apr
  
  
  result_Apr<-lapply(AprSplit,function(x){
    
    left_join(x,Lat_long_data_Apr, by = c("x" = "x", "y" = "y"))
  })
  
  Res_Apr<-list()
  
  for(i in names(result_Apr)){
    
    
    w<-c("X", "y","Year","Month","value", names(result_Apr))
    
    data.frame(w) ->Y
    data.frame(result_Apr[i])->j1dataframe
    names(j1dataframe)<-Y$w
    
    j1dataframe%>%dplyr::select(-i)->gh
    gh[-1:-5]->d
    
    Result<-data.frame(gh[1:5],rowMeans(d))
    # Result$anomalies<-c(rowMeans.d.-value )
    # Result%>%mutate(anomalies=(rowMeans.d.-value))
    
    names(Result )<-c("X","Y","Year","Month","Rainfall","Mean")
    
    Result$Anomalies<-c(Result$Rainfall-Result$Mean)
    Result$Date<-paste0(Result$Year,"",Result$Month)
    
    Result%>%dplyr::select(X,Y,Year,Month,Date,Rainfall,Anomalies)->ResultData
    # ResultDataSets<-melt(ResultData, id = c("X","Y","Year","Month","Rainfall","Anomalies"))
    
    Res_Apr[[i]] <-ResultData
    
  }
  lapply(Res_Apr,function(x){
    
    
    points <- x%>%st_as_sf(coords=c('X','Y'), crs=4326)
    
    poly <- point.in.poly(points, Arebcountries_Grid_Data)
    philly_sf_merged <- sp::merge(Arebcountries_Grid_Data,poly, by = c("X" = "x", "Y" = "y"))
  })->Apr_Result
  
  
  Apr_Result_Shapfile <- do.call(rbind, Apr_Result)
  
  
  data.frame(names(Apr_Result_Shapfile)[1:5])->Data1
  
  Apr_Result_Shapfile[-1:-5]->Apr_Result_Shapfile
  Apr_Result_Shapfile[-8:-9]->Apr_Result_Shapfile
  Apr_Result_Shapfile
  
  
  
  
  
  
  
  
  
  
  
  filter(joined_tibble_selection, Month == "May")->May
  
  
  
  
  
  
  
  
  
  
  split(May,May$Year)->MaySplit
  MaySplit[1]->firstMay
  lapply(MaySplit,function(x){x[5]})->MaySplitData
  do.call(cbind.data.frame, MaySplitData)->MaySplitDataTranspose
  
  Data_namesMay<-unique(May$Year)
  names(MaySplitDataTranspose)<-Data_namesMay
  
  
  firstMay[1:2]->Lat_long_May
  Lat_long_May[1]->Lat_long_May
  Lat_long_May$`2010`[1:2]->Lat_long_May
  
  
  # data.frame(Lat_long,do.call(cbind.data.frame, f))
  data.frame(Lat_long_May,MaySplitDataTranspose)->Lat_long_data_May
  
  
  result_May<-lapply(MaySplit,function(x){
    
    left_join(x,Lat_long_data_May, by = c("x" = "x", "y" = "y"))
  })
  
  Res_May<-list()
  
  for(i in names(result_May)){
    
    
    w<-c("X", "y","Year","Month","value", names(result_May))
    
    data.frame(w) ->Y
    data.frame(result_May[i])->j1dataframe
    names(j1dataframe)<-Y$w
    
    j1dataframe%>%dplyr::select(-i)->gh
    gh[-1:-5]->d
    
    Result<-data.frame(gh[1:5],rowMeans(d))
    # Result$anomalies<-c(rowMeans.d.-value )
    # Result%>%mutate(anomalies=(rowMeans.d.-value))
    
    names(Result )<-c("X","Y","Year","Month","Rainfall","Mean")
    
    Result$Anomalies<-c(Result$Rainfall-Result$Mean)
    Result$Date<-paste0(Result$Year,"",Result$Month)
    
    Result%>%dplyr::select(X,Y,Year,Month,Date,Rainfall,Anomalies)->ResultData
    # ResultDataSets<-melt(ResultData, id = c("X","Y","Year","Month","Rainfall","Anomalies"))
    
    Res_May[[i]] <-ResultData
    
  }
  lapply(Res_May,function(x){
    
    
    points <- x%>%st_as_sf(coords=c('X','Y'), crs=4326)
    
    poly <- point.in.poly(points, Arebcountries_Grid_Data)
    philly_sf_merged <- sp::merge(Arebcountries_Grid_Data,poly, by = c("X" = "x", "Y" = "y"))
  })->May_Result
  
  
  May_Result_Shapfile <- do.call(rbind, May_Result)
  
  
  data.frame(names(May_Result_Shapfile)[1:5])->Data1
  
  May_Result_Shapfile[-1:-5]->May_Result_Shapfile
  May_Result_Shapfile[-8:-9]->May_Result_Shapfile
  May_Result_Shapfile
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  filter(joined_tibble_selection, Month == "Jun")->Jun
  
  
  
  
  
  
  
  
  
  
  split(Jun,Jun$Year)->JunSplit
  JunSplit[1]->firstJun
  lapply(JunSplit,function(x){x[5]})->JunSplitData
  do.call(cbind.data.frame, JunSplitData)->JunSplitDataTranspose
  
  Data_namesJun<-unique(Jun$Year)
  names(JunSplitDataTranspose)<-Data_namesJun
  
  
  firstJun[1:2]->Lat_long_Jun
  Lat_long_Jun[1]->Lat_long_Jun
  Lat_long_Jun$`2010`[1:2]->Lat_long_Jun
  
  
  # data.frame(Lat_long,do.call(cbind.data.frame, f))
  data.frame(Lat_long_Jun,JunSplitDataTranspose)->Lat_long_data_Jun
  
  
  result_Jun<-lapply(JunSplit,function(x){
    
    left_join(x,Lat_long_data_Jun, by = c("x" = "x", "y" = "y"))
  })
  
  Res_Jun<-list()
  
  for(i in names(result_Jun)){
    
    
    w<-c("X", "y","Year","Month","value", names(result_Jun))
    
    data.frame(w) ->Y
    data.frame(result_Jun[i])->j1dataframe
    names(j1dataframe)<-Y$w
    
    j1dataframe%>%dplyr::select(-i)->gh
    gh[-1:-5]->d
    
    Result<-data.frame(gh[1:5],rowMeans(d))
    # Result$anomalies<-c(rowMeans.d.-value )
    # Result%>%mutate(anomalies=(rowMeans.d.-value))
    
    names(Result )<-c("X","Y","Year","Month","Rainfall","Mean")
    
    Result$Anomalies<-c(Result$Rainfall-Result$Mean)
    Result$Date<-paste0(Result$Year,"",Result$Month)
    
    Result%>%dplyr::select(X,Y,Year,Month,Date,Rainfall,Anomalies)->ResultData
    # ResultDataSets<-melt(ResultData, id = c("X","Y","Year","Month","Rainfall","Anomalies"))
    
    Res_Jun[[i]] <-ResultData
    
  }
  lapply(Res_Jun,function(x){
    
    
    points <- x%>%st_as_sf(coords=c('X','Y'), crs=4326)
    
    poly <- point.in.poly(points, Arebcountries_Grid_Data)
    philly_sf_merged <- sp::merge(Arebcountries_Grid_Data,poly, by = c("X" = "x", "Y" = "y"))
  })->Jun_Result
  
  
  Jun_Result_Shapfile <- do.call(rbind, Jun_Result)
  
  
  data.frame(names(Jun_Result_Shapfile)[1:5])->Data1
  
  Jun_Result_Shapfile[-1:-5]->Jun_Result_Shapfile
  Jun_Result_Shapfile[-8:-9]->Jun_Result_Shapfile
  Jun_Result_Shapfile
  
  
  
  
  
  
  
  
  
  
  
  filter(joined_tibble_selection, Month == "Jul")->Jul
  
  
  
  
  
  
  
  
  
  
  split(Jul,Jul$Year)->JulSplit
  JulSplit[1]->firstJul
  lapply(JulSplit,function(x){x[5]})->JulSplitData
  do.call(cbind.data.frame, JulSplitData)->JulSplitDataTranspose
  
  Data_namesJul<-unique(Jul$Year)
  names(JulSplitDataTranspose)<-Data_namesJul
  
  
  firstJul[1:2]->Lat_long_Jul
  Lat_long_Jul[1]->Lat_long_Jul
  Lat_long_Jul$`2010`[1:2]->Lat_long_Jul
  
  
  # data.frame(Lat_long,do.call(cbind.data.frame, f))
  data.frame(Lat_long_Jul,JulSplitDataTranspose)->Lat_long_data_Jul
  
  
  result_Jul<-lapply(JulSplit,function(x){
    
    left_join(x,Lat_long_data_Jul, by = c("x" = "x", "y" = "y"))
  })
  
  Res_Jul<-list()
  
  for(i in names(result_Jul)){
    
    
    w<-c("X", "y","Year","Month","value", names(result_Jul))
    
    data.frame(w) ->Y
    data.frame(result_Jul[i])->j1dataframe
    names(j1dataframe)<-Y$w
    
    j1dataframe%>%dplyr::select(-i)->gh
    gh[-1:-5]->d
    
    Result<-data.frame(gh[1:5],rowMeans(d))
    # Result$anomalies<-c(rowMeans.d.-value )
    # Result%>%mutate(anomalies=(rowMeans.d.-value))
    
    names(Result )<-c("X","Y","Year","Month","Rainfall","Mean")
    
    Result$Anomalies<-c(Result$Rainfall-Result$Mean)
    Result$Date<-paste0(Result$Year,"",Result$Month)
    
    Result%>%dplyr::select(X,Y,Year,Month,Date,Rainfall,Anomalies)->ResultData
    # ResultDataSets<-melt(ResultData, id = c("X","Y","Year","Month","Rainfall","Anomalies"))
    
    Res_Jul[[i]] <-ResultData
    
  }
  lapply(Res_Jul,function(x){
    
    
    points <- x%>%st_as_sf(coords=c('X','Y'), crs=4326)
    
    poly <- point.in.poly(points, Arebcountries_Grid_Data)
    philly_sf_merged <- sp::merge(Arebcountries_Grid_Data,poly, by = c("X" = "x", "Y" = "y"))
  })->Jul_Result
  
  
  Jul_Result_Shapfile <- do.call(rbind, Jul_Result)
  
  
  data.frame(names(Jul_Result_Shapfile)[1:5])->Data1
  
  Jul_Result_Shapfile[-1:-5]->Jul_Result_Shapfile
  Jul_Result_Shapfile[-8:-9]->Jul_Result_Shapfile
  Jul_Result_Shapfile
  
  
  
  
  
  
  
  filter(joined_tibble_selection, Month == "Aug")->Aug
  
  
  
  
  
  
  
  
  
  
  split(Aug,Aug$Year)->AugSplit
  AugSplit[1]->firstAug
  lapply(AugSplit,function(x){x[5]})->AugSplitData
  do.call(cbind.data.frame, AugSplitData)->AugSplitDataTranspose
  
  Data_namesAug<-unique(Aug$Year)
  names(AugSplitDataTranspose)<-Data_namesAug
  
  
  firstAug[1:2]->Lat_long_Aug
  Lat_long_Aug[1]->Lat_long_Aug
  Lat_long_Aug$`2010`[1:2]->Lat_long_Aug
  
  
  # data.frame(Lat_long,do.call(cbind.data.frame, f))
  data.frame(Lat_long_Aug,AugSplitDataTranspose)->Lat_long_data_Aug
  
  
  result_Aug<-lapply(AugSplit,function(x){
    
    left_join(x,Lat_long_data_Aug, by = c("x" = "x", "y" = "y"))
  })
  
  Res_Aug<-list()
  
  for(i in names(result_Aug)){
    
    
    w<-c("X", "y","Year","Month","value", names(result_Aug))
    
    data.frame(w) ->Y
    data.frame(result_Aug[i])->j1dataframe
    names(j1dataframe)<-Y$w
    
    j1dataframe%>%dplyr::select(-i)->gh
    gh[-1:-5]->d
    
    Result<-data.frame(gh[1:5],rowMeans(d))
    # Result$anomalies<-c(rowMeans.d.-value )
    # Result%>%mutate(anomalies=(rowMeans.d.-value))
    
    names(Result )<-c("X","Y","Year","Month","Rainfall","Mean")
    
    Result$Anomalies<-c(Result$Rainfall-Result$Mean)
    Result$Date<-paste0(Result$Year,"",Result$Month)
    
    Result%>%dplyr::select(X,Y,Year,Month,Date,Rainfall,Anomalies)->ResultData
    # ResultDataSets<-melt(ResultData, id = c("X","Y","Year","Month","Rainfall","Anomalies"))
    
    Res_Aug[[i]] <-ResultData
    
  }
  lapply(Res_Aug,function(x){
    
    
    points <- x%>%st_as_sf(coords=c('X','Y'), crs=4326)
    
    poly <- point.in.poly(points, Arebcountries_Grid_Data)
    philly_sf_merged <- sp::merge(Arebcountries_Grid_Data,poly, by = c("X" = "x", "Y" = "y"))
  })->Aug_Result
  
  
  Aug_Result_Shapfile <- do.call(rbind, Aug_Result)
  
  
  data.frame(names(Aug_Result_Shapfile)[1:5])->Data1
  
  Aug_Result_Shapfile[-1:-5]->Aug_Result_Shapfile
  Aug_Result_Shapfile[-8:-9]->Aug_Result_Shapfile
  Aug_Result_Shapfile
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  filter(joined_tibble_selection, Month == "Sep")->Sep
  
  
  
  
  
  
  
  
  split(Sep,Sep$Year)->SepSplit
  SepSplit[1]->firstSep
  lapply(SepSplit,function(x){x[5]})->SepSplitData
  do.call(cbind.data.frame, SepSplitData)->SepSplitDataTranspose
  
  Data_namesSep<-unique(Sep$Year)
  names(SepSplitDataTranspose)<-Data_namesSep
  
  
  firstSep[1:2]->Lat_long_Sep
  Lat_long_Sep[1]->Lat_long_Sep
  Lat_long_Sep$`2010`[1:2]->Lat_long_Sep
  
  
  # data.frame(Lat_long,do.call(cbind.data.frame, f))
  data.frame(Lat_long_Sep,SepSplitDataTranspose)->Lat_long_data_Sep
  
  
  result_Sep<-lapply(SepSplit,function(x){
    
    left_join(x,Lat_long_data_Sep, by = c("x" = "x", "y" = "y"))
  })
  
  
  Res_Sep<-list()
  
  for(i in names(result_Sep)){
    
    
    w<-c("X", "y","Year","Month","value", names(result_Sep))
    
    data.frame(w) ->Y
    data.frame(result_Sep[i])->j1dataframe
    names(j1dataframe)<-Y$w
    
    j1dataframe%>%dplyr::select(-i)->gh
    gh[-1:-5]->d
    
    Result<-data.frame(gh[1:5],rowMeans(d))
    # Result$anomalies<-c(rowMeans.d.-value )
    # Result%>%mutate(anomalies=(rowMeans.d.-value))
    
    names(Result )<-c("X","Y","Year","Month","Rainfall","Mean")
    
    Result$Anomalies<-c(Result$Rainfall-Result$Mean)
    Result$Date<-paste0(Result$Year,"",Result$Month)
    
    Result%>%dplyr::select(X,Y,Year,Month,Date,Rainfall,Anomalies)->ResultData
    # ResultDataSets<-melt(ResultData, id = c("X","Y","Year","Month","Rainfall","Anomalies"))
    
    Res_Sep[[i]] <-ResultData
    
  }
  
  lapply(Res_Sep,function(x){
    
    
    points <- x%>%st_as_sf(coords=c('X','Y'), crs=4326)
    
    poly <- point.in.poly(points, Arebcountries_Grid_Data)
    philly_sf_merged <- sp::merge(Arebcountries_Grid_Data,poly, by = c("X" = "x", "Y" = "y"))
  })->Sep_Result
  
  
  Sep_Result_Shapfile <- do.call(rbind, Sep_Result)
  
  
  data.frame(names(Sep_Result_Shapfile)[1:5])->Data1
  
  Sep_Result_Shapfile[-1:-5]->Sep_Result_Shapfile
  Sep_Result_Shapfile[-8:-9]->Sep_Result_Shapfile
  Sep_Result_Shapfile
  
  
  
  
  
  
  
  
  
  
  
  
  
  filter(joined_tibble_selection, Month == "Oct")->Oct
  
  
  
  
  
  
  
  
  split(Oct,Oct$Year)->OctSplit
  OctSplit[1]->firstOct
  lapply(OctSplit,function(x){x[5]})->OctSplitData
  do.call(cbind.data.frame, OctSplitData)->OctSplitDataTranspose
  
  Data_namesOct<-unique(Oct$Year)
  names(OctSplitDataTranspose)<-Data_namesOct
  
  
  firstOct[1:2]->Lat_long_Oct
  Lat_long_Oct[1]->Lat_long_Oct
  Lat_long_Oct$`2010`[1:2]->Lat_long_Oct
  
  
  # data.frame(Lat_long,do.call(cbind.data.frame, f))
  data.frame(Lat_long_Oct,OctSplitDataTranspose)->Lat_long_data_Oct
  
  
  result_Oct<-lapply(OctSplit,function(x){
    
    left_join(x,Lat_long_data_Oct, by = c("x" = "x", "y" = "y"))
  })
  
  
  Res_Oct<-list()
  
  for(i in names(result_Oct)){
    
    
    w<-c("X", "y","Year","Month","value", names(result_Oct))
    
    data.frame(w) ->Y
    data.frame(result_Oct[i])->j1dataframe
    names(j1dataframe)<-Y$w
    
    j1dataframe%>%dplyr::select(-i)->gh
    gh[-1:-5]->d
    
    Result<-data.frame(gh[1:5],rowMeans(d))
    # Result$anomalies<-c(rowMeans.d.-value )
    # Result%>%mutate(anomalies=(rowMeans.d.-value))
    
    names(Result )<-c("X","Y","Year","Month","Rainfall","Mean")
    
    Result$Anomalies<-c(Result$Rainfall-Result$Mean)
    Result$Date<-paste0(Result$Year,"",Result$Month)
    
    Result%>%dplyr::select(X,Y,Year,Month,Date,Rainfall,Anomalies)->ResultData
    # ResultDataSets<-melt(ResultData, id = c("X","Y","Year","Month","Rainfall","Anomalies"))
    
    Res_Oct[[i]] <-ResultData
    
  }
  
  lapply(Res_Oct,function(x){
    
    
    points <- x%>%st_as_sf(coords=c('X','Y'), crs=4326)
    
    poly <- point.in.poly(points, Arebcountries_Grid_Data)
    philly_sf_merged <- sp::merge(Arebcountries_Grid_Data,poly, by = c("X" = "x", "Y" = "y"))
  })->Oct_Result
  
  
  Oct_Result_Shapfile <- do.call(rbind, Oct_Result)
  
  
  data.frame(names(Oct_Result_Shapfile)[1:5])->Data1
  
  Oct_Result_Shapfile[-1:-5]->Oct_Result_Shapfile
  Oct_Result_Shapfile[-8:-9]->Oct_Result_Shapfile
  Oct_Result_Shapfile
  
  
  
  
  
  filter(joined_tibble_selection, Month == "Nov")->Nov
  
  
  
  
  
  
  
  
  split(Nov,Nov$Year)->NovSplit
  NovSplit[1]->firstNov
  lapply(NovSplit,function(x){x[5]})->NovSplitData
  do.call(cbind.data.frame, NovSplitData)->NovSplitDataTranspose
  
  Data_namesNov<-unique(Nov$Year)
  names(NovSplitDataTranspose)<-Data_namesNov
  
  
  firstNov[1:2]->Lat_long_Nov
  Lat_long_Nov[1]->Lat_long_Nov
  Lat_long_Nov$`2010`[1:2]->Lat_long_Nov
  
  
  # data.frame(Lat_long,do.call(cbind.data.frame, f))
  data.frame(Lat_long_Nov,NovSplitDataTranspose)->Lat_long_data_Nov
  
  
  result_Nov<-lapply(NovSplit,function(x){
    
    left_join(x,Lat_long_data_Nov, by = c("x" = "x", "y" = "y"))
  })
  
  
  Res_Nov<-list()
  
  for(i in names(result_Nov)){
    
    
    w<-c("X", "y","Year","Month","value", names(result_Nov))
    
    data.frame(w) ->Y
    data.frame(result_Nov[i])->j1dataframe
    names(j1dataframe)<-Y$w
    
    j1dataframe%>%dplyr::select(-i)->gh
    gh[-1:-5]->d
    
    Result<-data.frame(gh[1:5],rowMeans(d))
    # Result$anomalies<-c(rowMeans.d.-value )
    # Result%>%mutate(anomalies=(rowMeans.d.-value))
    
    names(Result )<-c("X","Y","Year","Month","Rainfall","Mean")
    
    Result$Anomalies<-c(Result$Rainfall-Result$Mean)
    Result$Date<-paste0(Result$Year,"",Result$Month)
    
    Result%>%dplyr::select(X,Y,Year,Month,Date,Rainfall,Anomalies)->ResultData
    # ResultDataSets<-melt(ResultData, id = c("X","Y","Year","Month","Rainfall","Anomalies"))
    
    Res_Nov[[i]] <-ResultData
    
  }
  
  
  lapply(Res_Nov,function(x){
    
    
    points <- x%>%st_as_sf(coords=c('X','Y'), crs=4326)
    
    poly <- point.in.poly(points, Arebcountries_Grid_Data)
    philly_sf_merged <- sp::merge(Arebcountries_Grid_Data,poly, by = c("X" = "x", "Y" = "y"))
  })->Nov_Result
  
  
  Nov_Result_Shapfile <- do.call(rbind, Nov_Result)
  
  
  data.frame(names(Nov_Result_Shapfile)[1:5])->Data1
  
  Nov_Result_Shapfile[-1:-5]->Nov_Result_Shapfile
  Nov_Result_Shapfile[-8:-9]->Nov_Result_Shapfile
  Nov_Result_Shapfile
  
  
  
  
  
  
  
  
  
  
  

  
  filter(joined_tibble_selection, Month == "Dece")->Dece

  
  
  
  
  
  
  
  
  split(Dece,Dece$Year)->DeceSplit
  DeceSplit[1]->firstDece
  lapply(DeceSplit,function(x){x[5]})->DeceSplitData
  do.call(cbind.data.frame, DeceSplitData)->DeceSplitDataTranspose
  
  Data_namesDece<-unique(Dece$Year)
  names(DeceSplitDataTranspose)<-Data_namesDece
  
  
  firstDece[1:2]->Lat_long_Dece
  Lat_long_Dece[1]->Lat_long_Dece
  Lat_long_Dece$`2010`[1:2]->Lat_long_Dece
  
  
  # data.frame(Lat_long,do.call(cbind.data.frame, f))
  data.frame(Lat_long_Dece,DeceSplitDataTranspose)->Lat_long_data_Dece
  
  
  result_Dece<-lapply(DeceSplit,function(x){
    
    left_join(x,Lat_long_data_Dece, by = c("x" = "x", "y" = "y"))
  })
  
  
  Res_Dece<-list()
  
  for(i in names(result_Dece)){
    
    
    w<-c("X", "y","Year","Month","value", names(result_Dece))
    
    data.frame(w) ->Y
    data.frame(result_Dece[i])->j1dataframe
    names(j1dataframe)<-Y$w
    
    j1dataframe%>%dplyr::select(-i)->gh
    gh[-1:-5]->d
    
    Result<-data.frame(gh[1:5],rowMeans(d))
    # Result$anomalies<-c(rowMeans.d.-value )
    # Result%>%mutate(anomalies=(rowMeans.d.-value))
    
    names(Result )<-c("X","Y","Year","Month","Rainfall","Mean")
    
    Result$Anomalies<-c(Result$Rainfall-Result$Mean)
    Result$Date<-paste0(Result$Year,"",Result$Month)
    
    Result%>%dplyr::select(X,Y,Year,Month,Date,Rainfall,Anomalies)->ResultData
    # ResultDataSets<-melt(ResultData, id = c("X","Y","Year","Month","Rainfall","Anomalies"))
    
    Res_Dece[[i]] <-ResultData
    
  }
  lapply(Res_Dece,function(x){
    
    
    points <- x%>%st_as_sf(coords=c('X','Y'), crs=4326)
    
    poly <- point.in.poly(points, Arebcountries_Grid_Data)
    philly_sf_merged <- sp::merge(Arebcountries_Grid_Data,poly, by = c("X" = "x", "Y" = "y"))
  })->Dece_Result
  
  
  Dece_Result_Shapfile <- do.call(rbind, Dece_Result)
  
  
  data.frame(names(Dece_Result_Shapfile)[1:5])->Data1
  
  Dece_Result_Shapfile[-1:-5]->Dece_Result_Shapfile
  Dece_Result_Shapfile[-8:-9]->Dece_Result_Shapfile
  Dece_Result_Shapfile
  
  
  
  
  
  
  Result_List<-list(
    
    
    Jan_Result_Shapfile,
    
    Feb_Result_Shapfile,
    
    
    
    
    Mar_Result_Shapfile,
    Apr_Result_Shapfile,
    
    May_Result_Shapfile,
    
    Jun_Result_Shapfile,
    
    Jul_Result_Shapfile,
    
    Aug_Result_Shapfile,
    Sep_Result_Shapfile,
    
    Oct_Result_Shapfile,
    Nov_Result_Shapfile,
    Dece_Result_Shapfile
    
    
    
    
    
    
    
  )
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  df<-do.call(rbind, Result_List)
  
  
  
  
  
  
shapefile(df, filename=Final_Mesh_Directerey,overwrite=TRUE)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}














# download the data sets in r 


download<-Dounwload_Data(URl = config$url,MainDir=config$mainDir,subDir=config$Download_Directory,Latest_Download=config$Last_download_Rifference)

# maske the Data Set to the Areab reagion 

Mask<-Admin_Maks(MainDir=config$mainDir,Adminboundaries = config$`Aruba Country Admin Boundaries`,Row_Images=config$Download_Directory,subDirMask=config$`Mask_Directory`)


# Resampling the masked Data sets 

Resample<-Resample_Raster(MainDir=config$mainDir,Image_to_resample=config$Mask_Directory,Sample_Raster=config$`Sample Raster Data`,subDirResample=config$Resample_Directory)



# Generate a Geo data Base to a Gird 

Final_Mesh<-Mesh_gneration(MainDir=config$mainDir,grid= config$`Aruba Country Admin Grid 20 Meter`,subDirResample=config$Resample_Directory,Final_Mesh_Directerey=config$`Finel Result`)


# Export the Grid 
