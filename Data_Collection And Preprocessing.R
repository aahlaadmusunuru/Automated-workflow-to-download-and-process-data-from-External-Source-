library(rgdal)
library(raster)
library(httr)
library(XML)
library(RCurl)
library(dplyr)
library(MASS)
library(reshape2)
library(reshape)
library(odbc)
library(yaml)
library(spatialEco)
library(odbc)
library(DBI)
library(sf)
# Pull the list of urels in from the link 
rm(list = ls())
setwd("C:\\Aruba Country Rainfall Data")
Aruba_Country<-readOGR("arebcountries_Disolved.shp")

Arebcountries_Grid_Data<-readOGR("arebcountries_Grid20KM.shp")
Resample_reference<-raster("CHIRP.2010.0405.tif")
url<-"https://data.chc.ucsb.edu/products/CHIRP/2-monthly/"
resource<-GET(url)
parse <-htmlParse(resource)
links<-xpathSApply(parse, path="//a", xmlGetAttr, "href")
lapply(links,function(x){rt<-paste0("https://data.chc.ucsb.edu/products/CHIRP/2-monthly/",x)})->linkslist



Organise_linkslist <- data.frame(matrix(unlist(linkslist), nrow=length(linkslist), byrow=TRUE))
names(Organise_linkslist)<-"Datasets"
data.frame(Organise_linkslist[-1:-5,])->Datasets




Linkes <- Datasets[-nrow(Datasets),]


c(Linkes)->Linkes_datasets







Linkes_datasets[-1:-348]->Linkes_datasets

options(timeout=10000000000)



for (i in Linkes_datasets) {
  q<-paste0(destfile = basename(i),".tiff")
  
  download.file(i,q, mode = 'wb')
}







# Aruba Country admin boundaries 



setwd("C:\\Aruba Country Rainfall Data\\Download Rainfall Data")
raster_Images<-list.files(path = getwd(),pattern = ".tiff$")
lapply(raster_Images,raster)->raster_image_data

data.frame(raster_Images)->data_names
data_names$Year<-substr(data_names$raster_Images,7,10)
data_names$ID<-substr(data_names$raster_Images,12,13)


ID<-c(1,2,3,4,5,6,7,8,9,10,11,12)

Month<-c("January","February","March","April","May","June ","July","August","September","October","November","December")


Month_ID<-data.frame(ID,Month)

data_names$ID<-as.numeric(data_names$ID)

joined_tibble <- left_join(data_names, Month_ID, 
                           by = c("ID" = "ID"))
joined_tibble$Data<-paste0("Year",joined_tibble$Year,"_",joined_tibble$Month)


setwd("C:\\Aruba Country Rainfall Data\\Clip to Arab")

for (i in raster_image_data) {
  q<-paste0(destfile = names(i))
  
  masked <- mask(x = i, mask = Aruba_Country)  
  
  
  writeRaster(masked,q, bylayer=TRUE)
}



# stack list of rasters in to one raster image in R

Mask_Arub<-list.files(path = getwd(),pattern = ".tiff$")

lapply(Mask_Arub,raster)->Mask_Arub_image_data

data.frame(raster_Images)->data_names
data_names$Year<-substr(data_names$raster_Images,7,10)
data_names$ID<-substr(data_names$raster_Images,12,13)


ID<-c(1,2,3,4,5,6,7,8,9,10,11,12)

Month<-c("Jan","Feb","Mar","Apr","May","Jun ","Jul","Aug","Sep","Oct","Nov","Dece")


Month_ID<-data.frame(ID,Month)

data_names$ID<-as.numeric(data_names$ID)

joined_tibble <- left_join(data_names, Month_ID, 
                           by = c("ID" = "ID"))
joined_tibble$Data<-paste0(joined_tibble$Year,"_",joined_tibble$Month)

names(Mask_Arub_image_data)<-joined_tibble$Data

setwd("C:\\Aruba Country Rainfall Data\\Resample")
for (i in Mask_Arub_image_data) {
  q<-paste0(destfile = names(i),".tif")
  
  fg<-resample(i,Resample_reference,"ngb")
  writeRaster(fg,q,bylayer=TRUE)
}

raster_Images_maks_Resample<-list.files(path = getwd(),pattern = ".tif$")
lapply(raster_Images_maks_Resample,raster)->raster_image_data_Resample


band_stack<-stack(raster_image_data_Resample)






as.data.frame(band_stack,xy=TRUE)%>%na.omit()->frame   


points <- frame%>%st_as_sf(coords=c('x','y'), crs=4326)


poly <- point.in.poly(points, Arebcountries_Grid_Data)

# st_write(poly, "Arebcountries_Grid_Data.shp")





philly_sf_merged <- sp::merge(Arebcountries_Grid_Data,poly, by = c("x" = "x", "y" = "y"))
data.frame(names(philly_sf_merged)[1:5])->Data1
names(Data1)<-"Data"
data.frame(c("lat","long"))->Data2
names(Data2)<-"Data"

data.frame(joined_tibble$Data)->Data3
names(Data3)<-"Data"
bind_rows(Data1,Data3,Data2)->Result
names(philly_sf_merged)<-Result$Data


shapefile(philly_sf_merged, filename='C:/Aruba Country Rainfall Data/Resample/filed.shp',overwrite=TRUE)


