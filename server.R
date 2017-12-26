setwd("D:\\R\\PipeMap2\\")   # set the default directory (only work in PC)/ this code will not work in server

library(leaflet)
library(rgdal)
library(sp)
library(dplyr)
library(ggplot2)
# try issue1
# try issue3
# update calculate map function

TWD97TM2toWGS84 <- function (input_lat, input_lon){  
  # input_lat: TWD97橫座標, 南北緯度, latitude N
  # input_lon: TWD97縱座標, 東西經度, longitude E
  
  input_lat <- input_lat %>% as.character %>% as.numeric()
  input_lon <- input_lon %>% as.character %>% as.numeric()
  
  a = 6378137.0
  b = 6356752.314245
  lon0 = 121 * pi / 180
  k0 = 0.9999
  dx = 250000
  dy = 0
  e = (1 - b^2 / a^2)^0.5
  
  
  x =  input_lat - dx # input_lat: TWD97橫座標, 緯度, latitude
  y =  input_lon - dy # input_lon: TWD97縱座標, 經度, longitude
  
  M = y/k0
  
  mu = M/(a*(1.0 - ( e**2 )/4.0 - 3* (e**4)/64.0 - 5* (e**6)/256.0))
  e1 = (1.0 -  ((1.0 -  (e**2))**0.5)) / (1.0 +  ((1.0 -  (e**2))**0.5))
  
  J1 = (3*e1/2 - 27* (e1**3)/32.0)
  J2 = (21* (e1**2)/16 - 55* (e1**4)/32.0)
  J3 = (151* (e1**3)/96.0)
  J4 = (1097* (e1**4)/512.0)
  
  fp = mu + J1*sin(2*mu) + J2*sin(4*mu) + J3*sin(6*mu) + J4*sin(8*mu)
  
  e2 =  ((e*a/b)**2)
  C1 =  (e2*cos(fp)**2)
  T1 =  (tan(fp)**2)
  R1 = a*(1- (e**2))/ ((1- (e**2)* (sin(fp)**2))**(3.0/2.0))
  N1 = a/ ((1- (e**2)* (sin(fp)**2))**0.5)
  
  D = x/(N1*k0)
  
  #緯度計算 latitude
  Q1 = N1*tan(fp)/R1
  Q2 = ( (D**2)/2.0)
  Q3 = (5 + 3*T1 + 10*C1 - 4* (C1**2) - 9*e2)* (D**4)/24.0
  Q4 = (61 + 90*T1 + 298*C1 + 45* (T1**2) - 3* (C1**2) - 252*e2)* (D**6)/720.0
  lat = fp - Q1*(Q2 - Q3 + Q4)
  
  #經度計算 longitude
  Q5 = D
  Q6 = (1 + 2*T1 + C1)* (D**3)/6
  Q7 = (5 - 2*C1 + 28*T1 - 3* (C1**2) + 8*e2 + 24* (T1**2))* (D**5)/120.0
  lon = lon0 + (Q5 - Q6 + Q7)/cos(fp)
  
  
  lat = (lat*180) /pi #南北緯度  latitude 
  lon = (lon*180)/ pi #東西經度  longitude
  
  WGS = list(lat = lat, lon = lon)
  return(WGS)
}

shinyServer(function(input, output) {
  output$targetData <- renderLeaflet({
    shp_1860 <- readOGR(".",'ming',stringsAsFactors = F ,encoding = "UTF8")
    shp_1860_df <- fortify(shp_1860, region="GISJOIN")
    
    temp1 <- TWD97TM2toWGS84(shp_1860_df$long,shp_1860_df$lat)
    nshp_1860_df <- shp_1860_df
    
    nshp_1860_df$long = temp1$lon 
    
    nshp_1860_df$lat = temp1$lat
    
    shp_g <- levels(factor(nshp_1860_df$group))
    
    shp_g_q <- length(shp_g)
    
    nshp<-nrow(nshp_1860_df)
    
    m = leaflet() %>% addTiles()
    
    # g_i <- c()
    
    for(i in 1:shp_g_q){
      tarData <- nshp_1860_df[which(nshp_1860_df$group==shp_g[i]),]
      # reLong <- median(tarData$long)
      # reLat <- median(tarData$lat)
      # m <- m %>% addPolylines(tarData$long , tarData$lat , color = "red" , group = i) %>% addMarkers(reLong , reLat , label=shp_g[i]) 
      # g_i <- c(g_i,i)
      m <- m %>% addPolylines(tarData$long , tarData$lat , color = "red" , group = "Pipe Map")
    }
    
    max_long <- max(temp1$lon)
    min_long <- min(temp1$lon)
    max_lat <- max(temp1$lat)
    min_lat <- min(temp1$lat)
    
    lat2 <- sum(max_lat,min_lat)/2
    print(paste(max_lat,min_lat,lat2))
    lat1 <- sum(max_lat,lat2)/2
    lat3 <- sum(min_lat,lat2)/2
    
    m <- m %>% addRectangles(max_long , lat1 , min_long ,max_lat, color = "red" , group = "area Map 1")
    m <- m %>% addRectangles(max_long , lat2 , min_long ,lat1, color = "red" , group = "area Map 2")
    m <- m %>% addRectangles(max_long , lat3 , min_long ,lat2, color = "red" , group = "area Map 3")
    m <- m %>% addRectangles(max_long , min_lat , min_long ,lat3, color = "red" , group = "area Map 4")
    
    t1<-0
    t2<-0
    t3<-0
    t4<-0
    for(i in 1:shp_g_q){
      tarData <- nshp_1860_df[which(nshp_1860_df$group==shp_g[i]),]
      reLong <- median(tarData$long)
      reLat <- median(tarData$lat)
      if(reLong <= max_long && reLong>= min_long){
        if(reLat<=max_lat && reLat >= lat1){
          m <- m %>% addMarkers(reLong , reLat , label=shp_g[i] , group = "point 1" ,labelOptions=labelOptions(textsize="25px") , popup=shp_1860@data$ROAD[i])
          t1<-t1+1
        }
      }
    }
    
    for(i in 1:shp_g_q){
      tarData <- nshp_1860_df[which(nshp_1860_df$group==shp_g[i]),]
      reLong <- median(tarData$long)
      reLat <- median(tarData$lat)
      if(reLong <= max_long && reLong>= min_long){
        if(reLat<=lat1 && reLat >= lat2){
          m <- m %>% addMarkers(reLong , reLat , label=shp_g[i] , group = "point 2" ,labelOptions=labelOptions(textsize="25px") , popup=shp_1860@data$ROAD[i])
          t2<-t2+1
        }
      }
    }
    
    for(i in 1:shp_g_q){
      tarData <- nshp_1860_df[which(nshp_1860_df$group==shp_g[i]),]
      reLong <- median(tarData$long)
      reLat <- median(tarData$lat)
      if(reLong <= max_long && reLong>= min_long){
        if(reLat<=lat2 && reLat >= lat3){
          m <- m %>% addMarkers(reLong , reLat , label=shp_g[i] , group = "point 3" ,labelOptions=labelOptions(textsize="25px") , popup=shp_1860@data$ROAD[i])
          t3<-t3+1
        }
      }
    }
    
    for(i in 1:shp_g_q){
      tarData <- nshp_1860_df[which(nshp_1860_df$group==shp_g[i]),]
      reLong <- median(tarData$long)
      reLat <- median(tarData$lat)
      if(reLong <= max_long && reLong>= min_long){
        if(reLat<=lat3 && reLat >= min_lat){
          m <- m %>% addMarkers(reLong , reLat , label=shp_g[i] , group = "point 4" ,labelOptions=labelOptions(textsize="25px") , popup=shp_1860@data$ROAD[i])
          t4<-t4+1
        }
      }
    }
    print(paste("t1 : ",t1,"| t2 : ",t2,"| t3 : ",t3,"| t4 : ",t4," | ",t1+t2+t3+t4))    
    
    
    
    # for(i in 1:shp_g_q){
    #   tarData <- nshp_1860_df[which(nshp_1860_df$group==shp_g[i]),]
    #   # reLong <- median(tarData$long)
    #   # reLat <- median(tarData$lat)
    #   # m <- m %>% addPolylines(tarData$long , tarData$lat , color = "red" , group = i) %>% addMarkers(reLong , reLat , label=shp_g[i]) 
    #   # g_i <- c(g_i,i)
    #   
    #   
    #   m <- m %>% addPolygons(tarData$long , tarData$lat , color = "red" , group = "area Map")
    # }
    
    # 有關label的部份，應可直接取用中文路名，但目前問題為1．資料缺漏；2．編碼方式需研究
    m <- m %>% addCircleMarkers(121.8686, 24.57625,label="南安國中_監測站",labelOptions=labelOptions(noHide=T,textsize="25px")) %>% 
      addLayersControl(overlayGroups = c("Pipe Map","point 1","point 2","point 3","point 4","area Map 1","area Map 2","area Map 3","area Map 4"))
    
    # m <- m %>% addMarkers(121.8686, 24.57625, label="南安國中_監測站",labelOptions=labelOptions(noHide=T,textsize="25px")) %>% addLayersControl(overlayGroups = as.character(g_i),
    #                                                                                                                                     options = layersControlOptions(collapsed = FALSE))
    
    m
    
  })
  
})