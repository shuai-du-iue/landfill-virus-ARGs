setwd("D:/01科研-城环所/01科研文章/垃圾填埋场-病毒/垃圾填埋场病毒ARGs/1composition/map")

library(maps);library(maptools);library(ggplot2);library(rgdal);library(sf)
library(grid);library(ggsn);library(forcats);library(ggpubr);library(plyr)

chinamap<-rgdal::readOGR("D:/01科研-城环所/01科研文章/垃圾填埋场-病毒/垃圾填埋场病毒ARGs/1composition/map/bou2_4p.shp")
plot(chinamap)
bianjiang<-rgdal::readOGR("D:/01科研-城环所/01科研文章/垃圾填埋场-病毒/垃圾填埋场病毒ARGs/1composition/map/bou2_4l.shp")
plot(bianjiang)
nine<-rgdal::readOGR("D:/01科研-城环所/01科研文章/垃圾填埋场-病毒/垃圾填埋场病毒ARGs/1composition/map/九段线.shp")
plot(nine)
nanhai<-rgdal::readOGR("D:/01科研-城环所/01科研文章/垃圾填埋场-病毒/垃圾填埋场病毒ARGs/1composition/map/南海诸岛及其它岛屿.shp")
plot(nanhai)

unique(chinamap@data$NAME)
#读取chinamap中的行政信息，并引入id信息，方便后面与chinamap1合并，-1为了与chinamap1一致同为从0开始
x<-chinamap@data
xs<-data.frame(x,id=seq(1:925)-1)
#将china_map转化为数据框，合并xs和china_map1 这两个数据框，joining by id
chinamap1<-fortify(chinamap)
chinamap_data<-plyr::join(chinamap1,xs,by="id",type = "full")
#引入南海九段线。结合南海九段线的经纬度，在边界信息中只选择九段线相关的数据。合并chinamap_data和chinamap2这两个数据框，joining by id
chinamap2<-fortify(bianjiang)
chinamap2<-subset(chinamap2,id==1087|id==1336|id==1377|id==1475|id==1481|id==1784|id==1769|id==1764|id==1508|id==1488)
chinamap_data<-plyr::join(chinamap_data,chinamap2,by="id",type = "full")

site<-read.csv("site.csv",row.names=1)

#################################################################################
createScaleBar <-
  function(lon,lat,distanceLon,distanceLat,distanceLegend, dist.units =
             "km"){
    # First rectangle
    bottomRight <- gcDestination(lon = lon, lat = lat, bearing = 90, dist = distanceLon, dist.units = dist.units, model = "WGS84")
    
    topLeft <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distanceLat, dist.units = dist.units, model = "WGS84")
    rectangle <- cbind(lon=c(lon, lon, bottomRight[1,"long"], bottomRight[1,"long"], lon),
                       lat = c(lat, topLeft[1,"lat"], topLeft[1,"lat"],lat, lat))
    rectangle <- data.frame(rectangle, stringsAsFactors = FALSE)
    
    # Second rectangle t right of the first rectangle
    bottomRight2 <- gcDestination(lon = lon, lat = lat, bearing = 90, dist = distanceLon*2, dist.units = dist.units, model = "WGS84")
    rectangle2 <- cbind(lon = c(bottomRight[1,"long"], bottomRight[1,"long"], bottomRight2[1,"long"], bottomRight2[1,"long"],
                                bottomRight[1,"long"]),
                        lat=c(lat, topLeft[1,"lat"], topLeft[1,"lat"], lat, lat))
    rectangle2 <- data.frame(rectangle2, stringsAsFactors = FALSE)
    
    # Now let's deal with the text
    onTop <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distanceLegend, dist.units = dist.units, model = "WGS84")
    onTop2 <- onTop3 <- onTop
    onTop2[1,"long"] <- bottomRight[1,"long"]
    onTop3[1,"long"] <- bottomRight2[1,"long"]
    
    legend <- rbind(onTop, onTop2, onTop3)
    legend <- data.frame(cbind(legend, text = c(0, distanceLon, distanceLon*2)), stringsAsFactors = FALSE, row.names = NULL)
    return(list(rectangle = rectangle, rectangle2 = rectangle2, legend = legend)) }
#########################################################################
createOrientationArrow <-
  function(scaleBar, length, distance = 1, dist.units = "km"){
    lon <- scaleBar$rectangle2[1,1]
    lat <- scaleBar$rectangle2[1,2]
    
    # Bottom point of the arrow
    begPoint <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distance, dist.units = dist.units, model = "WGS84")
    lon <- begPoint[1,"long"]
    lat <- begPoint[1,"lat"]
    
    # Let us create the endpoint
    onTop <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = length, dist.units = dist.units, model = "WGS84")
    
    leftArrow <- gcDestination(lon = onTop[1,"long"], lat = onTop[1,"lat"], bearing = 225, dist = length/5, dist.units =
                                 dist.units, model = "WGS84")
    
    rightArrow <- gcDestination(lon = onTop[1,"long"], lat = onTop[1,"lat"], bearing = 135, dist = length/5, dist.units =
                                  dist.units, model = "WGS84")
    
    res <- rbind(
      cbind(x = lon, y = lat, xend = onTop[1,"long"], yend = onTop[1,"lat"]),
      cbind(x = leftArrow[1,"long"], y = leftArrow[1,"lat"], xend = onTop[1,"long"], yend = onTop[1,"lat"]),
      cbind(x = rightArrow[1,"long"], y = rightArrow[1,"lat"], xend = onTop[1,"long"], yend = onTop[1,"lat"]))
    
    res <- as.data.frame(res, stringsAsFactors = FALSE)
    
    # Coordinates from which "N" will be plotted
    coordsN <- cbind(x = lon, y = (lat + onTop[1,"lat"])/2)
    
    return(list(res = res, coordsN = coordsN)) } 
###########################################################################
scaleBar <- function(lon, lat, distanceLon, distanceLat, distanceLegend,
                     dist.unit = "km", rec.fill = "white", rec.colour = "black", rec2.fill
                     = "black", rec2.colour = "black", legend.colour = "black", legend.size = 3, orientation = TRUE, arrow.length = 500, arrow.distance = 300, arrow.North.size = 6){
  laScaleBar <- createScaleBar(lon = lon, lat = lat, distanceLon = distanceLon, distanceLat = distanceLat, distanceLegend =
                                 distanceLegend, dist.unit = dist.unit)
  # First rectangle
  rectangle1 <- geom_polygon(data = laScaleBar$rectangle, aes(x = lon, y = lat), fill = rec.fill, colour = rec.colour)
  
  # Second rectangle
  rectangle2 <- geom_polygon(data = laScaleBar$rectangle2, aes(x = lon, y = lat), fill = rec2.fill, colour = rec2.colour)
  
  # Legend
  scaleBarLegend <- annotate("text", label = paste(laScaleBar$legend[,"text"], dist.unit, sep=""), x =
                               laScaleBar$legend[,"long"], y = laScaleBar$legend[,"lat"], size =
                               legend.size, colour = legend.colour)
  
  res <- list(rectangle1, rectangle2, scaleBarLegend)
  
  if(orientation){# Add an arrow pointing North
    coordsArrow <- createOrientationArrow(scaleBar = laScaleBar, length = arrow.length, distance = arrow.distance, dist.unit =
                                            dist.unit)
    arrow <- list(geom_segment(data = coordsArrow$res, aes(x = x, y = y, xend = xend, yend = yend)), annotate("text", label = "N", x =
                                                                                                                coordsArrow$coordsN[1,"x"], y = coordsArrow$coordsN[1,"y"], size =
                                                                                                                arrow.North.size, colour = "black"))
    res <- c(res, arrow)
  }
  return(res) }
#########################################################################
polygon<-geom_polygon(aes(group=group),fill="white",colour="black")
point<-geom_point(data=site,mapping=aes(x=Longitude,y=Latitude),col="cornflowerblue",shape=16,
                  size=2,alpha=0.8,show.legend=TRUE)
theme<-theme(panel.border=element_rect(fill='transparent',color='black',size=1),
             panel.background=element_rect(fill='white',color='black'),
             panel.grid=element_blank(),
             axis.text=element_text(size=10,face="bold",color='black'),
             axis.title=element_text(size=12,face="bold",color='black'),
             axis.ticks=element_line(size=1,color='black'),
             axis.ticks.length=unit(0.1,"cm"),
             legend.text=element_text(size=10,face="bold"),
             legend.title=element_text(size=12,face="bold"),
             legend.key=element_rect(fill='white'),
             legend.key.height=unit(0.5,"cm"),
             legend.position="right",
             legend.background=element_blank(),
             title=element_text(size=12,face="bold")
)
color<-scale_color_manual(values=c("mediumseagreen","cornflowerblue","orange"))
shape<-scale_shape_manual(values=c(16,17))
labs<-labs(x="Longitude",y="Latitude",title="")

p<-ggplot(chinamap_data,aes(x=long,y=lat))+polygon+point+theme+
  color+shape+labs+#guides(col=FALSE)+
  #coord_cartesian(ylim=c(23,53),xlim=c(105,135))+
  #scale_x_continuous(breaks=seq(105,135,10))+scale_y_continuous(breaks=seq(23,53,10))+
  scaleBar(lon=125,lat=23,distanceLon=500,distanceLat=100,distanceLegend=200,dist.unit="km") #8.5*11
p

ggarrange(p,
          ncol=1,nrow=1,
          align="hv",
          common.legend=TRUE,
          legend="right") #5.67*5.67

