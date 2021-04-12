library(tmap)
library(tmaptools)
library(grid)
load("rda/reclas_raster.rda")
years<-list("2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018")

#1.- testing data. 
#Arranging plots.
d2008<-tm_shape(feed.inre[[1]])+tm_raster(col="layer",title="values",
                                              palette = "Spectral",style="pretty")+
  tm_layout(frame=T, title='2008')

d2009<-tm_shape(feed.inre[[2]])+tm_raster(col="layer",title="values",
                                          palette = "Spectral",style="pretty")+
  tm_layout(frame=T,title='2009')
d2010<-tm_shape(feed.inre[[3]])+tm_raster(col="layer",title="values",
                                          palette = "Spectral",style="pretty")+
  tm_layout(frame=T, title='2010')
d2011<-tm_shape(feed.inre[[4]])+tm_raster(col="layer",title="values",
                                          palette = "Spectral",style="pretty")+
  tm_layout(frame=T)
d2012<-tm_shape(feed.inre[[5]])+tm_raster(col="layer",title="values",
                                          palette = "Spectral",style="pretty")+
  tm_layout(frame=T,title='2012')
d2013<-tm_shape(feed.inre[[6]])+tm_raster(col="layer",title="values",
                                          palette = "Spectral",style="pretty")+
  tm_layout(frame=T,title='2013')
d2014<-tm_shape(feed.inre[[7]])+tm_raster(col="layer",title="values",
                                          palette = "Spectral",style="pretty")+
  tm_layout(frame=T, title='2014')
d2015<-tm_shape(feed.inre[[8]])+tm_raster(col="layer",title="values",
                                          palette = "Spectral",style="pretty")+
  tm_layout(frame=T, title='2015')
d2018<-tm_shape(feed.inre[[11]])+tm_raster(col="layer",title="values",
                                             palette = "Spectral",style="pretty")+
  tm_layout(frame=T, title='2018')

tmap_options(limits = c(facets.view = 9))
tmap_arrange(d2008,d2009,d2010,d2011,d2012,d2013,d2014,d2015,d2018, ncol=3,nrow=3)
tmap_arrange(d2008,d2018) 


#2.- plotting
setwd("Plots")
#in png 
for(i in 1:length(feed.inre)){
  sphr<-feed.inre[[i]]
  png(bg="grey98",filename=paste("CA_",years[[i]],".png",sep=""),width=480,height=480,units = "px",
      type=c("cairo"))
  par(omi=c(1,0.25,1,0.25), mai=c(0,1.25,1,1), family="Lato Light", las=1)
  #making the map
  Map<-tm_shape(sphr)+
    tm_raster(col="layer",title="values",
              palette = "Spectral",style="pretty")+
    tm_layout(frame=T,title=paste0("Catchment Area"," ",years[[i]]),
              legend.show = T,legend.position = c("left","bottom"))
  #print insets
  print(Map, vp=viewport(x=0.485,y=0.500,width = 0.93,height=1.0))
  #closing file
  dev.off()
}
#in pdf 
for(i in 1:length(feed.inre)){
  sphr<-feed.inre[[i]]
  cairo_pdf(bg="grey98",paste("CA_",years[[i]],".pdf",sep=""),width=6.328,height=5.625)
  par(omi=c(1,0.25,1,0.25), mai=c(0,1.25,1,1), family="Lato Light", las=1)
  #making the map
  Map<-tm_shape(sphr)+
    tm_raster(col="layer",title="values",
              palette = "Spectral",style="pretty")+
    tm_layout(frame=T,title=paste0("Catchment Area"," ",years[[i]]),
              legend.show = T,legend.position = c("left","bottom"))
  #print insets
  print(Map, vp=viewport(x=0.485,y=0.500,width = 0.93,height=1.0))
  #closing file
  dev.off()
}
#manual
tm_shape(reclas_raster[[1]])+
  tm_raster(col="layer",title="values",palette = "Spectral",style="pretty")+
  tm_layout(frame=T,title=paste0("Catchment Area"," ",years[[1]]),
            legend.title.fontfamily="Lato",legend.text.color="Black",
            legend.show = T,
            legend.position = c("left","bottom"))

