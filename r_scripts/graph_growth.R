##Graph Growth

#1.- to call libraries
library(readxl)#to manage excel data
library(gdata)

#in png
png(bg="grey98",filename="Plots/graph Growth.png",width=1200,height=980,
    units = "px",res =100,pointsize=10,
    type=c("cairo"))

#in pdf
#pdf_file<-"Plots/graph Growth.pdf"
#cairo_pdf(bg="grey98",pdf_file,width=10,height =10)
par(mai=c(1.1,1.25,0.15,0), omi=c(1,0.5,1,0.5),mgp=c(4.5,1,0), family="Arial narrow", las=1)
#"Lato Light"
#2.- import data
selected_data <- read_excel("Tables/Outcome_silage_biogas_maize_ver_3.xlsx", sheet = 8)


attach(selected_data)
n<-nrow(selected_data)
grSM<-vector()
grSBIO<-vector()
for (i in 2:n){
  grSM[i]<-(Silage.Maize[i]-Silage.Maize[i-1])/Silage.Maize[i-1]
  grSBIO[i]<-(Likeliness.Biogas[i]-Likeliness.Biogas[i-1])/Likeliness.Biogas[i-1]          
}

selected_data$grSM<-grSM*100
selected_data$grSBIO<-grSBIO*100
selected_data<-selected_data[2:n,]

n<-nrow(selected_data)

t<-1:n
ts<-seq(1,n,by=1/10)
xs<-splinefun(t, selected_data$grSM)(ts)
ys<-splinefun(t, selected_data$grSBIO)(ts)

#Define Chart and other elements
plot(selected_data$grSM,selected_data$grSBIO, type="n", xlab="Growth rate silage maize (%)", ylab="Growth rate biogas silage maize (%)", cex.lab=2.5,
     axes=F)

axis(1,col=par("bg"),col.ticks = "grey81",lwd.ticks = 0.5,tck=-0.025,cex.axis=2)
axis(2,at=c(-10,0,10,20,30,40,50,60), col=par("bg"),col.ticks = "grey81",lwd.ticks = 0.5,tck=-0.025,cex.axis=2)
lines(xs,ys,lwd=7,col="grey")
for(i in 1:n)
{
symbols(selected_data$grSM[i],selected_data$grSBIO[i],bg="brown",fg="white",
        circles = 1,inches=0.28,add=T)  
text(selected_data$grSM[i],selected_data$grSBIO[i],selected_data$Years[i],col="white",cex=1.5)
}

#Titling
#mtext("Silage Maize Uses in Brandenburg",3,adj=0,line=1.5,cex=2.5,family="Lato Black",outer=T)
#mtext("Correlation of growth rates, 2009-2018",3,adj=0,line=0.25,cex=1.5,font=3,outer=T)
#mtext("Source:Amt für Statistik Brandenburg & Author",1,line=2,adj=1,cex=1.25,font=3,outer=T)
dev.off()


