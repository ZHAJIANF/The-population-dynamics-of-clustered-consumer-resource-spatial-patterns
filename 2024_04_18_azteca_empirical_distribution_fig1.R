
rm(list=ls())


setwd("/users/cx21atek/desktop/Desktop 1.0/Azteca_DSP_local/input data")
dir()
data <- read.csv("Azteca_census_data_Jan_2018.csv")
head(data)
library(viridis)
## test that looks at all of the individual nest scale distance correlation/regression coefficents.
### note that this is distinct from the origional test which pools together all of the age classes

## get a nest df of nests which are alive 
nests.df <- data[data$y16 > 0, ] 


head(data)
dev.new(height=5.3,width=4.2)
par(mfrow=c(1,1),mai=c(0.1,0.1,0.1,0.1))
plot(nests.df $y_coord, nests.df$x_coord,cex=0.1,xlab="",ylab="",xlim=c(600,0),ylim=c(-300,500),lty=0.5,pch=1,col="black",bty="n",yaxt="n",xaxt="n")
points(nests.df $y_coord, nests.df$x_coord,cex=0.6,col=turbo(max(nests.df$y16))[nests.df$y16],pch=19)

segments(90,-300,-10,-300)
segments(-10,-300,-10,-200)
text(40,-280,"100m")

#delinate the plot
segments(100,-305,100,0,lty=1,lwd=1)
segments(100,0,0,0,lty=1,lwd=1)
segments(0,0,0,501,lty=1,lwd=1)
segments(600,501,0,501,lty=1,lwd=1)
segments(600,-305,600,501,lty=1,lwd=1)
segments(600,-305,100,-305,lty=1,lwd=1)




#dev.new(height=5.3,width=4.2)
dev.new(height=4.3,width=3.2)

par(mfrow=c(1,1),mai=c(0.1,0.1,0.1,0.1))
plot(data $y_coord, data$x_coord,cex=0.03,xlab="",ylab="",xlim=c(600,0),ylim=c(-300,500),lty=0.5,pch=1,bty="n",yaxt="n",xaxt="n",col="grey")
points(nests.df $y_coord, nests.df$x_coord,cex=0.6,col=viridis(max(nests.df$y16+1),direction=-1)[nests.df$y16],pch=19,lwd=1)
segments(90,-300,-10,-300)
segments(-10,-300,-10,-200)
text(40,-280,"100m")

#delinate the plot
segments(100,-305,100,0,lty=1,lwd=1)
segments(100,0,0,0,lty=1,lwd=1)
segments(0,0,0,501,lty=1,lwd=1)
segments(600,501,0,501,lty=1,lwd=1)
segments(600,-305,600,501,lty=1,lwd=1)
segments(600,-305,100,-305,lty=1,lwd=1)

legend(69,-2,col=viridis(max(nests.df$y16),direction=-1),legend=seq(1,max(nests.df$y16),1),cex=0.4,pch=19,bty="n",title="nest age")





#dev.new(height=5.3,width=4.2)
dev.new(height=4.3,width=3.2)

par(mfrow=c(1,1),mai=c(0.1,0.1,0.1,0.1))
plot(data $y_coord, data$x_coord,cex=0.03,xlab="",ylab="",xlim=c(600,0),ylim=c(-300,500),lty=0.5,pch=1,bty="n",yaxt="n",xaxt="n",col="grey")
points(nests.df $y_coord, nests.df$x_coord,cex=0.6,col="black",pch=19,lwd=1)
segments(90,-300,-10,-300)
segments(-10,-300,-10,-200)
text(40,-280,"100m")

#delinate the plot
segments(100,-305,100,0,lty=1,lwd=1)
segments(100,0,0,0,lty=1,lwd=1)
segments(0,0,0,501,lty=1,lwd=1)
segments(600,501,0,501,lty=1,lwd=1)
segments(600,-305,600,501,lty=1,lwd=1)
segments(600,-305,100,-305,lty=1,lwd=1)




### Trying to plot in order to better see the older less abundant nests 

#dev.new(height=5.3,width=4.2)
dev.new(height=4.3,width=3.2)

par(mfrow=c(1,1),mai=c(0.1,0.1,0.1,0.1))
plot(data $y_coord, data$x_coord,cex=0.03,xlab="",ylab="",xlim=c(600,0),ylim=c(-300,500),lty=0.5,pch=1,bty="n",yaxt="n",xaxt="n",col="grey")
segments(90,-300,-10,-300)
segments(-10,-300,-10,-200)
text(40,-280,"100m")

#delinate the plot
segments(100,-305,100,0,lty=1,lwd=1)
segments(100,0,0,0,lty=1,lwd=1)
segments(0,0,0,501,lty=1,lwd=1)
segments(600,501,0,501,lty=1,lwd=1)
segments(600,-305,600,501,lty=1,lwd=1)
segments(600,-305,100,-305,lty=1,lwd=1)


for(xx in 1:max(nests.df$y16)){
	
	#xx <- 2
	single.yr.nests <- nests.df[nests.df$y16 == xx, ]
	points(single.yr.nests $y_coord, single.yr.nests $x_coord,cex=0.6,col=viridis(max(nests.df$y16+1),direction=-1)[single.yr.nests$y16],pch=19,lwd=1)

}




legend(69,-2,col=viridis(max(nests.df$y16),direction=-1),legend=seq(1,max(nests.df$y16),1),cex=0.4,pch=19,bty="n",title="nest age")






















#### Making a timeseries figure 
head(data)
dim(data)


dev.new(height=3,width=9)

## getting the columns that correspond to these years for a multi-panel figures 
#2007, 2010, 2013,2016
par(mfrow=c(1,4),mai=c(0.1,0.1,0.1,0.1))

for(yr.loop in c(17,20,23,26)){

#yr.loop <- 23
raw.nest.col <- data[, yr.loop]

nests.df <- data[raw.nest.col > 0, ]

plot(data $y_coord, data$x_coord,cex=0.05,xlab="",ylab="",xlim=c(600,0),ylim=c(-300,500),lty=0.5,pch=1,bty="n",yaxt="n",xaxt="n",col="grey")
points(nests.df $y_coord, nests.df$x_coord,cex=1,col=viridis(max(data$y16+2),direction=-1)[raw.nest.col[raw.nest.col > 0]],pch=1,lwd=1.5)

segments(90,-300,-10,-300)
segments(-10,-300,-10,-200)
text(40,-280,"100m")

#delinate the plot
segments(100,-305,100,0,lty=1,lwd=1)
segments(100,0,0,0,lty=1,lwd=1)
segments(0,0,0,501,lty=1,lwd=1)
segments(600,501,0,501,lty=1,lwd=1)
segments(600,-305,600,501,lty=1,lwd=1)
segments(600,-305,100,-305,lty=1,lwd=1)

}







dev.new(height=3,width=9)

## getting the columns that correspond to these years for a multi-panel figures 
#2007, 2010, 2013,2016
par(mfrow=c(1,4),mai=c(0.1,0.1,0.1,0.1))

for(yr.loop in c(16,17,18,19)){

#yr.loop <- 23
raw.nest.col <- data[, yr.loop]

nests.df <- data[raw.nest.col > 0, ]

plot(data $y_coord, data$x_coord,cex=0.05,xlab="",ylab="",xlim=c(600,0),ylim=c(-300,500),lty=0.5,pch=1,bty="n",yaxt="n",xaxt="n",col="grey")
points(nests.df $y_coord, nests.df$x_coord,cex=1,col=viridis(max(data$y16+2),direction=-1)[raw.nest.col[raw.nest.col > 0]],pch=1,lwd=1.5)

segments(90,-300,-10,-300)
segments(-10,-300,-10,-200)
text(40,-280,"100m")

#delinate the plot
segments(100,-305,100,0,lty=1,lwd=1)
segments(100,0,0,0,lty=1,lwd=1)
segments(0,0,0,501,lty=1,lwd=1)
segments(600,501,0,501,lty=1,lwd=1)
segments(600,-305,600,501,lty=1,lwd=1)
segments(600,-305,100,-305,lty=1,lwd=1)

}

