########################################################################################### DSP Test that pools all age classes together for the statistic ###########################################################################################rm(list=ls())

setwd("/users/cx21atek/desktop/Azteca_DSP_local/output data")

RANDOMIZED.DSP.summary.df  <- read.csv("pooled_DSP_randomization.csv")
RANDOMIZED.DSP.summary.df2 <- RANDOMIZED.DSP.summary.df[RANDOMIZED.DSP.summary.df$radius != 0, ]
DSP.summary.df  <- read.csv("pooled_DSP.csv")



par(mfrow=c(1,3))

estimate.bootstrap.pval <- numeric(length(unique(DSP.summary.df$radius)))
Rsquare.bootstrap.pval  <- numeric(length(unique(DSP.summary.df$radius)))
pval.bootstrap.pval <- numeric(length(unique(DSP.summary.df$radius)))
dat.indexer <- 1
for(rad.looper in min(DSP.summary.df$radius):max(DSP.summary.df$radius)){ 
a.radii <- rad.looper
real.dsp.df <- DSP.summary.df[DSP.summary.df$radius == a.radii,]
rand.dsp.df <- RANDOMIZED.DSP.summary.df2[RANDOMIZED.DSP.summary.df2$radius == a.radii,]


## prob of finding a more negative DSP trend?
hist(rand.dsp.df $L.DSP.LE.estimates,breaks=100)
abline(v= real.dsp.df $L.DSP.LE.estimates)
estimate.pval <- sum(rand.dsp.df $L.DSP.LE.estimates <= real.dsp.df $L.DSP.LE.estimates)/length(rand.dsp.df $L.DSP.LE.estimates)

## prob of finding a bigger rsquared?
hist(rand.dsp.df $L.DSP.LE.rsquareds,breaks=100)
abline(v= real.dsp.df $L.DSP.LE.rsquareds)
rsquared.pval <- sum(rand.dsp.df $L.DSP.LE.rsquareds >= real.dsp.df $L.DSP.LE.rsquareds)/length(rand.dsp.df $L.DSP.LE.rsquareds)

## prob of finding a lower p value??
hist(rand.dsp.df $L.DSP.LE.pvals,breaks=100)
abline(v= real.dsp.df $L.DSP.LE.pvals)
pvals.pval <- sum(rand.dsp.df $L.DSP.LE.pvals <= real.dsp.df $L.DSP.LE.pvals)/length(rand.dsp.df $L.DSP.LE.pvals)


estimate.bootstrap.pval[dat.indexer] <- estimate.pval
Rsquare.bootstrap.pval[dat.indexer] <- rsquared.pval
pval.bootstrap.pval[dat.indexer] <- pvals.pval
dat.indexer <- dat.indexer + 1
}


plot(estimate.bootstrap.pval)
abline(h=0.05)
plot(Rsquare.bootstrap.pval)
abline(h=0.05)
plot(pval.bootstrap.pval)
abline(h=0.05)
bs.rsquared.sig.pvals <- radius.vals[Rsquare.bootstrap.pval <= 0.01]
bs.pval.sig.pvals <- radius.vals[pval.bootstrap.pval <= 0.01]
bs.estimate.sig.pvals <- radius.vals[estimate.bootstrap.pval <= 0.01]

points(radius.vals, Rsquare.bootstrap.pval)




dev.new(height=3.2,width=9)
par(mfrow=c(1,3),mai=c(0.6,0.6,0.3,0.1))
label.cex <- 0.8
plot(DSP.summary.df$radius, DSP.summary.df$L.DSP.LE.estimates,ylim=c(-0.06,0.04),xlab="",ylab="",yaxt="n")
axis(2,las=2)
abline(h=0.0,lty=3)
points(DSP.summary.df$radius, DSP.summary.df$L.DSP.LE.estimates,pch=19)
points(DSP.summary.df$radius, DSP.summary.df$L.DSP.LE.estimates,pch=19,type="l")
means <- aggregate(RANDOMIZED.DSP.summary.df2 $L.DSP.LE.estimates,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=mean)
points(means[,1],means[,2],col="red",pch=19)
points(means[,1],means[,2],col="red",type="l")
stdevs <- aggregate(RANDOMIZED.DSP.summary.df2 $L.DSP.LE.estimates,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=sd)
arrows(means[,1],means[,2]+ stdevs[,2],means[,1],means[,2]- stdevs[,2],length=0.0,angle=90,code=3,col="red")
mtext("spatial scale(m)",side=1,line=2.3,cex= label.cex)
mtext("DSP estimate",side=2,line=3,cex= label.cex)
#mtext("Linear DSP (linear pooled)",line=0.5,cex= label.cex)
points(bs.estimate.sig.pvals,rep(0.04,length(bs.estimate.sig.pvals)),pch="*",cex=1)
legend(35,0.035,legend=c("empirical","randomized"),col=c("black","red"),pch=19,bty="n")


plot(DSP.summary.df$radius, DSP.summary.df$L.DSP.LE.rsquareds,ylim=c(0,1),xlab="",ylab="",yaxt="n")
axis(2,las=2)
abline(h=0.0,lty=3)
means <- aggregate(RANDOMIZED.DSP.summary.df2 $L.DSP.LE.rsquareds,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=mean)
points(means[,1],means[,2],col="red",pch=19)
points(means[,1],means[,2],col="red",type="l")
stdevs <- aggregate(RANDOMIZED.DSP.summary.df2 $L.DSP.LE.rsquareds,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=sd)
arrows(means[,1],means[,2]+ stdevs[,2],means[,1],means[,2]- stdevs[,2],length=0.0,angle=90,code=3,col="red")
points(DSP.summary.df$radius, DSP.summary.df$L.DSP.LE.rsquareds,pch=19)
points(DSP.summary.df$radius, DSP.summary.df$L.DSP.LE.rsquareds,pch=19,type="l")
mtext("spatial scale(m)",side=1,line=2.3,cex= label.cex)
mtext("DSP R-squared",side=2,line=3,cex= label.cex)
points(bs.rsquared.sig.pvals,rep(1,length(bs.rsquared.sig.pvals)),pch="*",cex=1)


plot(DSP.summary.df$radius, DSP.summary.df$L.DSP.LE.pvals,ylim=c(0,1),xlab="",ylab="",yaxt="n")
axis(2,las=2)
abline(h=0.05,lty=3)
means <- aggregate(RANDOMIZED.DSP.summary.df2 $L.DSP.LE.pvals,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=mean)
points(means[,1],means[,2],col="red",pch=19)
points(means[,1],means[,2],col="red",type="l")
stdevs <- aggregate(RANDOMIZED.DSP.summary.df2 $L.DSP.LE.pvals,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=sd)
arrows(means[,1],means[,2]+ stdevs[,2],means[,1],means[,2]- stdevs[,2],length=0.0,angle=90,code=3,col="red")
points(DSP.summary.df$radius, DSP.summary.df$L.DSP.LE.pvals,pch=19)
points(DSP.summary.df$radius, DSP.summary.df$L.DSP.LE.pvals,pch=19,type="l")
mtext("spatial scale(m)",side=1,line=2.3,cex= label.cex)
mtext("DSP p-value",side=2,line=3,cex= label.cex)
points(bs.pval.sig.pvals,rep(1,length(bs.rsquared.sig.pvals)),pch="*",cex=1)



### final dsp statistics output figure 


dev.new(height=3.5,width=9)
par(mfrow=c(1,3),mai=c(0.6,0.6,0.3,0.1))
label.cex <- 0.8
plot(DSP.summary.df$radius, DSP.summary.df$L.DSP.LE.estimates,ylim=c(-0.06,0.04),xlab="",ylab="",yaxt="n")
axis(2,las=2)
abline(h=0.0,lty=3)
points(DSP.summary.df$radius, DSP.summary.df$L.DSP.LE.estimates,pch=19)
points(DSP.summary.df$radius, DSP.summary.df$L.DSP.LE.estimates,pch=19,type="l")
means <- aggregate(RANDOMIZED.DSP.summary.df2 $L.DSP.LE.estimates,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=mean)
points(means[,1],means[,2],col="red",pch=19)
points(means[,1],means[,2],col="red",type="l")
stdevs <- aggregate(RANDOMIZED.DSP.summary.df2 $L.DSP.LE.estimates,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=sd)
arrows(means[,1],means[,2]+ stdevs[,2],means[,1],means[,2]- stdevs[,2],length=0.0,angle=90,code=3,col="red")
mtext("spatial scale(m)",side=1,line=2.3,cex= label.cex)
mtext("DSP estimate",side=2,line=3,cex= label.cex)
#mtext("Linear DSP (linear pooled)",line=0.5,cex= label.cex)

plot(DSP.summary.df$radius, DSP.summary.df$L.DSP.LE.rsquareds,ylim=c(0,1),xlab="",ylab="",yaxt="n")
axis(2,las=2)
abline(h=0.0,lty=3)
means <- aggregate(RANDOMIZED.DSP.summary.df2 $L.DSP.LE.rsquareds,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=mean)
points(means[,1],means[,2],col="red",pch=19)
points(means[,1],means[,2],col="red",type="l")
stdevs <- aggregate(RANDOMIZED.DSP.summary.df2 $L.DSP.LE.rsquareds,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=sd)
arrows(means[,1],means[,2]+ stdevs[,2],means[,1],means[,2]- stdevs[,2],length=0.0,angle=90,code=3,col="red")
points(DSP.summary.df$radius, DSP.summary.df$L.DSP.LE.rsquareds,pch=19)
points(DSP.summary.df$radius, DSP.summary.df$L.DSP.LE.rsquareds,pch=19,type="l")
mtext("spatial scale(m)",side=1,line=2.3,cex= label.cex)
mtext("DSP R-squared",side=2,line=3,cex= label.cex)

plot(DSP.summary.df$radius, DSP.summary.df$L.DSP.LE.pvals,ylim=c(0,1),xlab="",ylab="",yaxt="n")
axis(2,las=2)
abline(h=0.05,lty=3)
means <- aggregate(RANDOMIZED.DSP.summary.df2 $L.DSP.LE.pvals,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=mean)
points(means[,1],means[,2],col="red",pch=19)
points(means[,1],means[,2],col="red",type="l")
stdevs <- aggregate(RANDOMIZED.DSP.summary.df2 $L.DSP.LE.pvals,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=sd)
arrows(means[,1],means[,2]+ stdevs[,2],means[,1],means[,2]- stdevs[,2],length=0.0,angle=90,code=3,col="red")
points(DSP.summary.df$radius, DSP.summary.df$L.DSP.LE.pvals,pch=19)
points(DSP.summary.df$radius, DSP.summary.df$L.DSP.LE.pvals,pch=19,type="l")
mtext("spatial scale(m)",side=1,line=2.3,cex= label.cex)
mtext("DSP p-value",side=2,line=3,cex= label.cex)





### Grouped figures below for supplamentary material  

par(mfcol=c(3,2),mai=c(0.6,0.6,0.1,0.1),oma=c(0,0,2,1))
plot(DSP.summary.df$radius, DSP.summary.df$L.DSP.LE.estimates,ylim=c(-0.06,0.04),xlab="",ylab="",yaxt="n")
axis(2,las=2)
abline(h=0.0,lty=3)
points(DSP.summary.df$radius, DSP.summary.df$L.DSP.LE.estimates,pch=19)
points(DSP.summary.df$radius, DSP.summary.df$L.DSP.LE.estimates,pch=19,type="l")
means <- aggregate(RANDOMIZED.DSP.summary.df2 $L.DSP.LE.estimates,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=mean)
points(means[,1],means[,2],col="red",pch=19)
points(means[,1],means[,2],col="red",type="l")
stdevs <- aggregate(RANDOMIZED.DSP.summary.df2 $L.DSP.LE.estimates,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=sd)
arrows(means[,1],means[,2]+ stdevs[,2],means[,1],means[,2]- stdevs[,2],length=0.0,angle=90,code=3,col="red")
mtext("spatial scale(m)",side=1,line=2.3,cex= label.cex)
mtext("DSP estimate",side=2,line=3,cex= label.cex)
mtext("Linear DSP (linear pooled)",line=0.5,cex= label.cex)

plot(DSP.summary.df$radius, DSP.summary.df$L.DSP.LE.rsquareds,ylim=c(0,1),xlab="",ylab="",yaxt="n")
axis(2,las=2)
abline(h=0.0,lty=3)
means <- aggregate(RANDOMIZED.DSP.summary.df2 $L.DSP.LE.rsquareds,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=mean)
points(means[,1],means[,2],col="red",pch=19)
points(means[,1],means[,2],col="red",type="l")
stdevs <- aggregate(RANDOMIZED.DSP.summary.df2 $L.DSP.LE.rsquareds,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=sd)
arrows(means[,1],means[,2]+ stdevs[,2],means[,1],means[,2]- stdevs[,2],length=0.0,angle=90,code=3,col="red")
points(DSP.summary.df$radius, DSP.summary.df$L.DSP.LE.rsquareds,pch=19)
points(DSP.summary.df$radius, DSP.summary.df$L.DSP.LE.rsquareds,pch=19,type="l")
mtext("spatial scale(m)",side=1,line=2.3,cex= label.cex)
mtext("DSP R-squared",side=2,line=3,cex= label.cex)

plot(DSP.summary.df$radius, DSP.summary.df$L.DSP.LE.pvals,ylim=c(0,1),xlab="",ylab="",yaxt="n")
axis(2,las=2)
abline(h=0.05,lty=3)
means <- aggregate(RANDOMIZED.DSP.summary.df2 $L.DSP.LE.pvals,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=mean)
points(means[,1],means[,2],col="red",pch=19)
points(means[,1],means[,2],col="red",type="l")
stdevs <- aggregate(RANDOMIZED.DSP.summary.df2 $L.DSP.LE.pvals,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=sd)
arrows(means[,1],means[,2]+ stdevs[,2],means[,1],means[,2]- stdevs[,2],length=0.0,angle=90,code=3,col="red")
points(DSP.summary.df$radius, DSP.summary.df$L.DSP.LE.pvals,pch=19)
points(DSP.summary.df$radius, DSP.summary.df$L.DSP.LE.pvals,pch=19,type="l")
mtext("spatial scale(m)",side=1,line=2.3,cex= label.cex)
mtext("DSP p-value",side=2,line=3,cex= label.cex)

##################################################
#### Linear DSP with spearman 
head(DSP.summary.df)
#par(mfrow=c(1,3),mai=c(0.6,0.6,0.1,0.1),oma=c(0,0,0,0))
plot(DSP.summary.df$radius, DSP.summary.df$L.DSP.SE.estimates,ylim=c(-0.06,0.04),xlab="",ylab="",yaxt="n")
axis(2,las=2)
abline(h=0.0,lty=3)
points(DSP.summary.df$radius, DSP.summary.df$L.DSP.SE.estimates,pch=19)
points(DSP.summary.df$radius, DSP.summary.df$L.DSP.SE.estimates,pch=19,type="l")
means <- aggregate(RANDOMIZED.DSP.summary.df2 $L.DSP.SE.estimates,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=mean)
points(means[,1],means[,2],col="red",pch=19)
points(means[,1],means[,2],col="red",type="l")
stdevs <- aggregate(RANDOMIZED.DSP.summary.df2 $L.DSP.SE.estimates,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=sd)
arrows(means[,1],means[,2]+ stdevs[,2],means[,1],means[,2]- stdevs[,2],length=0.0,angle=90,code=3,col="red")
#points(RANDOMIZED.DSP.summary.df$radius, RANDOMIZED.DSP.summary.df$L.DSP.LE.estimates,col="red")
mtext("spatial scale(m)",side=1,line=2.3,cex= label.cex)
mtext("DSP estimate",side=2,line=3,cex= label.cex)
mtext("Linear DSP (spearman pooled)",line=0.5,cex= label.cex)

plot(DSP.summary.df$radius, DSP.summary.df$L.DSP.SE.rsquareds,ylim=c(0,1),xlab="",ylab="",yaxt="n")
axis(2,las=2)
abline(h=0.0,lty=3)
means <- aggregate(RANDOMIZED.DSP.summary.df2 $L.DSP.SE.rsquareds,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=mean)
points(means[,1],means[,2],col="red",pch=19)
points(means[,1],means[,2],col="red",type="l")
stdevs <- aggregate(RANDOMIZED.DSP.summary.df2 $L.DSP.SE.rsquareds,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=sd)
arrows(means[,1],means[,2]+ stdevs[,2],means[,1],means[,2]- stdevs[,2],length=0.0,angle=90,code=3,col="red")
#points(RANDOMIZED.DSP.summary.df$radius, RANDOMIZED.DSP.summary.df$L.DSP.LE.rsquareds,col="red")
points(DSP.summary.df$radius, DSP.summary.df$L.DSP.SE.rsquareds,pch=19)
points(DSP.summary.df$radius, DSP.summary.df$L.DSP.SE.rsquareds,pch=19,type="l")
mtext("spatial scale(m)",side=1,line=2.3,cex= label.cex)
mtext("DSP R-squared",side=2,line=3,cex= label.cex)

plot(DSP.summary.df$radius, DSP.summary.df$L.DSP.SE.pvals,ylim=c(0,1),xlab="",ylab="",yaxt="n")
axis(2,las=2)
abline(h=0.05,lty=3)
means <- aggregate(RANDOMIZED.DSP.summary.df2 $L.DSP.SE.pvals,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=mean)
points(means[,1],means[,2],col="red",pch=19)
points(means[,1],means[,2],col="red",type="l")
stdevs <- aggregate(RANDOMIZED.DSP.summary.df2 $L.DSP.SE.pvals,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=sd)
arrows(means[,1],means[,2]+ stdevs[,2],means[,1],means[,2]- stdevs[,2],length=0.0,angle=90,code=3,col="red")
#points(RANDOMIZED.DSP.summary.df$radius, RANDOMIZED.DSP.summary.df$L.DSP.LE.pvals,col="red")
points(DSP.summary.df$radius, DSP.summary.df$L.DSP.SE.pvals,pch=19)
points(DSP.summary.df$radius, DSP.summary.df$L.DSP.SE.pvals,pch=19,type="l")
mtext("spatial scale(m)",side=1,line=2.3,cex= label.cex)
mtext("DSP p-value",side=2,line=3,cex= label.cex)


##########################################
############# Now spearman DSP ########### 
##########################################



#### Linear DSP with spearman 
head(DSP.summary.df)
par(mfcol=c(2,2))
plot(DSP.summary.df$radius, DSP.summary.df$S.DSP.LE.estimates,ylim=c(-1,1),xlab="",ylab="",yaxt="n")
axis(2,las=2)
abline(h=0.0,lty=3)
points(DSP.summary.df$radius, DSP.summary.df$S.DSP.LE.estimates,pch=19)
points(DSP.summary.df$radius, DSP.summary.df$S.DSP.LE.estimates,pch=19,type="l")
means <- aggregate(RANDOMIZED.DSP.summary.df2 $S.DSP.LE.estimates,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=mean)
points(means[,1],means[,2],col="red",pch=19)
points(means[,1],means[,2],col="red",type="l")
stdevs <- aggregate(RANDOMIZED.DSP.summary.df2 $S.DSP.LE.estimates,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=sd)
arrows(means[,1],means[,2]+ stdevs[,2],means[,1],means[,2]- stdevs[,2],length=0.0,angle=90,code=3,col="red")
mtext("Spear DSP (linear non-pooled)",line=0.5,cex= label.cex)
mtext("spatial scale(m)",side=1,line=2.3,cex= label.cex)
mtext("DSP coeff",side=2,line=2.2,cex= label.cex)


plot(DSP.summary.df$radius, DSP.summary.df$S.DSP.LE.pvals,ylim=c(0,1),xlab="",ylab="",yaxt="n")
axis(2,las=2)
abline(h=0.05,lty=3)
means <- aggregate(RANDOMIZED.DSP.summary.df2 $S.DSP.LE.pvals,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=mean)
points(means[,1],means[,2],col="red",pch=19)
points(means[,1],means[,2],col="red",type="l")
stdevs <- aggregate(RANDOMIZED.DSP.summary.df2 $S.DSP.LE.pvals,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=sd)
arrows(means[,1],means[,2]+ stdevs[,2],means[,1],means[,2]- stdevs[,2],length=0.0,angle=90,code=3,col="red")
points(DSP.summary.df$radius, DSP.summary.df$S.DSP.LE.pvals,pch=19)
points(DSP.summary.df$radius, DSP.summary.df$S.DSP.LE.pvals,pch=19,type="l")
mtext("spatial scale(m)",side=1,line=2.3,cex= label.cex)
mtext("DSP p-value",side=2,line=2.2,cex= label.cex)


plot(DSP.summary.df$radius, DSP.summary.df$S.DSP.SE.estimates,ylim=c(-1,1),xlab="",ylab="",yaxt="n")
axis(2,las=2)
abline(h=0.0,lty=3)
points(DSP.summary.df$radius, DSP.summary.df$S.DSP.SE.estimates,pch=19)
points(DSP.summary.df$radius, DSP.summary.df$S.DSP.SE.estimates,pch=19,type="l")
means <- aggregate(RANDOMIZED.DSP.summary.df2 $S.DSP.SE.estimates,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=mean)
points(means[,1],means[,2],col="red",pch=19)
points(means[,1],means[,2],col="red",type="l")
stdevs <- aggregate(RANDOMIZED.DSP.summary.df2 $S.DSP.SE.estimates,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=sd)
arrows(means[,1],means[,2]+ stdevs[,2],means[,1],means[,2]- stdevs[,2],length=0.0,angle=90,code=3,col="red")
mtext("Spear DSP (spear non-pooled)",line=0.5,cex= label.cex)
mtext("spatial scale(m)",side=1,line=2.3,cex= label.cex)
mtext("DSP coeff",side=2,line=2.2,cex= label.cex)


plot(DSP.summary.df$radius, DSP.summary.df$S.DSP.SE.pvals,ylim=c(0,1),xlab="",ylab="",yaxt="n")
axis(2,las=2)
abline(h=0.05,lty=3)
means <- aggregate(RANDOMIZED.DSP.summary.df2 $S.DSP.SE.pvals,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=mean)
points(means[,1],means[,2],col="red",pch=19)
points(means[,1],means[,2],col="red",type="l")
stdevs <- aggregate(RANDOMIZED.DSP.summary.df2 $S.DSP.SE.pvals,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=sd)
arrows(means[,1],means[,2]+ stdevs[,2],means[,1],means[,2]- stdevs[,2],length=0.0,angle=90,code=3,col="red")
points(DSP.summary.df$radius, DSP.summary.df$S.DSP.SE.pvals,pch=19)
points(DSP.summary.df$radius, DSP.summary.df$S.DSP.SE.pvals,pch=19,type="l")
mtext("spatial scale(m)",side=1,line=2.3,cex= label.cex)
mtext("DSP p-value",side=2,line=2.2,cex= label.cex)




































































############### OLDER FIGURES ##### updated Nov 6 2023
par(mfrow=c(1,3))
plot(DSP.summary.df$radius, DSP.summary.df$L.DSP.LE.estimates,ylim=c(-0.05,0.05))
abline(h=0.0,lty=3)
points(DSP.summary.df$radius, DSP.summary.df$L.DSP.LE.estimates,pch=19)
points(DSP.summary.df$radius, DSP.summary.df$L.DSP.LE.estimates,pch=19,type="l")
means <- aggregate(RANDOMIZED.DSP.summary.df2 $L.DSP.LE.estimates,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=mean)
points(means[,1],means[,2],col="red",pch=19)
points(means[,1],means[,2],col="red",type="l")
stdevs <- aggregate(RANDOMIZED.DSP.summary.df2 $L.DSP.LE.estimates,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=sd)
arrows(means[,1],means[,2]+ stdevs[,2],means[,1],means[,2]- stdevs[,2],length=0.0,angle=90,code=3,col="red")


#points(RANDOMIZED.DSP.summary.df$radius, RANDOMIZED.DSP.summary.df$L.DSP.LE.estimates,col="red")


plot(DSP.summary.df$radius, DSP.summary.df$L.DSP.LE.rsquareds,ylim=c(0,1))
abline(h=0.0,lty=3)
means <- aggregate(RANDOMIZED.DSP.summary.df2 $L.DSP.LE.rsquareds,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=mean)
points(means[,1],means[,2],col="red",pch=19)
points(means[,1],means[,2],col="red",type="l")
stdevs <- aggregate(RANDOMIZED.DSP.summary.df2 $L.DSP.LE.rsquareds,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=sd)
arrows(means[,1],means[,2]+ stdevs[,2],means[,1],means[,2]- stdevs[,2],length=0.0,angle=90,code=3,col="red")

#points(RANDOMIZED.DSP.summary.df$radius, RANDOMIZED.DSP.summary.df$L.DSP.LE.rsquareds,col="red")
points(DSP.summary.df$radius, DSP.summary.df$L.DSP.LE.rsquareds,pch=19)
points(DSP.summary.df$radius, DSP.summary.df$L.DSP.LE.rsquareds,pch=19,type="l")


plot(DSP.summary.df$radius, DSP.summary.df$L.DSP.LE.pvals,ylim=c(0,1))
abline(h=0.05,lty=3)
means <- aggregate(RANDOMIZED.DSP.summary.df2 $L.DSP.LE.pvals,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=mean)
points(means[,1],means[,2],col="red",pch=19)
points(means[,1],means[,2],col="red",type="l")
stdevs <- aggregate(RANDOMIZED.DSP.summary.df2 $L.DSP.LE.pvals,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=sd)
arrows(means[,1],means[,2]+ stdevs[,2],means[,1],means[,2]- stdevs[,2],length=0.0,angle=90,code=3,col="red")

#points(RANDOMIZED.DSP.summary.df$radius, RANDOMIZED.DSP.summary.df$L.DSP.LE.pvals,col="red")
points(DSP.summary.df$radius, DSP.summary.df$L.DSP.LE.pvals,pch=19)
points(DSP.summary.df$radius, DSP.summary.df$L.DSP.LE.pvals,pch=19,type="l")



#### Linear DSP with spearman 
head(DSP.summary.df)
plot(DSP.summary.df$radius, DSP.summary.df$L.DSP.SE.estimates,ylim=c(-0.05,0.05))
abline(h=0.0,lty=3)
points(DSP.summary.df$radius, DSP.summary.df$L.DSP.SE.estimates,pch=19)
points(DSP.summary.df$radius, DSP.summary.df$L.DSP.SE.estimates,pch=19,type="l")
means <- aggregate(RANDOMIZED.DSP.summary.df2 $L.DSP.SE.estimates,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=mean)
points(means[,1],means[,2],col="red",pch=19)
points(means[,1],means[,2],col="red",type="l")
stdevs <- aggregate(RANDOMIZED.DSP.summary.df2 $L.DSP.SE.estimates,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=sd)
arrows(means[,1],means[,2]+ stdevs[,2],means[,1],means[,2]- stdevs[,2],length=0.0,angle=90,code=3,col="red")
#points(RANDOMIZED.DSP.summary.df$radius, RANDOMIZED.DSP.summary.df$L.DSP.LE.estimates,col="red")


plot(DSP.summary.df$radius, DSP.summary.df$L.DSP.SE.rsquareds,ylim=c(0,1))
abline(h=0.0,lty=3)
means <- aggregate(RANDOMIZED.DSP.summary.df2 $L.DSP.SE.rsquareds,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=mean)
points(means[,1],means[,2],col="red",pch=19)
points(means[,1],means[,2],col="red",type="l")
stdevs <- aggregate(RANDOMIZED.DSP.summary.df2 $L.DSP.SE.rsquareds,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=sd)
arrows(means[,1],means[,2]+ stdevs[,2],means[,1],means[,2]- stdevs[,2],length=0.0,angle=90,code=3,col="red")
#points(RANDOMIZED.DSP.summary.df$radius, RANDOMIZED.DSP.summary.df$L.DSP.LE.rsquareds,col="red")
points(DSP.summary.df$radius, DSP.summary.df$L.DSP.SE.rsquareds,pch=19)
points(DSP.summary.df$radius, DSP.summary.df$L.DSP.SE.rsquareds,pch=19,type="l")


plot(DSP.summary.df$radius, DSP.summary.df$L.DSP.SE.pvals,ylim=c(0,1))
abline(h=0.05,lty=3)
means <- aggregate(RANDOMIZED.DSP.summary.df2 $L.DSP.SE.pvals,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=mean)
points(means[,1],means[,2],col="red",pch=19)
points(means[,1],means[,2],col="red",type="l")
stdevs <- aggregate(RANDOMIZED.DSP.summary.df2 $L.DSP.SE.pvals,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=sd)
arrows(means[,1],means[,2]+ stdevs[,2],means[,1],means[,2]- stdevs[,2],length=0.0,angle=90,code=3,col="red")

#points(RANDOMIZED.DSP.summary.df$radius, RANDOMIZED.DSP.summary.df$L.DSP.LE.pvals,col="red")
points(DSP.summary.df$radius, DSP.summary.df$L.DSP.SE.pvals,pch=19)
points(DSP.summary.df$radius, DSP.summary.df$L.DSP.SE.pvals,pch=19,type="l")



#### Spear DSP with linear 

#### Linear DSP with spearman 
head(DSP.summary.df)
par(mfrow=c(2,2))
plot(DSP.summary.df$radius, DSP.summary.df$S.DSP.LE.estimates,ylim=c(-1,1))
abline(h=0.0,lty=3)
points(DSP.summary.df$radius, DSP.summary.df$S.DSP.LE.estimates,pch=19)
points(DSP.summary.df$radius, DSP.summary.df$S.DSP.LE.estimates,pch=19,type="l")
means <- aggregate(RANDOMIZED.DSP.summary.df2 $S.DSP.LE.estimates,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=mean)
points(means[,1],means[,2],col="red",pch=19)
points(means[,1],means[,2],col="red",type="l")
stdevs <- aggregate(RANDOMIZED.DSP.summary.df2 $S.DSP.LE.estimates,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=sd)
arrows(means[,1],means[,2]+ stdevs[,2],means[,1],means[,2]- stdevs[,2],length=0.0,angle=90,code=3,col="red")
#points(RANDOMIZED.DSP.summary.df$radius, RANDOMIZED.DSP.summary.df$L.DSP.LE.estimates,col="red")


plot(DSP.summary.df$radius, DSP.summary.df$S.DSP.LE.pvals,ylim=c(0,1))
abline(h=0.05,lty=3)
means <- aggregate(RANDOMIZED.DSP.summary.df2 $S.DSP.LE.pvals,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=mean)
points(means[,1],means[,2],col="red",pch=19)
points(means[,1],means[,2],col="red",type="l")
stdevs <- aggregate(RANDOMIZED.DSP.summary.df2 $S.DSP.LE.pvals,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=sd)
arrows(means[,1],means[,2]+ stdevs[,2],means[,1],means[,2]- stdevs[,2],length=0.0,angle=90,code=3,col="red")
#points(RANDOMIZED.DSP.summary.df$radius, RANDOMIZED.DSP.summary.df$L.DSP.LE.pvals,col="red")
points(DSP.summary.df$radius, DSP.summary.df$S.DSP.LE.pvals,pch=19)
points(DSP.summary.df$radius, DSP.summary.df$S.DSP.LE.pvals,pch=19,type="l")


plot(DSP.summary.df$radius, DSP.summary.df$S.DSP.SE.estimates,ylim=c(-1,1))
abline(h=0.0,lty=3)
points(DSP.summary.df$radius, DSP.summary.df$S.DSP.SE.estimates,pch=19)
points(DSP.summary.df$radius, DSP.summary.df$S.DSP.SE.estimates,pch=19,type="l")
means <- aggregate(RANDOMIZED.DSP.summary.df2 $S.DSP.SE.estimates,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=mean)
points(means[,1],means[,2],col="red",pch=19)
points(means[,1],means[,2],col="red",type="l")
stdevs <- aggregate(RANDOMIZED.DSP.summary.df2 $S.DSP.SE.estimates,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=sd)
arrows(means[,1],means[,2]+ stdevs[,2],means[,1],means[,2]- stdevs[,2],length=0.0,angle=90,code=3,col="red")
#points(RANDOMIZED.DSP.summary.df$radius, RANDOMIZED.DSP.summary.df$L.DSP.LE.estimates,col="red")


plot(DSP.summary.df$radius, DSP.summary.df$S.DSP.SE.pvals,ylim=c(0,1))
abline(h=0.05,lty=3)
means <- aggregate(RANDOMIZED.DSP.summary.df2 $S.DSP.SE.pvals,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=mean)
points(means[,1],means[,2],col="red",pch=19)
points(means[,1],means[,2],col="red",type="l")
stdevs <- aggregate(RANDOMIZED.DSP.summary.df2 $S.DSP.SE.pvals,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=sd)
arrows(means[,1],means[,2]+ stdevs[,2],means[,1],means[,2]- stdevs[,2],length=0.0,angle=90,code=3,col="red")
#points(RANDOMIZED.DSP.summary.df$radius, RANDOMIZED.DSP.summary.df$L.DSP.LE.pvals,col="red")
points(DSP.summary.df$radius, DSP.summary.df$S.DSP.LE.pvals,pch=19)
points(DSP.summary.df$radius, DSP.summary.df$S.DSP.LE.pvals,pch=19,type="l")



