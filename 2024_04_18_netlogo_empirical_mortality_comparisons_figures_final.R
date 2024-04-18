rm(list=ls())
setwd("/users/cx21atek/desktop/Azteca_DSP_local/input data")
library(viridis)

death.rate.df <- read.csv("2023_11_15_Azteca_DSP_empirical_death_rates_full_df.csv")
head(death.rate.df)
#### now calculating the average age specific mortality 

#age_mortality_mat <-age_mortality_mat[3:12,]## adjust so we dont include 2004 data.. since it may fuck with 1 yr death rate estimates
linear.age_class_mort_mean <- numeric(length(unique(death.rate.df $age))) # for each age 
linear.age_class_mort_error <- numeric(length(unique(death.rate.df $age))) 
logged.age_class_mort_mean <- numeric(length(unique(death.rate.df $age))) # for each age 
logged.age_class_mort_error <- numeric(length(unique(death.rate.df $age))) 

for(avg.age.mort.looper in 1:12){
	#avg.age.mort.looper <- 2
	an_class_data <- death.rate.df[death.rate.df $age == avg.age.mort.looper,]$abs.death
	age_class_morting <- an_class_data[!is.nan(an_class_data)]
	age_class_morting <- age_class_morting[!is.na(age_class_morting)]
	
	linear.age.mort.vec <- abs(age_class_morting)
	linear.age_class_mort_mean[avg.age.mort.looper] <- mean(linear.age.mort.vec) 
	linear.age_class_mort_error[avg.age.mort.looper ]<-  sd(linear.age.mort.vec)/sqrt(length(linear.age.mort.vec))
	
	logged.age.mort.vec <- log(abs(age_class_morting))
	logged.age_class_mort_mean[avg.age.mort.looper] <- mean(logged.age.mort.vec) 
	logged.age_class_mort_error[avg.age.mort.looper ]<-  sd(logged.age.mort.vec)/sqrt(length(logged.age.mort.vec))
}



################ Empricial death rate figure with two colors rectangles 
### Nov 13 2023 updated ### 
log_age <- log(seq(1,12,1))
dev.new(width=3.5, height=3.3)
par(mfrow=c(1,1),mai=c(0.7,0.7,0.1,0.1))
plot(log_age, logged.age_class_mort_mean,xlab="",ylab="",yaxt="n",pch=19,col="black",ylim=c(-2.2,-0.6))

mycol <- rgb(0, 90, 0, max = 100, alpha = 10, names = "blue50")
mycol2 <- rgb(90, 0, 0, max = 100, alpha = 10, names = "red50")

rect(par("usr")[1], par("usr")[3],log(5.5), par("usr")[4],col = mycol) # Color
rect(log(5.5), par("usr")[3],par("usr")[2], par("usr")[4],col = mycol2) # Color
par(new=T)
points(jitter(log(death.rate.df$age),amount=0.03), log(death.rate.df$abs.death),col="grey")
points(log_age, logged.age_class_mort_mean,xlab="",ylab="",yaxt="n",pch=19,col="black",ylim=c(-2.2,-0.6))
arrows(log_age, logged.age_class_mort_mean + logged.age_class_mort_error , log_age, logged.age_class_mort_mean-logged.age_class_mort_error,angle=90,length=0.03,code=3,col="black",lwd=2)
axis(2,las=2)
mtext("ln(Nest Age Class)",side=1,line=2.2)
mtext("ln(Death rate)",side=2,line=2.5)
abline(v=log(12),lty=2,lwd=2)



trunc.df <- death.rate.df[death.rate.df$age < 6, ]
log.log.full.model <- lm(log(trunc.df$abs.death) ~ log(trunc.df$age) )
summary(log.log.full.model)
#abline(log.log.mean.model,col="red")
text(2.05,-0.7,"Death due to",cex=0.6)
text(2.07,-0.8,"cluster density",cex=0.6)
text(0.45,-0.7,"Death due to DSP",cex=0.7)

### Add the regression line to the front of the plot? 
clip(-1,log(5.5),-10,10)
abline(log.log.full.model $coeff[1], log.log.full.model $coeff[2])
summary(log.log.full.model)

mean.df <- data.frame(log_age, logged.age_class_mort_mean)

trunc.mean.df <- mean.df[mean.df$log_age < log(6),]

log.log.mean.model <- lm(trunc.mean.df$logged.age_class_mort_mean ~ trunc.mean.df$log_age)
abline(log.log.mean.model)



##############################################################################################################
############################################### Model analysis ###############################################
##############################################################################################################

## this is the netlogo data that has the replicate information for the fixed effect models 

final.death.rate.df <- read.csv("2023_11_19_azteca_DSP_netlogo_mortality_df_with_replicates.csv")

final.death.rate.df$death.rate.abs <- abs(final.death.rate.df$death.rate.avg)
pred.diff.vec <- unique(final.death.rate.df$pred.diff)

library(viridis)

### Running the mixed effects models 
dev.new(width=10.4, height=5.3)
par(mfcol=c(2,4),mai=c(0.5,0.5,0.1,0.1),oma=c(0,0,1,0))

for(trunc.looper in c(2,2.5,3,3.5)){

truc.threshold <- trunc.looper

estimates <- numeric(length(pred.diff.vec))
upper.estimates <- numeric(length(pred.diff.vec))
lower.estimates <- numeric(length(pred.diff.vec))

intercepts <- numeric(length(pred.diff.vec))

library(lme4)


final.death.rate.df$death.rate.abs <- abs(final.death.rate.df$death.rate.avg)
pred.diff.vec <- unique(final.death.rate.df$pred.diff)

plot(NA,ylim=c(-9,-0),xlim=c(0,log(500)),yaxt="n",xlab="",ylab="")
mtext("ln(age class)",side=1,line=2.2,cex=0.8)
mtext("ln(death rate)",side=2,line=2.2,cex=0.8)
axis(2,las=2)
abline(v= truc.threshold,lty=3)
mtext(paste("ln(age class) threshold:", truc.threshold),side=3,line=0.2,cex=0.7)

for(pred.diff.looping.vec in 1:length(pred.diff.vec)){
#x <- 1
a.diff.df <- final.death.rate.df[final.death.rate.df$pred.diff == pred.diff.vec[pred.diff.looping.vec], ]
points(log(a.diff.df$age),log(a.diff.df$death.rate.abs),col=viridis(length(pred.diff.vec))[pred.diff.looping.vec],type="p",cex=0.5,lwd=0.1,ylim=c(-8,0),xlim=c(0,log(500)),yaxt="n",xlab="",ylab="")


## clean up the data from NAs and NaNs
xval.raw <- log(a.diff.df$age)
yval.raw <- log(a.diff.df$death.rate.abs)

clean.diff.df <- a.diff.df[!is.na(log(a.diff.df$death.rate.abs)) & !is.nan(log(a.diff.df$death.rate.abs)) & log(a.diff.df$death.rate.abs) > -Inf, ]

clean.diff.df$log.age <- log(clean.diff.df$age)
head(clean.diff.df)

trunc.df <- clean.diff.df[clean.diff.df$log.age <= truc.threshold, ]

head(trunc.df)
mixed.eff.trunc.model <- lmer(log(death.rate.abs) ~ log.age + (1 |replicate),data= trunc.df)
summary(mixed.eff.trunc.model)

points(trunc.df$log.age, log(trunc.df$death.rate.abs),col=viridis(length(pred.diff.vec))[pred.diff.looping.vec],type="p",cex=0.5,lwd=1,pch=19  )

estimates[pred.diff.looping.vec] <- summary(mixed.eff.trunc.model)$coeff[2]
intercepts[pred.diff.looping.vec] <- summary(mixed.eff.trunc.model)$coeff[1]
upper.estimates[pred.diff.looping.vec] <- summary(mixed.eff.trunc.model)$coeff[2] + summary(mixed.eff.trunc.model)$coeff[4]
lower.estimates[pred.diff.looping.vec] <- summary(mixed.eff.trunc.model)$coeff[2] - summary(mixed.eff.trunc.model)$coeff[4]
abline(intercepts[pred.diff.looping.vec],estimates[pred.diff.looping.vec],lwd=2,col=viridis(length(pred.diff.vec))[pred.diff.looping.vec])


}

#dev.new(width=3.5, height=3.3)
#par(mfrow=c(1,1),mai=c(0.7,0.8,0.2,0.1))
plot(pred.diff.vec, estimates,ylim=c(-0.8,-0.1),col=viridis(length(pred.diff.vec)),pch=19,yaxt="n",xlab="",ylab="")
points(pred.diff.vec, upper.estimates,col=viridis(length(pred.diff.vec)),cex=0.5)
points(pred.diff.vec, lower.estimates,col=viridis(length(pred.diff.vec)),cex=0.5)
mtext(expression(estimate["age class death rate"]),side=2,line=2.6,cex=0.8)
mtext("Consumer diffusion rate",side=1,line=2.2,cex=0.8)
axis(2,las=2)

## hard code
#empirical.estimate <- -0.3217294
#empirical.estimate.upper <- -0.3041126
#empirical.estimate.lower <- -0.3393463

## this is the empirical model that was run above. 
empirical.log.log.mean.model <- lm(trunc.mean.df$logged.age_class_mort_mean ~ trunc.mean.df$log_age)

empirical.estimate <- summary(log.log.mean.model)$coeff[2]
empirical.estimate.upper <- summary(log.log.mean.model)$coeff[2] + summary(log.log.mean.model)$coeff[4]
empirical.estimate.lower <- summary(log.log.mean.model)$coeff[2] - summary(log.log.mean.model)$coeff[4]
abline(h= empirical.estimate,col="red")
abline(h= c(empirical.estimate.upper, empirical.estimate.lower ),col="red",lty=3)



}




##### Just lookin at 2.25 threshold for the article 



truc.threshold <- 2.25

estimates <- numeric(length(pred.diff.vec))
upper.estimates <- numeric(length(pred.diff.vec))
lower.estimates <- numeric(length(pred.diff.vec))

intercepts <- numeric(length(pred.diff.vec))

library(lme4)


final.death.rate.df$death.rate.abs <- abs(final.death.rate.df$death.rate.avg)
pred.diff.vec <- unique(final.death.rate.df$pred.diff)


dev.new(width=8, height=4)
par(mfrow=c(1,2),mai=c(0.5,0.7,0.1,0.1),oma=c(1,1,1,0))


plot(NA,ylim=c(-9,-0),xlim=c(0,log(500)),yaxt="n",xlab="",ylab="")
mtext("ln(age class)",side=1,line=2.2,cex=0.8)
mtext("ln(death rate)",side=2,line=2.2,cex=0.8)
axis(2,las=2)
abline(v= truc.threshold,lty=3)
#mtext(paste("ln(age class) threshold:", truc.threshold),side=3,line=0.2,cex=0.7)

for(pred.diff.looping.vec in 1:length(pred.diff.vec)){
#x <- 1
a.diff.df <- final.death.rate.df[final.death.rate.df$pred.diff == pred.diff.vec[pred.diff.looping.vec], ]
points(log(a.diff.df$age),log(a.diff.df$death.rate.abs),col=viridis(length(pred.diff.vec))[pred.diff.looping.vec],type="p",cex=0.5,lwd=0.1,ylim=c(-8,0),xlim=c(0,log(500)),yaxt="n",xlab="",ylab="")


## clean up the data from NAs and NaNs
xval.raw <- log(a.diff.df$age)
yval.raw <- log(a.diff.df$death.rate.abs)

clean.diff.df <- a.diff.df[!is.na(log(a.diff.df$death.rate.abs)) & !is.nan(log(a.diff.df$death.rate.abs)) & log(a.diff.df$death.rate.abs) > -Inf, ]

clean.diff.df$log.age <- log(clean.diff.df$age)
head(clean.diff.df)

trunc.df <- clean.diff.df[clean.diff.df$log.age <= truc.threshold, ]

head(trunc.df)
mixed.eff.trunc.model <- lmer(log(death.rate.abs) ~ log.age + (1 |replicate),data= trunc.df)
summary(mixed.eff.trunc.model)

points(trunc.df$log.age, log(trunc.df$death.rate.abs),col=viridis(length(pred.diff.vec))[pred.diff.looping.vec],type="p",cex=0.5,lwd=1,pch=19  )

estimates[pred.diff.looping.vec] <- summary(mixed.eff.trunc.model)$coeff[2]
intercepts[pred.diff.looping.vec] <- summary(mixed.eff.trunc.model)$coeff[1]
upper.estimates[pred.diff.looping.vec] <- summary(mixed.eff.trunc.model)$coeff[2] + summary(mixed.eff.trunc.model)$coeff[4]
lower.estimates[pred.diff.looping.vec] <- summary(mixed.eff.trunc.model)$coeff[2] - summary(mixed.eff.trunc.model)$coeff[4]
abline(intercepts[pred.diff.looping.vec],estimates[pred.diff.looping.vec],lwd=2,col=viridis(length(pred.diff.vec))[pred.diff.looping.vec])


}

## this is the empirical model that was run above. 
empirical.log.log.mean.model <- lm(trunc.mean.df$logged.age_class_mort_mean ~ trunc.mean.df$log_age)


abline(empirical.log.log.mean.model,col="red",lwd=1.5)


#dev.new(width=3.5, height=3.3)
#par(mfrow=c(1,1),mai=c(0.7,0.8,0.2,0.1))
plot(pred.diff.vec, estimates,ylim=c(-0.8,-0.1),col=viridis(length(pred.diff.vec)),pch=19,yaxt="n",xlab="",ylab="")
points(pred.diff.vec, upper.estimates,col=viridis(length(pred.diff.vec)),cex=0.5)
points(pred.diff.vec, lower.estimates,col=viridis(length(pred.diff.vec)),cex=0.5)
mtext(expression(estimate["age class death rate"]),side=2,line=2.6,cex=0.8)
mtext("Consumer diffusion rate",side=1,line=2.2,cex=0.8)
axis(2,las=2)



empirical.estimate <- summary(log.log.mean.model)$coeff[2]
empirical.estimate.upper <- summary(log.log.mean.model)$coeff[2] + summary(log.log.mean.model)$coeff[4]
empirical.estimate.lower <- summary(log.log.mean.model)$coeff[2] - summary(log.log.mean.model)$coeff[4]
abline(h= empirical.estimate,col="red",lwd=1.5)
abline(h= c(empirical.estimate.upper, empirical.estimate.lower ),col="red",lty=3)




##### End of mixed effect model estimates 
##### End of mixed effect model estimates 
##### End of mixed effect model estimates 
##### End of mixed effect model estimates 
##### End of mixed effect model estimates 



###### Start of pooled average parameter estimates ###### 

#### Illustrating the curve in age specific mortality from the model ####
####### Note the basic prediction is that the DSP is less pronouced with higer diffusion. This is apparaent in the figure 
final.death.rate.df <- read.csv("2023_11_15_azteca_DSP_netlogo_mortality_df.csv") ## This is with the pooled death rates and means 
final.death.rate.df $death.rate.abs <- abs(final.death.rate.df $death.rate.avg)

dev.new(width=11, height=4.5)
par(mfrow=c(2,5),mai=c(0.5,0.5,0.1,0.1),oma=c(0,0,1,0))

pred.diff.vec <- unique(final.death.rate.df$pred.diff)
for(pred.diff.looping.vec in 1:length(pred.diff.vec)){
#x <- 1

	plot(NA,ylim=c(-8,-2),xlim=c(0,log(500)),yaxt="n",xlab="",ylab="")
	mtext("ln(age class)",side=1,line=2.2,cex=0.8)
	mtext("ln(death rate)",side=2,line=2.2,cex=0.8)
	axis(2,las=2)
	mtext(paste("consumer difussion:", pred.diff.vec[pred.diff.looping.vec]),side=3,line=0.2,cex=0.7)


	a.diff.df <- final.death.rate.df[final.death.rate.df$pred.diff == pred.diff.vec[pred.diff.looping.vec], ]
	points(log(a.diff.df$age),log(a.diff.df$death.rate.abs),col=viridis(length(pred.diff.vec))[pred.diff.looping.vec],type="p",cex=0.5,lwd=1,ylim=c(-8,0),xlim=c(0,log(500)),yaxt="n",xlab="",ylab="")
	#abline(v=c(2,3),lty=3)

}




final.death.rate.df <- read.csv("2023_11_15_azteca_DSP_netlogo_mortality_df.csv") ## This is with the pooled death rates and means 
final.death.rate.df $death.rate.abs <- abs(final.death.rate.df $death.rate.avg)

dev.new(width=10.4, height=5.3)
par(mfcol=c(2,4),mai=c(0.5,0.5,0.1,0.1),oma=c(0,0,1,0))

for(trunc.looper in c(2,2.5,3,3.5)){

truc.threshold <- trunc.looper

estimates <- numeric(length(pred.diff.vec))
upper.estimates <- numeric(length(pred.diff.vec))
lower.estimates <- numeric(length(pred.diff.vec))

intercepts <- numeric(length(pred.diff.vec))


final.death.rate.df$death.rate.abs <- abs(final.death.rate.df$death.rate.avg)
pred.diff.vec <- unique(final.death.rate.df$pred.diff)

plot(NA,ylim=c(-9,-0),xlim=c(0,log(500)),yaxt="n",xlab="",ylab="")
mtext("ln(age class)",side=1,line=2.2,cex=0.8)
mtext("ln(death rate)",side=2,line=2.2,cex=0.8)
axis(2,las=2)
abline(v= truc.threshold,lty=3)
mtext(paste("ln(age class) threshold:", truc.threshold),side=3,line=0.2,cex=0.7)

for(pred.diff.looping.vec in 1:length(pred.diff.vec)){
#x <- 1


a.diff.df <- final.death.rate.df[final.death.rate.df$pred.diff == pred.diff.vec[pred.diff.looping.vec], ]
points(log(a.diff.df$age),log(a.diff.df$death.rate.abs),col=viridis(length(pred.diff.vec))[pred.diff.looping.vec],type="p",cex=0.5,lwd=0.1,ylim=c(-8,0),xlim=c(0,log(500)),yaxt="n",xlab="",ylab="")


## clean up the data from NAs and NaNs
xval.raw <- log(a.diff.df$age)
yval.raw <- log(a.diff.df$death.rate.abs)

clean.diff.df <- a.diff.df[!is.na(log(a.diff.df$death.rate.abs)) & !is.nan(log(a.diff.df$death.rate.abs)) & log(a.diff.df$death.rate.abs) > -Inf, ]

clean.diff.df$log.age <- log(clean.diff.df$age)
head(clean.diff.df)

trunc.df <- clean.diff.df[clean.diff.df$log.age <= truc.threshold, ]

head(trunc.df)
model.trunc.model <- lm(log(death.rate.abs) ~ log.age,data= trunc.df)
summary(model.trunc.model)

points(trunc.df$log.age, log(trunc.df$death.rate.abs),col=viridis(length(pred.diff.vec))[pred.diff.looping.vec],type="p",cex=0.5,lwd=1,pch=19  )

estimates[pred.diff.looping.vec] <- summary(model.trunc.model)$coeff[2]
intercepts[pred.diff.looping.vec] <- summary(model.trunc.model)$coeff[1]
upper.estimates[pred.diff.looping.vec] <- summary(model.trunc.model)$coeff[2] + summary(model.trunc.model)$coeff[4]
lower.estimates[pred.diff.looping.vec] <- summary(model.trunc.model)$coeff[2] - summary(model.trunc.model)$coeff[4]
abline(intercepts[pred.diff.looping.vec],estimates[pred.diff.looping.vec],lwd=2,col=viridis(length(pred.diff.vec))[pred.diff.looping.vec])


}

#dev.new(width=3.5, height=3.3)
#par(mfrow=c(1,1),mai=c(0.7,0.8,0.2,0.1))
plot(pred.diff.vec, estimates,ylim=c(-0.8,-0.1),col=viridis(length(pred.diff.vec)),pch=19,yaxt="n",xlab="",ylab="")
points(pred.diff.vec, upper.estimates,col=viridis(length(pred.diff.vec)),cex=0.5)
points(pred.diff.vec, lower.estimates,col=viridis(length(pred.diff.vec)),cex=0.5)
mtext(expression(estimate["age class death rate"]),side=2,line=2.6,cex=0.8)
mtext("Consumer diffusion rate",side=1,line=2.2,cex=0.8)
axis(2,las=2)

## hard code
#empirical.estimate <- -0.3217294
#empirical.estimate.upper <- -0.3041126
#empirical.estimate.lower <- -0.3393463

## this is the empirical model that was run above. 
empirical.log.log.mean.model <- lm(trunc.mean.df$logged.age_class_mort_mean ~ trunc.mean.df$log_age)

empirical.estimate <- summary(log.log.mean.model)$coeff[2]
empirical.estimate.upper <- summary(log.log.mean.model)$coeff[2] + summary(log.log.mean.model)$coeff[4]
empirical.estimate.lower <- summary(log.log.mean.model)$coeff[2] - summary(log.log.mean.model)$coeff[4]
abline(h= empirical.estimate,col="red")
abline(h= c(empirical.estimate.upper, empirical.estimate.lower ),col="red",lty=3)



}

###### End of Age specific mortality anlaysis 
###### End of Age specific mortality anlaysis 
###### End of Age specific mortality anlaysis 
###### End of Age specific mortality anlaysis 
###### End of Age specific mortality anlaysis 
###### End of Age specific mortality anlaysis 
###### End of Age specific mortality anlaysis 

##################################################################################### 
################# Spatial pattern analysis comparisons ##############################
##################################################################################### 
##################################################################################### 
################# Spatial pattern analysis comparisons ##############################
#####################################################################################
##################################################################################### 
################# Spatial pattern analysis comparisons ##############################
#####################################################################################
#### Below is the pooling method that will put all of the replicate runrs together. 



data <- read.csv("azteca_phorid_model_data_for_paper_Feb27_202.csv",header=T,skip=6)
data$replicate <- data$X.run.number.
data$sim.step <- data$X.step.
pred.diff.val.vec <- unique(data$normal_mean_walk)

## what is the maximum size cluster we can detect? 
max.cluster.size.to.use <- 10000



## gathering all of the model cluster size frequency distributions 
dev.new(width=10.2, height=4.5)
par(mfrow=c(2,5),mai=c(0.5,0.5,0.3,0.1),oma=c(0,0,0,0))
clust.df.colnames <- c("pred.diff","rep.id","cluster.size","cum.freq")
clust.freq.df.raw <- data.frame(matrix(0, ncol=length(clust.df.colnames)))
colnames(clust.freq.df.raw) <- clust.df.colnames
for(walk.val.looper in 1:length(pred.diff.val.vec)){ ## select a diffusion parameter 
	
	#walk.val.looper <- 2
	a.walk.df <- data[data$normal_mean_walk == pred.diff.val.vec[walk.val.looper], ]
	
	rep.counter.indexy <- 1
	cluster_sizes <- c()
	for(replicate.looper in unique(a.walk.df$replicate)){### going through the unique replicates of the simluations 
		
		
	#replicate.looper <- 17
	a.replicate.walk.df <- a.walk.df[a.walk.df$replicate == replicate.looper,] 

	
	raw_cluster <- as.character(a.replicate.walk.df$cluster.freq[1001])
	split_cluster <- unlist(strsplit(raw_cluster, split = " "))
	first <- split_cluster[1]
	last <- split_cluster[length(split_cluster)]
	split_cluster[1] <- gsub("\\[","",first)
	split_cluster[length(split_cluster)] <- gsub("\\]","",last)
	split_cluster <- as.numeric(split_cluster)
	split_cluster <- rev(sort(split_cluster))
	## the empty space is counted as a cluster in netlogo. so it has to be removed. It is always the biggest cluster 
	clusters_minus_biggest <- split_cluster[split_cluster != max(split_cluster)]
	cluster_sizes.temp <- clusters_minus_biggest
	
	cluster_sizes <- c(cluster_sizes, cluster_sizes.temp)
	} ## end of replicate looper to pool 
	cluster_freq <- numeric(max.cluster.size.to.use)
	for( x in 1: max.cluster.size.to.use){
		cluster_freq[x] <- sum(cluster_sizes == x)
	}
	
	cumm_freq_dist <-numeric(length(cluster_freq))
	cumm_freq_dist[length(cumm_freq_dist)] <- cluster_freq[length(cumm_freq_dist)]
	for(dingers in (length(cumm_freq_dist)-1):1){
		cumm_freq_dist[dingers] <- cumm_freq_dist[dingers + 1] + cluster_freq[dingers]
	}
	
	index <- seq(1,length(cumm_freq_dist),1)
	plot(NA,xlim=c(0,9),ylim=c(0,9),yaxt="n",xlab="",ylab="",xaxt="n")
	axis(2,las=2,cex.axis=1)
	axis(1,cex.axis=1)
	mtext("log(cluster size)",side=1,line=2.2,cex=0.8)
	mtext("log(frequency)",side=2,line=2.2,cex=0.8)

	if(sum((cumm_freq_dist) > 0)){
		
		points(log(index),log(cumm_freq_dist),cex=0.2,pch=19)
		clean.x <- log(index)[log(cumm_freq_dist) != -Inf]
		clean.y <- log(cumm_freq_dist)[log(cumm_freq_dist) != -Inf]
		abline(lm(clean.y ~ clean.x))
		text(6,8,round(summary(lm(clean.y ~ clean.x))$coeff[2],digits=4),cex=0.8)
	
	}
	
	#if(rep.counter.indexy == 10){
	mtext(paste("con diff:",pred.diff.val.vec[walk.val.looper]),side=3,line=0.2,cex=0.6)
	#}
	temp.df.storage <- data.frame(pred.diff.val.vec[walk.val.looper], rep.counter.indexy ,index, cumm_freq_dist)
	colnames(temp.df.storage) <- clust.df.colnames
	
	clust.freq.df.raw <- rbind(clust.freq.df.raw, temp.df.storage)
	rep.counter.indexy <- rep.counter.indexy  + 1
	
}# end of walk.val.looper


## the bunch of zeros is fucking up the models. Since it is cummulative dsitributions we shoudl only be interested in non-zero y values
### we can the nremove the zeros from the cum.freq
clust.freq.df <- clust.freq.df.raw[clust.freq.df.raw$rep.id != 0 & clust.freq.df.raw$cum.freq > 0 , ]

head(clust.freq.df)




### Hierarchial model to approximate the slope of the powerlaw from the replications of a given diffusion rate. 
library(lme4)

head(clust.freq.df)


clust.freq.stat.df <- data.frame(0,0,0,0,0)
clust.freq.stat.df.names <- c("pred.diff","intercept","intercept.error","estimate","estimate.error")
colnames(clust.freq.stat.df) <- clust.freq.stat.df.names
pred.diff.vec <- unique(clust.freq.df$pred.diff)
for(pred.diff.stat.looper in 1:length(pred.diff.vec)){
	## choose a diffusion rate.
	a.diff.df <- clust.freq.df[clust.freq.df$pred.diff == pred.diff.vec[pred.diff.stat.looper], ]

	## run the model 
	power.model <- lm(log(cum.freq) ~ log(cluster.size),data= a.diff.df)
	summary(power.model)

	an.intercept <- summary(power.model)$coeff[1]
	an.intercept.error <- summary(power.model)$coeff[3]
	an.estimate <- summary(power.model)$coeff[2]
	an.estimate.error <- summary(power.model)$coeff[4]

	temp.df <- data.frame(pred.diff.vec[pred.diff.stat.looper], an.intercept, an.intercept.error, an.estimate, an.estimate.error)
	colnames(temp.df) <- clust.freq.stat.df.names
	clust.freq.stat.df <- rbind(clust.freq.stat.df, temp.df)
}


clust.freq.stat.df <- clust.freq.stat.df[clust.freq.stat.df$pred.diff != 0, ]








############# Below is method that keeps all of the varabilty from run to run in the estimates.
############# Below is method that keeps all of the varabilty from run to run in the estimates.
############# Below is method that keeps all of the varabilty from run to run in the estimates.
############# Below is method that keeps all of the varabilty from run to run in the estimates.
############# Below is method that keeps all of the varabilty from run to run in the estimates.
############# Below is method that keeps all of the varabilty from run to run in the estimates.

data <- read.csv("azteca_phorid_model_data_for_paper_Feb27_202.csv",header=T,skip=6)
data$replicate <- data$X.run.number.
data$sim.step <- data$X.step.
pred.diff.val.vec <- unique(data$normal_mean_walk)

## what is the maximum size cluster we can detect? 
max.cluster.size.to.use <- 10000



## gathering all of the model cluster size frequency distributions 
makeplots <- F
if(makeplots == T){
	
dev.new(width=7.2, height=7)
par(mfrow=c(10,10),mai=c(0.05,0.05,0.05,0.05),oma=c(0,0,0,2))

}

clust.df.colnames <- c("pred.diff","rep.id","cluster.size","cum.freq")
clust.freq.df.raw <- data.frame(matrix(0, ncol=length(clust.df.colnames)))
colnames(clust.freq.df.raw) <- clust.df.colnames
for(walk.val.looper in 1:length(pred.diff.val.vec)){ ## select a diffusion parameter 
	
	#walk.val.looper <- 2
	a.walk.df <- data[data$normal_mean_walk == pred.diff.val.vec[walk.val.looper], ]
	
	rep.counter.indexy <- 1
	for(replicate.looper in unique(a.walk.df$replicate)){### going through the unique replicates of the simluations 
		
		
	#replicate.looper <- 17
	a.replicate.walk.df <- a.walk.df[a.walk.df$replicate == replicate.looper,] 

	
	raw_cluster <- as.character(a.replicate.walk.df$cluster.freq[1001])
	split_cluster <- unlist(strsplit(raw_cluster, split = " "))
	first <- split_cluster[1]
	last <- split_cluster[length(split_cluster)]
	split_cluster[1] <- gsub("\\[","",first)
	split_cluster[length(split_cluster)] <- gsub("\\]","",last)
	split_cluster <- as.numeric(split_cluster)
	split_cluster <- rev(sort(split_cluster))
	## the empty space is counted as a cluster in netlogo. so it has to be removed. It is always the biggest cluster 
	clusters_minus_biggest <- split_cluster[split_cluster != max(split_cluster)]
	cluster_sizes <- clusters_minus_biggest
	


	cluster_freq <- numeric(max.cluster.size.to.use)
	for( x in 1: max.cluster.size.to.use){
		cluster_freq[x] <- sum(cluster_sizes == x)
	}
	
	cumm_freq_dist <-numeric(length(cluster_freq))
	cumm_freq_dist[length(cumm_freq_dist)] <- cluster_freq[length(cumm_freq_dist)]
	for(dingers in (length(cumm_freq_dist)-1):1){
		cumm_freq_dist[dingers] <- cumm_freq_dist[dingers + 1] + cluster_freq[dingers]
	}
	
		index <- seq(1,length(cumm_freq_dist),1)

	if(makeplots ==T){
	index <- seq(1,length(cumm_freq_dist),1)
	plot(NA,xlim=c(0,9),ylim=c(0,9),yaxt="n",xlab="",ylab="",xaxt="n")
	#axis(2,las=2,cex.axis=0.6)
	#axis(1,cex.axis=0.6)

	if(sum((cumm_freq_dist) > 0)){
		
		points(log(index),log(cumm_freq_dist),cex=0.2,pch=19)
		clean.x <- log(index)[log(cumm_freq_dist) != -Inf]
		clean.y <- log(cumm_freq_dist)[log(cumm_freq_dist) != -Inf]
		abline(lm(clean.y ~ clean.x))
		text(6,8,round(summary(lm(clean.y ~ clean.x))$coeff[2],digits=4),cex=0.8)
	
	}
	
	
	if(rep.counter.indexy == 10){
		mtext(paste("con diff:",pred.diff.val.vec[walk.val.looper]),side=4,line=0.2,cex=0.6)
	}
	
	}## end of make plots 


	temp.df.storage <- data.frame(pred.diff.val.vec[walk.val.looper], rep.counter.indexy ,index, cumm_freq_dist)
	colnames(temp.df.storage) <- clust.df.colnames
	
	clust.freq.df.raw <- rbind(clust.freq.df.raw, temp.df.storage)
	rep.counter.indexy <- rep.counter.indexy  + 1
	}# end of replicate.looper
	
}# end of walk.val.looper


## the bunch of zeros is fucking up the models. Since it is cummulative dsitributions we shoudl only be interested in non-zero y values
### we can the nremove the zeros from the cum.freq
clust.freq.df <- clust.freq.df.raw[clust.freq.df.raw$rep.id != 0 & clust.freq.df.raw$cum.freq > 0 , ]

head(clust.freq.df)




### Hierarchial model to approximate the slope of the powerlaw from the replications of a given diffusion rate. 
library(lme4)

head(clust.freq.df)


clust.freq.stat.df <- data.frame(0,0,0,0,0)
clust.freq.stat.df.names <- c("pred.diff","intercept","intercept.error","estimate","estimate.error")
colnames(clust.freq.stat.df) <- clust.freq.stat.df.names
pred.diff.vec <- unique(clust.freq.df$pred.diff)
for(pred.diff.stat.looper in 1:length(pred.diff.vec)){
	## choose a diffusion rate.
	a.diff.df <- clust.freq.df[clust.freq.df$pred.diff == pred.diff.vec[pred.diff.stat.looper], ]

	## run the model 
	power.model <- lmer(log(cum.freq) ~ log(cluster.size) + (log(cluster.size)|rep.id),data= a.diff.df)

	summary(power.model)

	an.intercept <- summary(power.model)$coeff[1]
	an.intercept.error <- summary(power.model)$coeff[3]
	an.estimate <- summary(power.model)$coeff[2]
	an.estimate.error <- summary(power.model)$coeff[4]

	temp.df <- data.frame(pred.diff.vec[pred.diff.stat.looper], an.intercept, an.intercept.error, an.estimate, an.estimate.error)
	colnames(temp.df) <- clust.freq.stat.df.names
	clust.freq.stat.df <- rbind(clust.freq.stat.df, temp.df)
}


clust.freq.stat.df.lme4 <- clust.freq.stat.df[clust.freq.stat.df$pred.diff != 0, ]



### empirical azteac comparison 
setwd("/users/cx21atek/desktop/Azteca_DSP_local/input data")
dir()
data <- read.csv("Azteca_census_data_Jan_2018.csv")
library(igraph)

nests_2016 <- data[data$y16 > 0, ]
nests_2016_DM <- as.matrix(dist(data.frame(as.numeric(nests_2016$x_coord),as.numeric(nests_2016$y_coord) )))
head(nests_2016_DM)



dev.new(width=9.2, height=7)
par(mfrow=c(5,6),mai=c(0.2,0.2,0.2,0.2),oma=c(3,3,1,1))
empirical.clust.freq.stat.df <- data.frame(0,0,0,0,0)
clust.freq.stat.df.names <- c("cluster.scale","intercept","intercept.error","estimate","estimate.error")
colnames(empirical.clust.freq.stat.df) <- clust.freq.stat.df.names
cluster.scale.vec <- seq(5,34,1)

for(clust.scale.stat.looper in 1:length(cluster.scale.vec)){
	## choose a cluster sclae 
		
#clust.scale.stat.looper <- 1
Cluster_scale <- cluster.scale.vec[clust.scale.stat.looper] #dingus_smingus
adj_matrix <- matrix(0,ncol=dim(nests_2016)[1],nrow=dim(nests_2016)[1])
for(i in 1:dim(nests_2016_DM)[1]){
	for( j in 1:dim(nests_2016_DM)[1]){
		if(nests_2016_DM[i,j] <= Cluster_scale & i != j ){
			adj_matrix[i,j] <- 1
		}else{
			adj_matrix[i,j] <- 0
			}
	}
}


nest_graph <- graph.adjacency(adj_matrix,mode="undirected")
groups <- unlist(clusters(nest_graph)[1])
V(nest_graph)$color <- groups

nests_2016$cluster_ID <- groups
nest.df <- nests_2016 ## transfer to new data frame name 

cluster_sizes <-numeric(max(nest.df$cluster_ID))
for(zip in 1:max(nest.df $cluster_ID)){
	cluster_sizes[zip] <-  sum(nest.df $cluster_ID == zip)
}

## frequency of the cluste rsizes 
freq_dist <-max(cluster_sizes)
for(dip in 1:max(cluster_sizes)){
	freq_dist[dip] <- sum(cluster_sizes == dip) 
}

cum_freq_dist <-numeric(length(freq_dist))
cum_freq_dist[length(cum_freq_dist)] <- freq_dist[length(freq_dist)]
for(eip in (length(cum_freq_dist)-1):1){
	cum_freq_dist[eip] <- cum_freq_dist[eip + 1] + freq_dist[eip]
}

#par(mfrow=c(1,1))
index <- seq(1,length(cum_freq_dist),1)
x <- index
x_log <- log(index)
y_log <- log(cum_freq_dist)
plot(x_log, y_log,yaxt="n",xlab="",ylab="",pch=1,lwd=1,xlim=c(0,8),ylim=c(0,8),cex=0.5)
#mtext("ln(cluster size)",side=1,line=2)
#mtext("ln(frequency)",side=2,line=2.0)
axis(2,las=2)
power_model <- lm(y_log ~ x_log)
abline(power_model,lwd=1)
summary(power_model)




potenital.tail.check <- y_log == 0
potenital.stag.tail.check <- y_log == log(2)

if(sum(potenital.tail.check == T) > 5){
	trunc.y  <- y_log[1:(length(y_log) - (sum(potenital.tail.check == T) - 2 ))]
	trunc.x <- x_log[1:(length(y_log) - (sum(potenital.tail.check == T) - 2 ))]
	power_model <- lm(trunc.y ~ trunc.x)
	points(trunc.x, trunc.y,yaxt="n",xlab="",ylab="",pch=1,lwd=1,col="red",cex=0.5)
	abline(power_model,lwd=1,col="red")
	summary(power_model)
	}
	if(sum(potenital.stag.tail.check == T) > 5){
		
	trunc.y  <- y_log[1:(length(y_log) - (sum(potenital.stag.tail.check == T) - 2 ))]
	trunc.x <- x_log[1:(length(y_log) - (sum(potenital.stag.tail.check == T) - 2 ))]
	power_model <- lm(trunc.y ~ trunc.x)
	points(trunc.x, trunc.y,yaxt="n",xlab="",ylab="",pch=1,lwd=1,col="dark red",cex=0.5)
	abline(power_model,lwd=1,col="dark red")
	summary(power_model)

}

mtext(paste("scale(m):", Cluster_scale),cex=0.5)


	an.intercept <- summary(power_model)$coeff[1]
	an.intercept.error <- summary(power_model)$coeff[3]
	an.estimate <- summary(power_model)$coeff[2]
	an.estimate.error <- summary(power_model)$coeff[4]
	
	text(6,7,round(an.estimate,digits=4),cex=0.8)

	temp.df <- data.frame(cluster.scale.vec[clust.scale.stat.looper], an.intercept, an.intercept.error, an.estimate, an.estimate.error)
	colnames(temp.df) <- clust.freq.stat.df.names
	empirical.clust.freq.stat.df <- rbind(empirical.clust.freq.stat.df, temp.df)
}

mtext("ln(cluster size)",side=1,line=1,outer=T)
mtext("ln(frequency)",side=2,line=1,outer=T)

empirical.clust.freq.stat.df <- empirical.clust.freq.stat.df[empirical.clust.freq.stat.df $cluster.scale != 0, ]

empirical.clust.freq.stat.df
clust.freq.stat.df




### Her the empricail dsp stuff in the file 2023_11_18_DSP_pooled_final_v1
## use the df: DSP.summary.df
dir()
DSP.summary.df  <- read.csv("empirical_pooled_DSP_results.csv")

head(DSP.summary.df)




### Here looking at al of the predictions together 


### Plot with R sqaured thresholds
Rsquared_criteria <- DSP.summary.df[DSP.summary.df$L.DSP.LE.rsquareds >= 0.6,]
#Rsquared_criteria <- DSP.summary.df[DSP.summary.df$L.DSP.LE.rsquareds == max(DSP.summary.df$L.DSP.LE.rsquareds),]

dev.new(width=5, height=4)
par(mfrow=c(1,1),mai=c(0.7,0.9,0.1,0.1))
death.rate.prediction.df <- clust.freq.stat.df.lme4#[clust.freq.stat.df.lme4 $pred.diff %in% c(2,2.25,2.5,2.75),] ## these are the diffusion rates predicted by the age specific mortality  
plot(cluster.scale.vec ,empirical.clust.freq.stat.df$estimate,xlab="",ylab="",yaxt="n",ylim=c(-3,-0.5),cex=0.5,pch=19)
arrows(cluster.scale.vec ,empirical.clust.freq.stat.df$estimate - (empirical.clust.freq.stat.df$estimate.error*1.96), cluster.scale.vec ,empirical.clust.freq.stat.df$estimate + (empirical.clust.freq.stat.df$estimate.error*1.96),angle=90,code=3,length=0.0)
mtext("spatial scale(m)",side=1,line=2.3)
#mtext("ln(freq) ~ ln(clust size) slope",side=2,line=3)
mtext("Spatial pattern parameter",side=2,line=3)

axis(2,las=2)
abline(h= death.rate.prediction.df$estimate,col=viridis(10),lwd=2)
abline(h= c(death.rate.prediction.df$estimate + (death.rate.prediction.df$estimate.error),death.rate.prediction.df$estimate - (death.rate.prediction.df$estimate.error)),col=viridis(10),lty=3,lwd=1)
abline(v=c(min(Rsquared_criteria$radius),max(Rsquared_criteria$radius)),lty=3,col="red")
points(cluster.scale.vec ,empirical.clust.freq.stat.df$estimate,pch=19,)
arrows(cluster.scale.vec ,empirical.clust.freq.stat.df$estimate - (empirical.clust.freq.stat.df$estimate.error*1.96), cluster.scale.vec ,empirical.clust.freq.stat.df$estimate + (empirical.clust.freq.stat.df$estimate.error*1.96),angle=90,code=3,length=0.0)

legend(25,-1.5,col=viridis(10),legend=unique(death.rate.prediction.df$pred.diff),lwd=2,cex=0.5,bty="n",title="Diffusion rate prediction")
legend(25.1,-2.6,col="red",legend="",lwd=2,lty=3,cex=0.5,bty="n",title="DSP spatial scale")



head(death.rate.prediction.df)


#L.DSP.LE # Linear DSP with linear estimates
#L.DSP.SE # Linear DSP with spearman estimates
#S.DSP.LE # Spearman DSP with linear estimates
#S.DSP.SE # Spearman DSP with spearman estimates

#sig.lin.dsp <- DSP.summary.df[DSP.summary.df$L.DSP.LE.pvals <= 0.05,]

#sig.lin.dsp <- DSP.summary.df[DSP.summary.df$L.DSP.LE.estimate == min(DSP.summary.df$L.DSP.LE.estimate),]
#sig.lin.dsp <- DSP.summary.df[DSP.summary.df$L.DSP.LE.rsquareds == max(DSP.summary.df$L.DSP.LE.rsquareds),]

#### LINEAR estimates LINEAR DSP
### Plot with R sqaured thresholds
Rsquared_criteria <- DSP.summary.df[DSP.summary.df$L.DSP.LE.rsquareds >= 0.6,]
#Rsquared_criteria <- DSP.summary.df[DSP.summary.df$L.DSP.LE.rsquareds == max(DSP.summary.df$L.DSP.LE.rsquareds),]

dev.new(width=5, height=4)
par(mfrow=c(1,1),mai=c(0.7,0.9,0.1,0.1))
death.rate.prediction.df <- clust.freq.stat.df.lme4[clust.freq.stat.df.lme4 $pred.diff %in% c(2,2.25,2.5,2.75),] ## these are the diffusion rates predicted by the age specific mortality  
plot(cluster.scale.vec ,empirical.clust.freq.stat.df$estimate,xlab="",ylab="",yaxt="n",ylim=c(-3,-0.5),cex=0.5,pch=19)
arrows(cluster.scale.vec ,empirical.clust.freq.stat.df$estimate - (empirical.clust.freq.stat.df$estimate.error*1.96), cluster.scale.vec ,empirical.clust.freq.stat.df$estimate + (empirical.clust.freq.stat.df$estimate.error*1.96),angle=90,code=3,length=0.0)
mtext("spatial scale(m)",side=1,line=2.3)
mtext("ln(freq) ~ ln(clust size) slope",side=2,line=3)
axis(2,las=2)
abline(h= death.rate.prediction.df$estimate,col=viridis(10)[7:10])
abline(h= c(death.rate.prediction.df$estimate + (death.rate.prediction.df$estimate.error*1.96),death.rate.prediction.df$estimate - (death.rate.prediction.df$estimate.error*1.96)),col=viridis(10)[7:10],lty=3)
abline(v=(Rsquared_criteria$radius),lty=3,col="red")
points(cluster.scale.vec ,empirical.clust.freq.stat.df$estimate,pch=19,)
arrows(cluster.scale.vec ,empirical.clust.freq.stat.df$estimate - (empirical.clust.freq.stat.df$estimate.error*1.96), cluster.scale.vec ,empirical.clust.freq.stat.df$estimate + (empirical.clust.freq.stat.df$estimate.error*1.96),angle=90,code=3,length=0.0)




#### Spearman estimates LINEAR DSP
### Plot with R sqaured thresholds


head(DSP.summary.df)
Rsquared_criteria <- DSP.summary.df[DSP.summary.df$L.DSP.SE.rsquareds >= 0.6,]

dev.new(width=5, height=4)
par(mfrow=c(1,1),mai=c(0.7,0.9,0.1,0.1))
death.rate.prediction.df <- clust.freq.stat.df[clust.freq.stat.df$pred.diff %in% c(2,2.25,2.5,2.75),] ## these are the diffusion rates predicted by the age specific mortality  
plot(cluster.scale.vec ,empirical.clust.freq.stat.df$estimate,xlab="",ylab="",yaxt="n",ylim=c(-3,-0.5),cex=0.5,pch=19)
arrows(cluster.scale.vec ,empirical.clust.freq.stat.df$estimate - empirical.clust.freq.stat.df$estimate.error, cluster.scale.vec ,empirical.clust.freq.stat.df$estimate + empirical.clust.freq.stat.df$estimate.error,angle=90,code=3,length=0.0)
mtext("spatial scale(m)",side=1,line=2.3)
mtext("ln(freq) ~ ln(clust size) slope",side=2,line=3)
axis(2,las=2)
abline(h= death.rate.prediction.df$estimate,col=viridis(10)[7:10])
abline(h= c(death.rate.prediction.df$estimate + death.rate.prediction.df$estimate.error,death.rate.prediction.df$estimate - death.rate.prediction.df$estimate.error),col=viridis(10)[7:10],lty=3)
abline(v=(Rsquared_criteria$radius),lty=3,col="red")
points(cluster.scale.vec ,empirical.clust.freq.stat.df$estimate,pch=19,)
arrows(cluster.scale.vec ,empirical.clust.freq.stat.df$estimate - empirical.clust.freq.stat.df$estimate.error, cluster.scale.vec ,empirical.clust.freq.stat.df$estimate + empirical.clust.freq.stat.df$estimate.error,angle=90,code=3,length=0.0)







head(DSP.summary.df)
##3 with pvalue critera 

pval.critera <- DSP.summary.df[DSP.summary.df$L.DSP.LE.pvals <= 0.01,]

dev.new(width=5, height=4)
par(mfrow=c(1,1),mai=c(0.7,0.9,0.1,0.1))
death.rate.prediction.df <- clust.freq.stat.df[clust.freq.stat.df$pred.diff %in% c(2,2.25,2.5,2.75),] ## these are the diffusion rates predicted by the age specific mortality  
plot(cluster.scale.vec ,empirical.clust.freq.stat.df$estimate,xlab="",ylab="",yaxt="n",ylim=c(-3,-0.5),cex=0.5,pch=19)
arrows(cluster.scale.vec ,empirical.clust.freq.stat.df$estimate - empirical.clust.freq.stat.df$estimate.error, cluster.scale.vec ,empirical.clust.freq.stat.df$estimate + empirical.clust.freq.stat.df$estimate.error,angle=90,code=3,length=0.0)
mtext("spatial scale(m)",side=1,line=2.3)
mtext("ln(freq) ~ ln(clust size) slope",side=2,line=3)
axis(2,las=2)
abline(h= death.rate.prediction.df$estimate,col=viridis(10)[7:10])
abline(h= c(death.rate.prediction.df$estimate + death.rate.prediction.df$estimate.error,death.rate.prediction.df$estimate - death.rate.prediction.df$estimate.error),col=viridis(10)[7:10],lty=3)

abline(v=(pval.critera $radius),lty=3,col="red")
points(cluster.scale.vec ,empirical.clust.freq.stat.df$estimate,pch=19,)
arrows(cluster.scale.vec ,empirical.clust.freq.stat.df$estimate - empirical.clust.freq.stat.df$estimate.error, cluster.scale.vec ,empirical.clust.freq.stat.df$estimate + empirical.clust.freq.stat.df$estimate.error,angle=90,code=3,length=0.0)


























par(mfrow=c(1,1),mai=c(0.7,0.7,0.1,0.1))
death.rate.prediction.df <- clust.freq.stat.df[clust.freq.stat.df$pred.diff %in% c(2,2.25,2.5),]
death.rate.prediction.df <- clust.freq.stat.df
plot(cluster.scale.vec ,empirical.clust.freq.stat.df$estimate,xlab="",ylab="",yaxt="n",ylim=c(-4,-0),cex=0.5,pch=19)
#points(cluster.scale.vec ,empirical.clust.freq.stat.df$estimate + empirical.clust.freq.stat.df$estimate.error, cex=0.5)
#points(cluster.scale.vec ,empirical.clust.freq.stat.df$estimate - empirical.clust.freq.stat.df$estimate.error, cex=0.5)
arrows(cluster.scale.vec ,empirical.clust.freq.stat.df$estimate - empirical.clust.freq.stat.df$estimate.error, cluster.scale.vec ,empirical.clust.freq.stat.df$estimate + empirical.clust.freq.stat.df$estimate.error,angle=90,code=3,length=0.01)

mtext("spatial scale(m)",side=1,line=2.3)
mtext("ln(freq) ~ ln(clust size) slope",side=2,line=3)
axis(2,las=2)
abline(h= death.rate.prediction.df $estimate,col=viridis(10))
#abline(v=24)
#abline(v=13)


abline(v=16) ## highest linear linear pooling 
abline(v=26) ## highest linear linear pooling 

