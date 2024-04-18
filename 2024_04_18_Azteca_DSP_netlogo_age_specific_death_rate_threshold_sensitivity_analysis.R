
setwd("/users/cx21atek/desktop/Azteca_DSP_local/input data")


az.df <- read.csv("2023_11_15_Azteca_DSP_empirical_death_rates_full_df.csv")
head(az.df)

### get the mean mortalities for the data 
linear.age_class_mort_mean <- numeric(max(az.df$age)) # for each age 
linear.age_class_mort_error <- numeric(max(az.df$age)) 
logged.age_class_mort_mean <- numeric(max(az.df$age)) # for each age 
logged.age_class_mort_error <- numeric(max(az.df$age)) 

for(avg.age.mort.looper in 1:max(az.df$age)){
	#avg.age.mort.looper <- 4
	an_class_data <- az.df[az.df$age == avg.age.mort.looper,]$abs.death

	linear.age.mort.vec <- an_class_data
	linear.age_class_mort_mean[avg.age.mort.looper] <- mean(linear.age.mort.vec) 
	linear.age_class_mort_error[avg.age.mort.looper ]<-  sd(linear.age.mort.vec)/sqrt(length(linear.age.mort.vec))
	
	logged.age.mort.vec <- log(an_class_data)
	logged.age_class_mort_mean[avg.age.mort.looper] <- mean(logged.age.mort.vec) 
	logged.age_class_mort_error[avg.age.mort.looper ]<-  sd(logged.age.mort.vec)/sqrt(length(logged.age.mort.vec))
}




#################################################################################
############# Empirical Age specific mortality all data figure with means ####### 
#################################################################################
########################## TRUNCATED FIT MODEL ##############
head(az.df)
### Here is the truncated fit for all of the data. using 6 year old as the cuttoff 
log_age <- log(seq(1,13,1))
emp.xval.raw <- log_age
emp.yval.raw  <- logged.age_class_mort_mean
emp.trunc.y <- logged.age_class_mort_mean[log_age < log(6)]
emp.trunc.x <- log_age[log_age < log(6)]

emp.trunc.df <- data.frame(emp.trunc.y, emp.trunc.x)











##### Now reading in the netlogo data 


model.df <- read.csv("2023_11_19_azteca_DSP_netlogo_mortality_df_with_replicates.csv")
head(model.df)
model.df$death.rate.abs <- abs(model.df$death.rate.avg)
head(model.df)
pred.diff.vec <- unique(model.df $pred.diff)


df.names <- c("trunc.thres","pred.diff","estimate", "uppper.estimate", "lower.estimates")
model.mort.sens.analysis.df <- data.frame(matrix(0,ncol=length(df.names)))
colnames(model.mort.sens.analysis.df) <- df.names

for(trunc.looper in seq(2,3.9,0.1)){

truc.threshold <- trunc.looper
trunc.thres.vals <- numeric(length(pred.diff.vec))
estimates <- numeric(length(pred.diff.vec))
upp.lin.estimates <- numeric(length(pred.diff.vec))
lower.linestimates <- numeric(length(pred.diff.vec))


#library(brms)
for(x in 1:length(pred.diff.vec)){
#x <- 1 

a.diff.df <- model.df[model.df $pred.diff == pred.diff.vec[x], ]


## clean up the data from NAs and NaNs
clean.diff.df <- a.diff.df[!is.na(log(a.diff.df$death.rate.abs)) & !is.nan(log(a.diff.df$death.rate.abs)) & log(a.diff.df$death.rate.abs) > -Inf, ]

clean.diff.df$log.age <- log(clean.diff.df$age)
head(clean.diff.df)

trunc.df <- clean.diff.df[clean.diff.df$log.age <= truc.threshold, ]

head(trunc.df)
mixed.eff.trunc.model <- lmer(log(death.rate.abs) ~ log.age + (1 |replicate),data= trunc.df)
summary(mixed.eff.trunc.model)


estimates[x] <- summary(mixed.eff.trunc.model)$coeff[2]
upp.lin.estimates[x] <- summary(mixed.eff.trunc.model)$coeff[2] + summary(mixed.eff.trunc.model)$coeff[4]
lower.linestimates[x] <- summary(mixed.eff.trunc.model)$coeff[2] -  summary(mixed.eff.trunc.model)$coeff[4]


## put together the dataframe
tempper.df <- data.frame(truc.threshold,pred.diff.vec[x], estimates[x], upp.lin.estimates[x], lower.linestimates[x])
colnames(tempper.df) <- df.names


model.mort.sens.analysis.df  <- rbind(model.mort.sens.analysis.df, tempper.df)


} # end of big loop

}## end of looping through trunc thresholds 



model.mort.sens.analysis.df <- model.mort.sens.analysis.df[model.mort.sens.analysis.df$trunc.thres != 0, ]
head(model.mort.sens.analysis.df)
trunc.vals <- unique(model.mort.sens.analysis.df$trunc.thres)



empirical.linear.model <- lm(emp.trunc.df$emp.trunc.y ~ emp.trunc.df$emp.trunc.x)

dev.new(width=10, height=7.5)
par(mfrow=c(4,5),mai=c(0.6,0.6,0.1,0.1),oma=c(0,0,1,01))
for(x in 1:20){

 #x <- 1
trunc.df <- model.mort.sens.analysis.df[model.mort.sens.analysis.df$trunc.thres == trunc.vals[x], ]

pred.diff.vec <- trunc.df$pred.diff
estimates <- trunc.df$estimate
upp.lin.estimates <- trunc.df$uppper.estimate
lower.linestimates <- trunc.df$lower.estimates
## adding the emprical slope
#abline(-3.1,log.log.mean.model$coeff[2],col="red",lwd=3)

plot(pred.diff.vec ,estimates,ylim=c(-0.75,-0.1),col=viridis(length(pred.diff.vec)),pch=19,yaxt="n",xlab="",ylab="")
points(pred.diff.vec ,upp.lin.estimates,col=viridis(length(pred.diff.vec)),pch=1,cex=0.5)
points(pred.diff.vec ,lower.linestimates,col=viridis(length(pred.diff.vec)),pch=1,cex=0.5)
mtext(expression(estimate["age class death rate"]),side=2,line=2.6,cex=0.6)
mtext("Consumer diffusion rate",side=1,line=2.2,cex=0.6)
axis(2,las=2)
mtext(paste("thres age:",trunc.vals[x]),cex=0.7,line=0.2)
#abline(h= empirical.linear.model,col="red")


## need to get empirical values from the comparision R script. 
abline(h=summary(empirical.linear.model)$coeff[2],col="red",lwd=1)
abline(h=summary(empirical.linear.model)$coeff[2] + summary(empirical.linear.model)$coeff[4],col="red",lwd=1,lty=3)
abline(h=summary(empirical.linear.model)$coeff[2] - summary(empirical.linear.model)$coeff[4],col="red",lwd=1,lty=3)


}










##### OIncluding more and more data to the right to see when regression flattens out. 

# the idea here is to look at see when hte slopes approach zero. to try to see where the flatting out starts. 
df.names <- c("trunc.thres","pred.diff","estimate", "uppper.estimate", "lower.estimates","window.size")
model.mort.sens.analysis.df <- data.frame(matrix(0,ncol=length(df.names)))
colnames(model.mort.sens.analysis.df) <- df.names
trun.thres.vals.vec <- (seq(1,7,0.1))
window.size <- 1 ## how many points to include 
trunc.thres.vals <- numeric(length(trun.thres.vals.vec))
estimates <- numeric(length(trun.thres.vals.vec))
upp.lin.estimates <- numeric(length(trun.thres.vals.vec))
lower.linestimates <- numeric(length(trun.thres.vals.vec))


#dev.new(width=10, height=7.5)
#par(mfrow=c(4,5),mai=c(0.6,0.6,0.1,0.1),oma=c(0,0,1,01))
#library(brms)
for(x in 1:length(pred.diff.vec)){
#x <- 1

a.diff.df <- model.df[model.df $pred.diff == pred.diff.vec[x], ]
head(a.diff.df)
plot(log(a.diff.df$age),log(a.diff.df $death.rate.abs),cex=0.1,xlab="",ylab="")
mtext("ln(age class)",side=1,line=2.2)
mtext("ln(death rate)",side=2,line=2.2)


for(trunc.looper in 1:length(trun.thres.vals.vec)){
#trunc.looper <- 1
truc.threshold <- trun.thres.vals.vec[trunc.looper]


## clean up the data from NAs and NaNs
clean.diff.df <- a.diff.df[!is.na(log(a.diff.df$death.rate.abs)) & !is.nan(log(a.diff.df$death.rate.abs)) & log(a.diff.df$death.rate.abs) > -Inf, ]

clean.diff.df$log.age <- log(clean.diff.df$age)
head(clean.diff.df)

trunc.df <- clean.diff.df[clean.diff.df$log.age <= truc.threshold, ]
head(trunc.df)

#points(trunc.df$log.age,log(trunc.df$death.rate.abs),col=viridis(length(trun.thres.vals.vec))[trunc.looper],cex=0.2)

if(dim(trunc.df)[1] > 3 & length(unique(trunc.df$replicate)) > 1 & length(unique(trunc.df$age)) > 1){ ## need data and multiple groups to estimate the slope 
mixed.eff.trunc.model <- lmer(log(death.rate.abs) ~ log.age + (1 |replicate),data= trunc.df)
summary(mixed.eff.trunc.model)
abline(summary(mixed.eff.trunc.model)$coeff[1],summary(mixed.eff.trunc.model)$coeff[2],col=viridis(length(trun.thres.vals.vec))[trunc.looper],lwd=2)

#plot(trunc.df$log.age,log(trunc.df$death.rate.abs))


estimates[trunc.looper] <- summary(mixed.eff.trunc.model)$coeff[2]
upp.lin.estimates[trunc.looper] <- summary(mixed.eff.trunc.model)$coeff[2] + summary(mixed.eff.trunc.model)$coeff[4]
lower.linestimates[trunc.looper] <- summary(mixed.eff.trunc.model)$coeff[2] -  summary(mixed.eff.trunc.model)$coeff[4]

} ## need data to run the mixed effect model
} # end looping truncation thresholds 


#plot(trun.thres.vals.vec, estimates)
#points(trun.thres.vals.vec, upp.lin.estimates,cex=0.5)
#points(trun.thres.vals.vec, lower.linestimates,cex=0.5)
#abline(h=0)

## put together the dataframe
tempper.df <- data.frame(trun.thres.vals.vec,pred.diff.vec[x], estimates, upp.lin.estimates, lower.linestimates,window.size)
colnames(tempper.df) <- df.names

model.mort.sens.analysis.df  <- rbind(model.mort.sens.analysis.df, tempper.df)

}## end of looping though differetn predator diffusion rates 





model.mort.sens.analysis.df <- model.mort.sens.analysis.df[model.mort.sens.analysis.df$pred.diff != 0, ]
head(model.mort.sens.analysis.df)
par(mfrow=c(1,1))
plot(NA ,xlim=c(1,6),ylim=c(-1.5,0.2),xlab="",ylab="")
mtext("Resource age",side=1,line=2.2)
mtext("Estimated age-specific mortality",side=2,line=2.2)
abline(h=0,lty=2)
abline(v=seq(2,2.5,0.5),lty=3)
for(X in 1:10){
	#X <- 1
	df <- model.mort.sens.analysis.df[model.mort.sens.analysis.df$pred.diff == pred.diff.vec[X], ]
	points(df $trunc.thres ,(df $estimate),type="l",col=viridis(10)[X])
	#points(df $trunc.thres ,(df $lower.estimates),type="l",col=viridis(10)[X])
	#points(df $trunc.thres ,(df $uppper.estimate),type="l",col=viridis(10)[X])
	points(df $trunc.thres ,(df $estimate),type="p",col=viridis(10)[X],cex=0.5,pch=19)


}







model.mort.sens.analysis.df <- model.mort.sens.analysis.df[model.mort.sens.analysis.df$pred.diff != 0, ]
head(model.mort.sens.analysis.df)
par(mfrow=c(1,1))
plot(NA ,xlim=c(1,6),ylim=c(-0.5,1))
for(X in 1:10){
	#X <- 1
	df <- model.mort.sens.analysis.df[model.mort.sens.analysis.df$pred.diff == pred.diff.vec[X], ]
	points(df $trunc.thres ,log(df $estimate+2),type="l",col=viridis(10)[X])
	points(df $trunc.thres ,log(df $estimate+2),type="p",col=viridis(10)[X],cex=0.6,pch=19)

	#points(df $trunc.thres ,(df $lower.estimates),type="l",col=viridis(10)[X])
	#points(df $trunc.thres ,(df $uppper.estimate),type="l",col=viridis(10)[X])

}

abline(v=2.5)
abline(v=2)
abline(v=1.5)
abline(v=3)

abline(h=log(0+2),lty=3)






#### Older sensitivity things that I dont think we should include. 


#### This one is moving backwards trying to detect a threshold. When does it go from postiive fo flat??
dev.new(width=3.5, height=3.3)
par(mfrow=c(1,1),mai=c(0.7,0.7,0.1,0.1))
## what threshold for the truncate fit? 
truc.threshold <- 1.6
xmax.val <- log(500)# is the max we can have 
ymin.val <- -5.5 # for full data 
plot(NA,ylim=c(ymin.val,-2.5),xlim=c(0, xmax.val),yaxt="n",xlab="",ylab="")
mtext("ln(age class)",side=1,line=2.2)
mtext("ln(death rate)",side=2,line=2.6)
axis(2,las=2)
abline(v= truc.threshold,lty=3)

### Trying to detect a threshold in the data 
# the idea here is to look at see when hte slopes approach zero. to try to see where the flatting out starts. 
df.names <- c("trunc.thres","pred.diff","estimate", "uppper.estimate", "lower.estimates")
model.mort.sens.analysis.df <- data.frame(matrix(0,ncol=length(df.names)))
colnames(model.mort.sens.analysis.df) <- df.names
trun.thres.vals.vec <- rev(seq(0.001,6,0.1))


trunc.thres.vals <- numeric(length(trun.thres.vals.vec))
estimates <- numeric(length(trun.thres.vals.vec))
upp.lin.estimates <- numeric(length(trun.thres.vals.vec))
lower.linestimates <- numeric(length(trun.thres.vals.vec))

## 

#library(brms)
for(x in 1:length(pred.diff.vec)){
#x <- 1 

a.diff.df <- model.df[model.df $pred.diff == pred.diff.vec[x], ]

for(trunc.looper in 1:length(trun.thres.vals.vec)){

truc.threshold <- trun.thres.vals.vec[trunc.looper]


## clean up the data from NAs and NaNs
clean.diff.df <- a.diff.df[!is.na(log(a.diff.df$death.rate.abs)) & !is.nan(log(a.diff.df$death.rate.abs)) & log(a.diff.df$death.rate.abs) > -Inf, ]

clean.diff.df$log.age <- log(clean.diff.df$age)
head(clean.diff.df)

trunc.df <- clean.diff.df[clean.diff.df$log.age >= truc.threshold , ]
head(trunc.df)
if(dim(trunc.df)[1] > 3 & length(unique(trunc.df$replicate)) > 1 ){ ## need data and multiple groups to estimate the slope 
mixed.eff.trunc.model <- lmer(log(death.rate.abs) ~ log.age + (1 |replicate),data= trunc.df)
summary(mixed.eff.trunc.model)

#plot(trunc.df$log.age,log(trunc.df$death.rate.abs))


estimates[trunc.looper] <- summary(mixed.eff.trunc.model)$coeff[2]
upp.lin.estimates[trunc.looper] <- summary(mixed.eff.trunc.model)$coeff[2] + summary(mixed.eff.trunc.model)$coeff[4]
lower.linestimates[trunc.looper] <- summary(mixed.eff.trunc.model)$coeff[2] -  summary(mixed.eff.trunc.model)$coeff[4]

} ## need data to run the mixed effect model
} # end looping truncation thresholds 


## put together the dataframe
tempper.df <- data.frame(trun.thres.vals.vec,pred.diff.vec[x], estimates, upp.lin.estimates, lower.linestimates)
colnames(tempper.df) <- df.names

model.mort.sens.analysis.df  <- rbind(model.mort.sens.analysis.df, tempper.df)

}## end of looping though differetn predator diffusion rates 


model.mort.sens.analysis.df <- model.mort.sens.analysis.df[model.mort.sens.analysis.df$pred.diff != 0, ]
head(model.mort.sens.analysis.df)

plot(NA ,xlim=c(0,6.5),ylim=c(-0.1,4))
abline(h=0)
for(X in 1:10){
	#X <- 1
	df <- model.mort.sens.analysis.df[model.mort.sens.analysis.df$pred.diff == pred.diff.vec[X], ]
	points(df $trunc.thres ,(df $estimate),type="l",col=viridis(10)[X])
	points(df $trunc.thres ,(df $lower.estimates),type="l",col=viridis(10)[X])
	points(df $trunc.thres ,(df $uppper.estimate),type="l",col=viridis(10)[X])

}

plot(df $trunc.thres,log(df $estimate),type="l",col=viridis(10)[X])
log(0.000000000000000000000000000000000001)


#### Moving window sensitivity 
par(mfcol=c(2,5),mai=c(0.3,0.4,0.1,0.1),oma=c(1,0,1,0))
##### Trying a moving window approach here now 
# the idea here is to look at see when hte slopes approach zero. to try to see where the flatting out starts. 
df.names <- c("trunc.thres","pred.diff","estimate", "uppper.estimate", "lower.estimates","window.size")
model.mort.sens.analysis.df <- data.frame(matrix(0,ncol=length(df.names)))
colnames(model.mort.sens.analysis.df) <- df.names
trun.thres.vals.vec <- (seq(1,6,1))
window.size <- 1 ## how many points to include 
trunc.thres.vals <- numeric(length(trun.thres.vals.vec))
estimates <- numeric(length(trun.thres.vals.vec))
upp.lin.estimates <- numeric(length(trun.thres.vals.vec))
lower.linestimates <- numeric(length(trun.thres.vals.vec))

#library(brms)
for(x in 1:length(pred.diff.vec[1:5])){
#x <- 5

a.diff.df <- model.df[model.df $pred.diff == pred.diff.vec[x], ]
head(a.diff.df)
plot(log(a.diff.df$age),log(a.diff.df $death.rate.abs),cex=0.1,yaxt="n",ylab="",xlab="")
axis(2,las=2)
mtext(paste("pred diff:",pred.diff.vec[x]),cex=0.5,line=0.2)
mtext("log(age class)",side=1,cex=0.5,line=1.6)
mtext("log(death rate)",side=2,cex=0.5,line=2)

for(trunc.looper in 1:length(trun.thres.vals.vec)){
#trunc.looper <- 3
truc.threshold <- trun.thres.vals.vec[trunc.looper]


## clean up the data from NAs and NaNs
clean.diff.df <- a.diff.df[!is.na(log(a.diff.df$death.rate.abs)) & !is.nan(log(a.diff.df$death.rate.abs)) & log(a.diff.df$death.rate.abs) > -Inf, ]

clean.diff.df$log.age <- log(clean.diff.df$age)
head(clean.diff.df)

trunc.df <- clean.diff.df[clean.diff.df$log.age >= truc.threshold & clean.diff.df$log.age <= truc.threshold + window.size, ]
head(trunc.df)

points(trunc.df$log.age,log(trunc.df$death.rate.abs),col=viridis(length(trun.thres.vals.vec))[trunc.looper],cex=0.2)

if(dim(trunc.df)[1] > 3 & length(unique(trunc.df$replicate)) > 1 & length(unique(trunc.df$age)) > 1){ ## need data and multiple groups to estimate the slope 
mixed.eff.trunc.model <- lmer(log(death.rate.abs) ~ log.age + (1 |replicate),data= trunc.df)
summary(mixed.eff.trunc.model)
abline(summary(mixed.eff.trunc.model)$coeff[1],summary(mixed.eff.trunc.model)$coeff[2],col=viridis(length(trun.thres.vals.vec))[trunc.looper],lwd=1)

#plot(trunc.df$log.age,log(trunc.df$death.rate.abs))


estimates[trunc.looper] <- summary(mixed.eff.trunc.model)$coeff[2]
upp.lin.estimates[trunc.looper] <- summary(mixed.eff.trunc.model)$coeff[2] + summary(mixed.eff.trunc.model)$coeff[4]
lower.linestimates[trunc.looper] <- summary(mixed.eff.trunc.model)$coeff[2] -  summary(mixed.eff.trunc.model)$coeff[4]

} ## need data to run the mixed effect model
} # end looping truncation thresholds 


plot(trun.thres.vals.vec, estimates,yaxt="n",xlab="",ylab="",ylim=c(-1,1),cex=0.5,pch=19)
mtext("age class window",side=1,cex=0.5,line=1.6)
mtext("estimate",side=2,cex=0.5,line=2.3)

points(trun.thres.vals.vec, estimates,type="l")
axis(2,las=2)
points(trun.thres.vals.vec, upp.lin.estimates,cex=0.5,type="l",lty=3)
points(trun.thres.vals.vec, lower.linestimates,cex=0.5,type="l",lty=3)
abline(h=0,lty=2)
points(trun.thres.vals.vec, estimates,col=viridis(6),pch=19,cex=1.5)



## put together the dataframe
tempper.df <- data.frame(trun.thres.vals.vec,pred.diff.vec[x], estimates, upp.lin.estimates, lower.linestimates,window.size)
colnames(tempper.df) <- df.names

model.mort.sens.analysis.df  <- rbind(model.mort.sens.analysis.df, tempper.df)

}## end of looping though differetn predator diffusion rates 


model.mort.sens.analysis.df <- model.mort.sens.analysis.df[model.mort.sens.analysis.df$pred.diff != 0, ]
head(model.mort.sens.analysis.df)



par(mfrow=c(2,5))
for(X in 1:10){
	#X <- 1
	df <- model.mort.sens.analysis.df[model.mort.sens.analysis.df$pred.diff == pred.diff.vec[X], ]
	head(df)
	plot(df$trunc.thres, df$estimate,yaxt="n",xlab="",ylab="",ylim=c(-0.6,0.6),cex=0.5,pch=19)
	mtext("age class window",side=1,cex=0.5,line=1.6)
	mtext("estimate",side=2,cex=0.5,line=2.3)

	points(df$trunc.thres, df$estimate,type="l")
	axis(2,las=2)
	points(df$trunc.thres, df$uppper.estimate,cex=0.5,type="l",lty=3)
	points(df$trunc.thres, df$lower.estimates,cex=0.5,type="l",lty=3)
	abline(h=0,lty=1)
	abline(v=c(2,2.5,3,3.5),lty=4,col="red")

}








plot(NA ,xlim=c(0,6.5),ylim=c(-1,1))
abline(h=0)
for(X in 1:10){
	#X <- 1
	df <- model.mort.sens.analysis.df[model.mort.sens.analysis.df$pred.diff == pred.diff.vec[X], ]
	points(df $trunc.thres ,(df $estimate),type="l",col=viridis(10)[X])
	points(df $trunc.thres ,(df $lower.estimates),type="l",col=viridis(10)[X])
	points(df $trunc.thres ,(df $uppper.estimate),type="l",col=viridis(10)[X])

}



