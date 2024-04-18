
#dir()
#setwd("/Users/zhajianf/Desktop/research 2019/Azteca irlanda hamburgo/Azteca spatial demography article ")
#setwd("/Users/zhajianf/Desktop/July 2021 desktop/Oct 30 desktop/research 2019/Azteca irlanda hamburgo/azteca spatial demography article ")


rm(list=ls())
#dir()

setwd("/users/cx21atek/desktop/desktop 1.0/Azteca_DSP_local/input data")


#data<-read.csv("AZTECA_PHORID_MODEL_V_July19_2019_PR_for_article_pred_difussion_sweep difussion 0.5-2 by .25 for article-table.csv",header=T,skip=6)## 10 replicates of each random walk parameter ## THIS DATA DOES NOT HAVE THE AGES TO CALC DEATHRATE

#data <- read.csv("AZTECA_PHORID_MODEL_V_July19_2019_PR_for_article_pred_difussion_sweep difussion 0.5-2 by .25 for article-table_11AM_FINAL?FIELD.csv",header=T,skip=6)

rm(list=ls())
data <- read.csv("azteca_phorid_model_data_for_paper_Feb27_202.csv",header=T,skip=6)
library(gdata)

head(data)
dim(data)
head(data)

data$replicate <- data$X.run.number.
data$sim.step <- data$X.step.



pred.diff.val.vec <- unique(data$normal_mean_walk)

final.death.rate.df <- data.frame(0,0,0,0,0)
colnames(final.death.rate.df) <- c("pred.diff","replicate","age","death.rate.avg","death.rate.sd")
for(walk.val.looper in 1:length(pred.diff.val.vec)){

cat("processing pred diff:", walk.val.looper,"\n")
#walk.val.looper <- 3

a.walk.df <- data[data$normal_mean_walk == pred.diff.val.vec[walk.val.looper], ]


par.set.death.rate.mat.list <- list(NA)
par.set.death.rate.mat.list.indexer <- 1 
for(replicate.looper in unique(a.walk.df$replicate)){### going through the unique replicates of the simluations 
cat("processing replicate:", replicate.looper,"\n")

#replicate.looper <- unique(a.walk.df$replicate)[2]
a.rep.df <- a.walk.df[a.walk.df$replicate == replicate.looper,  ]


biglist <- as.character(a.rep.df$biglist[length(a.rep.df$biglist)]) # this takes the biglist from the final step in the simulation
#biglist[1]
	
age_dist_lists <- unlist(strsplit(biglist, split = "\\] \\[")) # this breaks apart age dist lists. So we have the frequency distribution fo cluster siezes for the last 100 steps in the simulation
#age_dist_lists[1] # now you can look at the data in individual simulation steps 

	nest_age_ts_list <- list(NA)
	## This works on the last 50 nest age distributions
	for(time.steps.for.age.list in 1:length(age_dist_lists)){
		#time.steps.for.age.list <- 2
		split_age <- unlist(strsplit(age_dist_lists[time.steps.for.age.list], split = " ")) # split character into sepearte numbers
		split_age_first <- split_age[1] # get first one
		split_age_last <- split_age[length(split_age)] # get last one
		split_age[1] <- gsub("\\[\\[","",split_age_first) # replace it without brackets
		split_age[length(split_age)] <- gsub("\\]\\]","",split_age_last) # replace it without brackets
		age_dist_num <- as.numeric(split_age) # now age distribution is numeric again

		nest_age_ts_list[[time.steps.for.age.list]] <- age_dist_num
	}
	length(nest_age_ts_list) ## holds the ages of every nest for the last 100 time steps of the simluation 


### This below looks at indivial ticks for the age distribution
age_frequency_ts_df <- data.frame(matrix(nrow=500,ncol=100)) # 500 rows (ages) 100 columns (time steps)
colnames(age_frequency_ts_df) <- seq(1,100,1)
for(age.list.time.step.looper in 1:length(nest_age_ts_list) ){
	#age.list.time.step.looper <- 1
	a.ts.age.vec <- nest_age_ts_list[[age.list.time.step.looper]]
	
	ts.age.freq <- numeric(500) #make the max age 500 (no nests go that far)
	for(x in 1:length(ts.age.freq)){
		ts.age.freq[x] <- sum(a.ts.age.vec == x)
	}	
	
	age_frequency_ts_df[, age.list.time.step.looper] <- ts.age.freq
}

head(age_frequency_ts_df)

## what do the age distributions look like? 
##a_dist <- age_frequency_ts_df[[1]]
#index <- seq(1,length(a_dist),1)
#plot(log(index) ,log(a_dist))
#plot((index) ,(a_dist))

par(mfrow=c(1,1),mai=c(0.8,0.8,0.1,0.1))
plot((index) ,log(a_dist),xlab="",ylab="",yaxt="n")
axis(2,las=2) ## ages are exponentially distributed more or less
mtext("Resource age class",side=1,line=2.2)
mtext("log(frequency)",side=2,line=2.6)

##### 
## now that I have the frequency distribtion of nest ages I can calculate the death rate
head(age_frequency_ts_df)
dim(age_frequency_ts_df) ## frist number is the max age the second number is the number of time steps

death_rate_matrix <-matrix(NA,ncol=dim(age_frequency_ts_df)[2]-1,nrow=dim(age_frequency_ts_df)[1])
for(time.step.death.rate.looper in (dim(age_frequency_ts_df)[2]-1):1){
	#time.step.death.rate.looper <- 99
	year_transition_vector <- numeric(dim(age_frequency_ts_df)[1])
	for(age.class.death.rate.looper in 1:dim(age_frequency_ts_df)[1]){ ## going through all of the ages 
		#age.class.death.rate.looper <- 2
		this_year_age <- age_frequency_ts_df[age.class.death.rate.looper, time.step.death.rate.looper]
		next_year_age_trans <- age_frequency_ts_df[age.class.death.rate.looper + 1, time.step.death.rate.looper + 1]
		
		## how many of age1 at time t1 became age2 at t2
		age.specific.death.rate.val <- (next_year_age_trans - this_year_age)/this_year_age
		
		## store the result
		year_transition_vector[age.class.death.rate.looper] <- age.specific.death.rate.val
	}
	death_rate_matrix[, time.step.death.rate.looper] <- year_transition_vector # replaced it with a dataframe to pool across samples 
	
}## end of putting to gether the death rate df 


## here is the death rate matrix for a single run of the model for a given parameter 
## each column is a death transition from t1 -> t2 
## each row is the death rate for the ageclass at the point in time.
colnames(death_rate_matrix) <- paste(rep("ts:",99), seq(1,99,1)) ## 99 transitions
rownames(death_rate_matrix) <- paste(rep("age:",500), seq(1,500,1)) ## 500 age classes

head(death_rate_matrix)

## if we transpose then we make each column an age and each row is across the time steps 
age_mortality_mat <- t(death_rate_matrix)

#### Update Nov 19 2023
# instead of pooling together the runs for a given parameter set we are going to keep them isolated so that we can model
## the varability in trends with a hierarhical model 

### Here we can calculate the means to look at them 
age_class_mort_mean <- numeric(dim(age_mortality_mat)[2])
age_class_mort_sd <- numeric(dim(age_mortality_mat)[2])
for(avg.age.death.rate.age.looper in 1:dim(age_mortality_mat)[2]){ ## looping through the ages 
	#avg.age.death.rate.age.looper <- 50
	an_class_data <- age_mortality_mat[, avg.age.death.rate.age.looper]
	age_class_morting <- an_class_data[!is.nan(an_class_data)]
	age_class_morting.clean <- age_class_morting[!is.na(age_class_morting)]

	age_class_mort_mean[avg.age.death.rate.age.looper] <- mean(age_class_morting.clean)
	age_class_mort_sd[avg.age.death.rate.age.looper] <- sd(age_class_morting.clean)
}

## making a figure of the death rates from the model. 
par(mfrow=c(1,1),mai=c(0.9,0.9,0.1,0.1))
index <- seq(1,500,1)
plot((index),abs(age_class_mort_mean),xlab="",ylab="",yaxt="n",ylim=c(0,0.03),xlim=c(0,225))
mtext("Resource age class",line=2.2,side=1)
mtext("Death rate",line=3.5,side=2)
axis(2,las=2)



par(mfrow=c(1,1),mai=c(0.9,0.9,0.1,0.1))
index <- seq(1,500,1)
plot(log((index)),log(abs(age_class_mort_mean)),xlab="",ylab="",yaxt="n")
mtext("ln(Resource age class)",line=2.2,side=1)
mtext("ln(Death rate)",line=3,side=2)
axis(2,las=2)



temp.df <- data.frame(pred.diff.val.vec[walk.val.looper], replicate.looper ,seq(1,500,1),age_class_mort_mean, age_class_mort_sd)

colnames(temp.df) <- c("pred.diff","replicate","age","death.rate.avg","death.rate.sd")

final.death.rate.df <- rbind(final.death.rate.df, temp.df)

} ## end of going through a given par combinations replicates 

} # end of looping through predator diffusion 


final.death.rate.df <- final.death.rate.df[final.death.rate.df$pred.diff != 0, ] # getting rid of the first dummy row
head(final.death.rate.df) 

#write.csv(final.death.rate.df,"2023_11_19_azteca_DSP_netlogo_mortality_df_with_replicates.csv",row.names=F)

dir()

final.death.rate.df <- read.csv("2023_11_19_azteca_DSP_netlogo_mortality_df_with_replicates.csv")

final.death.rate.df$death.rate.abs <- abs(final.death.rate.df$death.rate.avg)
pred.diff.vec <- unique(final.death.rate.df$pred.diff)

library(viridis)



#### THIS IS THE FIGURE FOR THE SUPPLAMENTARY MATERIAL 
### Running the mixed effects models 
dev.new(width=10.4, height=5.3)
par(mfcol=c(2,4),mai=c(0.5,0.5,0.1,0.1),oma=c(0,0,1,0))

for(trunc.looper in seq(2,2.6,0.2)){

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
empirical.estimate <- -0.3217294
empirical.estimate.upper <- -0.3041126
empirical.estimate.lower <- -0.3393463
abline(h= empirical.estimate,col="red")
abline(h= c(empirical.estimate.upper, empirical.estimate.lower ),col="red",lty=3)



}


legend(2,-0.4,legend= pred.diff.vec,col=viridis(length(pred.diff.vec)),pch=19,cex=0.7,bty="n",title="consumer diff")


empirical.estimate <- summary(log.log.mean.model)$coeff[2] 
empirical.estimate.upper <- summary(log.log.mean.model)$coeff[2]  + summary(log.log.mean.model)$coeff[4]
empirical.estimate.lower<- summary(log.log.mean.model)$coeff[2]  - summary(log.log.mean.model)$coeff[4]




abline(h= mean.truc.emprical.log.log.fit)





dev.new(width=6.8,height=3.5)
par(mfrow=c(1,2),mai=c(0.7,0.7,0.1,0.1))

truc.threshold <- 2.25

estimates <- numeric(length(pred.diff.vec))
upper.estimates <- numeric(length(pred.diff.vec))
lower.estimates <- numeric(length(pred.diff.vec))

intercepts <- numeric(length(pred.diff.vec))

final.death.rate.df$death.rate.abs <- abs(final.death.rate.df$death.rate.avg)
pred.diff.vec <- unique(final.death.rate.df$pred.diff)

plot(NA,ylim=c(-9,-0),xlim=c(0,log(500)),yaxt="n",xlab="",ylab="")
mtext("ln(Resource age class)",side=1,line=2.2,cex=0.8)
mtext("ln(Resource death rate)",side=2,line=2.2,cex=0.8)
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


#dev.new(width=3.5, height=3.3)
#par(mfrow=c(1,1),mai=c(0.7,0.8,0.2,0.1))
plot(pred.diff.vec, estimates,ylim=c(-0.8,-0.1),col=viridis(length(pred.diff.vec)),pch=19,yaxt="n",xlab="",ylab="")
#points(pred.diff.vec, upper.estimates,col=viridis(length(pred.diff.vec)),cex=0.5)
#points(pred.diff.vec, lower.estimates,col=viridis(length(pred.diff.vec)),cex=0.5)
arrows(pred.diff.vec, upper.estimates, pred.diff.vec,lower.estimates ,,col=viridis(length(pred.diff.vec)),code=3,angle=90,length=0.0,lwd=3)
mtext(expression(estimate["age class death rate"]),side=2,line=2.6,cex=0.8)
mtext("Consumer diffusion rate",side=1,line=2.2,cex=0.8)
axis(2,las=2)

## hard code from the empirical data
empirical.estimate <- -0.3217294
empirical.estimate.upper <- -0.3041126
empirical.estimate.lower <- -0.3393463
abline(h= empirical.estimate,col="red",lwd=2)
abline(h= c(empirical.estimate.upper, empirical.estimate.lower ),col="red",lty=3,lwd=2)


legend(2.2,-0.4,legend= pred.diff.vec,col=viridis(length(pred.diff.vec)),pch=19,cex=0.6,bty="n",title="consumer diff")


