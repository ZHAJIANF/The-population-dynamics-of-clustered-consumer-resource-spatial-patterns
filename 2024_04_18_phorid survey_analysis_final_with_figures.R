rm(list=ls())
setwd("/Users/cx21atek/Desktop/Azteca_DSP_local/input data")
dir()


## making sure the ages are matching bewteen the dat a
p.dat <- read.csv("Azteca_Phorid_FINAL_DATA.csv", header=TRUE)
a.dat <- read.csv("Azteca_census_data_Jan_2018.csv")

### here are just double checking that the ages for the nests are the same across both data sets. 
head(p.dat)
x <- 8
age.checking <- numeric(dim(p.dat)[1])
for(x in 1:dim(p.dat)[1]){
	a.pdat.entry <- p.dat[x,]
	a.dat.w.record <- a.dat[a.dat$record == a.pdat.entry$record, ]


	phorid.dat.age <- a.pdat.entry$new_age
	azteca.dat.age <- a.dat.w.record$y16

	if(length(azteca.dat.age) > 1){
		cat("Explosion at:",x)
	}
	
	age.checking[x] <- phorid.dat.age == azteca.dat.age
	
}


rm(list=ls())
data <- read.csv("Azteca_Phorid_FINAL_DATA.csv", header=TRUE)


library(fitdistrplus)
library(brms)

### Bayesian linear models of num phorids and arrival time 
library(performance)

head(data)



#### Final models 
#### Number of Phorids model 
num.phorid.pois.model_3 <- brm(log(num_phorids) ~ new_age + circ + (1|hectare) , data= data) 
num.phorid.pois.model_3
check_model(num.phorid.pois.model_3)
plot(num.phorid.pois.model_3)
plot(conditional_effects(num.phorid.pois.model_3),points=T)
pp_check(num.phorid.pois.model_3,ndraws=100)

### phorid arrival time model 
logged.arrival.phorid.linear.model_2 <- brm(log(arrival_time)  ~ new_age + circ + (1|hectare), data= data)
logged.arrival.phorid.linear.model_2
plot(logged.arrival.phorid.linear.model_2)
plot(conditional_effects(logged.arrival.phorid.linear.model_2),points=T)
pp_check(logged.arrival.phorid.linear.model_2,ndraws=100)
check_model(logged.arrival.phorid.linear.model_2)





### Figures for main text 

#### Figure with nest age 
dev.new(height=3,width=9.5)
par(mfrow=c(1,3),mai=c(0.6,0.6,0.1,0.1))
pch.val <- 19

plot(log(data$arrival_time) ~ jitter(data$new_age,amount=0.4),data=data,xlab="",ylab="",yaxt="n",pch=pch.val)
axis(2,las=2)
mtext("Ant nest age",side=1,line=2.2,cex=0.8)
mtext("ln(parasitoid arrival time (s))",side=2,line=2.5,cex=0.8)
abline(lm(log(data$arrival_time) ~ data$new_age),lty=3,lwd=2)

plot(log(data$num_phorids) ~ jitter(data$new_age,amount=0.4),data=data,xlab="",ylab="",yaxt="n",pch=pch.val)
axis(2,las=2)
mtext("Ant nest age",side=1,line=2.2,cex=0.8)
mtext("ln(# of parasitoids)",side=2,line=2.5,cex=0.8)
abline(lm(log(data$num_phorids) ~ data$new_age),lty=3,lwd=2)

plot(log(data$attack_time) ~ jitter(data$new_age,amount=0.4),data=data,xlab="",ylab="",yaxt="n",pch=pch.val)
axis(2,las=2)
mtext("Ant nest age",side=1,line=2.2,cex=0.8)
mtext("ln(parasitoid attack duration (s))",side=2,line=2.5,cex=0.8)
abline(lm(log(data$attack_time) ~ data$new_age),lty=3,lwd=2)












### Full figures for supp material 

#### Figure with nest age 
dev.new(height=3,width=12)
par(mfrow=c(1,4),mai=c(0.6,0.6,0.1,0.1))
pch.val <- 19

plot(log(data$arrival_time) ~ jitter(data$new_age,amount=0.4),data=data,xlab="",ylab="",yaxt="n",pch=pch.val)
axis(2,las=2)
mtext("Ant nest age",side=1,line=2.2,cex=0.8)
mtext("ln(parasitoid arrival time (s))",side=2,line=2.5,cex=0.8)
abline(lm(log(data$arrival_time) ~ data$new_age),lty=3,lwd=2)

plot(log(data$num_phorids) ~ jitter(data$new_age,amount=0.4),data=data,xlab="",ylab="",yaxt="n",pch=pch.val)
axis(2,las=2)
mtext("Ant nest age",side=1,line=2.2,cex=0.8)
mtext("ln(# of parasitoids)",side=2,line=2.5,cex=0.8)
abline(lm(log(data$num_phorids) ~ data$new_age),lty=3,lwd=2)

plot(log(data$attack_time) ~ jitter(data$new_age,amount=0.4),data=data,xlab="",ylab="",yaxt="n",pch=pch.val)
axis(2,las=2)
mtext("Ant nest age",side=1,line=2.2,cex=0.8)
mtext("ln(parasitoid attack duration (s))",side=2,line=2.5,cex=0.8)
abline(lm(log(data$attack_time) ~ data$new_age),lty=3,lwd=2)

plot(log(data$num_attacks+1) ~ jitter(data$new_age,amount=0.4),data=data,xlab="",ylab="",yaxt="n",pch=pch.val)
axis(2,las=2)
mtext("Ant nest age",side=1,line=2.2,cex=0.8)
mtext("ln(# parasitoid attacks)",side=2,line=2.5,cex=0.8)
abline(lm(log(data$num_attacks+1) ~ data$new_age),lty=3,lwd=2)

#### figure with tree circumference 

head(data)
dev.new(height=3,width=12)
par(mfrow=c(1,4),mai=c(0.6,0.6,0.1,0.1))
pch.val <- 19

plot(log(data$arrival_time) ~ jitter(data$circ,amount=0.1),data=data,xlab="",ylab="",yaxt="n",pch=pch.val,cex=(data$new_age*0.2))
axis(2,las=2)
mtext("Tree circumference",side=1,line=2.2,cex=0.8)
mtext("ln(parasitoid arrival time (s))",side=2,line=2.5,cex=0.8)
abline(lm(log(data$arrival_time) ~ data$circ),lty=3,lwd=2)

plot(log(data$num_phorids) ~ jitter(data$circ,amount=0.1),data=data,xlab="",ylab="",yaxt="n",pch=pch.val,cex=(data$new_age*0.2))
axis(2,las=2)
mtext("Tree circumference",side=1,line=2.2,cex=0.8)
mtext("ln(# of parasitoids)",side=2,line=2.5,cex=0.8)
abline(lm(log(data$num_phorids) ~ data$circ),lty=3,lwd=2)


plot(log(data$attack_time) ~ jitter(data$circ,amount=0.1),data=data,xlab="",ylab="",yaxt="n",pch=pch.val,cex=(data$new_age*0.2))
axis(2,las=2)
mtext("Tree circumference",side=1,line=2.2,cex=0.8)
mtext("ln(parasitoid attack duration (s))",side=2,line=2.5,cex=0.8)
abline(lm(log(data$attack_time) ~ data$circ),lty=3,lwd=2)


plot((data$num_attacks) ~ jitter(data$circ,amount=0.1),data=data,xlab="",ylab="",yaxt="n",pch=pch.val,cex=(data$new_age*0.2))
axis(2,las=2)
mtext("Tree circumference",side=1,line=2.2,cex=0.8)
mtext("ln(# parasitoid attacks)",side=2,line=2.5,cex=0.8)
abline(lm((data$num_attacks) ~ data$circ),lty=3,lwd=2)
mod <- lm((data$num_attacks) ~ data$circ)
summary(mod)




########################################################
############ NUM Phorids models ########################
########################################################

## Here we stipulate a larger model that accounts for the circumference of the tree as well as the tree species as fixed effects.
#### the hectare is used as a random effect on the intercepts 

#it seems like the model doesnt like that only one tree is Trema when I use it as a fixed effect 
no.trema.df <- data[data$tree_species != "Trema",]

#### Linear model with number of phorids, circ, tree species and HA as random intercept 
num.phorid.linear.model_1 <- brm(num_phorids ~ new_age + circ + tree_species + (1|hectare), data= no.trema.df)
num.phorid.linear.model_1
plot(num.phorid.linear.model_1)
plot(conditional_effects(num.phorid.linear.model_1),points=T)
pp_check(num.phorid.linear.model_1,ndraws=100)
check_model(num.phorid.linear.model_1)

#### Poisson model with number of phorids, circ, tree species and HA as random intercept 
### This poisson version of the model wont run. 
num.phorid.pois.model_1 <- brm(num_phorids ~ new_age + circ + tree_species + (1|hectare), data= no.trema.df, family=poisson( link="identity" )) # identity means no link function
num.phorid.pois.model_1
plot(conditional_effects(num.phorid.pois.model_1),points=T)
pp_check(num.phorid.pois.model_1,ndraws=100)
hypothesis(num.phorid.pois.model_1, "x>0",class="b")
plot(num.phorid.pois.model_1)
check_model(num.phorid.pois.model_1)

############# Now getting rid of tree as a fixed effect becuase the models dont work 

#### Poisson model with number of phorids, circ, tree species and HA as random intercept 
### This poisson version of the model wont run. 
num.phorid.pois.model_2 <- brm(num_phorids ~ new_age + circ +(1|hectare) , data= data, family=poisson( link="identity" ),inits=1) # identity means no link function
num.phorid.pois.model_2
plot(conditional_effects(num.phorid.pois.model_2),points=T)
pp_check(num.phorid.pois.model_2,ndraws=100)
plot(num.phorid.pois.model_2)
check_model(num.phorid.pois.model_2)


### Poisson models wont work so we can log transform the data and get a pretty decent model 
num.phorid.pois.model_3 <- brm(log(num_phorids) ~ new_age + circ + (1|hectare) , data= data) # identity means no link function
num.phorid.pois.model_3
check_model(num.phorid.pois.model_3)
plot(num.phorid.pois.model_3)
plot(conditional_effects(num.phorid.pois.model_3),points=T)
pp_check(num.phorid.pois.model_3,ndraws=100)


## Purely linear model? 
num.phorid.pois.model_4 <- brm((num_phorids) ~ new_age + circ + (1|hectare) , data= data) # identity means no link function
num.phorid.pois.model_4
check_model(num.phorid.pois.model_4)
plot(num.phorid.pois.model_4)
plot(conditional_effects(num.phorid.pois.model_4),points=T)
pp_check(num.phorid.pois.model_4,ndraws=100)


########################################################
############## Arrival time models ##################### 
########################################################

##### Now log-linear arrival time model with number of phorids, circ, tree species and HA as random intercept 
logged.arrival.phorid.linear.model_1 <- brm(log(arrival_time)  ~ new_age + circ + tree_species  + (1|hectare), data= no.trema.df)
logged.arrival.phorid.linear.model_1
plot(logged.arrival.phorid.linear.model_1)
plot(conditional_effects(logged.arrival.phorid.linear.model_1),points=T)
pp_check(logged.arrival.phorid.linear.model_1,ndraws=100)
check_model(logged.arrival.phorid.linear.model_1)


##### Now log-linear arrival time model with number of phorids, circ, and HA as random intercept 
logged.arrival.phorid.linear.model_2 <- brm(log(arrival_time)  ~ new_age + circ + (1|hectare), data= data)
logged.arrival.phorid.linear.model_2
plot(logged.arrival.phorid.linear.model_2)
plot(conditional_effects(logged.arrival.phorid.linear.model_2),points=T)
pp_check(logged.arrival.phorid.linear.model_2,ndraws=100)
check_model(logged.arrival.phorid.linear.model_2)


##### Now linear arrival time model with number of phorids, circ, and HA as random intercept 
logged.arrival.phorid.linear.model_3 <- brm((arrival_time)  ~ new_age + circ + (1|hectare), data= data)
logged.arrival.phorid.linear.model_3
plot(logged.arrival.phorid.linear.model_3)
plot(conditional_effects(logged.arrival.phorid.linear.model_3),points=T)
pp_check(logged.arrival.phorid.linear.model_3,ndraws=100)
check_model(logged.arrival.phorid.linear.model_3)




###################### Phorid attack duration models 
## PP check seems better there for the non-log transormed 
attack.time.model_1 <- brm(attack_time ~ new_age + circ + (1|hectare),data=data)
attack.time.model_1
plot(attack.time.model_1)
check_model(attack.time.model_1)
pp_check(attack.time.model_1,ndraws=200)
plot(conditional_effects(attack.time.model_1),points=T)


attack.time.model_2 <- brm(log(attack_time) ~ new_age + circ + (1|hectare),data=data)
attack.time.model_2
plot(attack.time.model_2)
check_model(attack.time.model_2)
pp_check(attack.time.model_2,ndraws=200)
plot(conditional_effects(attack.time.model_2),points=T)



head(data)
#### with truncated attack time data
###### This removes all fo the data points that were over 300m meaning the attack didnt stop 
attack.time.model_3 <- brm(attack_time_2 ~ new_age + circ + (1|hectare),data=data)
attack.time.model_3
plot(attack.time.model_3)
check_model(attack.time.model_3)
pp_check(attack.time.model_3,ndraws=200)

attack.time.model_4 <- brm(log(attack_time_2) ~ new_age + circ + (1|hectare),data=data)
attack.time.model_4
plot(attack.time.model_4)
check_model(attack.time.model_4)
pp_check(attack.time.model_4,ndraws=200)












### OLD STUFF AS OF Nov 10 2023

arrival.phorid.linear.model<- brm((arrival_time) ~ new_age, data= data)
arrival.phorid.linear.model
plot(arrival.phorid.linear.model)
plot(conditional_effects(arrival.phorid.linear.model),points=T)
pp_check(arrival.phorid.linear.model,ndraws=100)
check_model(arrival.phorid.linear.model)
 

data$normalized.arrival.phorids <- normalized.arrival.phorids
arrival.beta.model <- brm(normalized.arrival.phorids ~ new_age, data= data, family=Beta()) # identity means no link function
plot(conditional_effects(arrival.beta.model),points=T)
arrival.beta.model
pp_check(arrival.beta.model,ndraws=100)
plot(arrival.beta.model)




par(mfrow=c(1,1))
plotdist(data$arrival_time)
descdist(data $arrival_time, boot = 1000,discrete=F)
descdist(normalized.arrival.phorids, boot = 1000,discrete=F)



descdist(data $num_phorids, boot = 2000,discrete=T)


pois.n.phoirds.glm <- glm(data$num_phorids ~ data$new_age,family="poisson")
summary(pois.n.phoirds.glm)
#install.packages("betareg")
library(betareg)

normalized.arrival.phorids <- numeric(length(data$arrival_time))
for(x in 1:length(data$arrival_time)){
	#x <- 1
	normalized.arrival.phorids[x] <- data$arrival_time[x]/max(data$arrival_time + 1)
}

min(normalized.arrival.phorids)
max(normalized.arrival.phorids)

beta.arrival.glm <- betareg(normalized.arrival.phorids ~ data$new_age)
summary(beta.arrival.glm)





par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
fw <- fitdist(data $arrival_time, "exp")
fg <- fitdist(data $arrival_time, "beta")
fln <- fitdist(data $arrival_time, "lnorm")
plot.legend <- c("expo", "beta", "gamma")
denscomp(list(fw, fln, fg), legendtext = plot.legend)
qqcomp(list(fw, fln, fg), legendtext = plot.legend)
cdfcomp(list(fw, fln, fg), legendtext = plot.legend)
ppcomp(list(fw, fln, fg), legendtext = plot.legend)


max(data$arrival_time)
par(mfrow=c(1,2))
plot(jitter(data$new_age,amount=0.2),data$arrival_time,pch=1,xaxt="n",yaxt="n",ylab="",xlab="")
mtext(substitute(paste(italic('Azteca')," Nest Age")),side=1,line=2.1)
axis(1,at=seq(0,13,2),cex.axis=1)
mtext(substitute(paste(italic('Phoridae')," Arrival Time (s)")),side=2,line=2.5)
axis(2,at=seq(0,max(data$arrival_time),20),las=2,cex=0.5)
arrival.time.lm <- lm(data$arrival_time ~data$new_age)
summary(arrival.time.lm)
abline(arrival.time.lm,lwd=1,lty=1)
cor.test(data$new_age,data$arrival_time,method="spearman")


plot(jitter(data$new_age,amount=0.2),(data$num_phorids),pch=1,xaxt="n",yaxt="n",ylab="",xlab="")
mtext(substitute(paste(italic('Azteca')," Nest Age")),side=1,line=2.1)
axis(1,at=seq(0,13,2),cex.axis=1)
axis(2,at=seq(1,max(data$num_phorids),2),las=2,cex.axis=0.85)
mtext(substitute(paste("# of ",italic('Phoridae'))),side=2,line=1.9)
substitute(paste(italic('Phoridae'),"Nest Age"))
num.phorids.lm <- lm((data$num_phorids)~data$new_age)
summary(num.phorids.lm)
abline(num.phorids.lm,lwd=1)
cor.test(data$new_age,data$num_phorids,method="spearman")



plot(jitter(data$new_age,amount=0.2),(data$num_phorids),pch=1,xaxt="n",yaxt="n",ylab="",xlab="")
mtext(substitute(paste(italic('Azteca')," Nest Age")),side=1,line=2.1)
axis(1,at=seq(0,13,2),cex.axis=1)
axis(2,at=seq(1,max(data$num_phorids),2),las=2,cex.axis=0.85)
mtext(substitute(paste("# of ",italic('Phoridae'))),side=2,line=1.9)
substitute(paste(italic('Phoridae'),"Nest Age"))
num.phorids.lm <- lm((data$num_phorids)~data$new_age)
summary(num.phorids.lm)
abline(num.phorids.lm,lwd=1)
cor.test(data$new_age,data$num_phorids,method="spearman")






















par(mfrow=c(1,1))

## linear model 
plot(data$new_age,(data$num_phorids),pch=19,xaxt="n",yaxt="n",ylab="",xlab="")
mtext(substitute(paste(italic('Azteca')," Nest Age")),side=1,line=2.1)
axis(1,at=seq(0,13,2),cex.axis=1)
axis(2,at=seq(1,max(data$num_phorids),2),las=2,cex.axis=0.85)
mtext(substitute(paste("# of ",italic('Phoridae'))),side=2,line=1.9)
substitute(paste(italic('Phoridae'),"Nest Age"))
num.phorids.lm <- lm((data$num_phorids)~data$new_age)
summary(num.phorids.lm)
abline(num.phorids.lm,lwd=3,col="red")

cor.test(data$new_age,data$num_phorids,method="spearman")



## log model 
plot(data$new_age,log(data$num_phorids),pch=19,xaxt="n",yaxt="n",ylab="",xlab="")
mtext(substitute(paste(italic('Azteca')," Nest Age")),side=1,line=2.1)
axis(1,at=seq(0,13,2),cex.axis=1)
axis(2,las=2,cex.axis=0.85)
mtext(substitute(paste("ln(# of ",italic('Phoridae'),")")),side=2,line=1.9)
substitute(paste(italic('Phoridae'),"Nest Age"))

num.phorids.lm <- lm(log(data$num_phorids)~data$new_age)
summary(num.phorids.lm)
abline(num.phorids.lm,lwd=3,col="red")
cor.test(data$new_age,log(data$num_phorids),method="spearman")




### log arrival time model 
plot(data$new_age,log(data$arrival_time),pch=19,xaxt="n",yaxt="n",ylab="",xlab="")
mtext(substitute(paste(italic('Azteca')," Nest Age")),side=1,line=2.1)
axis(1,at=seq(0,13,2),cex.axis=1)
mtext(substitute(paste(italic('Phoridae')," Arrival Time")),side=2,line=2.5)
axis(2,las=2,cex=0.5)
model <- lm(log(data$arrival_time) ~data$new_age)
summary(model)
abline(model,col="Red",lwd=2)

cor.test(data$new_age,log(data$arrival_time),method="spearman")




plot(data$new_age,data$attack_time,pch=19,xaxt="n",yaxt="n",ylab="",xlab="")
mtext(substitute(paste(italic('Azteca')," Nest Age")),side=1,line=2.1)
axis(1,at=seq(0,13,2),cex.axis=1)
mtext(substitute(paste(italic('Phoridae')," Attack Duration")),side=2,line=2.5)
axis(2,at=seq(0,max(data$attack_time),30),las=2,cex=0.5)

model <- lm(data$attack_time ~data$new_age)
summary(model)
abline(model,col="Red",lwd=2)

plot(data$new_age,data$attack_time_2,pch=19,xaxt="n",yaxt="n",ylab="",xlab="")
mtext(substitute(paste(italic('Azteca')," Nest Age")),side=1,line=2.1)
axis(1,at=seq(0,13,2),cex.axis=1)
mtext(substitute(paste(italic('Phoridae')," Attack Duration")),side=2,line=2.5)
axis(2,at=seq(0,max(data$attack_time),30),las=2,cex=0.5)

model <- lm(data$attack_time_2 ~data$new_age)
summary(model)
abline(model,col="Red",lwd=2)

plot(data$new_age,data$num_attacks,pch=19,xaxt="n",yaxt="n",ylab="",xlab="")
mtext(substitute(paste(italic('Azteca')," Nest Age")),side=1,line=2.1)
axis(1,at=seq(0,13,2),cex.axis=1)
mtext(substitute(paste("# of ",italic('Phoridae'),"Attacks")),side=2,line=2.5)
axis(2,at=seq(0,max(data$num_attacks),10),las=2,cex=0.5)
model <- lm(data$num_attacks ~ data$new_age)
summary(model)
abline(model,col="Red",lwd=2)

### Polynomial model
model2<- lm(data$num_attacks ~ poly(data$new_age,2))
summary(model2)
lines(data$new_age, predict(model2),lwd=2,col="red")


plot(data$new_age,data$arrival_time,pch=19,xaxt="n",yaxt="n",ylab="",xlab="")
mtext(substitute(paste(italic('Azteca')," Nest Age")),side=1,line=2.1)
axis(1,at=seq(0,13,2),cex.axis=1)
mtext(substitute(paste(italic('Phoridae')," Arrival Time")),side=2,line=2.5)
axis(2,at=seq(0,max(data$arrival_time),20),las=2,cex=0.5)

model <- lm(data$arrival_time ~data$new_age)
summary(model)
abline(model,col="Red",lwd=2)

























unique(data$new_age)

arrival_time_mean <-c()
attack_time_mean <-c()
num_attacks_mean <-c()
num_phorids_mean <-c()
attack_time_2_mean <-c()

arrival_time_error <-c()
attack_time_error <-c()
num_attacks_error <-c()
num_phorids_error<-c()
attack_time_2_error <-c()
for(x in 1:max(data$new_age)){
	
	age_class <- data[data$new_age == x,]
	
arrival_time_mean <-c(arrival_time_mean,mean(age_class$arrival_time))
attack_time_mean <-c(attack_time_mean,mean(age_class$attack_time))
num_attacks_mean <-c(num_attacks_mean,mean(age_class$num_attacks))
num_phorids_mean <-c(num_phorids_mean,mean(age_class$num_phorids))
attack_time_2_mean <-c(attack_time_2_mean,mean(age_class$attack_time_2[!is.na(age_class$attack_time_2)]))

arrival_time_error <-c(arrival_time_error,sd(age_class$arrival_time)/sqrt(length(age_class$arrival_time)))
attack_time_error <-c(attack_time_error,sd(age_class$attack_time)/sqrt(length(age_class$attack_time)))
num_attacks_error <-c(num_attacks_error,sd(age_class$num_attacks)/sqrt(length(age_class$num_attacks)))
num_phorids_error<-c(num_phorids_error,sd(age_class$num_phorids)/sqrt(length(age_class$num_phorids)))
attack_time_2_error <-c(attack_time_2_error,sd(age_class$attack_time_2[!is.na(age_class$attack_time_2)])/sqrt(length(age_class$attack_time_2[!is.na(age_class$attack_time_2)])))
	
}
par(mfrow=c(1,2),mai=c(0.7,0.7,0.2,0.2))
index<-seq(1,max(data$new_age),1)
plot(index, arrival_time_mean,pch=19,cex=1,type="p",ylab="Arrival Time (s)",xlab="Nest Age",ylim=c(0,170))
points(index, arrival_time_mean,type="l")
arrows(index, arrival_time_mean - arrival_time_error, index, arrival_time_mean + arrival_time_error, angle=90,code=3, length = 0.05,col="black",lwd=1)


plot(index, num_phorids_mean,pch=19,cex=1,type="p",ylab="# Phorids",xlab="Nest Age",ylim=c(0,12))
points(index, num_phorids_mean,type="l")
arrows(index, num_phorids_mean - num_phorids_error, index, num_phorids_mean + num_phorids_error, angle=90,code=3, length = 0.05,col="red",lwd=1)


plot(index, attack_time_mean,pch=19,cex=1,type="p",ylab="Duration of Attack (s)",xlab="Nest Age",ylim=c(0,300))
arrows(index, attack_time_mean - attack_time_error, index, attack_time_mean + attack_time_error, angle=90,code=3, length = 0.05,col="red",lwd=1)

plot(index, num_attacks_mean,pch=19,cex=1,type="p",ylab="# Attacks",xlab="Nest Age",ylim=c(0,100))
arrows(index, num_attacks_mean - num_attacks_error, index, num_attacks_mean + num_attacks_error, angle=90,code=3, length = 0.05,col="red",lwd=1)



plot(index, attack_time_2_mean,pch=19,cex=1,type="p",ylab="Duration of Attack (s)",xlab="Nest Age",ylim=c(0,300))
arrows(index, attack_time_2_mean - attack_time_2_error, index, attack_time_2_mean + attack_time_2_error, angle=90,code=3, length = 0.05,col="red",lwd=1)

head(data)
par(mfrow=c(1,1))
plot(data$new_age,data$num_phorids,pch=19,xaxt="n",yaxt="n",ylab="",xlab="")
mtext(substitute(paste(italic('Azteca')," Nest Age")),side=1,line=2.1)
axis(1,at=seq(0,13,2),cex.axis=1)
axis(2,at=seq(1,max(data$num_phorids),2),las=2,cex.axis=0.85)
mtext(substitute(paste("# of ",italic('Phoridae'))),side=2,line=1.9)

model <- lm(data$num_phorids~data$new_age)
summary(model)
abline(model,lwd=3,col="red")

substitute(paste(italic('Phoridae'),"Nest Age"))



plot(data$new_age,data$attack_time,pch=19,xaxt="n",yaxt="n",ylab="",xlab="")
mtext(substitute(paste(italic('Azteca')," Nest Age")),side=1,line=2.1)
axis(1,at=seq(0,13,2),cex.axis=1)
mtext(substitute(paste(italic('Phoridae')," Attack Duration")),side=2,line=2.5)
axis(2,at=seq(0,max(data$attack_time),30),las=2,cex=0.5)

model <- lm(data$attack_time ~data$new_age)
summary(model)
abline(model,col="Red",lwd=2)

plot(data$new_age,data$attack_time_2,pch=19,xaxt="n",yaxt="n",ylab="",xlab="")
mtext(substitute(paste(italic('Azteca')," Nest Age")),side=1,line=2.1)
axis(1,at=seq(0,13,2),cex.axis=1)
mtext(substitute(paste(italic('Phoridae')," Attack Duration")),side=2,line=2.5)
axis(2,at=seq(0,max(data$attack_time),30),las=2,cex=0.5)

model <- lm(data$attack_time_2 ~data$new_age)
summary(model)
abline(model,col="Red",lwd=2)

plot(data$new_age,data$num_attacks,pch=19,xaxt="n",yaxt="n",ylab="",xlab="")
mtext(substitute(paste(italic('Azteca')," Nest Age")),side=1,line=2.1)
axis(1,at=seq(0,13,2),cex.axis=1)
mtext(substitute(paste("# of ",italic('Phoridae'),"Attacks")),side=2,line=2.5)
axis(2,at=seq(0,max(data$num_attacks),10),las=2,cex=0.5)
model <- lm(data$num_attacks ~ data$new_age)
summary(model)
abline(model,col="Red",lwd=2)

### Polynomial model
model2<- lm(data$num_attacks ~ poly(data$new_age,3))
summary(model2)
lines(data$new_age, predict(model2),lwd=2,col="red")


plot(data$new_age,data$arrival_time,pch=19,xaxt="n",yaxt="n",ylab="",xlab="")
mtext(substitute(paste(italic('Azteca')," Nest Age")),side=1,line=2.1)
axis(1,at=seq(0,13,2),cex.axis=1)
mtext(substitute(paste(italic('Phoridae')," Arrival Time")),side=2,line=2.5)
axis(2,at=seq(0,max(data$arrival_time),20),las=2,cex=0.5)

model <- lm(data$arrival_time ~data$new_age)
summary(model)
abline(model,col="Red",lwd=2)




#####
plot(arrival_time,attack_time_2)
balls <- lm(attack_time_2~arrival_time)
abline(balls)
summary(balls)


plot(arrival_time,num_attacks)
balls=lm(num_attacks~arrival_time)
abline(balls)
summary(balls)

plot(arrival_time,num_phorids)
abline(lm(num_phorids~arrival_time))
balls=lm(num_phorids~arrival_time)
summary(balls)

phoriddensityarrival=(arrival_time*num_phorids)
plot(new_age, phoriddensityarrival)
abline(lm(phoriddensityarrival ~ new_age))
balls=lm(phoriddensityarrival ~arrival_time)
summary(balls)

plot(num_attacks,num_phorids)
abline(lm(num_phorids~num_attacks))
balls=lm(num_attacks~num_phorids)
summary(balls)

attackrate=(num_attacks/num_phorids)
plot(new_age,attackrate)
balls=lm(attackrate~new_age)
abline(balls)
summary(balls)

plot(attack_time,num_phorids)
abline(lm(num_phorids~attack_time))
balls=lm(num_phorids~attack_time)
summary(balls)

attackdensity=(attack_time_2*num_phorids)
plot(new_age,attackdensity)
abline(lm(attackdensity~new_age))
balls=lm(attackdensity~new_age)
summary(balls)


plot(new_age,arrival_time,pch=19,col=1,cex=2.5,
xlab="Nest Age",ylab="Phorid Arrival Time")
line1=lm(arrival_time~new_age)
abline(line1,col="red",lwd=5)
summary(line1) 
plot(new_age,attack_time,pch=19,col=1,cex=2.5,
xlab="Nest Age",ylab="Phorid Attack Time")
line2=lm(attack_time~new_age)
abline(line2,col="red",lwd=5)
summary(line2)

plot(new_age,attack_time_2,pch=19,col=1,cex=2.5,
xlab="Nest Age",ylab="Phorid Attack Time")
line3=lm(attack_time_2~new_age)
abline(line3,col="red",lwd=5)
summary(line3)

plot(new_age,num_attacks,pch=19,col=1,cex=2.5,
xlab="Nest Age",ylab="# of Phorid Attacks")
line4=lm(num_attacks~new_age)
abline(line4,col="red",lwd=5)
summary(line4)

plot(new_age,num_phorids,pch=19,col=1, cex=2.5,
xlab="Nest Age",ylab="# of Phorids")
line5=lm(num_phorids~new_age)
abline(line5,col="red",lwd=5)
summary(line5)

##### CIRC PLOT
plot(data$circ,data$num_phorids, pch=19,col=1, cex=2.5,
xlab="Circ",ylab="# of Phorids")
line6=lm(data$num_phorids~data$circ)
abline(line6,col="red",lwd=5)
summary(line6)

plot(data$circ,data$num_attacks,pch=19,col=1,cex=2.5,
xlab="Circ",ylab="# of Phorid Attacks")
line7=lm(data$num_attacks~data$circ)
abline(line7,col="red",lwd=5)
summary(line7)

plot(data$circ,data$attack_time_2,pch=19,col=1,cex=2.5,
xlab="Circ",ylab="Phorid Attack Time")
line8=lm(data$attack_time_2~data$circ)
abline(line8,col="red",lwd=5)
summary(line8)

plot(data$circ,data$arrival_time,pch=19,col=1,cex=2.5,
xlab="circ",ylab="Phorid Arrival Time")
line9=lm(data$arrival_time~data$circ)
abline(line9,col="red",lwd=5)
summary(line9)

 plot(data$circ,data$new_age)
#############################
hist(data$arrival_time)
hist(data$attack_time)
hist(data$attack_time_2)
hist(data$num_phorids)
hist(data$num_attacks)

arrival <- glm(arrival_time~new_age+tree_species+circ+hectare,data=data)
summary(arrival)


attackduration=glm(attack_time~new_age+tree_species+circ+hectare,data=data)
summary(attackduration)

numphorid=glm(num_phorids~new_age+tree_species+circ+hectare,data=data)
summary(numphorid)

numattack=glm(num_attacks~new_age+tree_species+circ+hectare,data=data)
summary(numattack)
