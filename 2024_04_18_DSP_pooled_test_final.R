########################################################################################### DSP Test that pools all age classes together for the statistic ############################################################################################# November 4th 2023# DSP statistic that pools together all fo the data for each age class prior to  # quantifying the trends age class and neighbhoord ages. This method allows us # to keep more data compared to the individual nest scale test due to isolated nests# and the need for having multiple neighboors to calculate relationship.# Note that we have both linear regressions and spearman correlations as two methods# for quantifying the relationships used to quantify the final DSP statistic 
rm(list=ls())setwd("/users/cx21atek/desktop/Azteca_DSP_local")dir()data <- read.csv("Azteca_census_data_Jan_2018.csv")head(data)
## get a nest df of nests which are alive nests.df <- data[data$y16 > 0, ] # coordinates of the living nests for the distance matrix nest.coords <- data.frame(nests.df$x_coord,nests.df$y_coord)## here is a matrix of the pairwise distances nest.dist.mat <- as.matrix(dist(nest.coords,diag=F,method="euclidean"))## here randomizing the data #random_samples <- sample(nests.df$y16,size=length(nests.df$y16),replace=FALSE)#nests.df$y16 <- random_samples## here is the data frame to hold the different radii to test the DSP. age.class.radius.df.names <- c("radius.threshold","age.vec","linear.estimates","linear.pvals","spearman.estimates","spearman.pvals")age.class.radius.df <- data.frame(0,0,0,0,0,0)colnames(age.class.radius.df) <- age.class.radius.df.namesfor(radius.looper in 10:50){    ### store all of the pooled data  pooled.dsp.age.dist.df <- data.frame(0,0,0,0)  colnames(pooled.dsp.age.dist.df) <- c("nest.order.id","focal_age","age","dist")    for(single.nest.looper in 1:dim(nests.df)[1]){        single.nest.id <- single.nest.looper ## go through each nest    radius.threshold <- radius.looper ## what is the radius we are testing for the DSP pattern?         single.nest.distances <- nest.dist.mat[,single.nest.id]        # logical vector to subset data    nest.index.inside.threshold <-  single.nest.distances <= radius.threshold        # full nest df inside of distance threshold (including focal nest)    nest.df.inside.threshold.with.self <- nests.df[nest.index.inside.threshold,]        # all distances inside threshold (including focal nest)    distance.mat.inside.threshold.with.self <- single.nest.distances[nest.index.inside.threshold]        # take the focal nest out of the data    nest.df.inside.threshold <- nest.df.inside.threshold.with.self[distance.mat.inside.threshold.with.self != 0, ]    distance.mat.inside.threshold <- distance.mat.inside.threshold.with.self[distance.mat.inside.threshold.with.self != 0]    if(length(distance.mat.inside.threshold) > 0){      # store the focal nest age       focal.nest.age <- nests.df[single.nest.id,]$y16            temp.single.nest.df <- data.frame(single.nest.looper, focal.nest.age, nest.df.inside.threshold$y16, distance.mat.inside.threshold)      colnames(temp.single.nest.df) <- c("nest.order.id","focal_age","age","dist")            pooled.dsp.age.dist.df <- rbind(pooled.dsp.age.dist.df, temp.single.nest.df)    }  }  ## get rid of dummy first row  pooled.dsp.df <- pooled.dsp.age.dist.df[2:dim(pooled.dsp.age.dist.df)[1],]      ### visualizing the underlying relationships between pooled age class data and neighbhohoods   make_age_plots <- F    linear.estimates <- rep(NA,13)  linear.pvals <- rep(NA,13)  spearman.estimates <- rep(NA,13)  spearman.pvals <- rep(NA,13)  if(make_age_plots == T){    par(mfrow=c(3,6))  }    for(focal.age.looper in 1:13){    #focal.age.looper <- 6    a.focal.age.df <- pooled.dsp.df[pooled.dsp.df$focal_age == focal.age.looper, ]        linear.model <- lm(a.focal.age.df$age ~ a.focal.age.df$dist)    spearman.model <- cor.test(a.focal.age.df$dist,a.focal.age.df$age,method="spearman")            linear.estimates[focal.age.looper] <- summary(linear.model)$coeff[2] ## slope     linear.pvals[focal.age.looper]  <- summary(linear.model)$coeff[8] ## pval     spearman.estimates[focal.age.looper]  <- spearman.model$estimate    spearman.pvals[focal.age.looper]  <- spearman.model$p.value        if(make_age_plots == T){      plot(a.focal.age.df$dist,a.focal.age.df$age)      mtext(paste("age=",focal.age.looper))                  if(summary(linear.model)$coeff[8] <= 0.1 & summary(linear.model)$coeff[2] <= 0){        abline(linear.model,col="red")      }      if(summary(linear.model)$coeff[8] <= 0.1 & summary(linear.model)$coeff[2] > 0){        abline(linear.model,col="green")      }      if(spearman.model$p.value <= 0.1 & spearman.model$estimate <= 0){        points(a.focal.age.df$dist,a.focal.age.df$age,col="red")      }      if(spearman.model$p.value <= 0.1 & spearman.model$estimate > 0){        points(a.focal.age.df$dist,a.focal.age.df$age,col="green")      }    }    }# end of looping through ages     age.vec <- seq(1,13,1)  ## collect this data in a data frame then we will use it compute the DSP statistic   age.class.radius.df.temp <- data.frame(radius.threshold,age.vec,linear.estimates,linear.pvals,spearman.estimates,spearman.pvals)    age.class.radius.df <- rbind(age.class.radius.df,age.class.radius.df.temp)    }## end of looping through radii to collect datat to measure DSP 

#L.DSP.LE # Linear DSP with linear estimates
#L.DSP.SE # Linear DSP with spearman estimates
#S.DSP.LE # Spearman DSP with linear estimates
#S.DSP.SE # Spearman DSP with spearman estimates
colnames <- c("radius","L.DSP.LE.estimates", "L.DSP.LE.pvals","L.DSP.LE.intercepts","L.DSP.LE.rsquareds","L.DSP.SE.estimates", "L.DSP.SE.pvals","L.DSP.SE.intercepts","L.DSP.SE.rsquareds", "S.DSP.LE.estimates", "S.DSP.LE.pvals", "S.DSP.SE.estimates","S.DSP.SE.pvals" )



DSP.summary.df.raw <- data.frame(matrix(0,ncol=length(colnames)))
colnames(DSP.summary.df.raw) <- colnames

## Calculating the DSP statistics 
for(rad.looper in 10:50){
	#rad.looper <- 20
	a.radii.dsp.df <- age.class.radius.df[age.class.radius.df$radius.threshold == rad.looper,]
  
## Linear regression of neighborhood estimates for DSP
	linear.DSP.linear.model <- lm(a.radii.dsp.df$linear.estimates ~ a.radii.dsp.df$age.vec)
	linear.DSP.spearman.model <- lm(a.radii.dsp.df$spearman.estimates ~ a.radii.dsp.df$age.vec)

## Spearman correlation of neighbhoor estimates for DSP 
	spearman.DSP.linear.model <- cor.test(a.radii.dsp.df$age.vec, a.radii.dsp.df$linear.estimates,method="spearman" )
	spearman.DSP.spearman.model <- cor.test( a.radii.dsp.df$age.vec,a.radii.dsp.df$spearman.estimates,method="spearman")
 
  
	### collect our data here 
	## linear DSP with linear estimates 
L.DSP.LE.estimates <- summary(linear.DSP.linear.model)$coeff[2]
L.DSP.LE.pvals <- summary(linear.DSP.linear.model)$coeff[8]
L.DSP.LE.intercepts <- summary(linear.DSP.linear.model)$coeff[1]
L.DSP.LE.rsquareds <- summary(linear.DSP.linear.model)$r.squared
	# linear DSP with spearman estimates 
L.DSP.SE.estimates <- summary(linear.DSP.spearman.model)$coeff[2]
L.DSP.SE.pvals <- summary(linear.DSP.spearman.model)$coeff[8]
L.DSP.SE.intercepts <- summary(linear.DSP.spearman.model)$coeff[1]
L.DSP.SE.rsquareds <- summary(linear.DSP.spearman.model)$r.squared
	# spearman DSP with linear estimates 
S.DSP.LE.estimates <- as.numeric(spearman.DSP.linear.model$estimate)
S.DSP.LE.pvals <- spearman.DSP.linear.model$p.value
	#spearman DSP with spearman estimates 
S.DSP.SE.estimates <- as.numeric(spearman.DSP.spearman.model$estimate)
S.DSP.SE.pvals <- spearman.DSP.spearman.model $p.value
radius <- rad.looper

temp.df <- data.frame(radius,L.DSP.LE.estimates,L.DSP.LE.pvals,L.DSP.LE.intercepts,L.DSP.LE.rsquareds,L.DSP.SE.estimates,L.DSP.SE.pvals,L.DSP.SE.intercepts,L.DSP.SE.rsquareds, S.DSP.LE.estimates, S.DSP.LE.pvals, S.DSP.SE.estimates,S.DSP.SE.pvals )
DSP.summary.df.raw <- rbind(DSP.summary.df.raw, temp.df)
 	
}


DSP.summary.df <- DSP.summary.df.raw[DSP.summary.df.raw$radius != 0, ]


### Linear DSP with both linear and spearman estimates 

par(mfrow=c(2,3))
plot(DSP.summary.df$radius, DSP.summary.df$L.DSP.LE.estimates)
plot(DSP.summary.df$radius, DSP.summary.df$L.DSP.LE.rsquareds)
plot(DSP.summary.df$radius, DSP.summary.df$L.DSP.LE.pvals)
abline(h=0.05,lty=3)


plot(DSP.summary.df$radius, DSP.summary.df$L.DSP.SE.estimates)
plot(DSP.summary.df$radius, DSP.summary.df$L.DSP.SE.rsquareds)
plot(DSP.summary.df$radius, DSP.summary.df$L.DSP.SE.pvals)
abline(h=0.05,lty=3)



### Spearman DSP with both linear and spearman estimates 

par(mfrow=c(2,2))
plot(DSP.summary.df$radius, DSP.summary.df$S.DSP.LE.estimates)
plot(DSP.summary.df$radius, DSP.summary.df$S.DSP.LE.pvals)
abline(h=0.05,lty=3)

plot(DSP.summary.df$radius, DSP.summary.df$S.DSP.SE.estimates)
plot(DSP.summary.df$radius, DSP.summary.df$S.DSP.SE.pvals)
abline(h=0.05,lty=3)



#### Making some plots below 
length(12:50)### here visualizing the DSP statistic par(mfrow=c(5,8),mai=c(0.2,0.2,0.2,0.2),oma=c(3,3,1,1))for(rad.looper in 10:49){  a.radii.dsp.df <- age.class.radius.df[age.class.radius.df$radius.threshold == rad.looper,]  DSP.linear.model <- lm(a.radii.dsp.df$linear.estimates ~ a.radii.dsp.df$age.vec)

plot(a.radii.dsp.df$age.vec,a.radii.dsp.df$linear.estimates,xlab="",ylab="",yaxt="n",ylim=c(min(age.class.radius.df$linear.estimates),max(age.class.radius.df$linear.estimates)),xaxt="n")
 	 #mtext("Nest age",side=1,line=2.2)
  	#mtext(expression(paste("Neighborhood ", b[i])),side=2,line=2.5)
  	axis(2,las=2)
  	axis(1,at=seq(1,13,by=4))
  	#mtext("linear regression",line=0.5)
  	abline(h=0,lty=3)
  	points(a.radii.dsp.df$age.vec,a.radii.dsp.df$linear.estimates,pch=19)
  	DSP.linear.model <- lm(a.radii.dsp.df$linear.estimates ~ a.radii.dsp.df$age.vec)
 	mtext(paste("radius=",rad.looper),cex=0.5)
 	
 	if(summary(DSP.linear.model)$coeff[8] <= 0.05){
 		 abline(DSP.linear.model,col="red")
 	}else if(summary(DSP.linear.model)$coeff[8] <= 0.1){
 		 abline(DSP.linear.model,col="dark red")

 	}
}

mtext(expression(paste("Neighborhood ", b[i])),side=2,line=1,outer=T)
mtext("Nest age class",side=1,outer=T,line=1)




### here visualizing the LINEAR DSP with Spearman estimates  
par(mfrow=c(5,8),mai=c(0.2,0.2,0.2,0.2),oma=c(3,3,1,1))
for(rad.looper in 10:49){
  a.radii.dsp.df <- age.class.radius.df[age.class.radius.df$radius.threshold == rad.looper,]
  DSP.linear.model <- lm(a.radii.dsp.df$spearman.estimates ~ a.radii.dsp.df$age.vec)

plot(a.radii.dsp.df$age.vec,a.radii.dsp.df$spearman.estimates,xlab="",ylab="",yaxt="n",ylim=c(min(age.class.radius.df$spearman.estimates),max(age.class.radius.df$spearman.estimates)),xaxt="n")
 	 #mtext("Nest age",side=1,line=2.2)
  	#mtext(expression(paste("Neighborhood ", b[i])),side=2,line=2.5)
  	axis(2,las=2)
  	axis(1,at=seq(1,13,by=4))
  	#mtext("linear regression",line=0.5)
  	abline(h=0,lty=3)
  	points(a.radii.dsp.df$age.vec,a.radii.dsp.df$spearman.estimates,pch=19)
  	DSP.spearman.model <- lm(a.radii.dsp.df$spearman.estimates ~ a.radii.dsp.df$age.vec)
 	mtext(paste("radius=",rad.looper),cex=0.5)
 	
 	if(summary(DSP.spearman.model)$coeff[8] <= 0.05){
 		 abline(DSP.spearman.model,col="red")
 	}else if(summary(DSP.spearman.model)$coeff[8] <= 0.1){
 		 abline(DSP.spearman.model,col="dark red")

 	}
}

mtext(expression(paste("Neighborhood ", b[i])),side=2,line=1,outer=T)
mtext("Nest age class",side=1,outer=T,line=1)

######################################################################################## Randomized data #################################################
#############################################################################

#############################################################################
########### Randomized data #################################################
#############################################################################

#############################################################################
########### Randomized data #################################################
#############################################################################
########### Randomized data 
########### Randomized data 
########### Randomized data 
########### Randomized data 
########### Randomized data 
########### Randomized data 

colnames <- c("radius","L.DSP.LE.estimates", "L.DSP.LE.pvals","L.DSP.LE.intercepts","L.DSP.LE.rsquareds","L.DSP.SE.estimates", "L.DSP.SE.pvals","L.DSP.SE.intercepts","L.DSP.SE.rsquareds", "S.DSP.LE.estimates", "S.DSP.LE.pvals", "S.DSP.SE.estimates","S.DSP.SE.pvals" )

RANDOMIZED.DSP.summary.df  <- data.frame(matrix(0,ncol=length(colnames)))
colnames(RANDOMIZED.DSP.summary.df) <- colnames
num.randomizations <- 500


for(randomization.loopers in 1:num.randomizations){
	
cat(paste((randomization.loopers/num.randomizations)*100,"% done"),"\n")
	
## here randomizing the data 
random_samples <- sample(nests.df$y16,size=length(nests.df$y16),replace=FALSE)
nests.df$y16 <- random_samples


## here is the data frame to hold the different radii to test the DSP. 
age.class.radius.df.names <- c("radius.threshold","age.vec","linear.estimates","linear.pvals","spearman.estimates","spearman.pvals")
age.class.radius.df <- data.frame(0,0,0,0,0,0)
colnames(age.class.radius.df) <- age.class.radius.df.names


for(radius.looper in 10:50){
  
  ### store all of the pooled data
  pooled.dsp.age.dist.df <- data.frame(0,0,0,0)
  colnames(pooled.dsp.age.dist.df) <- c("nest.order.id","focal_age","age","dist")
  
  for(single.nest.looper in 1:dim(nests.df)[1]){
    
    single.nest.id <- single.nest.looper ## go through each nest
    radius.threshold <- radius.looper ## what is the radius we are testing for the DSP pattern? 
    
    single.nest.distances <- nest.dist.mat[,single.nest.id]
    
    # logical vector to subset data
    nest.index.inside.threshold <-  single.nest.distances <= radius.threshold
    
    # full nest df inside of distance threshold (including focal nest)
    nest.df.inside.threshold.with.self <- nests.df[nest.index.inside.threshold,]
    
    # all distances inside threshold (including focal nest)
    distance.mat.inside.threshold.with.self <- single.nest.distances[nest.index.inside.threshold]
    
    # take the focal nest out of the data
    nest.df.inside.threshold <- nest.df.inside.threshold.with.self[distance.mat.inside.threshold.with.self != 0, ]
    distance.mat.inside.threshold <- distance.mat.inside.threshold.with.self[distance.mat.inside.threshold.with.self != 0]
    if(length(distance.mat.inside.threshold) > 0){
      # store the focal nest age 
      focal.nest.age <- nests.df[single.nest.id,]$y16
      
      temp.single.nest.df <- data.frame(single.nest.looper, focal.nest.age, nest.df.inside.threshold$y16, distance.mat.inside.threshold)
      colnames(temp.single.nest.df) <- c("nest.order.id","focal_age","age","dist")
      
      pooled.dsp.age.dist.df <- rbind(pooled.dsp.age.dist.df, temp.single.nest.df)
    }
  }
  ## get rid of dummy first row
  pooled.dsp.df <- pooled.dsp.age.dist.df[2:dim(pooled.dsp.age.dist.df)[1],]
  
  
  ### visualizing the underlying relationships between pooled age class data and neighbhohoods 
  make_age_plots <- F
  
  linear.estimates <- rep(NA,13)
  linear.pvals <- rep(NA,13)
  spearman.estimates <- rep(NA,13)
  spearman.pvals <- rep(NA,13)
  if(make_age_plots == T){
    par(mfrow=c(3,6))
  }
  
  for(focal.age.looper in 1:13){
    #focal.age.looper <- 6
    a.focal.age.df <- pooled.dsp.df[pooled.dsp.df$focal_age == focal.age.looper, ]
    
    if(dim(a.focal.age.df)[1] > 3){
    linear.model <- lm(a.focal.age.df$age ~ a.focal.age.df$dist)
    spearman.model <- cor.test(a.focal.age.df$dist,a.focal.age.df$age,method="spearman")
    
    
    linear.estimates[focal.age.looper] <- summary(linear.model)$coeff[2] ## slope 
    linear.pvals[focal.age.looper]  <- summary(linear.model)$coeff[8] ## pval 
    spearman.estimates[focal.age.looper]  <- spearman.model$estimate
    spearman.pvals[focal.age.looper]  <- spearman.model$p.value
    }
    
    if(make_age_plots == T){
      plot(a.focal.age.df$dist,a.focal.age.df$age)
      mtext(paste("age=",focal.age.looper))
      
      
      if(summary(linear.model)$coeff[8] <= 0.1 & summary(linear.model)$coeff[2] <= 0){
        abline(linear.model,col="red")
      }
      if(summary(linear.model)$coeff[8] <= 0.1 & summary(linear.model)$coeff[2] > 0){
        abline(linear.model,col="green")
      }
      if(spearman.model$p.value <= 0.1 & spearman.model$estimate <= 0){
        points(a.focal.age.df$dist,a.focal.age.df$age,col="red")
      }
      if(spearman.model$p.value <= 0.1 & spearman.model$estimate > 0){
        points(a.focal.age.df$dist,a.focal.age.df$age,col="green")
      }
    }  
  }# end of looping through ages 
  
  age.vec <- seq(1,13,1)
  ## collect this data in a data frame then we will use it compute the DSP statistic 
  age.class.radius.df.temp <- data.frame(radius.threshold,age.vec,linear.estimates,linear.pvals,spearman.estimates,spearman.pvals)
  
  age.class.radius.df <- rbind(age.class.radius.df,age.class.radius.df.temp)
  
  
}## end of looping through radii to collect datat to measure DSP 


#L.DSP.LE # Linear DSP with linear estimates
#L.DSP.SE # Linear DSP with spearman estimates
#S.DSP.LE # Spearman DSP with linear estimates
#S.DSP.SE # Spearman DSP with spearman estimates
colnames <- c("radius","L.DSP.LE.estimates", "L.DSP.LE.pvals","L.DSP.LE.intercepts","L.DSP.LE.rsquareds","L.DSP.SE.estimates", "L.DSP.SE.pvals","L.DSP.SE.intercepts","L.DSP.SE.rsquareds", "S.DSP.LE.estimates", "S.DSP.LE.pvals", "S.DSP.SE.estimates","S.DSP.SE.pvals" )



rand.DSP.summary.df.raw <- data.frame(matrix(0,ncol=length(colnames)))
colnames(rand.DSP.summary.df.raw) <- colnames

## Calculating the DSP statistics 
for(rad.looper in 10:50){
	#rad.looper <- 20
	a.radii.dsp.df <- age.class.radius.df[age.class.radius.df$radius.threshold == rad.looper,]
  
## Linear regression of neighborhood estimates for DSP
	linear.DSP.linear.model <- lm(a.radii.dsp.df$linear.estimates ~ a.radii.dsp.df$age.vec)
	linear.DSP.spearman.model <- lm(a.radii.dsp.df$spearman.estimates ~ a.radii.dsp.df$age.vec)

## Spearman correlation of neighbhoor estimates for DSP 
	spearman.DSP.linear.model <- cor.test(a.radii.dsp.df$age.vec, a.radii.dsp.df$linear.estimates,method="spearman" )
	spearman.DSP.spearman.model <- cor.test( a.radii.dsp.df$age.vec,a.radii.dsp.df$spearman.estimates,method="spearman")
 
  
	### collect our data here 
	## linear DSP with linear estimates 
L.DSP.LE.estimates <- summary(linear.DSP.linear.model)$coeff[2]
L.DSP.LE.pvals <- summary(linear.DSP.linear.model)$coeff[8]
L.DSP.LE.intercepts <- summary(linear.DSP.linear.model)$coeff[1]
L.DSP.LE.rsquareds <- summary(linear.DSP.linear.model)$r.squared
	# linear DSP with spearman estimates 
L.DSP.SE.estimates <- summary(linear.DSP.spearman.model)$coeff[2]
L.DSP.SE.pvals <- summary(linear.DSP.spearman.model)$coeff[8]
L.DSP.SE.intercepts <- summary(linear.DSP.spearman.model)$coeff[1]
L.DSP.SE.rsquareds <- summary(linear.DSP.spearman.model)$r.squared
	# spearman DSP with linear estimates 
S.DSP.LE.estimates <- as.numeric(spearman.DSP.linear.model$estimate)
S.DSP.LE.pvals <- spearman.DSP.linear.model$p.value
	#spearman DSP with spearman estimates 
S.DSP.SE.estimates <- as.numeric(spearman.DSP.spearman.model$estimate)
S.DSP.SE.pvals <- spearman.DSP.spearman.model $p.value
radius <- rad.looper

temp.df <- data.frame(radius,L.DSP.LE.estimates,L.DSP.LE.pvals,L.DSP.LE.intercepts,L.DSP.LE.rsquareds,L.DSP.SE.estimates,L.DSP.SE.pvals,L.DSP.SE.intercepts,L.DSP.SE.rsquareds, S.DSP.LE.estimates, S.DSP.LE.pvals, S.DSP.SE.estimates,S.DSP.SE.pvals )
rand.DSP.summary.df.raw <- rbind(rand.DSP.summary.df.raw, temp.df)
 	
}

SINGLE.RANDOM.DSP.summary.df <- rand.DSP.summary.df.raw[rand.DSP.summary.df.raw $radius != 0, ]

## collect the data here 
RANDOMIZED.DSP.summary.df <- rbind(RANDOMIZED.DSP.summary.df, SINGLE.RANDOM.DSP.summary.df)

}


RANDOMIZED.DSP.summary.df2 <- RANDOMIZED.DSP.summary.df[RANDOMIZED.DSP.summary.df$radius > 0, ]

#write.csv(RANDOMIZED.DSP.summary.df2, "pooled_DSP_randomization.csv")
#write.csv(DSP.summary.df, "pooled_DSP.csv")



RANDOMIZED.DSP.summary.df  <- read.csv("pooled_DSP_randomization.csv")
RANDOMIZED.DSP.summary.df2 <- RANDOMIZED.DSP.summary.df[RANDOMIZED.DSP.summary.df$radius != 0, ]
DSP.summary.df  <- read.csv("pooled_DSP.csv")










par(mfrow=c(1,3))

estimate.bootstrap.pval <- numeric(length(unique(DSP.summary.df$radius)))
Rsquare.bootstrap.pval  <- numeric(length(unique(DSP.summary.df$radius)))
pval.bootstrap.pval <- numeric(length(unique(DSP.summary.df$radius)))
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


estimate.bootstrap.pval[rad.looper] <- estimate.pval
Rsquare.bootstrap.pval[rad.looper] <- rsquared.pval
pval.bootstrap.pval[rad.looper] <- pvals.pval
}


plot(estimate.bootstrap.pval)
abline(h=0.05)
plot(Rsquare.bootstrap.pval)
abline(h=0.05)
plot(pval.bootstrap.pval)
abline(h=0.05)
label.cex <- 0.8

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



