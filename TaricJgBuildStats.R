setwd("/Users/User/Desktop/League of Legends Excel Databases")
getwd()

library("readxl")

taric_data <- read_excel("TaricJgBuildStats.xlsx") #this command names the excel file in R and imports the actual data into the 
#console

head(taric_data) #this prints the first few lines of information in taric_data

summary(taric_data) #prints a five number summary of taric_data

taric.df=taric_data #imports all data from taric_data into taric.df. This will let us create
#a linear model

plot(taric.df$Wins ~ taric.df$Losses, xlab="TarJg Wins", ylab = "TarJg Losses")
par(mfrow=c(2, 2)) #reorders the space from 1x1 to 2x2 (two rows, two columns)

#the code below plots four relationships between particular variables from the taric.df dataset

plot(taric.df$Wins ~ taric.df$Losses, xlab= "TarJg Wins", ylab = "TarJg Losses")
plot(taric.df$Winrate ~ taric.df$TotalGames, xlab = "TarJg Games", ylab = "TarJg Winrate")
plot(taric.df$TotalGames ~ taric.df$Losses, xlab = "TarJg Losses", ylab = "TarJg Games")
plot(taric.df$LPChange ~ taric.df$Winrate, xlab = "TarJg Winrate", ylab = "TarJg LP Change")

#it seems that there cannot be spaces in variable names or R will give an error

taric.lm=lm(LPChange~Wins+Losses+Winrate,data=taric.df)

summary(taric.lm) #unsurprisingly, the R^2 value of this multivariate regression is high at 0.98 approx. This means that LP Change
#is highly correlated to wins and losses - as one would expect

taric.lm1 = lm(LPChange~TotalGames,data=taric.df)

summary(taric.lm1) #the R squared value for this model is very, very low at 0.132 meaning that there is minimal relationship
#between LP Change and the total games played. That is, total games explains very little variability in LP Change

#recall that a RESIDUAL is the difference between the observed y value and y value predicted by linear model (lm)

plot(taric.lm1)

taricSampleWr_data <- read_excel("TaricJgSampleWinrates.xlsx") #imports a new excel file into the console

head(taricSampleWr_data) #prints the first few lines of information
dim(taricSampleWr_data) #this gives the dimensions of our data. In this case, it is 98 rows and 4 columns

taricSampleWr.df = taricSampleWr_data #imports all the stuff in taricSampleWr_data into taricSampleWr.df
head(taricSampleWr.df) #checks to make sure the data was imported properly
par(mfrow = c(1,1)) #command creates 1 row and 1 column
plot(taricSampleWr.df$Sample, taricSampleWr.df$Winrate, xlab="Sample Number (10 games)", ylab="Sample Winrate")

taricSampleWr.lm = lm(Winrate~Sample, data=taricSampleWr_data)

Winrate = taricSampleWr.df$Winrate
mean(Winrate) #mean Winrate is 49.38776%

sd(Winrate) #calculates the standard deviation for winrate. It is 0.1673421

abline(taricSampleWr.lm$coefficients,lwd=2,col="purple")

n.samp=14 #we will take samples from this population of samples of size 14
par(mfrow=c(2,2)) #creates 3 rows, 3 columns (9 graphs total)
plot(taricSampleWr.df$Sample, taricSampleWr.df$Winrate, xlab="Sample Number (10 games)",
     ylab="Sample Winrate")

abline(taricSampleWr.lm$coefficients,lwd=2,col="purple") 

#in the following we are taking three samples of size 14 and finding the estimated regression lines for each one of these 
#samples. We use a for loop because we take one sample of size 14 find its estimated regression then do the same process 
#3 times
for(i in 1:3)
{
  tmp.df<-taricSampleWr.df[sample(1:nrow(taricSampleWr.df),size=n.samp),]
  tmp.lm=lm(Sample~Winrate,data=tmp.df)
  plot(tmp.df$Sample, tmp.df$Winrate)
  abline(tmp.lm$coefficients,lwd=2,lty=2) 
  abline(taricSampleWr.lm$coefficients,lwd=2,col="purple") #the regression line ascertained from the 14 samples
}

#notice how our samples of 14 data points give us slightly varying best fit lines. That is expected as each best fit line
#has slightly differing b0 and b1 coefficients (slope and intercept)

n.sim=1000
samp.coeff.df=data.frame(b0=rep(NA,n.sim),b1=NA)

for(i in 1:n.sim)
{
  # sample 2 random rows from the original data frame
  tmp.df<-taricSampleWr.df[sample(1:nrow(taricSampleWr.df),size=n.samp),]
  tmp.lm=lm(Sample~Winrate,data=tmp.df) # estimate the linear model for the sampled data
  samp.coeff.df[i,]<-tmp.lm$coefficients #has coefficients b0 and b1, with b0 being the intercept and b1 being the slope
}

# visualize the distributions with the "true" value on top
par(mfrow=c(1,2)) #1 row and 2 colums
hist(samp.coeff.df$b0,xlab="b0",main="Samples of size 14")
abline(v=taricSampleWr.lm$coefficients[1],lwd=2,col="purple")
hist(samp.coeff.df$b1,xlab="b1",main="Samples of size 14")
abline(v=taricSampleWr.lm$coefficients[2],lwd=2,col="purple")

taricSampleWr.lm$coefficients #true values of the coefficients for the simple linear regression model

#it has an intercept value of 0.4869987.. and a slope of 0.000139

mean(samp.coeff.df$b0)
mean(samp.coeff.df$b1) #this returns gibberish. confusing

par(mfrow=c(1,1))
hist(taricSampleWr.df$Winrate, xlab = "Sample Winrates", main = "Sample Winrate Frequencies") #this indicates that the sample
#winrates of size 10 are roughly normally distributed

list(Winrate) #lists all the values in Winrate object

par(mfrow = c(2,2))

plot(taricSampleWr.lm)
