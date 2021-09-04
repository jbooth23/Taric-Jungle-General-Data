setwd("/Users/User/Desktop/League of Legends Excel Databases")
getwd()

library("readxl")

glacialJg_data <- read_excel("GlacialGames.xlsx") #imports our desired excel file and data within

head(glacialJg_data) #prints the first few lines of data

summary(glacialJg_data)

plot(glacialJg_data)

glacialJg.df = glacialJg_data #creates a data frame, sets it equal to the data set imported from excel

glacialJg.lm=lm(ItemsCompleted~GameTime,data=glacialJg.df) #null model

par(mfrow=c(2, 2))

plot(glacialJg.lm)

summary(glacialJg.lm)

itemKillsJg.lm = lm(ItemsCompleted~Kills, data=glacialJg.df) #linear model to determine if there is a good relationship between
#items completed (y) and kills (x)

plot(itemKillsJg.lm)

summary(itemKillsJg.lm) #not a very good model. Only 28.39% of the variability in items purchased is explained by kills

itemDeathsJg.lm = lm(ItemsCompleted~Deaths, data=glacialJg.df) #linear model to determine if there is a good relationship between
#items completed (y) and deaths (x)

plot(deathsJg.lm)

summary(deathsJg.lm)

itemAssistsJg.lm = lm(ItemsCompleted~Assists, data=glacialJg.df) #linear model to determine if there is a good relatonship between
#items completed (y) and assists (x)

plot(assistsJg.lm)

summary(assistsJg.lm) #this is a better predictor than kills as 53.3% of variability in items completed is explained by assists

glacialJg.lm2 = lm(ItemsCompleted~GameTime+Kills+Assists+Deaths, data=glacialJg.df) #first full multivariate model between the predictor
#and the response variables

plot(glacialJg.lm2)

summary(glacialJg.lm2)

par(mfrow =c(2,2)) #sets the graphing window to 1 row, 1 column

hist(glacialJg.df$Kills, xlab = "Kills per Game, Glacial Shurelyia's", main = "Kills per Game Frequencies")
#makes a histogram of the number of kills per game for glacial taric jungle
hist(glacialJg.df$Deaths, xlab = "Deaths per Game, Glacial Shurelyia's", main = "Deaths per Game Frequencies") 
#makes a histogram of number of deaths per game for glacial taric jungle
hist(glacialJg.df$Assists, xlab = "Assists per Game, Glacial Shurelyia's", main = "Assists per Game Frequencies")
#makes a histogram of the number of assists per game for glacial taric jungle
hist(glacialJg.df$GameTime, xlab = "Game Times, Glacial Shurelyia's", main = "Game Time Frequencies")
#makes a histogram of the game times for glacial taric jungle


