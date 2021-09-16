setwd("/Users/User/Desktop/League of Legends Excel Databases")
getwd()

install.packages(yarrr)
library(yarrr) #the library command opens whatever installed package you wish to open and imports commands within that package
install.packages(car) #installs the car package which will allow us to do more advanced regression analysis
library(car) #opens car package for this R file
install.packages("rgl") #to actually use rgl we need to install multiple other packages...
install.packages("shiny")
install.packages("manipulateWidget")
library(manipulateWidget)
library(shiny)
library(rgl) #loads the rgl package which will allow us to view some 3D scatterplots of taric jg data
library(readxl)

glacialJg_data <- read_excel("GlacialGames.xlsx") #imports our desired excel file and data within

head(glacialJg_data) #prints the first few lines of data

summary(glacialJg_data)

plot(glacialJg_data)

glacialJg.df = glacialJg_data #creates a data frame, sets it equal to the data set imported from excel

par(mfrow=c(1,1)) #sets plot to 1 row, 1 column

plot(x = glacialJg.df$GameTime, y = glacialJg.df$Kills, col = "purple", pch = 16, main = "Kills vs Game Time, purple color")
#this plots kills vs gametime for glacial jungle data
#main sets the title of the graph. x and y set the variables plotted (dependent variable on y, independent on x)
#pch changes the icon of each point. for values around 16 its a small dot. for other values, the icon changes

plot(x = glacialJg.df$GameTime, y = glacialJg.df$ItemsCompleted, col = yarrr::transparent("purple", trans.val = .6), pch = 16, main = 
       "Items Completed vs Game Time, transparent color")
#trans.val sets te transparency of our color. trans.val = 1 means complete transparency and trans.val = 0 means no transparency

##FIRST LINEAR MODEL LISTED BELOW##

glacialJg.lm=lm(ItemsCompleted~GameTime,data=glacialJg.df)
glacialJg.aic = step(glacialJg.lm)
glacialJg.aic

glacialJg.mod1 <- lm(ItemsCompleted~GameTime,data=glacialJg.df)

par(mfrow=c(2, 2))

plot(glacialJg.lm)
summary(glacialJg.lm)

confint(glacialJg.lm) #creates a 95% confidence interval for the coefficients of glacialJg.lm
#neither interval includes zero meaning both values are statistically significant and we reject null hypothesis for both

plot(predict (glacialJg.lm), residuals (glacialJg.lm)) #these plot the PREDICTED number of completed items - from the linear
#model (glacialJg.lm) - versus the RESIDUALS. The purpose is to show how far off our predicted values were from the actual
#values, as indicated by the residuals
plot(predict (glacialJg.lm), rstudent (glacialJg.lm))

#there is some significant variation between predicted items and actual items
#the hatvalues function allows us to identify the LEVERAGE of each observation

plot(hatvalues(glacialJg.lm)) #vast majority of values have very low leverage

which.max(hatvalues(glacialJg.lm)) #which.max identifies which observation has the largest leverage
#here, observation 48 has the greatest leverage for our first linear model (around 0.030)

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

##SECOND LINEAR MODEL LISTED BELOW##

glacialJg.lm2 = lm(ItemsCompleted~GameTime+Kills+Assists+Deaths+CSperMin, data=glacialJg.df) #first full multivariate model between the predictor
#and the response variables

glacialJg.mod2 <- lm(ItemsCompleted~GameTime+Kills+Assists+Deaths+CSperMin, data=glacialJg.df)

plot(glacialJg.lm2)

summary(glacialJg.lm2)
vif(glacialJg.lm2) #provides the variance inflation factor (VIF) for our first multivariate model
#all VIF values are below 2.5 and well below 10 indicating minimal to no collinearity

which.max(hatvalues(glacialJg.lm2)) #observation 86 has the largest leverage in multivariate model, as indicated in
#residauls vs leverage plot

glacialJg.aov <- aov(glacialJg.lm2)

summary(glacialJg.aov) #these commands create and print an ANOVA (analysis of variance) table

anova(glacialJg.mod1, glacialJg.mod2) #compares the simple and multivariate model for glacial jungle data
#Df = 3 indicating 3 more degrees of freedom in the multivariate model (3 more predictors means 3 more varying coefficients)
#extremely small p value of 2.2*10^-16 means that the multivariate model is significantly better than the simple model

par(mfrow=c(1,1))

plot(glacialJg.df$CSperMin~glacialJg.df$Deaths, data=glacialJg.df)
plot(glacialJg.df$CSperMin~glacialJg.df$GameTime, data=glacialJg.df)
plot(glacialJg.df$CSperMin~glacialJg.df$ItemsCompleted, data=glacialJg.df)

CSTime.lm = lm(glacialJg.df$CSperMin~glacialJg.df$GameTime, data=glacialJg.df)

par(mfrow=c(2,2))

plot(CSTime.lm)
summary(CSTime.lm) #extremely slight negative association between CS per minute and game time
#the result IS statistically significant as the p value is < 0.05

CSDeath.lm = lm(glacialJg.df$CSperMin~glacialJg.df$Deaths, data=glacialJg.df)

plot(CSDeath.lm)
summary(CSDeath.lm) #good association here. R^2 value of 0.5133. CS per min generally dropping with increased deaths

##THIRD LINEAR MODEL LISTED HERE##

glacialJg.lm3 = lm(ItemsCompleted~GameTime+Assists+Deaths, data=glacialJg.df) #second multivariate model, but excluding kills

glacialJg.mod3 <- lm(ItemsCompleted~GameTime+Assists+Deaths, data=glacialJg.df)

anova(glacialJg.mod3, glacialJg.mod2) #our second linear model is meaningfully better than the third by inclusion of the Kills
#predictor

par(mfrow =c(2,2)) #sets the graphing window to 1 row, 1 column

#the code below makes a bunch of glacial jungle histograms

hist(glacialJg.df$Kills, xlab = "Kills per Game, Glacial Shurelyia's", main = "Kills per Game Frequencies")
#makes a histogram of the number of kills per game for glacial taric jungle
hist(glacialJg.df$Deaths, xlab = "Deaths per Game, Glacial Shurelyia's", main = "Deaths per Game Frequencies") 
#makes a histogram of number of deaths per game for glacial taric jungle
hist(glacialJg.df$Assists, xlab = "Assists per Game, Glacial Shurelyia's", main = "Assists per Game Frequencies")
#makes a histogram of the number of assists per game for glacial taric jungle
hist(glacialJg.df$GameTime, xlab = "Game Times, Glacial Shurelyia's", main = "Game Time Frequencies")
#makes a histogram of the game times for glacial taric jungle

?hist #gives us information about histograms in R

par(mfrow=c(1,1))

#below is a fancier version of a previously plotted histogram
hist(glacialJg.df$Kills, xlab = "Kills per Game, Glacial Shurelyia's", xlim = c(0, 10), ylim = c(0, 0.5), 
     main = "Kills per Game Probabilities", freq = FALSE, col = "lightpink", border = "grey")

#freq = FALSE means this is a probability distribution and NOT a frequency distribution

summary(glacialJg.df$Kills)

par(bg = gray(0.9))
hist(glacialJg.df$Kills, xlab = "Kills per Game, Glacial Shurelyia's", xlim = c(0, 8), ylim = c(0, 120),
     main = "Kills per Game Frequencies", freq = TRUE, col  = "purple", border = "white")

#fancier frequency histogram

par(mfrow=c(2,2))

summary(glacialJg.df$WinLoss) #mean or average of WinLoss is 48.18 meaning the winrate of Glacial Shurelyia's is 48.18%

##FIRST MULTIVARIATE LOGISTIC MODEL WRITTEN HERE##

glacial_logistic.fits = glm(WinLoss ~ GameTime + ItemsCompleted + Kills + Assists + Deaths + CSperMin, data = glacialJg.df, 
                        family = binomial) 

#creates a logistic model to represent the discrete binary variable WinLoss predicted by 6 predictors

plot(glacial_logistic.fits)
summary(glacial_logistic.fits) #here, the only predictor that does not have a statistically significant p value is GameTime
#at p = 0.662039

vif(glacial_logistic.fits) #low collinearities observed between the predictors

glacial_logistic.aic = step(glacial_logistic.fits)
glacial_logistic.aic

vif(glacial_logistic.aic) #gametime was removed as a predictor and collinearities for every predictor were reduced upon
#application of the AIC

plot(glacial_logistic.aic)

names(glacial_logistic.aic)
glacial_logistic.aic$coefficients #lists all the coefficients and their names for the logistic model

glacialWins.prob = predict(glacial_logistic.aic, type="response")
glacial.pred = rep("0", dim(glacialJg.df)[1])
glacial.pred[glacialWins.prob > .5] = "1"
table(glacial.pred, glacialJg.df$WinLoss)

mean(glacial.pred == glacialJg.df$WinLoss)

#the resulting 2x2 matrix indicates that our logistic model, improved by the AIC, is 89% accurate for our known data
#now we will make formal predictions about lightrocket's glacial jungle games with our logistic model. We do this by
#creating a new dataframe and assigning new values to predictors from glacial_logistic.aic

predict(glacial_logistic.aic, newdata=data.frame(ItemsCompleted= c(3, 4), Kills= c(4, 2), Assists= c(7, 13), 
                              Deaths= c(7, 4), CSperMin= c(4, 6.4)), type="response")

#this model turned out to be around 90% accurate for new data

##ALTERNATE LOGISTIC MODEL##

glacial2_logistic.fits <- glm(WinLoss ~ GameTime + ItemsCompleted + Kills*Assists + Deaths + CSperMin + 
                                I(CSperMin^2), data = glacialJg.df, family = binomial)
#creates an altered logistic model with an interaction term and a squared predictor

plot(glacial2_logistic.fits)
summary(glacial2_logistic.fits)

#this hsa a slightly lower AIC but the p values are worse for most of the coefficients

coef(glacial_logistic.fits) #gives us ONLY the coefficients for the logistic model

summary(glacial_logistic.fits)$coef #and this gives us the summary information only for the coefficients. The p values are
#statistically significant for all except GameTime

#AIC is also very high at 159.99 indicating a large degree of collinearity in the logistic model

glacialTime_logistic.fits = glm(WinLoss ~ GameTime, data = glacialJg.df) #new logistic to determine if any relationship exists

plot(glacialTime_logistic.fits)
summary(glacialTime_logistic.fits) #still no statistical significance with p = 0.1851 - though this value IS smaller than
#in the multivariate logistic model
#much larger AIC of 401.6

glacialDeaths_logistic.fits = glm(WinLoss ~ Deaths, data = glacialJg.df)

par(mfrow = c(2,2))

plot(glacialDeaths_logistic.fits)
summary(glacialDeaths_logistic.fits) #Deaths has statistical significance with a very low p value, and is negatively associated
#with WinLoss. model has high AIC value of 317.66

glacialKills_logistic.fits = glm(WinLoss ~ Kills, data = glacialJg.df)

par(mfrow = c(2,2))

plot(glacialKills_logistic.fits)
summary(glacialKills_logistic.fits) #Modest association between probability of a win and kills

glacialAssists_logistic.fits = glm(WinLoss ~ Assists, data = glacialJg.df)

plot(glacialAssists_logistic.fits)
summary(glacialAssists_logistic.fits) #intercept found not to be statistically significant. High AIC value of 323.85
#modest positive association observed between assists and probability of a win

hist(glacialJg.df$WinLoss, xlab= "Wins (1) and Losses (0)", ylab="Total Wins or Losses", main = "Frequency of Wins and Losses,
     Glacial Shurelyia's", ylim = c(0, 150))

plot3d( 
  x=glacialJg.df$Kills, y=glacialJg.df$Deaths, z=glacialJg.df$ItemsCompleted, 
  col = "pink",
  type = 's', 
  radius = .1,
  xlab="Kills", ylab="Deaths", zlab="Completed Items") #creates a 3D plot between kills, deaths, and completed items

movie3d(spin3d(axis=c(0,0,1), rpm=3), duration=15, dir = "./") #rotates the plot. also creates a GIF with an appropriate package
