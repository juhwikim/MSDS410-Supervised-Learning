train <- read.csv('/Users/juhwikim/Desktop/MSDS/410 Supervised Learning/moneyball_train.csv')
test <- read.csv('/Users/juhwikim/Desktop/MSDS/410 Supervised Learning/moneyball_test.csv')

library(corrplot)
library(PerformanceAnalytics)

head(train, n=5)

head(test, n=5)

str(train)

summary(train)

apply(train, 2, sd)

train_ZScore <- train
train_ZScore$TARGET_WINS_ZS <- (train_ZScore$TARGET_WINS - mean(train_ZScore$TARGET_WINS, na.rm = TRUE))/sd(train_ZScore$TARGET_WINS, na.rm = TRUE)
train_ZScore$TEAM_BATTING_H_ZS <- (train_ZScore$TEAM_BATTING_H - mean(train_ZScore$TEAM_BATTING_H,  na.rm = TRUE))/sd(train_ZScore$TEAM_BATTING_H,  na.rm = TRUE)
train_ZScore$TEAM_BATTING_2B_ZS <- (train_ZScore$TEAM_BATTING_2B - mean(train_ZScore$TEAM_BATTING_2B,  na.rm = TRUE))/sd(train_ZScore$TEAM_BATTING_2B,  na.rm = TRUE)
train_ZScore$TEAM_BATTING_3B_ZS <- (train_ZScore$TEAM_BATTING_3B - mean(train_ZScore$TEAM_BATTING_3B, na.rm = TRUE))/sd(train_ZScore$TEAM_BATTING_3B,  na.rm = TRUE)
train_ZScore$TEAM_BATTING_HR_ZS <- (train_ZScore$TEAM_BATTING_HR - mean(train_ZScore$TEAM_BATTING_HR,  na.rm = TRUE))/sd(train_ZScore$TEAM_BATTING_HR,  na.rm = TRUE)
train_ZScore$TEAM_BATTING_BB_ZS <- (train_ZScore$TEAM_BATTING_BB - mean(train_ZScore$TEAM_BATTING_BB,  na.rm = TRUE))/sd(train_ZScore$TEAM_BATTING_BB,  na.rm = TRUE)
train_ZScore$TEAM_BATTING_SO_ZS <- (train_ZScore$TEAM_BATTING_SO - mean(train_ZScore$TEAM_BATTING_SO,  na.rm = TRUE))/sd(train_ZScore$TEAM_BATTING_SO,  na.rm = TRUE)
train_ZScore$TEAM_BASERUN_SB_ZS <- (train_ZScore$TEAM_BASERUN_SB - mean(train_ZScore$TEAM_BASERUN_SB,  na.rm = TRUE))/sd(train_ZScore$TEAM_BASERUN_SB,  na.rm = TRUE)
train_ZScore$TEAM_BASERUN_CS_ZS <- (train_ZScore$TEAM_BASERUN_CS - mean(train_ZScore$TEAM_BASERUN_CS,  na.rm = TRUE))/sd(train_ZScore$TEAM_BASERUN_CS,  na.rm = TRUE)
train_ZScore$TEAM_BATTING_HBP_ZS <- (train_ZScore$TEAM_BATTING_HBP - mean(train_ZScore$TEAM_BATTING_HBP,  na.rm = TRUE))/sd(train_ZScore$TEAM_BATTING_HBP,  na.rm = TRUE)
train_ZScore$TEAM_PITCHING_H_ZS <- (train_ZScore$TEAM_PITCHING_H - mean(train_ZScore$TEAM_PITCHING_H,  na.rm = TRUE))/sd(train_ZScore$TEAM_PITCHING_H,  na.rm = TRUE)
train_ZScore$TEAM_PITCHING_HR_ZS <- (train_ZScore$TEAM_PITCHING_HR - mean(train_ZScore$TEAM_PITCHING_HR, na.rm = TRUE))/sd(train_ZScore$TEAM_PITCHING_HR,  na.rm = TRUE)
train_ZScore$TEAM_PITCHING_BB_ZS <- (train_ZScore$TEAM_PITCHING_BB - mean(train_ZScore$TEAM_PITCHING_BB,  na.rm = TRUE))/sd(train_ZScore$TEAM_PITCHING_BB,  na.rm = TRUE)
train_ZScore$TEAM_PITCHING_SO_ZS <- (train_ZScore$TEAM_PITCHING_SO - mean(train_ZScore$TEAM_PITCHING_SO,  na.rm = TRUE))/sd(train_ZScore$TEAM_PITCHING_SO,  na.rm = TRUE)
train_ZScore$TEAM_FIELDING_E_ZS <- (train_ZScore$TEAM_FIELDING_E - mean(train_ZScore$TEAM_FIELDING_E,  na.rm = TRUE))/sd(train_ZScore$TEAM_FIELDING_E,  na.rm = TRUE)
train_ZScore$TEAM_FIELDING_DP_ZS <- (train_ZScore$TEAM_FIELDING_DP - mean(train_ZScore$TEAM_FIELDING_DP,  na.rm = TRUE))/sd(train_ZScore$TEAM_FIELDING_DP,  na.rm = TRUE)

head(train_ZScore,n=5)

str(train_ZScore)

par("mar")
par(mar=c(7,5,1,1), mfrow=c(2,2))
boxplot(train_ZScore[19:22])
boxplot(train_ZScore[23:26])
boxplot(train_ZScore[27:30])
boxplot(train_ZScore[31:34])

attach(mtcars)
par(mar=c(7,5,1,1), mfrow=c(2,2))
plot(train$TEAM_BATTING_H, train$TARGET_WINS, main = "Scatter", col = "blue",
     cex = 1.0, pch = 16,  xlab = "TEAM_BATTING_H", ylab = "TARGET_WINS")
plot(train$TEAM_BATTING_2B, train$TARGET_WINS, main = "Scatter", col = "green",
     cex = 1.0, pch = 16,  xlab = "TEAM_BATTING_2B", ylab = "TARGET_WINS")
plot(train$TEAM_BATTING_3B, train$TARGET_WINS, main = "Scatter", col = "red",
     cex = 1.0, pch = 16,  xlab = "TEAM_BATTING_3B", ylab = "TARGET_WINS")
plot(train$TEAM_BATTING_HR, train$TARGET_WINS, main = "Scatter", col = "black",
     cex = 1.0, pch = 16,  xlab = "TEAM_BATTING_HR", ylab = "TARGET_WINS")

par(mar=c(7,5,1,1), mfrow=c(2,2))
plot(train$TEAM_BATTING_BB, train$TARGET_WINS, main = "Scatter", col = "blue",
     cex = 1.0, pch = 16,  xlab = "TEAM_BATTING_BB", ylab = "TARGET_WINS")
plot(train$TEAM_BATTING_SO, train$TARGET_WINS, main = "Scatter", col = "green",
     cex = 1.0, pch = 16,  xlab = "TEAM_BATTING_SO", ylab = "TARGET_WINS")
plot(train$TEAM_BASERUN_SB, train$TARGET_WINS, main = "Scatter", col = "blue",
     cex = 1.0, pch = 16,  xlab = "TEAM_BASERUN_SB", ylab = "TARGET_WINS")
plot(train$TEAM_BASERUN_CS, train$TARGET_WINS, main = "Scatter", col = "green",
     cex = 1.0, pch = 16,  xlab = "TEAM_BASERUN_CS", ylab = "TARGET_WINS")

par(mar=c(7,5,1,1), mfrow=c(2,2))
plot(train$TEAM_BATTING_HBP, train$TARGET_WINS, main = "Scatter", col = "blue",
     cex = 1.0, pch = 16,  xlab = "TEAM_BATTING_HBP", ylab = "TARGET_WINS")
plot(train$TEAM_PITCHING_H, train$TARGET_WINS, main = "Scatter", col = "green",
     cex = 1.0, pch = 16,  xlab = "TEAM_PITCHING_H", ylab = "TARGET_WINS")
plot(train$TEAM_PITCHING_HR, train$TARGET_WINS, main = "Scatter", col = "blue",
     cex = 1.0, pch = 16,  xlab = "TEAM_PITCHING_HR", ylab = "TARGET_WINS")
plot(train$TEAM_PITCHING_BB, train$TARGET_WINS, main = "Scatter", col = "green",
     cex = 1.0, pch = 16,  xlab = "TEAM_PITCHING_BB", ylab = "TARGET_WINS")

par(mar=c(7,5,1,1))
plot(train$TEAM_PITCHING_SO, train$TARGET_WINS, main = "Scatter", col = "blue",
     cex = 1.0, pch = 16,  xlab = "TEAM_PITCHING_SO", ylab = "TARGET_WINS")
plot(train$TEAM_FIELDING_E, train$TARGET_WINS, main = "Scatter", col = "green",
     cex = 1.0, pch = 16,  xlab = "TEAM_FIELDING_E", ylab = "TARGET_WINS")
plot(train$TEAM_FIELDING_DP, train$TARGET_WINS, main = "Scatter", col = "blue",
     cex = 1.0, pch = 16,  xlab = "TEAM_FIELDING_DP", ylab = "TARGET_WINS")

# Replacing NAs with Median in Train File
train$TEAM_BATTING_SO[is.na(train$TEAM_BATTING_SO)] <- median(train$TEAM_BATTING_SO, trim = 0, na.rm = TRUE)
train$TEAM_BASERUN_SB[is.na(train$TEAM_BASERUN_SB)] <- median(train$TEAM_BASERUN_SB, trim = 0, na.rm = TRUE)
train$TEAM_BASERUN_CS[is.na(train$TEAM_BASERUN_CS)] <- median(train$TEAM_BASERUN_CS, trim = 0, na.rm = TRUE)
train$TEAM_BATTING_HBP[is.na(train$TEAM_BATTING_HBP)] <- median(train$TEAM_BATTING_HBP, trim = 0, na.rm = TRUE)
train$TEAM_PITCHING_SO[is.na(train$TEAM_PITCHING_SO)] <- median(train$TEAM_PITCHING_SO, trim = 0, na.rm = TRUE)
train$TEAM_FIELDING_DP[is.na(train$TEAM_FIELDING_DP)] <- median(train$TEAM_FIELDING_DP, trim = 0, na.rm = TRUE)

# Replacing NAs with Median in Test File
test$TEAM_BATTING_SO[is.na(test$TEAM_BATTING_SO)] <- median(test$TEAM_BATTING_SO, trim = 0, na.rm = TRUE)
test$TEAM_BASERUN_SB[is.na(test$TEAM_BASERUN_SB)] <- median(test$TEAM_BASERUN_SB, trim = 0, na.rm = TRUE)
test$TEAM_BASERUN_CS[is.na(test$TEAM_BASERUN_CS)] <- median(test$TEAM_BASERUN_CS, trim = 0, na.rm = TRUE)
test$TEAM_BATTING_HBP[is.na(test$TEAM_BATTING_HBP)] <- median(test$TEAM_BATTING_HBP, trim = 0, na.rm = TRUE)
test$TEAM_PITCHING_SO[is.na(test$TEAM_PITCHING_SO)] <- median(test$TEAM_PITCHING_SO, trim = 0, na.rm = TRUE)
test$TEAM_FIELDING_DP[is.na(test$TEAM_FIELDING_DP)] <- median(test$TEAM_FIELDING_DP, trim = 0, na.rm = TRUE)

train$TEAM_BATTING_1B <- train$TEAM_BATTING_H-train$TEAM_BATTING_2B-train$TEAM_BATTING_3B-train$TEAM_BATTING_HR
train$TEAM_BATTING_H_RATIO <- train$TEAM_BATTING_H/(train$TEAM_BATTING_H+train$TEAM_BATTING_BB+train$TEAM_BATTING_HBP+train$TEAM_BATTING_SO)
train$TEAM_BATTING_WALK <- (train$TEAM_BATTING_BB+train$TEAM_BATTING_HBP)
train$TEAM_BASERUN_SB_RATE <- train$TEAM_BASERUN_SB/(train$TEAM_BASERUN_SB+train$TEAM_BASERUN_CS)
train$TEAM_PITCHING_SO_RATIO <- train$TEAM_PITCHING_SO/(train$TEAM_PITCHING_SO+train$TEAM_PITCHING_BB+train$TEAM_PITCHING_H+train$TEAM_PITCHING_HR)

##Adding new variables to Test file
test$TEAM_BATTING_1B <- test$TEAM_BATTING_H-test$TEAM_BATTING_2B-test$TEAM_BATTING_3B-test$TEAM_BATTING_HR
test$TEAM_BATTING_H_RATIO <- test$TEAM_BATTING_H/(test$TEAM_BATTING_H+test$TEAM_BATTING_BB+test$TEAM_BATTING_HBP+test$TEAM_BATTING_SO)
test$TEAM_BATTING_WALK <- (test$TEAM_BATTING_BB+test$TEAM_BATTING_HBP)
test$TEAM_BASERUN_SB_RATE <- test$TEAM_BASERUN_SB/(test$TEAM_BASERUN_SB+test$TEAM_BASERUN_CS)
test$TEAM_PITCHING_SO_RATIO <- test$TEAM_PITCHING_SO/(test$TEAM_PITCHING_SO+test$TEAM_PITCHING_BB+test$TEAM_PITCHING_H+test$TEAM_PITCHING_HR)

summary(train)

#BASERUN_SB_RATE returned one NA value
which(is.na(train$TEAM_BASERUN_SB_RATE))
train[1211,] #NA value occured due to dividing 0 by 0 
train$TEAM_BASERUN_SB_RATE[is.na(train$TEAM_BASERUN_SB_RATE)] <- 0 #Change NA value with 0

#Replicate the step to Test file
which(is.na(test$TEAM_BASERUN_SB_RATE))
test[153,] #NA value occured due to dividing 0 by 0 
test$TEAM_BASERUN_SB_RATE[is.na(test$TEAM_BASERUN_SB_RATE)] <- 0 #Change NA value with 0

par(mfrow=c(2,2), mai=c(0.5,0.5,0.5,0.2))
par(fig=c(0,0.5,0.25,1))
hist(train$TARGET_WINS, main="TARGET WINS", breaks=30, col="Blue")
par(fig=c(0.5,1,0.25,1), new=TRUE)
hist(train$TEAM_BATTING_H, main="TOTAL HITS", breaks=30, col="Blue")
par(mfrow=c(2,2), mai=c(0.5,0.5,0.5,0.2))
par(fig=c(0,0.5,0,0.3), new=TRUE)
boxplot(train$TARGET_WINS, horizontal=TRUE, width=1, col="Purple")
par(fig=c(0.5,1,0,0.3), new=TRUE)
boxplot(train$TEAM_BATTING_H, horizontal=TRUE, width=1, col="Purple")
par(mfrow=c(1,1))


par(mfrow=c(2,2), mai=c(0.5,0.5,0.5,0.2))
par(fig=c(0,0.5,0.25,1))
hist(train$TEAM_BATTING_1B, main="FIRST BASE", breaks=30, col="Blue")
par(fig=c(0.5,1,0.25,1), new=TRUE)
hist(train$TEAM_BATTING_2B, main="SECOND BASE", breaks=30, col="Blue")
par(fig=c(0,0.5,0,0.3), new=TRUE)
boxplot(train$TEAM_BATTING_1B, horizontal=TRUE, width=1, col="Purple")
par(fig=c(0.5,1,0,0.3), new=TRUE)
boxplot(train$TEAM_BATTING_2B, horizontal=TRUE, width=1, col="Purple")
par(mfrow=c(1,1))

par(mfrow=c(2,2), mai=c(0.5,0.5,0.5,0.2))
par(fig=c(0,0.5,0.25,1))
hist(train$TEAM_BATTING_3B, main="BATTING TRIPLE", breaks=30, col="Blue")
par(fig=c(0.5,1,0.25,1), new=TRUE)
hist(train$TEAM_BATTING_HR, main="BATTING HOMERUN", breaks=30, col="Blue")
par(fig=c(0,0.5,0,0.3), new=TRUE)
boxplot(train$TEAM_BATTING_3B, horizontal=TRUE, width=1, col="Purple")
par(fig=c(0.5,1,0,0.3), new=TRUE)
boxplot(train$TEAM_BATTING_HR, horizontal=TRUE, width=1, col="Purple")
par(mfrow=c(1,1))



par(mfrow=c(2,2), mai=c(0.5,0.5,0.5,0.2))
par(fig=c(0,0.5,0.25,1))
hist(train$TEAM_BATTING_HBP, main="HIT BY PITCH", breaks=30, col="Blue")
par(fig=c(0.5,1,0.25,1), new=TRUE)
hist(train$TEAM_BATTING_WALK, main="TOTAL WALKS", breaks=30, col="Blue")
par(fig=c(0,0.5,0,0.3), new=TRUE)
boxplot(train$TEAM_BATTING_HBP, horizontal=TRUE, width=1, col="Purple")
par(fig=c(0.5,1,0,0.3), new=TRUE)
boxplot(train$TEAM_BATTING_WALK, horizontal=TRUE, width=1, col="Purple")
par(mfrow=c(1,1))

par(mfrow=c(2,2), mai=c(0.5,0.5,0.5,0.2))
par(fig=c(0,0.5,0.25,1))
hist(train$TEAM_BATTING_H_RATIO, main="TOTAL HIT RATIO", breaks=30, col="Blue")
par(fig=c(0.5,1,0.25,1), new=TRUE)
hist(train$TEAM_BASERUN_SB_RATE, main="STEALING BASE RATIO", breaks=30, col="Blue")
par(fig=c(0,0.5,0,0.3), new=TRUE)
boxplot(train$TEAM_BATTING_H_RATIO, horizontal=TRUE, width=1, col="Purple")
par(fig=c(0.5,1,0,0.3), new=TRUE)
boxplot(train$TEAM_BASERUN_SB_RATE, horizontal=TRUE, width=1, col="Purple")
par(mfrow=c(1,1))

par(mfrow=c(2,2), mai=c(0.5,0.5,0.5,0.2))
par(fig=c(0,0.5,0.25,1))
hist(train$TEAM_BASERUN_SB, main="STOLE BASE", breaks=30, col="Blue")
par(fig=c(0.5,1,0.25,1), new=TRUE)
hist(train$TEAM_BASERUN_CS, main="CAUGHT STEALING", breaks=30, col="Blue")
par(fig=c(0,0.5,0,0.3), new=TRUE)
boxplot(train$TEAM_BASERUN_SB, horizontal=TRUE, width=1, col="Purple")
par(fig=c(0.5,1,0,0.3), new=TRUE)
boxplot(train$TEAM_BASERUN_CS, horizontal=TRUE, width=1, col="Purple")
par(mfrow=c(1,1))

par(mfrow=c(2,2), mai=c(0.5,0.5,0.5,0.2))
par(fig=c(0,0.5,0.25,1))
hist(train$TEAM_PITCHING_H, main="PITCHING HITS", breaks=30, col="Blue")
par(fig=c(0.5,1,0.25,1), new=TRUE)
hist(train$TEAM_PITCHING_HR, main="PITCHING HOMERUN", breaks=30, col="Blue")
par(fig=c(0,0.5,0,0.3), new=TRUE)
boxplot(train$TEAM_PITCHING_H, horizontal=TRUE, width=1, col="Purple")
par(fig=c(0.5,1,0,0.3), new=TRUE)
boxplot(train$TEAM_PITCHING_HR, horizontal=TRUE, width=1, col="Purple")
par(mfrow=c(1,1))

par(mfrow=c(2,2), mai=c(0.5,0.5,0.5,0.2))
par(fig=c(0,0.5,0.25,1))
hist(train$TEAM_PITCHING_BB, main="PITCHER ALLOW WALK", breaks=30, col="Blue")
par(fig=c(0.5,1,0.25,1), new=TRUE)
hist(train$TEAM_PITCHING_SO, main="PITCHING STRIKEOUT", breaks=30, col="Blue")
par(fig=c(0,0.5,0,0.3), new=TRUE)
boxplot(train$TEAM_PITCHING_BB, horizontal=TRUE, width=1, col="Purple")
par(fig=c(0.5,1,0,0.3), new=TRUE)
boxplot(train$TEAM_PITCHING_SO, horizontal=TRUE, width=1, col="Purple")
par(mfrow=c(1,1))

par(mfrow=c(2,2), mai=c(0.5,0.5,0.5,0.2))
par(fig=c(0,0.5,0.25,1))
hist(train$TEAM_FIELDING_E, main="FIELDING ERROR", breaks=30, col="Blue")
par(fig=c(0.5,1,0.25,1), new=TRUE)
hist(train$TEAM_FIELDING_DP, main="DOUBLE PLAY", breaks=30, col="Blue")
par(fig=c(0,0.5,0,0.3), new=TRUE)
boxplot(train$TEAM_FIELDING_E, horizontal=TRUE, width=1, col="Purple")
par(fig=c(0.5,1,0,0.3), new=TRUE)
boxplot(train$TEAM_FIELDING_DP, horizontal=TRUE, width=1, col="Purple")
par(mfrow=c(1,1))

corrplot(cor(train[3:23], use="complete.obs"), method="color", type="upper", tl.col="black", tl.cex=.7, 
         addCoef.col="black", number.cex=.8)

## Determine Quantile to Trim Outliers
quantile(train$TEAM_BATTING_H, probs=c(0.01,0.025,0.975,0.99))
quantile(train$TEAM_BATTING_2B, probs=c(0.01,0.025,0.975,0.99))
quantile(train$TEAM_BATTING_3B, probs=c(0.01,0.025,0.975,0.99))
quantile(train$TEAM_BATTING_HR, probs=c(0.01,0.025,0.975,0.99))
quantile(train$TEAM_BATTING_BB, probs=c(0.01,0.025,0.975,0.99))
quantile(train$TEAM_BATTING_SO, probs=c(0.01,0.025,0.975,0.99))
quantile(train$TEAM_BASERUN_SB, probs=c(0.01,0.025,0.975,0.99))
quantile(train$TEAM_BASERUN_CS, probs=c(0.01,0.025,0.975,0.99))
quantile(train$TEAM_BATTING_HBP, probs=c(0.01,0.025,0.975,0.99))
quantile(train$TEAM_PITCHING_H, probs=c(0.01,0.025,0.975,0.99))
quantile(train$TEAM_PITCHING_HR, probs=c(0.01,0.025,0.975,0.99))
quantile(train$TEAM_PITCHING_BB, probs=c(0.01,0.025,0.975,0.99))
quantile(train$TEAM_PITCHING_SO, probs=c(0.01,0.025,0.975,0.99))
quantile(train$TEAM_FIELDING_E, probs=c(0.01,0.025,0.975,0.99))
quantile(train$TEAM_FIELDING_DP, probs=c(0.01,0.025,0.975,0.99))
quantile(train$TEAM_BATTING_1B, probs=c(0.01,0.025,0.975,0.99))
quantile(train$TEAM_BATTING_H_RATIO, probs=c(0.01,0.025,0.975,0.99))
quantile(train$TEAM_BATTING_WALK, probs=c(0.01,0.025,0.975,0.99))
quantile(train$TEAM_BASERUN_SB_RATE, probs=c(0.01,0.025,0.975,0.99))
quantile(train$TEAM_PITCHING_SO_RATIO, probs=c(0.01,0.025,0.975,0.99))

##Trim out 1st and 99th 
#Replace Upper Outliers with 99th Cutoff Value
train99 <- train
train99$TEAM_BATTING_H[train99$TEAM_BATTING_H > 1945.500] <- 1945.500
train99$TEAM_BATTING_2B[train99$TEAM_BATTING_2B > 351.25] <- 351.25
train99$TEAM_BATTING_3B[train99$TEAM_BATTING_3B > 133.250] <- 133.250
train99$TEAM_BATTING_HR[train99$TEAM_BATTING_HR > 235.00] <- 235.00
train99$TEAM_BATTING_BB[train99$TEAM_BATTING_BB > 752.750] <- 752.750
train99$TEAM_BATTING_SO[train99$TEAM_BATTING_SO > 1191.25] <- 1191.25

train99$TEAM_BASERUN_SB[train99$TEAM_BASERUN_SB > 434.250] <- 434.250
train99$TEAM_BASERUN_CS[train99$TEAM_BASERUN_CS > 123.500] <- 123.500
train99$TEAM_BASERUN_SB[train99$TEAM_BATTING_HBP > 74.25] <- 74.25 
train99$TEAM_PITCHING_H[train99$TEAM_PITCHING_H > 7054.000] <- 7054.000
train99$TEAM_PITCHING_HR[train99$TEAM_PITCHING_HR > 244] <- 244
train99$TEAM_PITCHING_BB[train99$TEAM_PITCHING_BB > 921.00] <- 921.00
train99$TEAM_PITCHING_SO[train99$TEAM_PITCHING_SO > 1461.75] <- 1461.75 

train99$TEAM_FIELDING_E[train99$TEAM_FIELDING_E > 1228.000] <- 1228.000
train99$TEAM_FIELDING_DP[train99$TEAM_FIELDING_DP > 202.000] <- 202.000 

train99$TEAM_BATTING_1B[train99$TEAM_BATTING_1B > 1558.250] <- 1558.250
train99$TEAM_BATTING_H_RATIO[train99$TEAM_BATTING_H_RATIO > 0.9049695] <- 0.9049695
train99$TEAM_BATTING_WALK[train99$TEAM_BATTING_WALK > 807.750] <- 807.750
train99$TEAM_BASERUN_SB_RATE[train99$TEAM_BASERUN_SB_RATE > 0.8986116] <- 0.8986116
train99$TEAM_PITCHING_SO_RATIO[train99$TEAM_PITCHING_SO_RATIO > 0.36787623] <- 0.36787623

#Replace Lower Outliers with 1st Cutoff Value
train99$TEAM_BATTING_H[train99$TEAM_BATTING_H < 1193.250] <- 1193.250
train99$TEAM_BATTING_2B[train99$TEAM_BATTING_2B < 141.75] <- 141.75
train99$TEAM_BATTING_3B[train99$TEAM_BATTING_3B < 17.000] <- 17.000
train99$TEAM_BATTING_HR[train99$TEAM_BATTING_HR < 4.75] <- 4.75
train99$TEAM_BATTING_BB[train99$TEAM_BATTING_BB < 79.000] <- 79.000
train99$TEAM_BATTING_SO[train99$TEAM_BATTING_SO < 72.00] <- 72.00

train99$TEAM_BASERUN_SB[train99$TEAM_BASERUN_SB < 24.000] <- 24.000
train99$TEAM_BASERUN_CS[train99$TEAM_BASERUN_CS < 18.750] <- 18.750
train99$TEAM_BASERUN_SB[train99$TEAM_BATTING_HBP < 45.00] <- 45.00
train99$TEAM_PITCHING_H[train99$TEAM_PITCHING_H < 1244.000] <- 1244.000
train99$TEAM_PITCHING_HR[train99$TEAM_PITCHING_HR < 8] <- 8
train99$TEAM_PITCHING_BB[train99$TEAM_PITCHING_BB < 240.00] <- 240.00
train99$TEAM_PITCHING_SO[train99$TEAM_PITCHING_SO < 241.00] <- 241.00 

train99$TEAM_FIELDING_E[train99$TEAM_FIELDING_E < 86.000] <- 86.000
train99$TEAM_FIELDING_DP[train99$TEAM_FIELDING_DP < 80.000] <- 80.000

train99$TEAM_BATTING_1B[train99$TEAM_BATTING_1B < 881.000] <- 881.000
train99$TEAM_BATTING_H_RATIO[train99$TEAM_BATTING_H_RATIO < 0.4280044] <- 0.4280044
train99$TEAM_BATTING_WALK[train99$TEAM_BATTING_WALK < 137.000] <- 137.000
train99$TEAM_BASERUN_SB_RATE[train99$TEAM_BASERUN_SB_RATE < 0.3875000] <- 0.3875000
train99$TEAM_PITCHING_SO_RATIO[train99$TEAM_PITCHING_SO_RATIO < 0.03293516] <- 0.03293516

## Determine Quantile to Trim Outliers in Test dataset
quantile(test$TEAM_BATTING_H, probs=c(0.01,0.025,0.975,0.99))
quantile(test$TEAM_BATTING_2B, probs=c(0.01,0.025,0.975,0.99))
quantile(test$TEAM_BATTING_3B, probs=c(0.01,0.025,0.975,0.99))
quantile(test$TEAM_BATTING_HR, probs=c(0.01,0.025,0.975,0.99))
quantile(test$TEAM_BATTING_BB, probs=c(0.01,0.025,0.975,0.99))
quantile(test$TEAM_BATTING_SO, probs=c(0.01,0.025,0.975,0.99))
quantile(test$TEAM_BASERUN_SB, probs=c(0.01,0.025,0.975,0.99))
quantile(test$TEAM_BASERUN_CS, probs=c(0.01,0.025,0.975,0.99))
quantile(test$TEAM_BATTING_HBP, probs=c(0.01,0.025,0.975,0.99))
quantile(test$TEAM_PITCHING_H, probs=c(0.01,0.025,0.975,0.99))
quantile(test$TEAM_PITCHING_HR, probs=c(0.01,0.025,0.975,0.99))
quantile(test$TEAM_PITCHING_BB, probs=c(0.01,0.025,0.975,0.99))
quantile(test$TEAM_PITCHING_SO, probs=c(0.01,0.025,0.975,0.99))
quantile(test$TEAM_FIELDING_E, probs=c(0.01,0.025,0.975,0.99))
quantile(test$TEAM_FIELDING_DP, probs=c(0.01,0.025,0.975,0.99))
quantile(test$TEAM_BATTING_1B, probs=c(0.01,0.025,0.975,0.99))
quantile(test$TEAM_BATTING_H_RATIO, probs=c(0.01,0.025,0.975,0.99))
quantile(test$TEAM_BATTING_WALK, probs=c(0.01,0.025,0.975,0.99))
quantile(test$TEAM_BASERUN_SB_RATE, probs=c(0.01,0.025,0.975,0.99))
quantile(test$TEAM_PITCHING_SO_RATIO, probs=c(0.01,0.025,0.975,0.99))

##Trim out outliers using 1st and 99th in Test dataset
#Replace Upper Outliers with 99th Cutoff Value
test99 <- test
test99$TEAM_BATTING_H[test99$TEAM_BATTING_H > 1934.52] <- 1934.52
test99$TEAM_BATTING_2B[test99$TEAM_BATTING_2B > 340.68] <- 340.68
test99$TEAM_BATTING_3B[test99$TEAM_BATTING_3B > 136.68] <- 136.68
test99$TEAM_BATTING_HR[test99$TEAM_BATTING_HR > 208.78] <- 208.78
test99$TEAM_BATTING_BB[test99$TEAM_BATTING_BB > 753.98] <- 753.98
test99$TEAM_BATTING_SO[test99$TEAM_BATTING_SO > 1147.52] <- 1147.52

test99$TEAM_BASERUN_SB[test99$TEAM_BASERUN_SB > 418.74] <- 418.74
test99$TEAM_BASERUN_CS[test99$TEAM_BASERUN_CS > 126.94] <- 126.94
test99$TEAM_BASERUN_SB[test99$TEAM_BATTING_HBP > 73.42] <- 73.42 
test99$TEAM_PITCHING_H[test99$TEAM_PITCHING_H > 8817.06] <- 8817.06
test99$TEAM_PITCHING_HR[test99$TEAM_PITCHING_HR > 226.34] <- 226.34
test99$TEAM_PITCHING_BB[test99$TEAM_PITCHING_BB > 1131.04] <- 1131.04 
test99$TEAM_PITCHING_SO[test99$TEAM_PITCHING_SO > 1279.34] <- 1279.34

test99$TEAM_FIELDING_E[test99$TEAM_FIELDING_E > 1239.54] <- 1239.54
test99$TEAM_FIELDING_DP[test99$TEAM_FIELDING_DP > 202.42] <- 202.42

test99$TEAM_BATTING_1B[test99$TEAM_BATTING_1B > 1508.28] <- 1508.28
test99$TEAM_BATTING_H_RATIO[test99$TEAM_BATTING_H_RATIO > 0.8977210] <- 0.8977210
test99$TEAM_BATTING_WALK[test99$TEAM_BATTING_WALK > 815.98] <- 815.98 
test99$TEAM_BASERUN_SB_RATE[test99$TEAM_BASERUN_SB_RATE > 0.8941337] <- 0.8941337
test99$TEAM_PITCHING_SO_RATIO[test99$TEAM_PITCHING_SO_RATIO > 0.37525962] <- 0.37525962

#Replace Lower Outliers with 1st Cutoff Value
test99$TEAM_BATTING_H[test99$TEAM_BATTING_H < 1119.58] <- 1119.58
test99$TEAM_BATTING_2B[test99$TEAM_BATTING_2B < 117.16] <- 117.16
test99$TEAM_BATTING_3B[test99$TEAM_BATTING_3B < 18.58] <- 18.58
test99$TEAM_BATTING_HR[test99$TEAM_BATTING_HR < 5.00] <- 5.00
test99$TEAM_BATTING_BB[test99$TEAM_BATTING_BB < 89.96] <- 89.96
test99$TEAM_BATTING_SO[test99$TEAM_BATTING_SO < 59.08] <- 59.08

test99$TEAM_BASERUN_SB[test99$TEAM_BASERUN_SB < 17.58] <- 17.58
test99$TEAM_BASERUN_CS[test99$TEAM_BASERUN_CS < 15.48] <- 15.48
test99$TEAM_BASERUN_SB[test99$TEAM_BATTING_HBP < 50.74] <- 50.74
test99$TEAM_PITCHING_H[test99$TEAM_PITCHING_H < 1209.00] <- 1209.00
test99$TEAM_PITCHING_HR[test99$TEAM_PITCHING_HR < 9.16] <- 9.16 
test99$TEAM_PITCHING_BB[test99$TEAM_PITCHING_BB < 241.88] <- 241.88
test99$TEAM_PITCHING_SO[test99$TEAM_PITCHING_SO < 319.64] <- 319.64

test99$TEAM_FIELDING_E[test99$TEAM_FIELDING_E < 93.00] <- 93.00
test99$TEAM_FIELDING_DP[test99$TEAM_FIELDING_DP < 81.32] <- 81.32

test99$TEAM_BATTING_1B[test99$TEAM_BATTING_1B < 894.06] <- 894.06
test99$TEAM_BATTING_H_RATIO[test99$TEAM_BATTING_H_RATIO < 0.4310525] <- 0.4310525
test99$TEAM_BATTING_WALK[test99$TEAM_BATTING_WALK < 151.96] <- 151.96
test99$TEAM_BASERUN_SB_RATE[test99$TEAM_BASERUN_SB_RATE < 0.4016864] <- 0.4016864
test99$TEAM_PITCHING_SO_RATIO[test99$TEAM_PITCHING_SO_RATIO < 0.02852929] <- 0.02852929

corrplot(cor(train99[3:23]), method="color", type="upper", tl.col="black", tl.cex=.7, 
         addCoef.col="black", number.cex=.8)

# Model 1 Testing TARGET_WINS & TEAM_BATTING_3B
model_1 <- lm(TARGET_WINS ~TEAM_BATTING_3B, data = train99)
summary(model_1)
coef(model_1)

# Model 2 Testing TARGET_WINS & TEAM_BATTING_H_RATIO
model_2 <- lm(TARGET_WINS ~TEAM_BATTING_H_RATIO, data = train99)
summary(model_2)
coef(model_2)

# Model 3 Testing TARGET_WINS & TEAM_BATTING_WALK
model_3 <- lm(TARGET_WINS ~TEAM_BATTING_WALK, data = train99)
summary(model_3)
coef(model_3)

df_train <- train[ , c("INDEX", "TEAM_BATTING_WALK","TARGET_WINS")]
p <- predict(model_3, df_train)
P_TARGET_WINS <- predict(model_3, test99)
P_TARGET_WINS

df_out <- data.frame(p,train99$TARGET_WINS,train99$INDEX)
head(df_out, n=5)

mse <- sqrt(mean((p - df_out$train99.TARGET_WINS)^2, na.rm=TRUE))
round(mse, digits = 0)

df_test_out <- data.frame(P_TARGET_WINS,test$INDEX)
head(df_test_out, n=5)

write.csv(df_test_out, '/Users/juhwikim/Desktop/MSDS/410 Supervised Learning/MA1_Output.csv', row.names = TRUE)
