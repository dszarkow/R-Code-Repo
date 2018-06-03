# File: ReadCsv.R

# D. Szarkowicz
# April, 2018

# Set the working directory - Use Session...
setwd("C:/Users/dszar/Desktop/R Work/RScripts")

# Read the file Data/SampleData.csv
sampleData <- read.csv("Data/SampleData.csv", header=TRUE, sep=",")

# Extract the components
x <- sampleData[,1]
y <- sampleData[,2]
z <- sampleData[,3]

# Plot the values of x, y and z
plot(x,type='l', col="red",lty=1,ylim=c(-6,6),main="Main Title",xlab="Time (sec)",ylab='X, Y or Z Values (units)')
lines(y, col="green",lty=1,ylim=c(-6,6))
lines(z, col="blue",lty=1,ylim=c(-6,6))
grid()
legend("topleft", inset=0.05, "Three Plots", c("X","Y","Z"),
       col=c("red","green","blue"),lty=c(1,1,1))

# Get the mean values
meanX <- mean(x)
meanX

meanY <- mean(y)
meanY

meanZ <- mean(z)
meanZ

# Get the mean value of each column
columnMeans <- colMeans(sampleData)
columnMeans

# Get the standard deviation of each column
# Use the apply() function to apply the function sd() to all columns
columnStdDevs <- apply(sampleData, 2, sd)
columnStdDevs

# Find the correlation between x and y
cxy <- cor(x,y)
cxy

# Find the correlation between x and z
cxz <- cor(x,z)
cxz

# Find the correlation between y and z
cyz <- cor(y,z)
cyz

# Find all correlations at once
corSd <- cor(sampleData)
corSd

quantile(x)
min(sampleData)
min(x)
min(y)
min(z)
summary(sampleData)

# Using basic plot functions
hist(x)
hist(y)
hist(z, breaks=30)

# Using library ggplot2
library(ggplot2);

# Basic histogram
ggplot(sampleData, aes(x=x)) + geom_histogram(color='black',fill='red',binwidth=0.25)
ggplot(sampleData, aes(x=y)) + geom_histogram(color='black',fill='green',binwidth=0.25)
ggplot(sampleData, aes(x=z)) + geom_histogram(color='black',fill='blue',binwidth=0.25)

df <- data.frame(coord=factor(rep(c("X", "Y", "Z"), each=100)),
                 value=c(x,y,z))

# Overlaid histograms
ggplot(df, aes(x=value, fill=coord, color='black')) +
  geom_histogram(alpha=1.0, color='black', position="dodge", binwidth=0.50)

# Scatter plot
scatter.smooth(x,y, main="X ~ Y")
scatter.smooth(y,z, main="Y ~ Z")
scatter.smooth(x,z, main="X ~ Z")

# Box plots
par(mfrow=c(1, 3))  # divide graph area in 3 columns
# box plot for 'x'
boxplot(x, main="X", sub=paste("Outlier rows: ", boxplot.stats(x)$out))  
# box plot for 'y'
boxplot(y, main="Y", sub=paste("Outlier rows: ", boxplot.stats(x)$out)) 
# box plot for 'z'
boxplot(y, main="z", sub=paste("Outlier rows: ", boxplot.stats(z)$out))

# Density plots
library(e1071)

par(mfrow=c(1, 3))  # divide graph area in 3 columns
# density plot for 'x'
plot(density(x), main="Density Plot: X", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(x), 2)))
polygon(density(x), col="red")

# density plot for 'y'
plot(density(y), main="Density Plot: Y", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(y), 2)))
polygon(density(y), col="green")

# density plot for 'z'
plot(density(z), main="Density Plot: Z", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(z), 2)))
polygon(density(z), col="blue")

# Linear regression model
#linearMod <- lm(x ~ y, data=sampleData)
linearModXY <- lm(x ~ y)
linearModXY

linearModYZ <- lm(y ~ z)
linearModYZ

linearModXZ <- lm(x ~ z)
linearModXZ

summary(linearModXY)
summary(linearModYZ)
summary(linearModXZ)

par(mfrow=c(1, 1))  # divide graph area in 1 columns

# Scatter plots with regression lines
plot(x,y, main="Y ~ X")
abline(lm(y ~ x))

plot(z,y, main="Y ~ Z")
abline(lm(y ~ z))

plot(x,z, main="Z ~ X")
abline(lm(z ~ x))

# Look for p-values < 0.05

# Using package 'psych'
library(psych)

describe(sampleData)





