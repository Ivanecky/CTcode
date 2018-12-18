# analysis of video game dataset
# import libraries
library(tidyverse)
library(ggplot2)
library(maps)
library(RColorBrewer)
library(ellipse)
library(highcharter)
library(ggiraph)
library(leaps)
library(caret)
library(MASS)
library(modelr)
library(Hmisc)
library(corrplot)
library(gplots)
library(zoo)
library(lubridate)
library(arsenal)
library(pastecs)
library(car)
library(ggpubr)

# import dataset from csv
df <- read.csv("video_game_sales.csv")
df$User_Score <- as.double(df$User_Score)
# changes Year_of_Release to a Date object
df$Year_of_Release <- as.Date(as.character(df$Year_of_Release), format = "%Y")

attach(df)

# print out information on data variables
str(df)

# generate initial correlation matrix of variables
cor_matrix <- rcorr(as.matrix(df[,6:14]))
cor_matrix

# matrix of correlation coefficients
cor_mat <- cor(df[,6:14], use = "pairwise")

# correlation matrix discluding all NA values and only displaying significant p-values
corrplot(cor_mat, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, p.mat = cor_matrix$P,
         sig.level = 0.01, insig = "blank", 
         title = "Significant Correlation Plot")

df$log_global <- log10(df$Global_Sales)

# generate histograms for checking distributions
ggplot(df, aes(log_global)) + geom_histogram(binwidth = 0.1, color="darkblue", fill="lightblue") + 
  ggtitle("Distribution of Global Sales") +
  geom_vline(aes(xintercept=mean(log_global)),color="blue", linetype="dashed", size=1)

# Critic_Score with NA values ignored
ggplot(data = subset(df, !is.na(Critic_Score)), aes(Critic_Score)) + geom_histogram(binwidth = 5, color="darkblue", fill="lightblue") + 
  ggtitle("Distribution of Critic Score") +
  geom_vline(aes(xintercept=mean(Critic_Score)),color="blue", linetype="dashed", size=1)

# generate new dataframe without rows that have NA
df2 <- na.omit(df)
View(df2)

# comparison of two datasets; one with and without NA values
ggplot(df2, aes(Year_of_Release)) + geom_density(color = "darkblue", fill = "lightblue") + 
  ggtitle("Density Plot of Release Year") +
  geom_vline(aes(xintercept=mean(Year_of_Release)),color="blue", linetype="dashed", size=1) +
  labs(x = "Year of Release", y = "Density")

ggplot(df, aes(Year_of_Release)) + geom_density(color = "darkblue", fill = "lightblue") + 
  ggtitle("Density Plot of Release Year") +
  geom_vline(aes(xintercept=mean(Year_of_Release)),color="blue", linetype="dashed", size=1) +
  labs(x = "Year of Release", y = "Density")

# generate summary of each dataframe to compare for similarities
summary(df)
summary(df2)

# run wilcox-tests for quantitative variables
# use wilcox as normal dist not assumed
wilcox.test(df$NA_Sales, df2$NA_Sales)
wilcox.test(df$EU_Sales, df2$EU_Sales)
wilcox.test(df$JP_Sales, df2$JP_Sales)
wilcox.test(df$Other_Sales, df2$Other_Sales)
wilcox.test(df$Global_Sales, df2$Global_Sales)
wilcox.test(df$Critic_Score, df2$Critic_Score)
wilcox.test(df$Critic_Count, df2$Critic_Count)
wilcox.test(df$User_Score, df2$User_Score)
wilcox.test(df$User_Count, df2$User_Count)

#---------------------------------------------------------------------------------------------------------
# handling NA_Sales

ggplot(df[which(NA_Sales <= 0.390),], aes(y=NA_Sales)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4) +
  ggtitle("NA Sales")

# histogram - note skewed data
ggplot(df[which(NA_Sales < 5 & NA_Sales > 0),], aes(NA_Sales)) + geom_histogram(binwidth = 0.1, color="darkblue", fill="lightblue") + 
  ggtitle("Distribution of NA Sales")

# plot to check for normality discluding some values that strongly skew
qqPlot(df[which(NA_Sales < 0.5 & NA_Sales > 0),]$NA_Sales, 
       main = "QQPlot for NA_Sales > 0 and NA_Sales < 0.5")

# create logged value of NA_Sales
# function to take log only if value > 0 
# takes in x - NA_Sales value
log_func <- function(x, mx){
  if(x > 0){
    x <- log10(x)
  }
  else{
    x <- log10(mx)
  }
  return(x)
}

# apply function to each row in dataframe
df$log_na <- lapply(df[,c("NA_Sales")], log_func, mx = mean(df$NA_Sales))

# set log_na to be numeric from unknown
df$log_na <- as.numeric(df$log_na)

# histogram of logged data
ggplot(df, aes(log_na)) + geom_histogram(binwidth = 0.2, color="darkblue", fill="lightblue") + 
  ggtitle("Distribution of NA Sales")

# boxplot of logged data
ggplot(df, aes(y=log_na)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4) +
  ggtitle("log10 value of NA_Sales")

# get statistics of log_na
summary(df$log_na)

nasales <- log10(df[which(df$NA_Sales > 0),]$NA_Sales)

# plot of logged data discluding NA_Sales = 0
ggqqplot(nasales)

# plot of data modified by function
ggqqplot(df$log_na)

#------------------------------------------------------------------------------------------------------------
# handling global_sales data
# histogram of initial data
ggplot(df, aes(Global_Sales)) + geom_histogram(binwidth = 0.5, color="darkblue", fill="lightblue") + 
  ggtitle("Distribution of Global Sales") +
  geom_vline(aes(xintercept=mean(Global_Sales)),color="blue", linetype="dashed", size=1)

# histogram of log10 data
ggplot(df, aes(log_global)) + geom_histogram(binwidth = 0.1, color="darkblue", fill="lightblue") + 
  ggtitle("Distribution of log10 Global Sales") +
  geom_vline(aes(xintercept=mean(log_global)),color="blue", linetype="dashed", size=1)

# get summary of log_global and run normality tests
summary(df$log_global)

# generate random sample for shapiro test
sample <- df[sample(1:nrow(df), 5000, replace=FALSE),]

shapiro.test(sample$log_global)

ggqqplot(df$log_global) + ggtitle("QQPlot of log_global")

# compare df with sample
# run wilcox-tests for quantitative variables
# use wilcox as normal dist not assumed
wilcox.test(df$NA_Sales, sample$NA_Sales)
wilcox.test(df$EU_Sales, sample$EU_Sales)
wilcox.test(df$JP_Sales, sample$JP_Sales)
wilcox.test(df$Other_Sales, sample$Other_Sales)
wilcox.test(df$Global_Sales, sample$Global_Sales)
wilcox.test(df$Critic_Score, sample$Critic_Score)
wilcox.test(df$Critic_Count, sample$Critic_Count)
wilcox.test(df$User_Score, sample$User_Score)
wilcox.test(df$User_Count, sample$User_Count)

attach(sample)

# boxplot of log data by platform
ggplot(sample, aes(x=Platform, y=log_global, color=Platform)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4) +
  ggtitle("log Global Sales by Platform") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# compute 1-way ANOVA test for global_sales by region
# use log_global as normality is required
gs.aov <- aov(log_global ~ Platform, data = sample)
summary(gs.aov)
TukeyHSD(gs.aov)

# chi squared between genre and platform
# create data table - drop levels to elimate zeros
tbl <- table(droplevels(Genre), droplevels(Platform))
chisq.test(tbl)

# generate interactive scatter plot of critic score vs global sales
# first check normality of critic score
ggplot(sample, aes(Critic_Score)) + geom_histogram(binwidth = 1, color="darkblue", fill="lightblue") + 
  ggtitle("Distribution of Critic Score") +
  geom_vline(aes(xintercept=mean(Critic_Score)),color="blue", linetype="dashed", size=1, na.rm = T)

# perform Shapiro-Wilk test
shapiro.test(Critic_Score)

# take log10 of critic score
sample$log_cs <- log10(Critic_Score)

# replot histogram
ggplot(sample, aes(log_cs)) + geom_histogram(binwidth = 0.01, color="darkblue", fill="lightblue") + 
  ggtitle("Distribution of log Critic Score") +
  geom_vline(aes(xintercept=mean(log_cs)),color="blue", linetype="dashed", size=1, na.rm = T)

# shapiro-wilk on logged data
sample$log_cs <- as.numeric(sample$log_cs)
shapiro.test(sample$log_cs)

# scatter plot of critic_score vs log_global
# data < q3
ggplot(sample[which(Global_Sales < 0.45),], aes(x = Critic_Score, y = Global_Sales, color = Platform)) + 
  geom_point() +
  ggtitle("Critic Score vs Global Sales using Data < Q3") +
  labs(x = "Critic Score", y = "Global Sales")

# all data
ggplot(sample, aes(x = Critic_Score, y = Global_Sales, color = Platform)) + 
  geom_point() +
  ggtitle("Critic Score vs Global Sales using all data") +
  labs(x = "Critic Score", y = "Global Sales")

# check normality of other sales data before using as predictor variables in linear model
# NA
ggplot(sample, aes(NA_Sales)) + geom_histogram(binwidth = 1, color="darkblue", fill="lightblue") + 
  ggtitle("Distribution of NA Sales") +
  geom_vline(aes(xintercept=mean(NA_Sales)),color="blue", linetype="dashed", size=1)

# EU
ggplot(sample, aes(EU_Sales)) + geom_histogram(binwidth = 0.1, color="darkblue", fill="lightblue") + 
  ggtitle("Distribution of EU Sales") +
  geom_vline(aes(xintercept=mean(EU_Sales)),color="blue", linetype="dashed", size=1)

# JP
ggplot(sample, aes(JP_Sales)) + geom_histogram(binwidth = 0.1, color="darkblue", fill="lightblue") + 
  ggtitle("Distribution of JP Sales") +
  geom_vline(aes(xintercept=mean(JP_Sales)),color="blue", linetype="dashed", size=1)

# Other
ggplot(sample, aes(Other_Sales)) + geom_histogram(binwidth = 0.1, color="darkblue", fill="lightblue") + 
  ggtitle("Distribution of Other Sales") +
  geom_vline(aes(xintercept=mean(Other_Sales)),color="blue", linetype="dashed", size=1)

# function that iterates rows and takes log of values. if value is zero, replaces it with mean and then logs it.
# this is to avoid -inf error because log10(0) is undefined and sales data contains zeros
log_func <- function(x, mx){
  if(x > 0){
    x <- log10(x)
  }
  else{
    x <- log10(mx)
  }
  return(x)
}

# generate log data for sales variables
sample$log_na <- lapply(sample[,c("NA_Sales")], log_func, mx = mean(df$NA_Sales))
sample$log_eu <- lapply(sample[,c("EU_Sales")], log_func, mx = mean(df$EU_Sales))
sample$log_jp <- lapply(sample[,c("JP_Sales")], log_func, mx = mean(df$JP_Sales))
sample$log_other <- lapply(sample[,c("Other_Sales")], log_func, mx = mean(df$Other_Sales))

# change to numeric type
sample$log_na <- as.numeric(sample$log_na)
sample$log_eu <- as.numeric(sample$log_eu)
sample$log_jp <- as.numeric(sample$log_jp)
sample$log_other <- as.numeric(sample$log_other)

# function to generate histogram for logged data
gen_log_hist <- function(x) {
  # histogram of log10 data
  ggplot(sample, aes(x)) + geom_histogram(binwidth = 0.1, color="darkblue", fill="lightblue") + 
    geom_vline(aes(xintercept=mean(x)),color="blue", linetype="dashed", size=1) +
    labs(x = deparse(substitute(x))) +
    ggtitle("Distribution of ", deparse(substitute(x)))
}

# generate histograms to check distributions
gen_log_hist(sample$log_na)
gen_log_hist(sample$log_eu)
gen_log_hist(sample$log_jp)
gen_log_hist(sample$log_other)

# generate linear model to predict global sales
temp.model <- lm(sample$log_global ~ sample$Critic_Count + sample$Critic_Score + sample$User_Count + 
                   sample$User_Score + sample$log_na + sample$log_eu + sample$log_jp + sample$log_other )
summary(temp.model)

# generate stepwise model using initial model
step.model <- stepAIC(temp.model, direction = "both", trace = FALSE)
summary(step.model)

# winner takes all versus "restaurant" competition
# reorder data by global_sales value
ordered_df <- df[order(-Global_Sales),]

# generate sum of global sales versus sum of top 20%
global_total_sum <- sum(df$Global_Sales)
# sum of top 20%
global_top20_sum <- sum(ordered_df[1:3343,]$Global_Sales)

# accounts fo 75% of total sales
top20_vs_total = ((global_top20_sum) / (global_total_sum)) * 100
