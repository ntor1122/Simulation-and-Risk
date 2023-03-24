# load the libraries
library(readxl)
library(quantmod)
library(graphics)
library(ks)
library(stats)
library(EnvStats)
library(gtools)
library(triangle)
library(car)
library(ggplot2)

# Load the data frame
df <- read_excel("Analysis_Data.xlsx",sheet = "Drilling Cost",skip = 2)

# Check the datatypes of the dataframe
str(df)

# Convert char to numeric variable
char_column <- df$`Arithmetic Return - Crude Oil`
non_numeric <- which(is.na(as.numeric(char_column)) & is.character(char_column))
char_column[non_numeric] <- 0
df$`Arithmetic Return - Crude Oil` <- as.numeric(char_column)

char_column <- df$`Arithmetic Return - Natural Gas`
non_numeric <- which(is.na(as.numeric(char_column)) & is.character(char_column))
char_column[non_numeric] <- 0
df$`Arithmetic Return - Natural Gas` <- as.numeric(char_column)

char_column <- df$`Arithmetic Return - Dry Well`
non_numeric <- which(is.na(as.numeric(char_column)) & is.character(char_column))
char_column[non_numeric] <- 0
df$`Arithmetic Return - Dry Well` <- as.numeric(char_column)


# Taking the average of Nominal Cost and Arithmetic Return
df$average_nominalcost <- rowMeans(df[, c("U.S. Nominal Cost per Crude Oil Well Drilled (Thousand Dollars per Well)",
                                          "U.S. Nominal Cost per Natural Gas Well Drilled (Thousand Dollars per Well)",
                                          "U.S. Nominal Cost per Dry Well Drilled (Thousand Dollars per Well)")])
df$average_arithmeticreturn <- rowMeans(df[, c("Arithmetic Return - Crude Oil", 
                                               "Arithmetic Return - Natural Gas",
                                               "Arithmetic Return - Dry Well")])


# Extract data between 1990 and 2006
subset_df <- subset(df, Date >= as.POSIXct("1990-06-30") & Date <= as.POSIXct("2006-06-30"))

# Check for Normality (Only on arithmetic changes as given on the RFP)
array_changes_48 <- c(subset_df$`Arithmetic Return - Crude Oil`, subset_df$`Arithmetic Return - Natural Gas`, subset_df$`Arithmetic Return - Dry Well`)
## Q-Q plot
qqPlot(array_changes_48, main = 'Normality Check on Arithmetic Return', xlab = "Theoretical Quantiles", ylab = "Sample Quantiles") # plot the data against the theoretical normal distribution
## Shapiro-Wilk test
shapiro.test(array_changes_48)
## The p-value > 0.05. In other words, we can assume the normality.

# Kernel Estimation
change_density <- density(array_changes_48) # Getting the bandwidth (h) value
set.seed(112358)
subset_df.avg_ar <- rkde(fhat=kde(array_changes_48, h=change_density$bw), n=100000)
# Plot the output
ggplot(data=as.data.frame(subset_df.avg_ar), aes(x=subset_df.avg_ar)) +
  geom_histogram(bins=round(sqrt(100000), 0), colour="black", fill="grey") +
  geom_vline(aes(xintercept=mean(subset_df.avg_ar)), color="red", linetype="dashed", size=1) +
  scale_x_continuous(labels = scales::percent)+
  labs(title="Kernel Density Estimate of Annual Changes", x="Average Arithmetic Returns", y="Frequency") +
  geom_text(label="Initial Inv.", x=mean(subset_df.avg_ar), y=max(density(subset_df.avg_ar)$y)/2, hjust=0,vjust=-41) +
  theme_classic()

# obtain the summary
summary(subset_df.avg_ar)
mean(subset_df.avg_ar) # 0.1315691
sd(subset_df.avg_ar) # 0.1886111


# 2006 to 2023
n = 100000 #number of simulations to run parameter
mu_ar = mean(array_changes_48) # 0.1314913
sd_ar = sd(array_changes_48) # 0.1784372

P_06 <- last(subset_df$average_nominalcost) # Initial Price in 2006

kde_23  <- rep() # KDE for 2023
n_23    <- rep() # Normal Distribution for 2023

set.seed(112358)
for(i in 1:n) {
  P_07_12_kde <- 1 + rkde(fhat=kde(array_changes_48, h=change_density$bw), n=6) # 6 values (2007-2012) from KDE and h from Kernel Estimation done before
  P_07_12_n   <- 1 + rnorm(n=6, mean=mu_ar, sd=sd_ar) # 6 values (2007-2012) from the Normal Distribution
  
  P_12_15 <- 1 + rtriangle(n=3, a=-0.22, b=-0.07, c=-0.0917) # Triangular distribution for 2012 to 2015 (-ve because decrease in trend - given in the question)
  
  P_15_23 <- 1 + rtriangle(n=8, a=0.02, b=0.06, c=0.05) # Triangular distribution for 2015 to 2023
  
  P_23_kde = P_06 * prod(P_07_12_kde) * prod(P_12_15) * prod(P_15_23) 
  kde_23 <- append(kde_23, P_23_kde) # KDE output for 2023
  
  P_23_n   = P_06 * prod(P_07_12_n)   * prod(P_12_15) * prod(P_15_23)
  n_23 <- append(n_23, P_23_n) # Normal Distribution output for 2023
}

# scale data 
kde_23 <- kde_23 * 1000
n_23 <- n_23 * 1000

# 5 number summary
summary(kde_23)
summary(n_23)


# KDE Histogram
ggplot(as.data.frame(kde_23), aes(x=kde_23)) + geom_histogram(bins=round(sqrt(n), 0), colour="black", fill="grey")+
  geom_vline(aes(xintercept=(P_06*1000)), color="red",
             linetype="dashed", size = 1)+ 
  scale_x_continuous(labels = scales::dollar)+
  labs(title="2023 Drilling Cost Under Kernel Density Estimate for 2006 – 2012 and Two different Triangular distribution for 2012-2023",x="Driling Cost", y = "Frequency")+
  geom_text(label="Initial Inv.", x=(P_06*1000), y=max(density(kde_23)$y)/2, hjust=0,vjust=-41) +
  theme_classic()

# Normal Distribution Histogram
ggplot(as.data.frame(n_23), aes(x=n_23)) + geom_histogram(bins=round(sqrt(n), 0), colour="black", fill="grey")+
  geom_vline(aes(xintercept=(P_06*1000)), color="red",
             linetype="dashed", size = 1)+
  scale_x_continuous(labels = scales::dollar)+
  labs(title="2023 Drilling Cost Under Normality Assumption for 2006 – 2012 and Two different Triangular distribution for 2012-2023",x="Driling Cost", y = "Frequency")+
  geom_text(label="Initial Inv.", x=(P_06*1000), y=max(density(n_23)$y)/2, hjust=0,vjust=-41) +
  theme_classic()
