setwd("C:\\Users\\dosman\\Dropbox\\1. GAVLE\\1. ONGOING ARTICLES\\5. Executive Function paper\\dataEval")

#install.packages("ggplot2")
library(ggplot2)
# install.packages("gridExtra")
library(gridExtra)
# install.packages("weights")
library(weights)
# install.packages("car")
library(car)
# install.packages("biotools")
library(biotools)
# install.packages("psych")
library(psych)
# install.packages("ez")
library(ez)
# install.packages("afex")
library(afex)
# install.packages("lattice")
library(lattice)
library(car)
# install.packages("compare")
library(compare)
# install.packages("dplyr")
library(dplyr)
#install.packages("bazar")
library(bazar)
# install.packages("plyr")
library(plyr)

MainDataExecFuncPaper <- read.csv(file="MainDataExecFuncPaper_intrusions_exclusions_1.csv", header=T,sep=";")

# Interaction plot

summ <- ddply(MainDataExecFuncPaper, .(supp, dose), summarise, len = mean(len))

ggplot(ToothGrowth, aes(as.factor(dose), len, colour=supp)) +
  geom_boxplot() +
  geom_point(data = summ, aes(group=supp), colour="blue", 
             position = position_dodge(width=0.75)) +
  geom_line(data = summ, aes(group=supp), 
            position = position_dodge(width=0.75)) +
  scale_x_discrete("Dose") +
  scale_y_continuous("Response") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 12, hjust = 0.54, vjust = 0),
        axis.title.y = element_text(size = 12, angle = 90,  vjust = 0.25))














