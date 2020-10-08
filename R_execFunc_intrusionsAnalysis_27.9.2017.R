setwd("C:\\Users\\currys\\Dropbox\\1. GAVLE\\1. ONGOING ARTICLES\\5. Executive Function paper\\dataEval")
# setwd("C:\\Users\\dosman\\Dropbox\\1. GAVLE\\1. ONGOING ARTICLES\\5. Executive Function paper\\dataEval")
# setwd("D:\\GoogleDrive\\iCARE\\Experiment_Anders\\Matlab\\results")

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

# Prepare dataframe
data_numberUpdate_intrusions <- read.csv(file="results_part2_wide.csv", header=T,sep=";")
    # remove unnecessary 
data_numberUpdate_intrusions[,c(1,2,3,4,5,8,9,12,13,16,17,20,21,24,25,28,29,32,33,36,37,
                                40,41,44,45,48,49,52)] <- list(NULL)

data_numberUpdate_intrusions[] <- data.frame(lapply(data_numberUpdate_intrusions[], 
                                                         as.character), stringsAsFactors=F)
sapply(data_numberUpdate_intrusions, class)


# Target dataframe
data_targets <- t(data.frame(strsplit(data_numberUpdate_intrusions[1,1], ","),
                      strsplit(data_numberUpdate_intrusions[1,3], ","),
strsplit(data_numberUpdate_intrusions[1,5], ","),
strsplit(data_numberUpdate_intrusions[1,7], ","),
strsplit(data_numberUpdate_intrusions[1,9], ","),
strsplit(data_numberUpdate_intrusions[1,12], ","),
strsplit(data_numberUpdate_intrusions[1,13], ","),
strsplit(data_numberUpdate_intrusions[1,15], ","),
strsplit(data_numberUpdate_intrusions[1,17], ","),
strsplit(data_numberUpdate_intrusions[1,19], ","),
strsplit(data_numberUpdate_intrusions[1,22], ","),
strsplit(data_numberUpdate_intrusions[1,23], ",")))
data_targets <- data.frame(data_targets,row.names=c("target1","target2",
                       "target3","target4",
                       "target5","target6",
                       "target7","target8",
                       "target9","target10",
                       "target11","target12"))
colnames(data_targets) <- c("T_A","T_B","T_C")
sapply(data_targets, class)
levels(data_targets[1,1])


# Answer datafile
data_answers <- t(data.frame(strsplit(data_numberUpdate_intrusions[,2], ","),
strsplit(data_numberUpdate_intrusions[,4], ","),
strsplit(data_numberUpdate_intrusions[,6], ","),
strsplit(data_numberUpdate_intrusions[,8], ","),
strsplit(data_numberUpdate_intrusions[,10], ","),
strsplit(data_numberUpdate_intrusions[,12], ","),
strsplit(data_numberUpdate_intrusions[,14], ","),
strsplit(data_numberUpdate_intrusions[,16], ","),
strsplit(data_numberUpdate_intrusions[,18], ","),
strsplit(data_numberUpdate_intrusions[,20], ","),
strsplit(data_numberUpdate_intrusions[,22], ","),
strsplit(data_numberUpdate_intrusions[,24], ",")))
class(data_answers)
sapply(data_answers, class)
data_answers <- data.frame(data_answers)
class(data_answers)
sapply(data_answers, class)
data_answers$answer_number <- c(1:12)
colnames(data_answers) <- c("resp1","resp2","resp3","answer_number")
row.names(data_answers) <- c(1:end(data_answers[,1]))
# Create participant number 
participant_number <- matrix(NA,948,1)
for (i in 1:79) {participant_number[c(i*12-11):(12*i),] <-  rep(c(i), times = 12)}
data_answers <- data.frame(data_answers,participant_number)

#Convert variables from factors to numeric
#install.packages("varhandle")
library(varhandle)

data_targets$T_A <- unfactor(data_targets$T_A)  
data_targets$T_B <- unfactor(data_targets$T_B)  
data_targets$T_C <- unfactor(data_targets$T_C)

data_answers$resp1 <- unfactor(data_answers$resp1)  
data_answers$resp2 <- unfactor(data_answers$resp2)  
data_answers$resp3 <- unfactor(data_answers$resp3)

sapply(data_answers, class)

# ANALYSIS

rm(x)
x <- matrix(NA,948,3)

for(i in 1:79) {
if(compareIgnoreOrder(data_targets[1,1],data_answers[i,1]) == TRUE) x[i,1] = 1 
if(compareIgnoreOrder(data_targets[1,1],data_answers[i,2]) == TRUE) x[i,1] = 1
if(compareIgnoreOrder(data_targets[1,1],data_answers[i,3]) == TRUE) x[i,1] = 1
if(compareIgnoreOrder(data_targets[1,2],data_answers[i,1]) == TRUE) x[i,2] = 1
if(compareIgnoreOrder(data_targets[1,2],data_answers[i,2])== TRUE) x[i,2] = 1
if(compareIgnoreOrder(data_targets[1,2],data_answers[i,3])== TRUE) x[i,2] = 1
if(compareIgnoreOrder(data_targets[1,3],data_answers[i,1])== TRUE) x[i,3] = 1
if(compareIgnoreOrder(data_targets[1,3],data_answers[i,2])== TRUE) x[i,3] = 1
if(compareIgnoreOrder(data_targets[1,3],data_answers[i,3])== TRUE) x[i,3] = 1
}

for(i in 80:158) {
  if(compareIgnoreOrder(data_targets[2,1],data_answers[i,1]) == TRUE) x[i,1] = 1 
  if(compareIgnoreOrder(data_targets[2,1],data_answers[i,2]) == TRUE) x[i,1] = 1
  if(compareIgnoreOrder(data_targets[2,1],data_answers[i,3]) == TRUE) x[i,1] = 1
  if(compareIgnoreOrder(data_targets[2,2],data_answers[i,1]) == TRUE) x[i,2] = 1
  if(compareIgnoreOrder(data_targets[2,2],data_answers[i,2])== TRUE) x[i,2] = 1
  if(compareIgnoreOrder(data_targets[2,2],data_answers[i,3])== TRUE) x[i,2] = 1
  if(compareIgnoreOrder(data_targets[2,3],data_answers[i,1])== TRUE) x[i,3] = 1
  if(compareIgnoreOrder(data_targets[2,3],data_answers[i,2])== TRUE) x[i,3] = 1
  if(compareIgnoreOrder(data_targets[2,3],data_answers[i,3])== TRUE) x[i,3] = 1
}

for(i in 159:237) {
  if(compareIgnoreOrder(data_targets[3,1],data_answers[i,1]) == TRUE) x[i,1] = 1 
  if(compareIgnoreOrder(data_targets[3,1],data_answers[i,2]) == TRUE) x[i,1] = 1
  if(compareIgnoreOrder(data_targets[3,1],data_answers[i,3]) == TRUE) x[i,1] = 1
  if(compareIgnoreOrder(data_targets[3,2],data_answers[i,1]) == TRUE) x[i,2] = 1
  if(compareIgnoreOrder(data_targets[3,2],data_answers[i,2])== TRUE) x[i,2] = 1
  if(compareIgnoreOrder(data_targets[3,2],data_answers[i,3])== TRUE) x[i,2] = 1
  if(compareIgnoreOrder(data_targets[3,3],data_answers[i,1])== TRUE) x[i,3] = 1
  if(compareIgnoreOrder(data_targets[3,3],data_answers[i,2])== TRUE) x[i,3] = 1
  if(compareIgnoreOrder(data_targets[3,3],data_answers[i,3])== TRUE) x[i,3] = 1
}

for(i in 238:316) {
  if(compareIgnoreOrder(data_targets[4,1],data_answers[i,1]) == TRUE) x[i,1] = 1 
  if(compareIgnoreOrder(data_targets[4,1],data_answers[i,2]) == TRUE) x[i,1] = 1
  if(compareIgnoreOrder(data_targets[4,1],data_answers[i,3]) == TRUE) x[i,1] = 1
  if(compareIgnoreOrder(data_targets[4,2],data_answers[i,1]) == TRUE) x[i,2] = 1
  if(compareIgnoreOrder(data_targets[4,2],data_answers[i,2])== TRUE) x[i,2] = 1
  if(compareIgnoreOrder(data_targets[4,2],data_answers[i,3])== TRUE) x[i,2] = 1
  if(compareIgnoreOrder(data_targets[4,3],data_answers[i,1])== TRUE) x[i,3] = 1
  if(compareIgnoreOrder(data_targets[4,3],data_answers[i,2])== TRUE) x[i,3] = 1
  if(compareIgnoreOrder(data_targets[4,3],data_answers[i,3])== TRUE) x[i,3] = 1
}

for(i in 317:395) {
  if(compareIgnoreOrder(data_targets[5,1],data_answers[i,1]) == TRUE) x[i,1] = 1 
  if(compareIgnoreOrder(data_targets[5,1],data_answers[i,2]) == TRUE) x[i,1] = 1
  if(compareIgnoreOrder(data_targets[5,1],data_answers[i,3]) == TRUE) x[i,1] = 1
  if(compareIgnoreOrder(data_targets[5,2],data_answers[i,1]) == TRUE) x[i,2] = 1
  if(compareIgnoreOrder(data_targets[5,2],data_answers[i,2])== TRUE) x[i,2] = 1
  if(compareIgnoreOrder(data_targets[5,2],data_answers[i,3])== TRUE) x[i,2] = 1
  if(compareIgnoreOrder(data_targets[5,3],data_answers[i,1])== TRUE) x[i,3] = 1
  if(compareIgnoreOrder(data_targets[5,3],data_answers[i,2])== TRUE) x[i,3] = 1
  if(compareIgnoreOrder(data_targets[5,3],data_answers[i,3])== TRUE) x[i,3] = 1
}

for(i in 369:474) {
  if(compareIgnoreOrder(data_targets[6,1],data_answers[i,1]) == TRUE) x[i,1] = 1 
  if(compareIgnoreOrder(data_targets[6,1],data_answers[i,2]) == TRUE) x[i,1] = 1
  if(compareIgnoreOrder(data_targets[6,1],data_answers[i,3]) == TRUE) x[i,1] = 1
  if(compareIgnoreOrder(data_targets[6,2],data_answers[i,1]) == TRUE) x[i,2] = 1
  if(compareIgnoreOrder(data_targets[6,2],data_answers[i,2])== TRUE) x[i,2] = 1
  if(compareIgnoreOrder(data_targets[6,2],data_answers[i,3])== TRUE) x[i,2] = 1
  if(compareIgnoreOrder(data_targets[6,3],data_answers[i,1])== TRUE) x[i,3] = 1
  if(compareIgnoreOrder(data_targets[6,3],data_answers[i,2])== TRUE) x[i,3] = 1
  if(compareIgnoreOrder(data_targets[6,3],data_answers[i,3])== TRUE) x[i,3] = 1
}

for(i in 475:553) {
  if(compareIgnoreOrder(data_targets[7,1],data_answers[i,1]) == TRUE) x[i,1] = 1 
  if(compareIgnoreOrder(data_targets[7,1],data_answers[i,2]) == TRUE) x[i,1] = 1
  if(compareIgnoreOrder(data_targets[7,1],data_answers[i,3]) == TRUE) x[i,1] = 1
  if(compareIgnoreOrder(data_targets[7,2],data_answers[i,1]) == TRUE) x[i,2] = 1
  if(compareIgnoreOrder(data_targets[7,2],data_answers[i,2])== TRUE) x[i,2] = 1
  if(compareIgnoreOrder(data_targets[7,2],data_answers[i,3])== TRUE) x[i,2] = 1
  if(compareIgnoreOrder(data_targets[7,3],data_answers[i,1])== TRUE) x[i,3] = 1
  if(compareIgnoreOrder(data_targets[7,3],data_answers[i,2])== TRUE) x[i,3] = 1
  if(compareIgnoreOrder(data_targets[7,3],data_answers[i,3])== TRUE) x[i,3] = 1
}

for(i in 554:632) {
  if(compareIgnoreOrder(data_targets[8,1],data_answers[i,1]) == TRUE) x[i,1] = 1 
  if(compareIgnoreOrder(data_targets[8,1],data_answers[i,2]) == TRUE) x[i,1] = 1
  if(compareIgnoreOrder(data_targets[8,1],data_answers[i,3]) == TRUE) x[i,1] = 1
  if(compareIgnoreOrder(data_targets[8,2],data_answers[i,1]) == TRUE) x[i,2] = 1
  if(compareIgnoreOrder(data_targets[8,2],data_answers[i,2])== TRUE) x[i,2] = 1
  if(compareIgnoreOrder(data_targets[8,2],data_answers[i,3])== TRUE) x[i,2] = 1
  if(compareIgnoreOrder(data_targets[8,3],data_answers[i,1])== TRUE) x[i,3] = 1
  if(compareIgnoreOrder(data_targets[8,3],data_answers[i,2])== TRUE) x[i,3] = 1
  if(compareIgnoreOrder(data_targets[8,3],data_answers[i,3])== TRUE) x[i,3] = 1
}

for(i in 633:711) {
  if(compareIgnoreOrder(data_targets[9,1],data_answers[i,1]) == TRUE) x[i,1] = 1 
  if(compareIgnoreOrder(data_targets[9,1],data_answers[i,2]) == TRUE) x[i,1] = 1
  if(compareIgnoreOrder(data_targets[9,1],data_answers[i,3]) == TRUE) x[i,1] = 1
  if(compareIgnoreOrder(data_targets[9,2],data_answers[i,1]) == TRUE) x[i,2] = 1
  if(compareIgnoreOrder(data_targets[9,2],data_answers[i,2])== TRUE) x[i,2] = 1
  if(compareIgnoreOrder(data_targets[9,2],data_answers[i,3])== TRUE) x[i,2] = 1
  if(compareIgnoreOrder(data_targets[9,3],data_answers[i,1])== TRUE) x[i,3] = 1
  if(compareIgnoreOrder(data_targets[9,3],data_answers[i,2])== TRUE) x[i,3] = 1
  if(compareIgnoreOrder(data_targets[9,3],data_answers[i,3])== TRUE) x[i,3] = 1
}

for(i in 712:790) {
  if(compareIgnoreOrder(data_targets[10,1],data_answers[i,1]) == TRUE) x[i,1] = 1 
  if(compareIgnoreOrder(data_targets[10,1],data_answers[i,2]) == TRUE) x[i,1] = 1
  if(compareIgnoreOrder(data_targets[10,1],data_answers[i,3]) == TRUE) x[i,1] = 1
  if(compareIgnoreOrder(data_targets[10,2],data_answers[i,1]) == TRUE) x[i,2] = 1
  if(compareIgnoreOrder(data_targets[10,2],data_answers[i,2])== TRUE) x[i,2] = 1
  if(compareIgnoreOrder(data_targets[10,2],data_answers[i,3])== TRUE) x[i,2] = 1
  if(compareIgnoreOrder(data_targets[10,3],data_answers[i,1])== TRUE) x[i,3] = 1
  if(compareIgnoreOrder(data_targets[10,3],data_answers[i,2])== TRUE) x[i,3] = 1
  if(compareIgnoreOrder(data_targets[10,3],data_answers[i,3])== TRUE) x[i,3] = 1
}

for(i in 791:869) {
  if(compareIgnoreOrder(data_targets[11,1],data_answers[i,1]) == TRUE) x[i,1] = 1 
  if(compareIgnoreOrder(data_targets[11,1],data_answers[i,2]) == TRUE) x[i,1] = 1
  if(compareIgnoreOrder(data_targets[11,1],data_answers[i,3]) == TRUE) x[i,1] = 1
  if(compareIgnoreOrder(data_targets[11,2],data_answers[i,1]) == TRUE) x[i,2] = 1
  if(compareIgnoreOrder(data_targets[11,2],data_answers[i,2])== TRUE) x[i,2] = 1
  if(compareIgnoreOrder(data_targets[11,2],data_answers[i,3])== TRUE) x[i,2] = 1
  if(compareIgnoreOrder(data_targets[11,3],data_answers[i,1])== TRUE) x[i,3] = 1
  if(compareIgnoreOrder(data_targets[11,3],data_answers[i,2])== TRUE) x[i,3] = 1
  if(compareIgnoreOrder(data_targets[11,3],data_answers[i,3])== TRUE) x[i,3] = 1
}

for(i in 870:948) {
  if(compareIgnoreOrder(data_targets[12,1],data_answers[i,1]) == TRUE) x[i,1] = 1 
  if(compareIgnoreOrder(data_targets[12,1],data_answers[i,2]) == TRUE) x[i,1] = 1
  if(compareIgnoreOrder(data_targets[12,1],data_answers[i,3]) == TRUE) x[i,1] = 1
  if(compareIgnoreOrder(data_targets[12,2],data_answers[i,1]) == TRUE) x[i,2] = 1
  if(compareIgnoreOrder(data_targets[12,2],data_answers[i,2])== TRUE) x[i,2] = 1
  if(compareIgnoreOrder(data_targets[12,2],data_answers[i,3])== TRUE) x[i,2] = 1
  if(compareIgnoreOrder(data_targets[12,3],data_answers[i,1])== TRUE) x[i,3] = 1
  if(compareIgnoreOrder(data_targets[12,3],data_answers[i,2])== TRUE) x[i,3] = 1
  if(compareIgnoreOrder(data_targets[12,3],data_answers[i,3])== TRUE) x[i,3] = 1
}

# Rename x
data_correct <- x 

# add answer number to dataframe
data_correct <- data.frame(x,data_answers$answer_number,data_answers$participant_number)
colnames(data_correct) <- c("resp1","resp2","resp3","question__number","participant_number") 
# Changes NAs to zeros, intrusions == 0
data_correct[is.na(data_correct)] <- 0
data_correct_boolean <- data_correct==0
Y <- data_answers[,1][data_correct_boolean=="TRUE"]

# How many wrong per participant

datafile_intrusions <- matrix(NA,79,8)
colnames(datafile_intrusions) <- c("ID","intrusions_col1","intrusions_col2","intrusions_col3",
                                   "intrusions_total","intrusions_late","intrusions_immediate","intrusions_list")
datafile_intrusions[,1] <- c(1:79)
for (i in 1:79) {datafile_intrusions[i,2] <- sum((data_correct$resp1 == 0) & (data_correct$participant_number == i))}
for (i in 1:79) {datafile_intrusions[i,3] <- sum((data_correct$resp2 == 0) & (data_correct$participant_number == i))}
for (i in 1:79) {datafile_intrusions[i,4] <- sum((data_correct$resp3 == 0) & (data_correct$participant_number == i))}
for (i in 1:79) {datafile_intrusions[i,5] <- sum(datafile_intrusions[i,2:4])}

# Match with list
# Extract intrusions and store in lists, one list for each participant

intrusions_list = list()
for (i in 1:79) {intrusions_list[[i]] <- data_answers[,1][data_correct$resp1==0 & data_correct$participant_number == i]}

# Creates datafile with intrusions only
intrusions_answers <- data_answers
intrusions_answers[,1:3][data_correct_boolean[,1:3] == FALSE] <- 0  



############ AUTOMATE THIS FOR 79 PARTICIPANTS #################################################
################################################################################################

# check participant 1's intrusions for question 2 against question 1 targets
# 1. extract participant 1's intrusions for question 2 from intrusions_answers, output saved

# Creates 2 lists of 4
for (i in 1:2) { 
  assign(paste("dougs_list",i,sep=""), 
                        vector("list",2))
}

# Put a value in to the first element of both lists

list_intrusions <- matrix(0,nrow=4,ncol=948,byrow=T)

for (i in 1:79) {
#1,12,23,34,45,etc
int_q1 <- as.integer(unlist(intrusions_answers[,1:3][intrusions_answers$participant_number==i &
                                 intrusions_answers$answer_number == 2 & intrusions_answers[,1:3] > 0]))
if (is.empty(int_q1) == T) int_q1 <- 0 # if there is no intrusion for this question, makes value zero
list_intrusions[1:length(int_q1),i*12-11] = int_q1

#2,13,24,35,46,etc
int_q2 <- unlist(intrusions_answers[,1:3][intrusions_answers$participant_number==i & 
                                            intrusions_answers$answer_number == 3 & 
                                            intrusions_answers[,1:3] > 0])
if (is.empty(int_q2) == T) int_q2 <- 0
list_intrusions[1:length(int_q2),i*12-10] = int_q2

#3,14,25,36,47,etc
int_q3 <- unlist(intrusions_answers[,1:3][intrusions_answers$participant_number==i & 
                                                 intrusions_answers$answer_number == 4 & 
                                                  intrusions_answers[,1:3] > 0])
if (is.empty(int_q3) == T) int_q3 <- 0
list_intrusions[1:length(int_q3),i*12-9] = int_q3

#4,15,26,37,48,etc
int_q4 <- unlist(intrusions_answers[,1:3][intrusions_answers$participant_number==i & 
                                                intrusions_answers$answer_number == 5 & 
                                                intrusions_answers[,1:3] > 0] )
if (is.empty(int_q4) == T) int_q4 <- 0
list_intrusions[1:length(int_q4),i*12-8] = int_q4

#5,16,27,38,49
int_q5 <- unlist(intrusions_answers[,1:3][intrusions_answers$participant_number==i & 
                                                intrusions_answers$answer_number == 6 & 
                                                intrusions_answers[,1:3] > 0])
if (is.empty(int_q5) == T) int_q5 <- 0
list_intrusions[1:length(int_q5),i*12-7] = int_q5

#6,17,28,39,50
int_q6 <- unlist(intrusions_answers[,1:3][intrusions_answers$participant_number==i & 
                                                  intrusions_answers$answer_number == 7 & 
                                                  intrusions_answers[,1:3] > 0])
if (is.empty(int_q6) == T) int_q6 <- 0
list_intrusions[1:length(int_q6),i*12-6] = int_q6

#7,18,29,40,51
int_q7 <- unlist(intrusions_answers[,1:3][intrusions_answers$participant_number==i & 
                                                  intrusions_answers$answer_number == 8 & 
                                                  intrusions_answers[,1:3] > 0])
if (is.empty(int_q7) == T) int_q7 <- 0
list_intrusions[1:length(int_q7),i*12-5] = int_q7

#8,19,30,41,52
int_q8 <- unlist(intrusions_answers[,1:3][intrusions_answers$participant_number==i & 
                                                  intrusions_answers$answer_number == 9 & 
                                                  intrusions_answers[,1:3] > 0])
if (is.empty(int_q8) == T) int_q8 <- 0
list_intrusions[1:length(int_q8),i*12-4] = int_q8

#9,20,31,42,53
int_q9 <- unlist(intrusions_answers[,1:3][intrusions_answers$participant_number==i & 
                                                  intrusions_answers$answer_number == 10 & 
                                                  intrusions_answers[,1:3] > 0])
if (is.empty(int_q9) == T) int_q9 <- 0
list_intrusions[1:length(int_q9),i*12-3] = int_q9

#10,21,32,43,54
int_q10 <- unlist(intrusions_answers[,1:3][intrusions_answers$participant_number==i & 
                                                  intrusions_answers$answer_number == 11 & 
                                                  intrusions_answers[,1:3] > 0])
if (is.empty(int_q10) == T) int_q10 <- 0 
list_intrusions[1:length(int_q10),i*12-2] = int_q10
 
#11,22,33,44,55
int_q11 <- unlist(intrusions_answers[,1:3][intrusions_answers$participant_number==i & 
                                                  intrusions_answers$answer_number == 12 & 
                                                  intrusions_answers[,1:3] > 0])
if (is.empty(int_q11) == T) int_q11 <- 0
list_intrusions[1:length(int_q11),i*12-1] = int_q11

#12,23,34,45,56
int_q12 <- unlist(intrusions_answers[,1:3][intrusions_answers$participant_number==i & 
                                             intrusions_answers$answer_number == 12 & 
                                             intrusions_answers[,1:3] > 0])
if (is.empty(int_q12) == T) int_q12 <- 0
list_intrusions[1:length(int_q12),i*12] = int_q12
}


# 2. compare intrusions output from 1. with question 1 targets. Save total of how many intrusions come from 
#    target 1 to list.

for (i in 1:79) {

list_intrusions[4,i*12-10] <- sum(list_intrusions[1,i*12-10] == data_targets[1:2,1:3],
                            list_intrusions[2,i*12-10] == data_targets[1:2,1:3],
                            list_intrusions[3,i*12-10] == data_targets[1:2,1:3])

list_intrusions[4,i*12-9] <- sum(list_intrusions[1,i*12-9] == data_targets[1:3,1:3],
                            list_intrusions[2,i*12-9] == data_targets[1:3,1:3],
                            list_intrusions[3,i*12-9] == data_targets[1:3,1:3])

list_intrusions[4,i*12-8] <- sum(list_intrusions[1,i*12-8] == data_targets[1:4,1:3],
                            list_intrusions[2,i*12-8] == data_targets[1:4,1:3],
                            list_intrusions[3,i*12-8] == data_targets[1:4,1:3])

list_intrusions[4,i*12-7] <- sum(list_intrusions[1,i*12-7] == data_targets[1:5,1:3],
                            list_intrusions[2,i*12-7] == data_targets[1:5,1:3],
                            list_intrusions[3,i*12-7] == data_targets[1:5,1:3])

list_intrusions[4,i*12-6] <- sum(list_intrusions[1,i*12-6] == data_targets[1:6,1:3],
                            list_intrusions[2,i*12-6] == data_targets[1:6,1:3],
                            list_intrusions[3,i*12-6] == data_targets[1:6,1:3])

list_intrusions[4,i*12-5] <- sum(list_intrusions[1,i*12-5] == data_targets[1:7,1:3],
                            list_intrusions[2,i*12-5] == data_targets[1:7,1:3],
                            list_intrusions[3,i*12-5] == data_targets[1:7,1:3])

list_intrusions[4,i*12-4] <- sum(list_intrusions[1,i*12-4] == data_targets[1:8,1:3],
                            list_intrusions[2,i*12-4] == data_targets[1:8,1:3],
                            list_intrusions[3,i*12-4] == data_targets[1:8,1:3])

list_intrusions[4,i*12-3] <- sum(list_intrusions[1,i*12-3] == data_targets[1:9,1:3],
                            list_intrusions[2,i*12-3] == data_targets[1:9,1:3],
                            list_intrusions[3,i*12-3] == data_targets[1:9,1:3])

list_intrusions[4,i*12-2] <- sum(list_intrusions[1,i*12-2] == data_targets[1:10,1:3],
                            list_intrusions[2,i*12-2] == data_targets[1:10,1:3],
                            list_intrusions[3,i*12-2] == data_targets[1:10,1:3])

list_intrusions[4,i*12-1] <- sum(list_intrusions[1,i*12-1] == data_targets[1:11,1:3],
                            list_intrusions[2,i*12-1] == data_targets[1:11,1:3],
                            list_intrusions[3,i*12-1] == data_targets[1:11,1:3])

list_intrusions[4,i*12] <- sum(list_intrusions[1,i*12] == data_targets[1:12,1:3],
                            list_intrusions[2,i*12] == data_targets[1:12,1:3],
                            list_intrusions[3,i*12] == data_targets[1:12,1:3])
}



# Create sum of list and export total number of previous list intrusions to datafile_intrusions column intrusions_late
prev_list_intrusions_p1_unlist <- unlist(prev_list_intrusions_p1[1:11])
datafile_intrusions[1,6] <- sum(prev_list_intrusions_p1_unlist)
datafile_intrusions[1,7] <- sum(datafile_intrusions[1,5]) - sum(prev_list_intrusions_p1_unlist) 

for (i in 1:79){

datafile_intrusions[i,6] <-  sum(list_intrusions[4,i*12-10], 
                                        list_intrusions[4,i*12-9],
                                        list_intrusions[4,i*12-8], 
                                        list_intrusions[4,i*12-7], 
                                        list_intrusions[4,i*12-6],
                                        list_intrusions[4,i*12-5], 
                                        list_intrusions[4,i*12-4], 
                                        list_intrusions[4,i*12-3], 
                                        list_intrusions[4,i*12-2], 
                                        list_intrusions[4,i*12-1], 
                                        list_intrusions[4,i*12])
}

for (i in 1:79){
  
  datafile_intrusions[i,7] <- datafile_intrusions[i,5] - datafile_intrusions[i,6]

}


MainDataExecFuncPaper_intrusions <- read.csv(file="MainDataExecFuncPaper_intrusions_exclusions_1.csv", header=T,sep=";")
MainDataExecFuncPaper_intrusions <- MainDataExecFuncPaper_intrusions[order(MainDataExecFuncPaper[,1]),]
MainDataExecFuncPaper_intrusions$intrusions_total <- datafile_intrusions[,5]
MainDataExecFuncPaper_intrusions$intrusions_delayed <- datafile_intrusions[,6]
MainDataExecFuncPaper_intrusions$intrusions_immediate <- datafile_intrusions[,7]
write.csv(MainDataExecFuncPaper_intrusions, file = "MainDataExecFuncPaper_intrusions.csv", header=T,sep=";")
