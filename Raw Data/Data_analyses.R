# Marta Topor December 2023 
# Linköping University 

library(tidyverse)

participants <- read.csv("participants.csv")

#add columns to be extracted from the Demographics file

participants$sex <- NA
participants$age <- NA
participants$multilingual <- NA
participants$handedness <- NA
participants$dev_cond <- NA



for (i in 1:length(participants$ID)){
  sub <- participants$ID[i]
  sub_path <- paste("~/1.Translation_Project/DCDQ_Danish_Translation/Raw Data/", sub, sep="")
  setwd(sub_path)
  
  demo_file <- paste(sub, "Demo", sep="_")
  sub_demo <- read.csv(paste(demo_file, "csv", sep="."))
  
  participants$sex[i] <- sub_demo$Sex
  participants$age[i] <- sub_demo$Age
  participants$multilingual[i] <- sub_demo$Multilingual
  participants$handedness[i] <- sub_demo$Handedness
  participants$dev_cond[i] <- sub_demo$Dev_cond
}

#The gender marker for female was "F" and was read in as FALSE so this needs to be corrected
participants$sex[participants$sex == "FALSE"] <- "F"


sum(participants$sex == "F")
sum(participants$sex == "M")

mean(participants$age)
sd(participants$age)

sum(participants$multilingual)
sum(participants$handedness == "R") + sum(participants$handedness == "r")
