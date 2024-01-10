# Marta Topor December 2023 
# Linköping University 

library(tidyverse)
library(correlation)

participants <- read.csv("participants.csv")

#add columns to be extracted from the Demographics file

participants$sex <- NA
participants$age <- NA
participants$multilingual <- NA
participants$handedness <- NA
participants$dev_cond <- NA


#Demographic information about the participants

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


#Add DCDQ scores 

participants$DCDQ <- NA


for (i in 1:length(participants$ID)){
  sub <- participants$ID[i]
  sub_path <- paste("~/1.Translation_Project/DCDQ_Danish_Translation/Raw Data/", sub, sep="")
  setwd(sub_path)
  
  DCDQ_file <- paste(sub, "DCDQ", sep="_")
  sub_DCDQ <- read.csv(paste(DCDQ_file, "csv", sep="."))
  
  participants$DCDQ[i] <- rowSums(sub_DCDQ[1,3:17])

}


#Add the motor task scores

participants$Flam_R_Num <- NA
participants$Flam_L_Num <- NA
participants$Flam_R_Time <- NA
participants$Flam_L_Time <- NA

participants$Peg_R_Time <- NA
participants$Peg_L_Time <- NA


for (i in 1:length(participants$ID)){
  sub <- participants$ID[i]
  sub_path <- paste("~/1.Translation_Project/DCDQ_Danish_Translation/Raw Data/", sub, sep="")
  setwd(sub_path)
  
  Flam_file <- paste(sub, "Flam", sep="_")
  sub_Flam <- read.csv(paste(Flam_file, "csv", sep="."))
  
  participants$Flam_R_Num[i] <- sub_Flam$R_Num
  participants$Flam_L_Num[i] <- sub_Flam$Left_Num
  participants$Flam_R_Time[i] <- sub_Flam$R_Time
  participants$Flam_L_Time[i] <- sub_Flam$Left_Time
  
  Peg_file <- paste(sub, "Peg", sep="_")
  sub_Peg <- read.csv(paste(Peg_file, "csv", sep="."))
  
  participants$Peg_R_Time[i] <- sub_Peg$R_Time
  participants$Peg_L_Time[i] <- sub_Peg$L_Time
  
}

#The time measure in the Flamingo test is best when it's long. But the best possible result is 0 seconds, when the child did not put down their leg at all. 
#The 0s should be changed to 60s 

participants$Flam_R_Time[participants$Flam_R_Time == 0.00] <- 60.00
participants$Flam_L_Time[participants$Flam_L_Time == 0.00] <- 60.00

#Log transformation to stadardise the variables

participants$Flam_R_Num_LOG <- log(participants$Flam_R_Num + 1)
participants$Flam_L_Num_LOG <- log(participants$Flam_L_Num + 1)
participants$Flam_R_Time_LOG <- log(participants$Flam_R_Time + 1)
participants$Flam_L_Time_LOG <- log(participants$Flam_L_Time + 1)

participants$Peg_R_Time_LOG <- log(participants$Peg_R_Time + 1)
participants$Peg_L_Time_LOG <- log(participants$Peg_L_Time + 1)

#Calculate the cumulative variable
# The Flam Time variable is beneficial when it's high
# The Flam Number and Peg Time is beneficial when it's low 
# Therefore, the Flam Number and Peg Time will be subtracted from Flam Time
# Still larger numbers will be better numbers

participants$motor <- rowSums(participants[,23:24]) - rowSums(participants[,c(21:22, 25:26)]) 

#Plot and test

ggplot(participants, aes(x=motor, y=DCDQ)) + 
  geom_point(size=3) +
  geom_smooth(method=lm, size = 1.5, color = "black") +
  theme_classic() + 
  labs(x = "Motor Coordination Composite Score", y = "DCDQ Questionnaire") +
  theme(axis.title = element_text(size = 20), axis.text = element_text(size = 20))   



plot(participants$motor, participants$DCDQ)
cor.test(participants$motor, participants$DCDQ)


#SAVE
setwd("~/1.Translation_Project/DCDQ_Danish_Translation/Raw Data")
write.csv(participants, "data_output.csv", row.names = FALSE)
