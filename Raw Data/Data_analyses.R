# Marta Topor December 2023 
# Linköping University 

library(tidyverse)
library(correlation)
library(psych)

#Read in the data about participants
participants <- read.csv("participants.csv")

#Add data from Demographics files
#add columns to be extracted from the Demographics file

participants$sex <- NA
participants$age <- NA
participants$multilingual <- NA
participants$handedness <- NA
participants$dev_cond <- NA


#Extract demographic information about the participants

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


# Extract DCDQ Total scores from the DCDQ files

participants$DCDQ <- NA


for (i in 1:length(participants$ID)){
  sub <- participants$ID[i]
  sub_path <- paste("~/1.Translation_Project/DCDQ_Danish_Translation/Raw Data/", sub, sep="")
  setwd(sub_path)
  
  DCDQ_file <- paste(sub, "DCDQ", sep="_")
  sub_DCDQ <- read.csv(paste(DCDQ_file, "csv", sep="."))
  
  participants$DCDQ[i] <- rowSums(sub_DCDQ[1,3:17])

}

#Extract the motor task scores from the Flamingo and Peg Board files

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

#The time measure in the Flamingo test is best when it's long. But the best possible result is recorded as 0 seconds in the file and this was in the case when the child did not put down their leg at all. 
#The 0s should be changed to 60s to numerically reflect best performance.

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
# Still, for the overall motor index, larger numbers will reflect better performance

participants$motor <- rowSums(participants[,23:24]) - rowSums(participants[,c(21:22, 25:26)]) 

#------------For the Report

#Check the DCDQ data 
mean(participants$DCDQ)
sd(participants$DCDQ)
min(participants$DCDQ)
head(sort(participants$DCDQ))
max(participants$DCDQ)

#Proportion of children with DCDQ scores in the "probable DCD category"
length(participants$ID[participants$DCDQ < 47])/length(participants$ID) * 100


#DCDQ Reliability
DCDQ <- as.data.frame(matrix(nrow = 36, ncol = 16))
colnames(DCDQ) <- c('ID', 'Item1', 'Item2', 'Item3', 'Item4', 'Item5', 'Item6', 'Item7', 'Item8', 'Item9', 'Item10', 'Item11', 'Item12', 'Item13', 'Item14', 'Item15')
#Extract data for all items for each participant

for (i in 1:length(participants$ID)){
  sub <- participants$ID[i]
  sub_path <- paste("~/1.Translation_Project/DCDQ_Danish_Translation/Raw Data/", sub, sep="")
  setwd(sub_path)
  
  DCDQ_file <- paste(sub, "DCDQ", sep="_")
  sub_DCDQ <- read.csv(paste(DCDQ_file, "csv", sep="."))
  
  DCDQ$ID[i] <- sub
  
  DCDQ$Item1[i] <- sub_DCDQ$Q1
  DCDQ$Item2[i] <- sub_DCDQ$Q2
  DCDQ$Item3[i] <- sub_DCDQ$Q3
  DCDQ$Item4[i] <- sub_DCDQ$Q4
  DCDQ$Item5[i] <- sub_DCDQ$Q5
  DCDQ$Item6[i] <- sub_DCDQ$Q6
  DCDQ$Item7[i] <- sub_DCDQ$Q7
  DCDQ$Item8[i] <- sub_DCDQ$Q8
  DCDQ$Item9[i] <- sub_DCDQ$Q9
  DCDQ$Item10[i] <- sub_DCDQ$Q10
  DCDQ$Item11[i] <- sub_DCDQ$Q11
  DCDQ$Item12[i] <- sub_DCDQ$Q12
  DCDQ$Item13[i] <- sub_DCDQ$Q13
  DCDQ$Item14[i] <- sub_DCDQ$Q14
  DCDQ$Item15[i] <- sub_DCDQ$Q15
  
}

consistency <- alpha(cov(DCDQ[,2:16]))
#Extract corrected item-total correlations for a table in the report
item_total <- consistency$item.stats[,"r.cor"]
#Extract correlations if item dropped
item_dropped <- consistency$alpha.drop[,"raw_alpha"]

#DCDQ with motor test - validity

#Plot and test DCDQ scores with motor perfomance

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



