library(dplyr)
library(purrr)
library(stringr)
library(tidyverse)
library(corrr)

#Reading file 1 - N3120.csv - E1 
EC1 <- read.csv("C:\\Users\\Nehal\\OneDrive\\Documents\\MSBA_StudyMaterial\\Fall2022\\MarketingIntelligence\\Project\\EC1.csv", header=TRUE, stringsAsFactors=TRUE)
head(EC1)
colnames(EC1)[1] = "vertical"

# Code to restore zip codes as char and append 0s to maintain uniformity if not present
EC1$zip <- as.character(EC1$zip)
for(i in 1:length(EC1$zip)){
  if(as.numeric(EC1$zip[i]) < 10000){
    EC1$zip[i] <- paste0("0", EC1$zip[i])
  }
}

EC1 <- as.data.frame(unclass(EC1), stringsAsFactors = TRUE)
str(EC1)
head(EC1)

#Reading file 2 - N9318.csv - E2
EC2 <- read.csv("C:\\Users\\Nehal\\OneDrive\\Documents\\MSBA_StudyMaterial\\Fall2022\\MarketingIntelligence\\Project\\EC2.csv", header=TRUE, stringsAsFactors=TRUE)
head(EC2)
colnames(EC2)[1] = "custid"

# Code to restore zip codes as char and append 0s to maintain uniformity if not present
EC2$zip <- as.character(EC2$zip)
for(i in 1:length(EC2$zip)){
  if(as.numeric(EC2$zip[i]) < 10000){
    EC2$zip[i] <- paste0("0", EC2$zip[i])
  }
}

EC2 <- as.data.frame(unclass(EC2), stringsAsFactors = TRUE)
str(EC2)
head(EC2)

#########################################################################
##Code to merge with US census data to add relevant zip code details - Micaiah's code

# Adding NumberHH and MedianIncome
## First read data
us_zip <- read.csv("C:\\Users\\Nehal\\OneDrive\\Documents\\MSBA_StudyMaterial\\Fall2022\\MarketingIntelligence\\Project\\zipData.csv")
## Code to remove first six characters before zipcode
us_zip$NAME[ us_zip$NAME != 'Geographic Area Name'] <- (gsub("^.{0,6}", "", us_zip$NAME[ us_zip$NAME != 'Geographic Area Name']))
## Remove first row
us_zip = us_zip[-1,]
## Create groups by first four numbers of zip
us_zip$group <- as.factor(str_sub(us_zip$NAME,1,4))
## Convert to numeric and make rest factors
us_zip <- as.data.frame(unclass(us_zip), stringsAsFactors = TRUE)
us_zip <- transform(us_zip, 
                    S1901_C01_001E = as.numeric(S1901_C01_001E), 
                    S1901_C01_012E = as.numeric(S1901_C01_012E))
## Create new data frame for imputed values
us_zip_group <- aggregate(cbind(S1901_C01_001E, S1901_C01_012E) ~ group, data = us_zip, FUN = median)
## Check updates
nrow(us_zip)
head(us_zip$NAME)

## Now merge data by zip code and save as new EC1
EC1 <- left_join(EC1, us_zip[ , c('NAME', 'S1901_C01_001E', 'S1901_C01_012E')], by=c("zip" = "NAME"))
## Change names
colnames(EC1)[27] <- 'NumberHH'
colnames(EC1)[28] <- 'MedianIncome'
## Add group to EC1
EC1$group <- str_sub(EC1$zip,1,4)
## Join with imputed data frame
EC1 <- left_join(EC1, us_zip_group, by='group')
## Impute nulls
EC1$NumberHH <- ifelse(is.na(EC1$NumberHH), EC1$S1901_C01_001E, EC1$NumberHH)
EC1$MedianIncome <- ifelse(is.na(EC1$MedianIncome), EC1$S1901_C01_012E, EC1$MedianIncome)
## Remove group and last two columns
EC1$group <- NULL
EC1$S1901_C01_001E <- NULL
EC1$S1901_C01_012E <- NULL
## Review structure
str(EC1)
## Check nulls in each column 
colSums(is.na(EC1))
## Zip codes with missing values
EC1[rowSums(is.na(EC1)) > 0, 'zip']
## Remove nulls
EC1 <- na.omit(EC1)
nrow(EC1)

## Now merge data by zip code and save as new EC2
EC2 <- left_join(EC2, us_zip[ , c('NAME', 'S1901_C01_001E', 'S1901_C01_012E')], by=c("zip" = "NAME"))
## Change names
colnames(EC2)[24] <- 'NumberHH'
colnames(EC2)[25] <- 'MedianIncome'
## Add group to EC2
EC2$group <- str_sub(EC2$zip,1,4)
## Join with imputed data frame
EC2 <- left_join(EC2, us_zip_group, by='group')
## Impute nulls
EC2$NumberHH <- ifelse(is.na(EC2$NumberHH), EC2$S1901_C01_001E, EC2$NumberHH)
EC2$MedianIncome <- ifelse(is.na(EC2$MedianIncome), EC2$S1901_C01_012E, EC2$MedianIncome)
## Remove group and last two columns
EC2$group <- NULL
EC2$S1901_C01_001E <- NULL
EC2$S1901_C01_012E <- NULL
## Review structure
str(EC2)
## Check nulls in each column 
colSums(is.na(EC2))
## Zip codes with missing values
EC2[rowSums(is.na(EC2)) > 0, 'zip']
## Remove nulls
EC2 <- na.omit(EC2)
nrow(EC2)


head(EC1)
head(EC2)
#########################################################################
#Data pre- processing
#To convert verticals from EC1 and EC2 into Dummy variables

EC1$isFinance <- ifelse(EC1$vertical == "finance", 1, 0)
EC1$isFitness <- ifelse(EC1$vertical == "fitness", 1, 0)
EC1$isHealthC <- ifelse(EC1$vertical == "healthca", 1, 0)
EC1$isHomeImp <- ifelse(EC1$vertical == "homeimp", 1, 0)
EC1$isLegal <- ifelse(EC1$vertical == "legal", 1, 0)
EC1$isOnline <- ifelse(EC1$vertical == "online", 1, 0)
EC1$isRealEst <- ifelse(EC1$vertical == "realesta", 1, 0)
EC1$isSecurity <- ifelse(EC1$vertical == "security", 1, 0)
EC1$isTherapy <- ifelse(EC1$vertical == "therapy", 1, 0)

head(EC1)

EC2$isFinance <- ifelse(EC2$vertical == "finance", 1, 0)
EC2$isFitness <- ifelse(EC2$vertical == "fitness", 1, 0)
EC2$isHealthC <- ifelse(EC2$vertical == "healthca", 1, 0)
EC2$isHomeImp <- ifelse(EC2$vertical == "homeimp", 1, 0)
EC2$isLegal <- ifelse(EC2$vertical == "legal", 1, 0)
EC2$isOnline <- ifelse(EC2$vertical == "online", 1, 0)
EC2$isRealEst <- ifelse(EC2$vertical == "realesta", 1, 0)
EC2$isSecurity <- ifelse(EC2$vertical == "security", 1, 0)
EC2$isTherapy <- ifelse(EC2$vertical == "therapy", 1, 0)

EC2$Cust_Psim <- 1
EC2$Cust_MHW <- 0
EC2$Cust_L360 <- 0
EC2$Cust_Lobb <- 0

head(EC2)


write.csv(EC1,"C:\\Users\\Nehal\\OneDrive\\Documents\\MSBA_StudyMaterial\\Fall2022\\MarketingIntelligence\\Project\\EC1_Updated.csv")
write.csv(EC2,"C:\\Users\\Nehal\\OneDrive\\Documents\\MSBA_StudyMaterial\\Fall2022\\MarketingIntelligence\\Project\\EC2_Updated.csv")

#########################################################################

##Further removing unecessary columns for correlation plot
unwanted_col <- c("vertical", "zip", "Cust_Psim", "Cust_MHW", "Cust_L360")

EC1_cor <- correlate(EC1[ , !names(EC1) %in% unwanted_col],
                     method = 'pearson',
                     use = 'pairwise.complete.obs')
EC1_cor %>% rplot() # this turns correlation table into a visual

#Detailed correlation matrix
corr_matrix <- cor(subset(EC1, select = -c(Cust_Psim, Cust_MHW, Cust_L360, vertical, zip)))
corr_matrix

##############################################################################

#Logistic regression model 

#One of the vertical has to be excluded say 'IsFinance' as when you build your model, you will need to treat one of these vertical dummies
# as the "reference category" (or a baseline group)
#Since 'vertical' column has been split into multiple binary features, we should not include it in our logistic regression model
EC1_test <- subset(EC1, select = -c(Cust_Psim, Cust_MHW, Cust_L360, vertical, zip))

#glm2 <- glm(Cust_Lobb ~ . - isFinance - PsimRev07 - PsimRev08 - PsimRev09 - PsimRev10 - PsimRev11 - PsimRev12 - PsimRev13 - PsimRev14 - PsimRev15 - PsimRev16 - tenure - latepay - Psim_vol - Psim_CC - Psim_mob - touches, family = "binomial", data=EC1_test)
glm2 <- glm(Cust_Lobb ~ . - isTherapy - PsimRev07 - PsimRev08 - PsimRev09 - PsimRev10 - PsimRev11 - PsimRev12 - PsimRev13 - PsimRev14 - PsimRev15 - PsimRev16 -  Psim_CC - Psim_mob - touches, family = "binomial", data=EC1_test)


#glm2 <- glm(Cust_Lobb ~ . - isFinance - Cust_Psim - Cust_MHW - Cust_L360 - vertical - zip - PsimRev07 - PsimRev08 - PsimRev09 - PsimRev10 - PsimRev11 - PsimRev12 - PsimRev13 - PsimRev14 - PsimRev15 - PsimRev16 - tenure - latepay - Psim_vol - Psim_CC - Psim_mob - touches, family = "binomial", data=EC1)
summary(glm2)

#Find the best threshold to determine model accuracy
glm2.probs <- predict(glm2, type='response', newdata = EC1)
maxAcc <- 0; maxThreshold <- 0; for (threshold in seq(1,100)) {
  glm2.pred <- rep(0, nrow(EC1))
  glm2.pred[glm2.probs > threshold/100 ] <- 1
  currentAcc <- mean(glm2.pred == EC1$Cust_Lobb)
  print(paste(threshold,currentAcc))
  if (currentAcc > maxAcc) {maxAcc <- currentAcc; maxThreshold <- threshold/100}
}

glm2.pred <- rep(0, nrow(EC1))
glm2.pred[glm2.probs > maxThreshold] <- 1
table(glm2.pred, EC1$Cust_Lobb)
#should be close to 70%, we have 70.27%

EC2_Pred <- subset(EC2, select = -c(Cust_Psim, Cust_MHW, Cust_L360, vertical, zip))

## to predict using logistic regression model, probabilities obtained
EC2_Pred$Cust_Lobb <- predict(glm2, EC2_Pred, type="response")

EC2_Pred$Cust_Lobb_Result <- ifelse(EC2_Pred$Cust_Lobb > 0.5068, 1, 0)

## Look at probability output
head(EC2_Pred, 10)
EC2_Final <- EC2_Pred[order(-EC2_Pred$Cust_Lobb),]
write.csv(EC2_Final,"C:\\Users\\Nehal\\OneDrive\\Documents\\MSBA_StudyMaterial\\Fall2022\\MarketingIntelligence\\Project\\EC2_Predictions.csv")


#For submission
EC2_Final2 <- subset(EC2_Final, select = c("custid"))
EC2_Final2 = EC2_Final2[0:1000,]
write.csv(EC2_Final2,"C:\\Users\\Nehal\\OneDrive\\Documents\\MSBA_StudyMaterial\\Fall2022\\MarketingIntelligence\\Project\\EC2_Predictions_SUBMISSION.csv")

