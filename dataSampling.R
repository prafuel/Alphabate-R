
# Feature selection using Impurity / Entropy technique and balancing dataset using z-score

library(dplyr)
df <- read.csv("/home/prafull/This_1/college/commonModule/Data_Science/project/R/heart.csv")
df
length(df$Cholesterol)

# df1 <- df[which()]

# end test
  
# EDA
str(df) # structure of dataset

df$Gender <- ifelse(df$Sex == "M" , 1, 0)
df$ExerBin <- ifelse(df$ExerciseAngina == "Y",1,0)


df1 <- data.frame(df[which(df$Cholesterol != 0),])
str(df1)
View(df1)

#  Removing null and missing values
df1 <- na.omit(df1)
summary(df1)

# majority class
mj <- df1[which(df1$HeartDisease == 0),]$HeartDisease
cat("Majority class=0 : length = ",length(mj))
# minority class
mn <- df1[which(df1$HeartDisease == 1),]$HeartDisease
cat("Minority class=1 : length = ",length(mn))

# length of dataset after removing null and missing values
length(df1$MaxHR) #746
hist(df1$MaxHR)

# Feature selection using impurity or gini impurity -> step1
# function 7 -->
totalEn <- function(ten,fen,ttotal,ftotal) {
  # we will calculating gain here
  return( (ten * (ttotal / (ttotal + ftotal))) + (fen * (ftotal / (ttotal + ftotal))) )
}

# function 5 --> used to calculate empurity of perticular column and perticular condition
en <- function(yes,no) {
    
  # entropy for perticular spliting point
  
  py <- yes / (yes + no)
  pn <- no / (yes + no)
  
  if(py != 0 & pn != 0) {
    E <- -((py * log(py , base = 2)) + (pn * log(pn , base = 2)))
  } else {
    if(py == 0) {
      E <- -(pn * log(pn , base = 2))
    }
    else {
      E <- -(py * log(py , base = 2))
    }
  }
  
  return (E)
}

# function 4
totalim <- function(tim,fim,ttotal,ftotal) {
  v <- (ttotal / (ttotal + ftotal) * tim) + (ftotal / (ttotal + ftotal) * fim)
  return(v)
}

# function 3  --> used to caculate gini impurity for perticular column
im <- function(yes,no) {
  return (1 - (yes / (yes + no))^2 - (no / (yes + no))^2 )
}

# function 2
impurity <- function(feature,lst,flen,ytarget,imp) {

  tcount0 <- 0
  tcount1 <- 0
  
  fcount0 <- 0
  fcount1 <- 0
  
  # print("inside impurity function")
  
  # creating dataframe to store all imp values like "iavg" "tcount0" "tcount1" "fcount0" "fcount1" "tim" "fim"
  index = 0
  
  for (iavg in lst) {
    # cat("\n iavg value : ",iavg)
    
    for (fvalue in 1:flen) {
      if(feature[fvalue] < iavg) {
        if(ytarget[fvalue] == 0) {
          tcount0 = tcount0 + 1
        }
        else{
          tcount1 = tcount1 + 1
        }
      }
      
      else{
        if(ytarget[fvalue] == 0) {
          fcount0 = fcount0 + 1
        }
        else{
          fcount1 = fcount1 + 1
        }
      }
    }
    
    # cat("\n tcount0 = ",tcount0)
    # cat("\n tcount1 = ",tcount1)
    # cat("\n fcount0 = ",fcount0)
    # cat("\n fcount1 = ",fcount1)
    # 
    
    # total count of yes and no values in both True and false table
    ttotal <- tcount0 + tcount1
    ftotal <- fcount0 + fcount1
    
    
    if(imp == TRUE) {
      # Getting impurity for indivisiual level
      tim <- im(tcount1,tcount0)
      fim <- im(fcount1,fcount0)
      total <- totalim(tim,fim,ttotal,ftotal)
    }
    else {
      ten <- en(tcount1,tcount0)
      fen <- en(fcount1,fcount0)
      E <- en(ttotal,ftotal)
      #  overall entropy of column
      entropyE <- totalEn(ten,fen,ttotal,ftotal)
      # Gain
      total <- E - entropyE
    }
    
    
    # cat("\n sample index = ",index)
    # cat("\n sample value = ",feature[index])
    # cat("\n tim = ",tim)
    # cat("\n fim = ",fim)
    # cat("\n total weighted avarage impurity ",total)
    
    # looking for lowest gini impurity in all over feature
    index = index + 1
    
    if(imp == TRUE) {
      if(index == 1) {
        minIm <- total
      } else {
        if (minIm > total) {
          minIm <- total
        }
      }
      cat("minimum impurity :", minIm)
    }
    else {
      if(index == 1) {
        maxEn <- total
      } else {
        if (maxEn < total) {
          maxEn <- total
          # cat("\n maximum gain: \n", maxEn)
        }
      }
      
    }
    
    # Resetting all values
    tcount0 <- 0
    tcount1 <- 0
    fcount0 <- 0
    fcount1 <- 0
  }
  if(imp == TRUE) {
    #  Minimum impurity found in this feature is
    # cat("\n lowest impurity found in this feature is : \n", minIm)
    return(minIm)
  }
  else {
    return(maxEn)
  }
}

# function 1
numericData <- function(orderD,c) {
  
  feature = orderD[1][,1]
  y = orderD[2][,1]
  
  len <- length(feature)
  l1 <- list()
  j = 0
  
  #  finding split point here
  for(i in 2:len) {
    if(y[i-1] != y[i]) {
      splitPoint <- (feature[i-1] + feature[i]) / 2
      l1[j] = splitPoint
      j = j + 1
      # print(splitPoint)
    }
    # splitPoint <- (feature[i-1] + feature[i]) / 2
    # l1[j] = splitPoint
    # j = j + 1
  }
  
  if(c == TRUE) {
    lowestIm <- impurity(feature,l1,len,y,c)  # here we are calling function 2
    return(lowestIm)
  }
  else{
    lowestEntro <- impurity(feature,l1,len,y,c)
    return(lowestEntro)
  }
}

# function 0
valueGetter <- function(fcol,target) {
  f <- data.frame(fcol,target)
  # for gini impurity
  df_sorted <- f[order(f$fcol),]
  return(df_sorted)
}

# declaring Target Variable
target <- df1$HeartDisease

# Numeric data
# for feature 1 (Age)

imp = FALSE #if it is false it will calculate entropy
imp = TRUE


i <- valueGetter(df1$Age,target)  
View(i)
attr1 <- numericData(i,imp)
attr1

# for feature 2 (RestingBP)
j <- valueGetter(df1$RestingBP,target)
attr2 <- numericData(j,imp)
attr2

# for feature 3 (Cholesterol)
k <- valueGetter(df1$Cholesterol,target)
attr3 <- numericData(k,imp)
attr3


# for feature 4 (MaxHR)
l <- valueGetter(df1$MaxHR,target)
attr4 <- numericData(l,imp)
attr4



# for feature 5 (oldpeak)
m <- valueGetter(df1$Oldpeak,target)
attr5 <- numericData(m,imp)
attr5

# function 9
categoricData <- function(orderD,imp) {
  feature <- orderD[1][,1]
  target <- orderD[2][,1]
  
  #  ***** using "which" by giving condition
  tcount0 <- length(which(orderD$fcol == 1 & orderD$target == 0))
  tcount1 <- length(which(orderD$fcol == 1 & orderD$target == 1))
  
  fcount0 <- length(which(orderD$fcol == 0 & orderD$target == 0))
  fcount1 <- length(which(orderD$fcol == 0 & orderD$target == 1))
  
  # total count of yes and no values in both True and false table
  ttotal <- tcount0 + tcount1
  ftotal <- fcount0 + fcount1
  
  if(imp == TRUE) {
    # Getting impurity for indivisiual level
    tim <- im(tcount1,tcount0)
    fim <- im(fcount1,fcount0)
    total <- totalim(tim,fim,ttotal,ftotal)
    
  } else{
    # Getting entropy for indivisiual level
    ten <- en(tcount1,tcount0)
    fen <- en(fcount1,fcount0)
    total <- totalEn(ten,fen,ttotal,ftotal)
  }
  
  return(total)
}


# finding impurity for categorical data

# FastingBS
n <- valueGetter(df1$FastingBS,target)
attr6 <- categoricData(n,imp)
attr6

# Gender
o <- valueGetter(df1$Gender , target)
attr7 <- categoricData(o,imp)
attr7

# Exercise
p <- valueGetter(df1$ExerBin,target)
attr8 <- categoricData(p,imp)
attr8


# categorical data can be helpful sometime


# here we are sorting feature according to there impurity level (numeric value column)
attr <- c(attr1,attr2,attr3,attr4,attr5,attr6,attr7,attr8)
if(imp == TRUE) {
  lowestim <- valueGetter(attr,c("age","restingBP","cholesterol","maxHR","oldpeak","FastingBS","Gender","Exercise"))
  View(lowestim)
} else {
  Gain <- valueGetter(attr,c("age","restingBP","cholesterol","maxHR","oldpeak","FastingBS","Gender","Exercise"))
  View(Gain)
}

# outlier part part is now going
# lowest impurity attribute : oldpeak but we are taking 2nd attribute MaxHR because it gives more realiable distribution and used to remove access or extra samples as we are doing undersampling

# undersampling and balacing dataset starts from here

# minority class (1s) : 356
# majority class (0s) : 390

# majority class is having sample "0" which are 390 


# Start of Zscore method --> undersampling

x <- (length(mj) - length(mn)) / length(mj) #this percentage of data will be removed from dataset
cat("\n this percentage of data will be removed from majority class : \n",x)
# number of samples to be removed from majority class (0s in this dataset) 
rmValues <- (length(mj) * x) / 100
cat("\n number of data to be removed : \n",rmValues)

# function 8 --> use to find zscore for every value in dataset
zscore <- function(d) {
  x <- mean(d)
  se <- sd(d)
  zsc <- (d - x) / se
  return(zsc)
}

# function 9 -> used to remove outliner data and balanced --> parameters are : "dataset" and "percentage of data" to be removed
balancingUsingZscore = function(dataS,x) {
  
  # we have to find zscore for "x" pvalue
  y <- x
  cat("\n amount of data to be removed : \n",y) 
  
  p <- abs(qnorm(y/2))
  cat("\n zscore for pvalue : \n",p)
  
  zscMaxhr <- zscore(dataS)
  
  # p <- 1.71
  cat("\n maximum zscore : \n",max(zscore(dataS)))
  cat("\n minimum zscore : \n",min(zscore(dataS)))
  
  new1 <- df1[which(df1$HeartDisease == 0),]
  df2 <- cbind(new1,data.frame(zscMaxhr))
  
  balancedDataZsc <- df2[which(df2$zscMaxhr >= -p & df2$zscMaxhr <= p),] # balanced dataset with equal amount of 1s and 0s
  length(balancedDataZsc$MaxHR)
  
  str(balancedDataZsc)
  hist(balancedDataZsc$MaxHR) #main histogram
  
  mnClass <- df1[which(df1$HeartDisease == 1),]
  
  b <- balancedDataZsc[, -ncol(balancedDataZsc)]
  str(b)
  balancedData <- rbind(b,mnClass)
  print("balanced Dataset is Ready!!! (zscore method)")
  
  return(balancedData)
} 

# End of Zscore method


new <- df1[which(df1$HeartDisease == 0),]$MaxHR
balancedData <- balancingUsingZscore(new,x) #this function is used to balanced dataset
View(balancedData)


cat("\n unbalaced number of zeroes : \n",length(which(df1$HeartDisease == 0)))
cat("\n unbalanced number of ones : \n",length(which(df1$HeartDisease == 1)))
cat("\n unbalanced total number of samples : \n",length(df1$HeartDisease))

cat("\n balanced number of zeroes : \n",length(which(balancedData$HeartDisease == 0)))
cat("\n balanced number of ones : \n",length(which(balancedData$HeartDisease == 1)))
cat("\n balanced total number of samples : \n",length(balancedData$HeartDisease))


# Oversampling
# SMOTE
sum(df1$HeartDisease == 0)

feature <- c("ExerBin","Oldpeak","MaxHR","Age","Gender")
feature <- data.frame(feature)
feature

library(ROSE)
data <- df1

table(data$HeartDisease)
oversampled_data <- ROSE(HeartDisease ~ ExerBin + Oldpeak + MaxHR + Age + Gender, data = data, seed = 123)$data
table(oversampled_data$HeartDisease)


confusionMtx <- function(model,test_data) {
  pred <- predict(model,newdata = test_data, type = "response")
  conf_mat <- table(pred,test_data$HeartDisease)
  print(conf_mat)
}


# model training  : Random Forest

balancedData

balancedData <- balancedData %>% 
  mutate(HeartDisease = factor(HeartDisease)) %>% 
  mutate(Gender = factor(Gender)) %>% 
  mutate(FastingBS = factor(FastingBS)) %>% 
  mutate(ExerBin = factor(ExerBin))

str(balancedData)

library(randomForest)

set.seed(1234)

# Randomly selecting values from balanced dataset (Random Sampling)
train_index <- sample(1:nrow(balancedData), size = floor(0.8 * nrow(balancedData)), replace = FALSE)

trainData <- data.frame(balancedData[train_index , ])
testData <- data.frame(balancedData[-train_index , ])


head(trainData)
str(trainData)



# using top 5 lowest gini impurity attributes
model1 <- randomForest(HeartDisease ~ ExerBin + Oldpeak + MaxHR + Age + Gender , data = trainData , proximity = TRUE)
model1 # 0.82
confusionMtx(model1,testData)

# using top 5 highest gain attributes
model2 <- randomForest(HeartDisease ~ FastingBS + Gender + ExerBin + Oldpeak + MaxHR + Age , data = trainData , proximity = TRUE)
model2 #0.83
confusionMtx(model2,testData)


