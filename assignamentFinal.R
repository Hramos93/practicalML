#Libraries
#processing
library(dplyr);library(data.table);library(plyr)
library(ggplot2);library(cowplot);library(tidyr);library(Hmisc);
library(hrbrthemes);library(viridis)

library(lattice);library(caret);library(corrplot);
library(rpart);library(rpart.plot);library(randomForest)


#LOAD DATA
urlTraining <- 'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv'
urlTest <- 'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv'

training <- read.csv(urlTraining)
test <- read.csv(urlTest)


#DATA PROCESSING 

str(training)

#DATA CLEANING

#NA
training[training==""] <- NA
test[test==""] <- NA

#Drop column with NA
na_count <-sapply(training, function(x) sum(length(which(is.na(x)))))
na_count <- data.frame(na_count)

removeCol <- na_count %>% {which(.>0)}
removeCol <- c(rownames(na_count)[removeCol])
training <- training[,-which(names(training) %in% removeCol)]
test <- test[,-which(names(test) %in% removeCol)]

#Drop column without values
removeCol2 <- c("X","user_name","cvtd_timestamp")
training <- training[,-which(names(training) %in% removeCol2)]
test <- test[,-which(names(test) %in% removeCol2)]


#Convert Character to number to new_window
unique(training$new_window)
training$new_window <- revalue(training$new_window,c("yes" = 1))
training$new_window <- revalue(training$new_window, c("no"=0))
test$new_window <- revalue(test$new_window, c("no"=0))
training$new_window<- as.numeric(training$new_window)
test$new_window <- as.numeric(test$new_window)


#COMPUTE CORRELATION
dataC <- training
dataC$classe <- as.numeric(as.factor(dataC$classe))


mat <- data.frame(formatC(cor(dataC , method ="pearson"), format="f", digits=1))
mat[] <- lapply(mat, as.numeric)

#Drop column without Correlation
lowCor <- mat%>% {which(mat$classe == 0.0)}
removeCol <- c(rownames(mat)[lowCor])

dataC <- dataC[,-which(names(dataC) %in% removeCol)]


test <- test[,-which(names(test) %in% removeCol)]


corr2 <- function(data,
                  method = "pearson",
                  sig.level = 0.05,
                  order = "original",
                  diag = FALSE,
                  type = "upper",
                  tl.srt = 90,
                  number.font = 0.5,
                  number.cex = 0.5,
                  mar = c(0, 0, 0, 0)) {
  

  data_incomplete <- data
  data <- data[complete.cases(data),]
  mat <- cor(data, method = method)
  cor.mtest <- function(mat, method) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat <- matrix(NA, n, n)
    diag(p.mat) <- 0
    for ( i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[,i], mat[, j], method = method)
        p.mat[i, j] <- p.mat[j,i] <- tmp$p.value
        
      }
    }
    
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
  p.mat <- cor.mtest(data, method = method)
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  corrplot(mat, 
           method = "color", col= col(200), number.font = number.font,
           mar = mar, number.cex = number.cex,
           type = type, order = order,
           addCoef.col = "black",
           # add correlation coefficient
           tl.col = "black", tl.srt = tl.srt, # Rotation of text labels,
           # combine with significance level
           p.mat = p.mat, sig.level = sig.level, insig = "blank",
           # hide correlation coefficiens on the diagonal
           diag = diag
  )
}


corr2(
  data = dataC,
  method= "pearson",
  sig.level = 0.05,
  order = "original",
  diag = FALSE,
  type = "upper",
  tl.srt = 90
  
)


par(mfrow=c(2,2))

boxplot(magnet_belt_y~classe,
        data=trainingdate,
        main="magnet_belt_y VS classe",
        xlab="classe",
        ylab="magnet_belt_y",
        col="orange",
        border="brown"
)
boxplot(magnet_arm_x~classe,
        data=trainingdate,
        main="magnet_arm_x VS classe",
        xlab="classe",
        ylab="magnet_arm_x",
        col="orange",
        border="brown"
)

boxplot(magnet_arm_y~classe,
        data=trainingdate,
        main="magnet_arm_y VS classe",
        xlab="classe",
        ylab="magnet_arm_y",
        col="orange",
        border="brown"
)

boxplot(accel_arm_x~classe,
        data=trainingdate,
        main="accel_arm_x VS classe",
        xlab="classe",
        ylab="accel_arm_x",
        col="orange",
        border="brown"
)


#Remove Col from principal dataset
training <- training[,-which(names(training) %in% removeCol)]
test <- test[,-which(names(test) %in% removeCol)]


######MODELS

inTrain <- createDataPartition(y=training$classe, p=0.75, list=FALSE)
trainM <- training[inTrain, ] 
testM <- training[-inTrain, ]


# Fit model
modFitDT <- rpart(classe ~ ., data=trainM, method="class")

# Perform prediction
predictDT <- predict(modFitDT, testM, type = "class")

# Plot result
rpart.plot(modFitDT, main="Classification Tree", extra=102, under=TRUE, faclen=0)
testM$classe <- as.factor(testM$classe)
confusionMatrix(predictDT, testM$classe)

#Random Forest
# Fit model

RF_modfit <- train(classe ~ ., data = trainM, method = "rf", ntree = 100)
RF_prediction <- predict(RF_modfit, testM)
RF_pred_conf <- confusionMatrix(RF_prediction, testM$classe)
RF_pred_conf


RF_pred_conf$overall
