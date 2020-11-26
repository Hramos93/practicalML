#Libraries
#processing
library(dplyr);library(data.table);library(plyr)
library(ggplot2);library(cowplot);library(tidyr);
library(lattice);library(caret);library(corrplot);





urlTraining <- 'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv'
urlTest <- 'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv'

training <- read.csv(urlTraining)
test <- read.csv(urlTest)

str(training)
training[training==""] <- NA


na_count <-sapply(training, function(x) sum(length(which(is.na(x)))))
na_count <- data.frame(na_count)

removeCol <- na_count %>% {which(.>0)}
removeCol <- c(rownames(na_count)[removeCol])
training <- training[,-which(names(training) %in% removeCol)]
test <- test[,-which(names(test) %in% removeCol)]

na_count2 <-sapply(training, function(x) sum(length(which(is.na(x)))))
na_count2 <- data.frame(na_count2)

removeCol2 <- c("X","user_name")
training <- training[,-which(names(training) %in% removeCol2)]
test <- test[,-which(names(test) %in% removeCol2)]

na_count2 <-sapply(training, function(x) sum(length(which(is.na(x)))))
na_count2 <- data.frame(na_count2)
######################################


unique(training$new_window)
training$new_window <- revalue(training$new_window,c("yes" = 1))
training$new_window <- revalue(training$new_window, c("no"=0))


test$new_window <- revalue(test$new_window, c("no"=0))
training$new_window<- as.numeric(training$new_window)
test$new_window <- as.numeric(test$new_window)
str(test)


test$classe <-as.numeric( as.factor(training$classe ))
test$classe <-as.numeric( as.factor(test$classe ))

trainingdate <- training[,-3]

mat <- data.frame(formatC(cor(trainingdate, method ="pearson"), format="f", digits=1))




lowCor <- mat%>% {which(mat$classe > 0)}
removeCol <- c(rownames(na_count)[removeCol])


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
  data = trainingdate,
  method= "pearson",
  sig.level = 0.05,
  order = "original",
  diag = FALSE,
  type = "upper",
  tl.srt = 90
  
)
trainingdate <- trainingdate[,-c(1,2,3,30 )]


mat <- cor(trainingdate, method ="pearson")
mat <- formatC(mat, format="f", digits = 2)