#import
rm(list = ls())
library(readxl)
library(randomForest)

#baca
data <- read_excel("Data.xlsx", sheet = 8)
data <- data[data$Ket == "Tidak" | data$Ket == "Tepat Waktu", ]
dim(data) #1333
data$Ket <- as.factor(data$Ket)
data$MAMPU_TIDAK <- as.factor(data$MAMPU_TIDAK)
data$BEASISWA <- as.factor(data$BEASISWA)
data$JurusanSMA <- as.factor(data$JurusanSMA)
data$Jenis_Ilmu <- as.factor(data$Jenis_Ilmu)
data$NIM <- NULL


set.seed(1234)

#ubah urutan data sehinga menjadi random
data <- data[sample(nrow(data)),]


klp <- data$JURUSAN
klpUnik <- unique(data$JURUSAN)
JML_KLP <- length(klpUnik)



for (i in 1:JML_KLP) {
  trainIndex <- klp != klpUnik[i]
  rf_model <-
    randomForest(
      Ket ~ .,
      data = data,
      subset = trainIndex,
      ntree = 50,
      na.action = na.omit,
      importance = T
    )
  result <- predict(rf_model, newdata = data[!trainIndex,])
  resultAll <- cbind(data[!trainIndex,], result)
  
  #Tentukan TP, TN, FP, FN
  resultAll$label <- ""
  resultAll$label[resultAll$Ket == "Tepat Waktu"& resultAll$result=="Tepat Waktu"] <- "TP"
  resultAll$label[resultAll$Ket== "Tidak"& resultAll$result=="Tidak"] <- "TN"
  resultAll$label[resultAll$Ket== "Tepat Waktu"& resultAll$result=="Tidak"] <- "FP"
  resultAll$label[resultAll$Ket== "Tidak"& resultAll$result=="Tepat Waktu"] <- "FN"
  
  
  
  acc <- ( sum(resultAll$label == "TP") + sum(resultAll$label == "TN"))/
    nrow(resultAll) * 100
  
  spe <- sum(resultAll$label == "TN")/
    ( sum(resultAll$label == "TN") + sum(resultAll$label == "FP")) * 100
  
  sen <- sum(resultAll$label == "TP")/
    ( sum(resultAll$label == "TP") + sum(resultAll$label == "FN")) * 100
  
  
  print("-------------------------------")
  print(paste("CV: ", klpUnik[i], "n: 50"))
  print(rf_model$confusion)
  print(paste("Akurasi      : ", acc))
  print(paste("Spesifisitas : ", spe))
  print(paste("Sensitifitas : ", sen))
  
  
  rf_model <-
    randomForest(
      Ket ~ .,
      data = data,
      subset = trainIndex,
      ntree = 100,
      na.action = na.omit,
      importance = T
    )
  
  #Lakukan prediksi di data uji
  result <- predict(rf_model, newdata = data[!trainIndex,])
  resultAll <- cbind(data[!trainIndex,], result)
  
  #Tentukan TP, TN, FP, FN
  resultAll$label <- ""
  resultAll$label[resultAll$Ket == "Tepat Waktu"& resultAll$result=="Tepat Waktu"] <- "TP"
  resultAll$label[resultAll$Ket== "Tidak"& resultAll$result=="Tidak"] <- "TN"
  resultAll$label[resultAll$Ket== "Tepat Waktu"& resultAll$result=="Tidak"] <- "FP"
  resultAll$label[resultAll$Ket== "Tidak"& resultAll$result=="Tepat Waktu"] <- "FN"
  
  
  
  acc <- ( sum(resultAll$label == "TP") + sum(resultAll$label == "TN"))/
    nrow(resultAll) * 100
  
  spe <- sum(resultAll$label == "TN")/
    ( sum(resultAll$label == "TN") + sum(resultAll$label == "FP")) * 100
  
  sen <- sum(resultAll$label == "TP")/
    ( sum(resultAll$label == "TP") + sum(resultAll$label == "FN")) * 100
  
  print("-------------------------------")
  print(paste("CV ke: ", klpUnik[i], "n: 100"))
  print(rf_model$confusion)
  print(paste("Akurasi      : ", acc))
  print(paste("Spesifisitas : ", spe))
  print(paste("Sensitifitas : ", sen))
}
