

setwd("D:/Ali/Si Krispi/DATA")

rm(list = ls())
#import
library(readxl)

library(randomForest)

#Baca data
data <- read_excel("Data.xlsx", sheet = 8) #1535
data <- data[data$Ket == "Tidak" | data$Ket == "Tepat Waktu", ]
dim(data) #1333

data$Ket <- as.factor(data$Ket)
data$MAMPU_TIDAK <- as.factor(data$MAMPU_TIDAK)
data$BEASISWA <- as.factor(data$BEASISWA)
data$JurusanSMA <- as.factor(data$JurusanSMA)
data$Jenis_Ilmu <- as.factor(data$Jenis_Ilmu)
data$NIM <- NULL
data$JURUSAN <- NULL

set.seed(1234)

#ubah urutan data sehinga menjadi random
data <- data[sample(nrow(data)),]

JML_KLP <- 10

jmlPerKlp <- round(nrow(data) / JML_KLP)
indeksAwal <- seq(1, nrow(data), jmlPerKlp)
indeksAwal <- indeksAwal[1:JML_KLP]
indeksAkhir <- indeksAwal - 1
indeksAkhir <- indeksAkhir[2:JML_KLP]
indeksAkhir <- c(indeksAkhir, nrow(data))


klp <- rep("", nrow(data))
for (i in 1:JML_KLP) {
  klp[indeksAwal[i]:indeksAkhir[i]] <- i
}

#Buat mmodel RF
for (i in 1:10) {
  trainIndex <- klp != i
  rf_model <-
    randomForest(
      Ket ~ .,
      data = data,
      subset = trainIndex,
      ntree = 50,
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
  print(paste("CV ke: ", i, "n: 50"))
  print(rf_model$confusion)
  print(paste("Akurasi      : ", acc))
  print(paste("Spesifisitas : "))
  print(paste("Sensitifitas : "))
  
  
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
  print(paste("CV ke: ", i, "n: 100"))
  print(rf_model$confusion)
  print(paste("Akurasi      : ", acc))
  print(paste("Spesifisitas : "))
  print(paste("Sensitifitas : "))
}

rf_model$importance
# Tambahkan warna
warna <- c("green", "blue", "orange", "red")
labels <- c("TP", "TN", "FP", "FN")

resultAll$warnaPlot <- ""
resultAll$warnaPlot[resultAll$label == "TP"] <- "green"
resultAll$warnaPlot[resultAll$label == "TN"] <- "blue"
resultAll$warnaPlot[resultAll$label == "FP"] <- "orange"
resultAll$warnaPlot[resultAll$label == "FN"] <- "red"

#Hasilkan Plot
plot(resultAll$IPS8, resultAll$IPK, col = resultAll$warnaPlot,
     pch = 20,
     xlab = "IPS8", ylab = "IPK",
     main = "Plot Data Prediksi Lama Studi Mahasiswa")

#tambahkan legenda 
do.call(legend, c("bottomleft", list(legend=labels, col=warna, pch=20)))

#simpan hasil prediksi
write.table(resultAll, file="HASIL5&25.txt",sep="\t")
         