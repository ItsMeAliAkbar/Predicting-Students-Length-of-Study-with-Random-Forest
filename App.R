rm(list=ls())
#import paket

library(shiny)


#baca data hasil prediksi
hasilprediksi <- read.table(file="HASIL.txt",
                            stringsAsFactors = T)

pilihannumerik <- colnames(hasilprediksi)[c(3,4,6:13)]
pilihankategori <- colnames(hasilprediksi)[c(1,2,5,14)]

#Kode UI
ui <- fluidPage(
  titlePanel("Visualisasi Prediksi Lama Studi Mahasiswa FMIPA UR"),
  
  sidebarLayout(
    sidebarPanel(
                  # membuat input untuk plot data numerik
                  br(),
                  selectInput("x_var", "Pilih Variabel X:", choices = pilihannumerik),
                  selectInput("y_var", "Pilih Variabel Y:", choices = pilihannumerik),
                  
                  # membuat input untuk plot data kategori
                  selectInput("var_barplot", "Pilih Variabel untuk Data Kategori:",
                              choices = pilihankategori)),
                  
    mainPanel(
      tabsetPanel(
        tabPanel("Data Numerik", plotOutput("scatter_plot")),
        tabPanel("Data Kategori", plotOutput("bar_plot"))
      )
    )
  )
)

#Kode server
server <- function(input, output) {
  
  data_x <- reactive({
   hasilprediksi[, input$x_var]
   })
  data_y <- reactive({
    hasilprediksi[, input$y_var]
  })
  
  hasilprediksi$warnaPlot <- "" #Penambahan warna 
  hasilprediksi$warnaPlot[hasilprediksi$label == "TP"] <- "green"
  hasilprediksi$warnaPlot[hasilprediksi$label == "TN"] <- "blue"
  hasilprediksi$warnaPlot[hasilprediksi$label == "FP"] <- "orange"
  hasilprediksi$warnaPlot[hasilprediksi$label == "FN"] <- "red"
  
  # Buat scatter plot untuk numerik
  output$scatter_plot <- renderPlot({
    x = data_x()
    y = data_y()
    
    x_label <- input$x_var #Pelabelan sumbu x dan y
    y_label <- input$y_var
    print(x)
    plot(x, data_y(), col = hasilprediksi$warnaPlot,
         pch = 20,
         main = "Visualisasi Plot Data Numerik",
         xlab = x_label,
         ylab = y_label)
    
    legend("bottomleft", legend = c("TP", "TN", "FP", "FN"), #Penambahan Legenda
           col = c("green","blue","orange","red"), pch = 20)
  })
  
  # Buat bar plot untuk kategori
  output$bar_plot <- renderPlot({
    barplot(table(hasilprediksi[, "label"], hasilprediksi[, input$var_barplot]), 
            xlab = input$var_barplot, #pelabelan sumbu x
            ylab = "Jumlah", 
            main = paste("Bar Plot untuk data", input$var_barplot),
            col = c("red","orange","blue","green"))
    
    legend("bottomleft", legend = c("TP", "TN", "FP", "FN"), #Penambahan Legenda
           col = c("green","blue","orange","red"), pch = 20)
  })
}

# Run 
shinyApp(ui = ui, server = server)

#Penyebaran aplikasi hasil prediksi
#Menginstall Paket
install.packages('rsconnect')

#Mengimport paket
library(rsconnect)

#Mengonfigurasi Akun
rsconnect::setAccountInfo(name='aliakbar',
                          token='C19F07FCF87E91D1F06A2D965C96E54C',
                          secret='2t6Bs1gdZxPc+kP3hE9ZlQ4AwFYsFTsf3iZETZBY')

#Menyebarkan Aplikasi
rsconnect::deployApp('newdir')
