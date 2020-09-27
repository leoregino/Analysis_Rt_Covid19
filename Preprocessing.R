
library(shiny)
library(dplyr)
library(jsonlite)
library(tidyverse)
library(ggplot2)
library(EpiEstim)

### 1. Load from web ### 
url <- "https://www.datos.gov.co/resource/gt2j-8ykr.json?$limit="
nb_limit <- "1000000"
datosImport <- fromJSON(paste0(url,nb_limit))

### 2. Columns containing NA in JSON ###
total_row <- nrow(datosImport)

col_NA <- data.frame( matrix(ncol = 2, nrow = 0) )
colnames(col_NA) <- c("Variable", "Perc_NA") 

### 2.1 Save proportion of NA in columns ###
for (nom_col in colnames(datosImport)){
    nro_NA <- nrow(datosImport[is.na.data.frame(datosImport[nom_col]), ]) 
    if (nro_NA>0){
      col_NA[nrow(col_NA) + 1,] = c(nom_col, round(nro_NA/total_row*100,1)  )
    }
}
col_NA$Nb_NA <- as.numeric(col_NA$Nb_NA) 

### 2.2 Plot Percentage of missing values in columns ###
q <- ggplot(data = col_NA, aes(x = Variable, y = Perc_NA) ) +
  geom_bar(stat="identity", fill = "blue") +
  geom_text(aes(label=Perc_NA), vjust = 1.6, color = "black", size = 3) +
  labs(title = "Percentage of NA per variable")

q

### 3. TREAT THE MISSING VALUES ###

# 3.1. [pa_s_de_procedencia]: If NA --> Assign = COLOMBIA
datosImport[is.na.data.frame(datosImport$pa_s_de_procedencia), ]$pa_s_de_procedencia <- "COLOMBIA"

# 3.2. [fecha_diagnostico]: If NA --> Assign = 0001-01-01T00:00:00.000
datosImport[is.na.data.frame(datosImport$fecha_diagnostico), ]$fecha_diagnostico <- datosImport[is.na.data.frame(datosImport$fecha_diagnostico), ]$fecha_reporte_web

# 3.3. [fis]: If NA --> Assign = fecha_diagnostico - 10
datosImport[is.na.data.frame(datosImport$fis), ]$fis <- datosImport[is.na.data.frame(datosImport$fis), ]$fecha_diagnostico

# 3.4. [fecha_recuperado]: IF NA --> 0001-01-01T00:00:00.000
datosImport[is.na.data.frame(datosImport$fecha_recuperado), ]$fecha_recuperado <-  "0001-01-01T00:00:00.000"

# 3.5. [tipo_recuperaci_n]: If NA (Vivo o Fallecido) --> Assign = "NA" 
datosImport[is.na.data.frame(datosImport$tipo_recuperaci_n), ]$tipo_recuperaci_n <-  "NA" 

# 3.6. [codigo_pais]: If NA (COLOMBIA) --> Assign = 170 (code ISO 3166-1)
datosImport[is.na.data.frame(datosImport$codigo_pais), ]$codigo_pais <-  170 

# 3.7. [nombre_grupo_etnico]: IF NA <-- Assign = "Otro"
datosImport[is.na.data.frame(datosImport$nombre_grupo_etnico), ]$nombre_grupo_etnico <-  "Otro" 

# 3.8. [fecha_de_muerte]: IF NA (vivo o Recuperado ) <-- Assign = 0001-01-01T00:00:00.000 
datosImport[is.na.data.frame(datosImport$fecha_de_muerte), ]$fecha_de_muerte <-  "0001-01-01T00:00:00.000" 

# 3.9. [pertenencia_etnica]: IG NA --> Assign = "Otro"
datosImport[is.na.data.frame(datosImport$pertenencia_etnica), ]$pertenencia_etnica <-  "Otro" 


### 4. WRITE CSV ###
file_name <- "C:/Users/Leonardo REGINO/Documents/DATAMINH/Covid19/Dashboards/Data/Data_Covid19_Colombia.csv"
write.csv(datosImport, file_name, row.names = FALSE  )

### 5. Test file by reading ###
myDataCovid <- read.csv(file = file_name, sep = ",", header = TRUE  )







