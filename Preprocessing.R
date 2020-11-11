
library(shiny)
library(dplyr)
library(jsonlite)
library(tidyverse)
library(ggplot2)
library(EpiEstim)

### 1. Load from web ### 
url <- "https://www.datos.gov.co/resource/gt2j-8ykr.json?$limit="
nb_limit <- "1500000"
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
col_NA$Perc_NA <- as.numeric(col_NA$Perc_NA) 

### 2.2 Plot Percentage of missing values in columns ###
q <- ggplot(data = col_NA, aes(x = Variable, y = Perc_NA) ) +
  geom_bar(stat="identity", fill = "blue") +
  geom_text(aes(label=Perc_NA), vjust = 1.6, color = "black", size = 3) +
  labs(title = "Percentage of NA per variable")

q

### 3. TREAT THE MISSING VALUES ###


# 3.1 [fecha_diagnostico]. If NA --> Assign = fecha_reporte_web
datosImport[is.na.data.frame(datosImport$fecha_diagnostico), ]$fecha_diagnostico <- datosImport[is.na.data.frame(datosImport$fecha_diagnostico), ]$fecha_reporte_web

# 3.2 [fecha_inicio_sintomas]. If NA --> ASINTOMATICO (create Variable: TIPO -> ("Sintomatico","Asintimatico"))


# 3.3 [fecha_muerte]. If NA --> NO FALLECIDO. Nothing to be done.


# 3.4 [fecha_recuperado]. If NA --> FALLECIDO o ABIERTO. Nothing to be done.


# 3.5 [nom_grupo_]. 98.5% NA --> Drop column.
datosImport <- select(datosImport, -nom_grupo_)

# 3.6 [pais_viajo_1_cod]. 99.9% NA --> Drop column. Info in fuente_tipo_contagio = IMPORTADO.
datosImport <- select(datosImport, -pais_viajo_1_cod)

# 3.7 [pais_viajo_1_nom]. 99.9% NA --> Drop column. Info in fuente_tipo_contagio = IMPORTADO.
datosImport <- select(datosImport, -pais_viajo_1_nom)

# 3.8 [per_etn_]. IF NA --> Assig: 0
datosImport[is.na.data.frame(datosImport$per_etn_),]$per_etn_ <- "0"

# 3.9 [recuperado]. IF NA --> If fecha_muerte IS NOT NULL --> Assign: FALLECIDO 


# 3.10 [tipo_recuperacion]. IF NA --> Not recovered yet. Assig -> "ACTIVO O FALLECIDO" 
datosImport[is.na.data.frame(datosImport$tipo_recuperacion),]$tipo_recuperacion <- "ACTIVO O FALLECIDO"



### 4. WRITE CSV ###
file_name <- "C:/Users/Leonardo REGINO/Documents/DATAMINH/Covid19/Dashboards/Data/Data_Covid19_Colombia.csv"
write.csv(datosImport, file_name, row.names = FALSE  )

### 5. Test file by reading ###
myDataCovid <- read.csv(file = file_name, sep = ",", header = TRUE  )







