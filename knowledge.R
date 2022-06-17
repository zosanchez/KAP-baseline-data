
#KAP 


library (dplyr)
library(tidyverse)
library(lubridate)
library(tidyr)

library(readr)
data<-read.csv("~/Immune/Data/original_data/immune_survey_23-05-2022.csv", header=TRUE, sep=";")

immune_survey_23_05_2022 <- read_delim("~/Immune/Data/original_data/immune_survey_23-05-2022.csv", 
                                       delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(immune_survey_23_05_2022)

#use thid to read
data<-read.csv("~/Immune/Data/original_data/immune_survey_23-05-2022.csv", header=TRUE, sep=";")

#Limpiar
#ID 63: Unicode registrado como 1.1.69.16 debe ser cambiado a 1.13.69.16
data$UNICODE[data$ID==63]<- "1.13.69.16"
#ID 83: Eliminar registro, est? duplicado. 
data <- data[which(data$ID!=83),]
#ID 84: Cambiar unicode 1.1.13.1409 por 1.13.51.1409
data$UNICODE[data$ID==84]<- "1.13.51.1409"
#ID 60: Cambiar unicode 1.1.51.1450 por 1.13.51.1450
data$UNICODE[data$ID==60]<- "1.13.51.1450"
#ID 58: cambiar unicode 1.1.51.1439 por 1.13.51.1439
data$UNICODE[data$ID==58]<- "1.13.51.1439"
#ID 55: cambiar UNICODE de1.1.509.93 a 1.13.50.93
data$UNICODE[data$ID==55]<- "1.13.50.93"
#Id 56: Eliminar registro, es duplicado de 55
data <- data[which(data$ID!=56),]
#ID 53 Cambiar unicode 1.1.51.1489 por 1.13.51.1489
data$UNICODE[data$ID==53]<- "1.13.51.1489"
#ID 48 Cambiar unicode 1.1.51.1705 por 1.13.51.1705
data$UNICODE[data$ID==48]<- "1.13.51.1705"
#ID 43 Cambiar unicode 1.1.51.1831 por 1.13.51.1831
data$UNICODE[data$ID==43]<- "1.13.51.1831"
#ID 47 Cambiar unicode 1.1.50.4 por 1.13.50.4
data$UNICODE[data$ID==47]<- "1.13.50.4"
#ID 49 Cambiar unicode 1.1.50.50 por 1.13.50.50
data$UNICODE[data$ID==49]<- "1.13.50.50"
#ID 56 Cambiar unicode 1.1.509.93 por 1.13.509.93
data$UNICODE[data$ID==56]<- "1.13.509.93"
#ID 61 Cambiar unicode 1.1.50.104 por 1.13.50.104
data$UNICODE[data$ID==61]<- "1.13.50.104"
#ID 62 Cambiar unicode 1.1.50.108A por 1.13.50.108A
data$UNICODE[data$ID==62]<- "1.13.50.108A"
#ID 67 Cambiar unicode  1.1.50.50A por 1.13.50.50A
data$UNICODE[data$ID==67]<- "1.13.50.50A"
#ID 75 Cambiar unicode  1.1.50.174 por 1.13.50.174
data$UNICODE[data$ID==75]<- "1.13.50.174"
#ID 85 Cambiar unicode 1.1.50.243 por 1.13.50.243
data$UNICODE[data$ID==85]<- "1.13.50.243"
#ID 51 cambiar unicode  1.1.50.946 por 1.13.50.946
data$UNICODE[data$ID==51]<- "1.13.50.946"
#ID 68 Cambiar unicode 1.1.13.50 por 1.13.50.939; cambiar edad de 939 a "52"
data$UNICODE[data$ID==68]<- "1.13.50.939"
data$EDAD[data$ID==68]<- "52"
#ID 63 Cambiar unicode 1.1.69.116 por 1.13.69.116"
data$UNICODE[data$ID==63]<- "1.13.69.116"
#ID 87 Cambiar unicode 1.1.48.29 por 1.13.48.29
data$UNICODE[data$ID==87]<- "1.13.48.29"
#ID 51 Cambiar unicode 1.1.50.946 por 1.13.50.946
data$UNICODE[data$ID==51]<- "1.13.50.946"
#ID 71 Cambiar unicode 1.1.1.190 por 
data$UNICODE[data$ID==71]<- "1.13.1.190"
#ID 82 Cambiar unicode 1.1.1.237 por 1.13.1.237
data$UNICODE[data$ID==82]<- "1.13.1.237"
#ID 46 Cambiar unicode 1.1.1.312 por 1.13.1.312
data$UNICODE[data$ID==46]<- "1.13.1.312"
#ID 52 Cambiar unicode 1.1.1.322 por 1.13.1.322
data$UNICODE[data$ID==52]<- "1.13.1.322"
#ID 66 Cambiar unicode 1.1.2.129 por 1.13.2.129
data$UNICODE[data$ID==66]<- "1.13.2.129"
#ID 102 Cambiar unicode de 1.13.52.784 a 1.13.51.784
data$UNICODE[data$ID==102]<- "1.13.51.784"
#ID 218 Cambiar unicode 1.25.1.241 a 1.25.1.247
data$UNICODE[data$ID==218]<- "1.25.1.247"
#ID 187 y 188 Eliminar, son duplicado de 186
data <- data[which(data$ID!=187),]
data <- data[which(data$ID!=188),]
#ID 206 Cambiar unicode de 1.1.12.25 a 1.25.12.25
data$UNICODE[data$ID==206]<- "1.25.12.25"
#ID 222 Cambiar edad de 164 a 64
data$EDAD[data$ID==222]<- "64"
#ID 277 Cambiar UNICODE de 1.25.14.14  a  1.25.14.137 y cambiar edad de 137 a 60 a?os
data$UNICODE[data$ID==277]<- "1.25.14.137"
data$EDAD[data$ID==277]<- "60"
#ID 285, 286, 287, 288, 289, 290, 291, 292, 293 Eliminar registros porque son duplicados del ID 284
data <- data[which(data$ID!=285),]
data <- data[which(data$ID!=286),]
data <- data[which(data$ID!=287),]
data <- data[which(data$ID!=288),]
data <- data[which(data$ID!=289),]
data <- data[which(data$ID!=290),]
data <- data[which(data$ID!=291),]
data <- data[which(data$ID!=292),]
data <- data[which(data$ID!=293),]
#ID 259 Cambiar unicode 1.25.32.79 por 1.25.32.70
data$UNICODE[data$ID==259]<- "1.25.32.70"
#ID 336 Cambiar UNICODE de 1.11.14.81 a 1.11.14 381
data$UNICODE[data$ID==336]<- "1.11.14 381"
#ID 374 Eliminar registro, est? duplicado con 373
data <- data[which(data$ID!=374),]
#ID 363 cambiar unicode de 1.25.35.88 a 1.25.33.88
data$UNICODE[data$ID==363]<- "1.25.33.88"
#ID 404 Cambiar unicode de 1.8.5.79 a 1.8.5.70
data$UNICODE[data$ID==404]<- "1.8.5.70"
#ID 532 Cambiar UNICODE de 1.3.7.20 a 1.7.32.20
data$UNICODE[data$ID==532]<- "1.7.32.20"
#ID 556 Cambiar UNICODE de 1.7.33.33 a 1.7.33.35
data$UNICODE[data$ID==556]<- "1.7.33.35"
#ID 670 cambiar UNICODE de 1.3.88.159 A 1.3.89.159
data$UNICODE[data$ID==670]<- "1.3.89.159"
#ID 336 cambiar UNICODE de 1.11.14.81 A 1.11.14.381
data$UNICODE[data$ID==336]<- "1.11.14.381"
#ID 330 Cambiar unicode 1.25.34.219 por 1.25.33.21
data$UNICODE[data$ID==330]<- "1.25.33.21"
#ID 556 Cambiar unicode 1.7.33.33 por 1.7.35.33
data$UNICODE[data$ID==556]<- "1.7.35.33"
#ID 592 Eliminar registro, est? duplicado con 591
data <- data[which(data$ID!=592),]
#ID 575 Eliminar registro, est? duplicado con 574
data <- data[which(data$ID!=575),]
#ID 625 Eliminar registro, est? duplicado con 624
data <- data[which(data$ID!=624),]





#to create a new data base just with the variables you are going to use
#df<- select(data, CUALES_PLACA_SON_CHIRI)

#to extract one answer/word from the column for vectors
data<- data%>%mutate(NS_NR=grepl("NS_NR", CUALES_PLACA_SON_CHIRI)*1)
data<- data%>%mutate(CHIRI_ADULTA=grepl("chiri_adulta", CUALES_PLACA_SON_CHIRI)*1)
data<- data%>%mutate(CHIRI_NIFA_3=grepl("chiri_nifa_3", CUALES_PLACA_SON_CHIRI)*1)
data<- data%>%mutate(CHIRI_NIFA_4_fl=grepl("chiri_nifa_4_fl", CUALES_PLACA_SON_CHIRI)*1)
data<- data%>%mutate(CHIRI_NINFA_4_alim=grepl("chiri_ninfa_4_alim", CUALES_PLACA_SON_CHIRI)*1)
data<- data%>%mutate(COREIDO=grepl("coreido", CUALES_PLACA_SON_CHIRI)*1)
data<- data%>%mutate(CHINCHE_CAMA=grepl("chinche_cama", CUALES_PLACA_SON_CHIRI)*1)
data<- data%>%mutate(CHINCHE_HEDIONDA=grepl("chinche_hedionda", CUALES_PLACA_SON_CHIRI)*1)
data<- data%>%mutate(CUCARACHA=grepl("cucaracha", CUALES_PLACA_SON_CHIRI)*1)
data<- data%>%mutate(CHINCHE_CINTURON_AMARILLO=grepl("chinche_cinturon_amarillo", CUALES_PLACA_SON_CHIRI)*1)
data<- data%>%mutate(ESCARBAJO=grepl("escarabajo", CUALES_PLACA_SON_CHIRI)*1)
data<- data%>%mutate(GORGOJO=grepl("gorgojo", CUALES_PLACA_SON_CHIRI)*1)
data<- data%>%mutate(MOSCA=grepl("mosca", CUALES_PLACA_SON_CHIRI)*1)

#to change 1 to NS_NR for vectors
data$CHIRI_ADULTA[data$NS_NR==1]<- "NS_NR"
data$CHIRI_NIFA_3[data$NS_NR==1]<- "NS_NR"
data$CHIRI_NIFA_4_fl[data$NS_NR==1]<- "NS_NR"
data$CHIRI_NINFA_4_alim[data$NS_NR==1]<- "NS_NR"
data$COREIDO[data$NS_NR==1]<- "NS_NR"
data$CHINCHE_CAMA[data$NS_NR==1]<- "NS_NR"
data$CHINCHE_HEDIONDA[data$NS_NR==1]<- "NS_NR"
data$CUCARACHA[data$NS_NR==1]<- "NS_NR"
data$CHINCHE_CINTURON_AMARILLO[data$NS_NR==1]<- "NS_NR"
data$ESCARBAJO[data$NS_NR==1]<- "NS_NR"
data$GORGOJO[data$NS_NR==1]<- "NS_NR"
data$MOSCA[data$NS_NR==1]<- "NS_NR"

#to change NS_NR to 10 for all possible vector columns and then change to numeric for scores
data["CHIRI_ADULTA"][data["CHIRI_ADULTA"]=="NS_NR"]<-"10"
data$CHIRI_ADULTA<-as.numeric(data$CHIRI_ADULTA)
data["CHIRI_NIFA_3"][data["CHIRI_NIFA_3"]=="NS_NR"]<-"10"
data$CHIRI_NIFA_3<-as.numeric(data$CHIRI_NIFA_3)
data["CHIRI_NIFA_4_fl"][data["CHIRI_NIFA_4_fl"]=="NS_NR"]<-"10"
data$CHIRI_NIFA_4_fl<-as.numeric(data$CHIRI_NIFA_4_fl)
data["CHIRI_NINFA_4_alim"][data["CHIRI_NINFA_4_alim"]=="NS_NR"]<-"10"
data$CHIRI_NINFA_4_alim<-as.numeric(data$CHIRI_NINFA_4_alim)
data["COREIDO"][data["COREIDO"]=="NS_NR"]<-"10"
data$COREIDO<-as.numeric(data$COREIDO)
data["CHINCHE_CAMA"][data["CHINCHE_CAMA"]=="NS_NR"]<-"10"
data$CHINCHE_CAMA<-as.numeric(data$CHINCHE_CAMA)
data["CHINCHE_HEDIONDA"][data["CHINCHE_HEDIONDA"]=="NS_NR"]<-"10"
data$CHINCHE_HEDIONDA<-as.numeric(data$CHINCHE_HEDIONDA)
data["CUCARACHA"][data["CUCARACHA"]=="NS_NR"]<-"10"
data$CUCARACHA<-as.numeric(data$CUCARACHA)
data["CHINCHE_CINTURON_AMARILLO"][data["CHINCHE_CINTURON_AMARILLO"]=="NS_NR"]<-"10"
data$CHINCHE_CINTURON_AMARILLO<-as.numeric(data$CHINCHE_CINTURON_AMARILLO)
data["ESCARBAJO"][data["ESCARBAJO"]=="NS_NR"]<-"10"
data$ESCARBAJO<-as.numeric(data$ESCARBAJO)
data["GORGOJO"][data["GORGOJO"]=="NS_NR"]<-"10"
data$GORGOJO<-as.numeric(data$GORGOJO)
data["MOSCA"][data["MOSCA"]=="NS_NR"]<-"10"
data$MOSCA<-as.numeric(data$MOSCA)

#create column that sums all rows from vector columns
data$sum_vector_ID=rowSums(data[,c("CHIRI_ADULTA","CHIRI_NIFA_3","CHIRI_NIFA_4_fl", "CHIRI_NINFA_4_alim","COREIDO","CHINCHE_CAMA","CHINCHE_HEDIONDA","CUCARACHA", "CHINCHE_CINTURON_AMARILLO","ESCARBAJO", "GORGOJO", "MOSCA")])


#create column that sums knowledge for vectors (score without subtraction)
data$sum_correct_vector=rowSums(data[,c("CHIRI_ADULTA","CHIRI_NIFA_3","CHIRI_NIFA_4_fl", "CHIRI_NINFA_4_alim")])
#create column that sums incorrect vector choices
data$sum_incorrect_vector=rowSums(data[,c("COREIDO","CHINCHE_CAMA","CHINCHE_HEDIONDA","CUCARACHA", "CHINCHE_CINTURON_AMARILLO","ESCARBAJO", "GORGOJO", "MOSCA")])
#create column with correct - incorrect scores
data$score_vectors<- (data$sum_correct_vector - data$sum_incorrect_vector)
#turn -40 into NA in vector score column
data["score_vectors"][data["score_vectors"]=="-40"]<-"NA"
data$score_vectors<- as.numeric(data$score_vectors)
#add 10 to each value so you can do mean, etc


#same as above, but for signs
data$sum_correct_sign=rowSums(data[,c("CHIRI_FECAL_TRAILS", "CHIRI_EGGS", "CHIRI_FECAL_TRAIL")])
data$sum_incorrect_sign=rowSums(data[,c("CHINCHE_CAMA_FECES", "MOUSE_FECES", "MOLD")])
data$score_signs <- (data$sum_correct_sign - data$sum_incorrect_sign)
#turn 0s into NS_NR in sign score column
data["score_signs"][data["score_signs"]=="0"]<-"NS_NR"
data["score_signs"][data["score_signs"]=="NS_NR"]<- "NA"
data$score_signs<-as.numeric(data$score_signs)

# for chiri adulta total
table (data$CHIRI_ADULTA)

#for chiri nifa 3 total
table (data$CHIRI_NIFA_3)

#for chiri nifa 4 fl total
table (data$CHIRI_NIFA_4_fl)

#for chiri nifa 4 alim total
table (data$CHIRI_NIFA_4_alim)

#for coriedo total
table (data$COREIDO)

#for chinche de cama total
table (data$CHINCHE_CAMA)

#for chinche hedionda total
table (data$CHINCHE_HEDIONDA)

#for cucaracha total
table (data$CUCARACHA)

#for chinche cinturon amarillo total
table (data$CHINCHE_CINTURON_AMARILLO)

#for escarbajo total
table (data$ESCARBAJO)

#for gorgojo total
table (data$GORGOJO)

#for mosca total
table (data$MOSCA)

#for NS_NR total
table(data$NS_NR)

#to extract one answer/word from the column for signs
data<- data%>%mutate(NS_NR_SENIALES=grepl("NS_NR", SABES_SENIALES_CHIRI)*1)
data<- data%>%mutate(CHIRI_FECAL_TRAILS=grepl("a", SABES_SENIALES_CHIRI)*1)
data<- data%>%mutate(CHINCHE_CAMA_FECES=grepl("b", SABES_SENIALES_CHIRI)*1)
data<- data%>%mutate(MOUSE_FECES=grepl("c", SABES_SENIALES_CHIRI)*1)
data<- data%>%mutate(MOLD=grepl("d", SABES_SENIALES_CHIRI)*1)
data<- data%>%mutate(CHIRI_EGGS=grepl("e", SABES_SENIALES_CHIRI)*1)
data<- data%>%mutate(CHIRI_FECAL_TRAIL=grepl("f", SABES_SENIALES_CHIRI)*1)

#to change 1 to NS_NR for signs
data$CHINCHE_CAMA_FECES[data$NS_NR_SENIALES==1]<- "NS_NR"
data$MOUSE_FECES[data$NS_NR_SENIALES==1]<- "NS_NR"
data$MOLD[data$NS_NR_SENIALES==1]<- "NS_NR"
data$CHIRI_EGGS[data$NS_NR_SENIALES==1]<- "NS_NR"
data$CHIRI_FECAL_TRAIL[data$NS_NR_SENIALES==1]<- "NS_NR"
data$CHIRI_FECAL_TRAILS[data$NS_NR_SENIALES==1]<- "NS_NR"

#to change NS_NR to 10 for all possible signs columns and then change to numeric for scores
data["CHIRI_FECAL_TRAILS"][data["CHIRI_FECAL_TRAILS"]=="NS_NR"]<-"10"
data$CHIRI_FECAL_TRAILS<-as.numeric(data$CHIRI_FECAL_TRAILS)
data["CHINCHE_CAMA_FECES"][data["CHINCHE_CAMA_FECES"]=="NS_NR"]<-"10"
data$CHINCHE_CAMA_FECES<-as.numeric(data$CHINCHE_CAMA_FECES)
data["MOUSE_FECES"][data["MOUSE_FECES"]=="NS_NR"]<-"10"
data$MOUSE_FECES<-as.numeric(data$MOUSE_FECES)
data["MOLD"][data["MOLD"]=="NS_NR"]<-"10"
data$MOLD<-as.numeric(data$MOLD)
data["CHIRI_EGGS"][data["CHIRI_EGGS"]=="NS_NR"]<-"10"
data$CHIRI_EGGS<-as.numeric(data$CHIRI_EGGS)
data["CHIRI_FECAL_TRAIL"][data["CHIRI_FECAL_TRAIL"]=="NS_NR"]<-"10"
data$CHIRI_FECAL_TRAIL<-as.numeric(data$CHIRI_FECAL_TRAIL)

#create column that sums all rows from vector columns
data$sum_sign_ID=rowSums(data[,c("CHIRI_FECAL_TRAILS","CHINCHE_CAMA_FECES","MOUSE_FECES", "MOLD","CHIRI_EGGS","CHIRI_FECAL_TRAIL")])


#for NS_NR_SENIALES total
table(data$NS_NR_SENIALES)

#for chiri fecal trails total
table(data$CHIRI_FECAL_TRAILS)

#for chinche de cama feces total
table(data$CHINCHE_CAMA_FECES)

#for mouse feces total
table(data$MOUSE_FECES)

#for mold total
table(data$MOLD)

#for chiri eggs total
table(data$CHIRI_EGGS)

#for chiri fecal trail total
table(data$CHIRI_FECAL_TRAIL)
