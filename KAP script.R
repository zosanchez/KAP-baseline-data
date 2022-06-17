#KAP 


library (dplyr)
library(tidyverse)
library(lubridate)
library(tidyr)

library(readr)
#use this to read
data <- read.csv("KAP-baseline-data/immune_survey_23-05-2022.csv", header=TRUE, sep=";")
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
#ID 262 probablemente hubo un error con el app en la pregunta sobre que pasa cuando reportan chirimachas, a pesar de que se selecciono fumigan, aparece una respuesta en otros, por lo cual se va a dejar esa respuesta como NA
data$SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA_OTRO[data$ID==262]<- NA

#change age from chr to numeric
data$EDAD<- as.numeric(data$EDAD)

#making age groups
#age1<- filter(data, EDAD <= 45)
data<-mutate(data, AGECLASS= ifelse(EDAD<=31,"18-31",
                                    ifelse(EDAD<=45,"32-45",
                                           ifelse(EDAD<=59, "46-59",
                                                  ifelse(EDAD<=100, "60-100", "0")))))


#combine jobs and other jobs to one column
data<-mutate(data, OCCUPATIONS=ifelse(ACT_ECONOMICA == "ama_casa", "ama_casa",
                                      ifelse(ACT_ECONOMICA == "estudiante", "estudiante",
                                             ifelse(ACT_ECONOMICA == "jubilado" | ACT_ECONOMICA == "otro" & ACT_ECONOMICA_OTRO == "Militar en retiro", "jubilado",
                                                    ifelse(ACT_ECONOMICA == "sin_empleo", "sin_empleo",
                                                           ifelse(ACT_ECONOMICA == "trabajador_independiente" | ACT_ECONOMICA == "otro" & ACT_ECONOMICA_OTRO =="Directora de I.E(vivienda es I.E Inicial)" | ACT_ECONOMICA == "otro" & ACT_ECONOMICA_OTRO =="Docente pronoi local publico" | ACT_ECONOMICA == "otro" & ACT_ECONOMICA_OTRO =="que mas pue" | ACT_ECONOMICA == "otro" & ACT_ECONOMICA_OTRO =="Vivienda es tienda" | ACT_ECONOMICA == "otro" & ACT_ECONOMICA_OTRO =="Vivienda es Tienda", "trabajador_independiente",
                                                                  ifelse(ACT_ECONOMICA == "trabajador_planilla", "trabajador_planilla", 0)))))))

#to make column for "other" answers to question about transmission
#"crecimiento de organos" and "enfermedad del corazón/pecho" could = Chagas 
data<-mutate(data, OTHER_DISEASES=ifelse(QUE_ENFERMEDADES_TRANSMITE_CHIRI_OTRO == "No sé acuerda" | QUE_ENFERMEDADES_TRANSMITE_CHIRI_OTRO =="No recuerda el nombre" | QUE_ENFERMEDADES_TRANSMITE_CHIRI_OTRO =="No se acuerda" | QUE_ENFERMEDADES_TRANSMITE_CHIRI_OTRO =="No se acuerda " | QUE_ENFERMEDADES_TRANSMITE_CHIRI_OTRO =="No se acuerda el nombre" | QUE_ENFERMEDADES_TRANSMITE_CHIRI_OTRO =="No se acuerda. " | QUE_ENFERMEDADES_TRANSMITE_CHIRI_OTRO =="No se acuerdan", "No se acuerda",
                                         ifelse(QUE_ENFERMEDADES_TRANSMITE_CHIRI_OTRO == "Enfermedad al corazón " | QUE_ENFERMEDADES_TRANSMITE_CHIRI_OTRO =="Enfermedad del corazón " | QUE_ENFERMEDADES_TRANSMITE_CHIRI_OTRO =="Enfermedad del pecho " | QUE_ENFERMEDADES_TRANSMITE_CHIRI_OTRO =="Enfermedad en el corazón " | QUE_ENFERMEDADES_TRANSMITE_CHIRI_OTRO =="Enfermedad en el pecho ", "Enfermedad del corazón/pecho",
                                                ifelse(QUE_ENFERMEDADES_TRANSMITE_CHIRI_OTRO == "Enrojecimiento de la piel" | QUE_ENFERMEDADES_TRANSMITE_CHIRI_OTRO =="Picazon,hace heridas", "Enfermedades de la piel",
                                                       ifelse(QUE_ENFERMEDADES_TRANSMITE_CHIRI_OTRO == "Fiebre" | QUE_ENFERMEDADES_TRANSMITE_CHIRI_OTRO =="Fiebrw", "Solo fiebre",
                                                              ifelse(QUE_ENFERMEDADES_TRANSMITE_CHIRI_OTRO == "Fiebre ,diarrea" | QUE_ENFERMEDADES_TRANSMITE_CHIRI_OTRO =="Fiebre,malestar", "Fiebre y otro",
                                                                     ifelse(QUE_ENFERMEDADES_TRANSMITE_CHIRI_OTRO == "Diarrea" | QUE_ENFERMEDADES_TRANSMITE_CHIRI_OTRO =="Indigestion", "Enfermedad del estomago",
                                                                            ifelse(QUE_ENFERMEDADES_TRANSMITE_CHIRI_OTRO == "Crecimiento de organos" | QUE_ENFERMEDADES_TRANSMITE_CHIRI_OTRO =="Crecimiento del estomago", "Crecimiento de organos",
                                                                                   ifelse(QUE_ENFERMEDADES_TRANSMITE_CHIRI_OTRO == "Alergia", "Alergia",
                                                                                          ifelse(QUE_ENFERMEDADES_TRANSMITE_CHIRI_OTRO == "Dengue", "Dengue",
                                                                                                 ifelse(QUE_ENFERMEDADES_TRANSMITE_CHIRI_OTRO == "Infección ", "Infección",
                                                                                                        ifelse(QUE_ENFERMEDADES_TRANSMITE_CHIRI_OTRO == "Lesmidiasis", "Lesmidiasis",
                                                                                                               ifelse(QUE_ENFERMEDADES_TRANSMITE_CHIRI_OTRO == "Paludismo", "Paludismo",
                                                                                                                      ifelse(QUE_ENFERMEDADES_TRANSMITE_CHIRI_OTRO == "Toxoplasma", "Toxoplasma",
                                                                                                                             ifelse(QUE_ENFERMEDADES_TRANSMITE_CHIRI_OTRO == "Tuberculosis", "Tuberculosis",
                                                                                                                                    ifelse(QUE_ENFERMEDADES_TRANSMITE_CHIRI_OTRO == "Enfermedad peligrosa ", "Enfermedad peligrosa", 0))))))))))))))))




#combine knowledge that chiris transmit, what disease, and other in one column
data<- mutate(data, KNOWLEDGE_OF_DISEASE_TRANSMISSION=ifelse(CHIRI_TRANSMITIR_ENFERMEDAD == "no", "no",
                                                             ifelse(CHIRI_TRANSMITIR_ENFERMEDAD == "NS_NR", "NS_NR",
                                                                    ifelse(CHIRI_TRANSMITIR_ENFERMEDAD == "si" & QUE_ENFERMEDADES_TRANSMITE_CHIRI == "chagas", "Chagas",
                                                                           ifelse(CHIRI_TRANSMITIR_ENFERMEDAD == "si" & QUE_ENFERMEDADES_TRANSMITE_CHIRI == "NS_NR", "Si, pero no sabe/responde", 
                                                                                  ifelse(CHIRI_TRANSMITIR_ENFERMEDAD == "si" & QUE_ENFERMEDADES_TRANSMITE_CHIRI == "otro" & OTHER_DISEASES == "Enfermedad peligrosa", "Enfermedad peligrosa", 
                                                                                         ifelse(CHIRI_TRANSMITIR_ENFERMEDAD == "si" & QUE_ENFERMEDADES_TRANSMITE_CHIRI == "otro" & OTHER_DISEASES == "Tuberculosis", "Tuberculosis", 
                                                                                                ifelse(CHIRI_TRANSMITIR_ENFERMEDAD == "si" & QUE_ENFERMEDADES_TRANSMITE_CHIRI == "otro" & OTHER_DISEASES == "Toxoplasma", "Toxoplasma", 
                                                                                                       ifelse(CHIRI_TRANSMITIR_ENFERMEDAD == "si" & QUE_ENFERMEDADES_TRANSMITE_CHIRI == "otro" & OTHER_DISEASES == "Paludismo", "Paludismo",
                                                                                                              ifelse(CHIRI_TRANSMITIR_ENFERMEDAD == "si" & QUE_ENFERMEDADES_TRANSMITE_CHIRI == "otro" & OTHER_DISEASES == "Lesmidiasis", "Lesmidiasis",
                                                                                                                     ifelse(CHIRI_TRANSMITIR_ENFERMEDAD == "si" & QUE_ENFERMEDADES_TRANSMITE_CHIRI == "otro" & OTHER_DISEASES == "Infección", "Infección",
                                                                                                                            ifelse(CHIRI_TRANSMITIR_ENFERMEDAD == "si" & QUE_ENFERMEDADES_TRANSMITE_CHIRI == "otro" & OTHER_DISEASES == "Dengue", "Dengue",
                                                                                                                                   ifelse(CHIRI_TRANSMITIR_ENFERMEDAD == "si" & QUE_ENFERMEDADES_TRANSMITE_CHIRI == "otro" & OTHER_DISEASES == "Alergia", "Alergia",
                                                                                                                                          ifelse(CHIRI_TRANSMITIR_ENFERMEDAD == "si" & QUE_ENFERMEDADES_TRANSMITE_CHIRI == "otro" & OTHER_DISEASES == "Crecimiento de organos", "Crecimiento de organos",
                                                                                                                                                 ifelse(CHIRI_TRANSMITIR_ENFERMEDAD == "si" & QUE_ENFERMEDADES_TRANSMITE_CHIRI == "otro" & OTHER_DISEASES == "Enfermedad del estomago", "Enfermedad del estomago",
                                                                                                                                                        ifelse(CHIRI_TRANSMITIR_ENFERMEDAD == "si" & QUE_ENFERMEDADES_TRANSMITE_CHIRI == "otro" & OTHER_DISEASES == "Fiebre y otro", "Fiebre y otro",
                                                                                                                                                               ifelse(CHIRI_TRANSMITIR_ENFERMEDAD == "si" & QUE_ENFERMEDADES_TRANSMITE_CHIRI == "otro" & OTHER_DISEASES == "Solo fiebre", "Solo fiebre",
                                                                                                                                                                      ifelse(CHIRI_TRANSMITIR_ENFERMEDAD == "si" & QUE_ENFERMEDADES_TRANSMITE_CHIRI == "otro" & OTHER_DISEASES == "Enfermedades de la piel", "Enfermedades de la piel",
                                                                                                                                                                             ifelse(CHIRI_TRANSMITIR_ENFERMEDAD == "si" & QUE_ENFERMEDADES_TRANSMITE_CHIRI == "otro" & OTHER_DISEASES == "Enfermedad del corazón/pecho", "Enfermedad del corazón/pecho",
                                                                                                                                                                                    ifelse(CHIRI_TRANSMITIR_ENFERMEDAD == "si" & QUE_ENFERMEDADES_TRANSMITE_CHIRI == "otro" & OTHER_DISEASES == "No se acuerda", "Si, pero no se acuerda", 0))))))))))))))))))))



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
data$sum_vector_ID=rowSums(data[,c("CHIRI_ADULTA","CHIRI_NIFA_3","CHIRI_NIFA_4_fl", "CHIRI_NINFA_4_alim","COREIDO","CHINCHE_CAMA","CHINCHE_HEDIONDA","CUCARACHA", "CHINCHE_CINTURON_AMARILLO","ESCARBAJO", "GORGOJO", "MOSCA")])

#create column that sums all rows from vector columns
data$sum_sign_ID=rowSums(data[,c("CHIRI_FECAL_TRAILS","CHINCHE_CAMA_FECES","MOUSE_FECES", "MOLD","CHIRI_EGGS","CHIRI_FECAL_TRAIL")])


#categories for vector ID (sumvectorID up to 5)
data<-mutate(data, vector_cat=ifelse(CHIRI_ADULTA ==1 & sum_vector_ID==1,"solo adulta", 
                                     ifelse(sum_vector_ID==120, "NS_NR",
                                            ifelse(CHIRI_NIFA_3 == 1 & sum_vector_ID==1, "solo ninf3",
                                                   ifelse(CHIRI_NIFA_4_fl == 1 & sum_vector_ID==1, "solo ninf4 fl", 
                                                          ifelse(CHIRI_NINFA_4_alim == 1 & sum_vector_ID==1, "solo ninf4 ali", 
                                                                 ifelse(CHIRI_ADULTA ==1 & (CHIRI_NINFA_4_alim == 1 | CHIRI_NIFA_3 == 1 |CHIRI_NIFA_4_fl == 1) & sum_vector_ID==2,  "adult+1ninf",
                                                                        ifelse(CHIRI_ADULTA ==1 & (CHIRI_NINFA_4_alim == 1 | CHIRI_NIFA_3 == 1 |CHIRI_NIFA_4_fl == 1) & sum_vector_ID==3,  "adult+2ninf",
                                                                               ifelse(CHIRI_ADULTA ==1 & (CHIRI_NINFA_4_alim == 1 | CHIRI_NIFA_3 == 1 |CHIRI_NIFA_4_fl == 1) & sum_vector_ID==4,  "adult+3ninf",
                                                                                      ifelse(CHIRI_ADULTA ==1 & (CHIRI_NINFA_4_alim == 1 | CHIRI_NIFA_3 == 1 |CHIRI_NIFA_4_fl == 1) & sum_vector_ID==5,  "adult+4ninf",
                                                                                             ifelse(CHIRI_ADULTA ==1 & CHIRI_NINFA_4_alim == 0 & CHIRI_NIFA_3 == 0 & CHIRI_NIFA_4_fl == 0 & sum_vector_ID==2,  "adult+1other",
                                                                                                    ifelse(CHIRI_ADULTA ==1 & CHIRI_NINFA_4_alim == 0 & CHIRI_NIFA_3 == 0 & CHIRI_NIFA_4_fl == 0 & sum_vector_ID==3,  "adult+2other",
                                                                                                           ifelse(CHIRI_ADULTA ==1 & CHIRI_NINFA_4_alim == 0 & CHIRI_NIFA_3 == 0 & CHIRI_NIFA_4_fl == 0 & sum_vector_ID==4,  "adult+3other",
                                                                                                                  ifelse(CHIRI_ADULTA ==0 & CHIRI_NINFA_4_alim == 0 & CHIRI_NIFA_3 == 0 & CHIRI_NIFA_4_fl == 0 & (COREIDO == 1 | CHINCHE_CAMA == 1 | CHINCHE_HEDIONDA ==1 | CUCARACHA == 1 |CHINCHE_CINTURON_AMARILLO ==1 | ESCARBAJO == 1 | GORGOJO == 1 | MOSCA ==1) & sum_vector_ID==1,  "incorrect only(1)",
                                                                                                                         ifelse(CHIRI_ADULTA ==0 & CHIRI_NINFA_4_alim == 0 & CHIRI_NIFA_3 == 0 & CHIRI_NIFA_4_fl == 0 & (COREIDO == 1 | CHINCHE_CAMA == 1 | CHINCHE_HEDIONDA ==1 | CUCARACHA == 1 |CHINCHE_CINTURON_AMARILLO ==1 | ESCARBAJO == 1 | GORGOJO == 1 | MOSCA ==1) & sum_vector_ID==2,  "incorrect only(2)",
                                                                                                                                ifelse(CHIRI_ADULTA ==0 & CHIRI_NINFA_4_alim == 0 & CHIRI_NIFA_3 == 0 & CHIRI_NIFA_4_fl == 0 & (COREIDO == 1 | CHINCHE_CAMA == 1 | CHINCHE_HEDIONDA ==1 | CUCARACHA == 1 |CHINCHE_CINTURON_AMARILLO ==1 | ESCARBAJO == 1 | GORGOJO == 1 | MOSCA ==1) & sum_vector_ID==3,  "incorrect only(3)",
                                                                                                                                       ifelse(CHIRI_ADULTA ==0 & CHIRI_NINFA_4_alim == 0 & CHIRI_NIFA_3 == 0 & CHIRI_NIFA_4_fl == 0 & (COREIDO == 1 | CHINCHE_CAMA == 1 | CHINCHE_HEDIONDA ==1 | CUCARACHA == 1 |CHINCHE_CINTURON_AMARILLO ==1 | ESCARBAJO == 1 | GORGOJO == 1 | MOSCA ==1) & sum_vector_ID==4,  "incorrect only(4)",
                                                                                                                                              ifelse(CHIRI_ADULTA ==0 & (CHIRI_NINFA_4_alim == 1 | CHIRI_NIFA_3 == 1 |CHIRI_NIFA_4_fl == 1) & sum_vector_ID==2,  "solo 2ninf",
                                                                                                                                                     ifelse(CHIRI_ADULTA ==0 & (CHIRI_NINFA_4_alim == 1 | CHIRI_NIFA_3 == 1 |CHIRI_NIFA_4_fl == 1) & sum_vector_ID==3,  "solo 3ninf",
                                                                                                                                                            ifelse(CHIRI_ADULTA ==0 & CHIRI_NINFA_4_alim == 1 & CHIRI_NIFA_3 == 0 & CHIRI_NIFA_4_fl == 0 & sum_vector_ID==2,  "ninf4ali+1other",
                                                                                                                                                                   ifelse(CHIRI_ADULTA ==0 & CHIRI_NINFA_4_alim == 1 & CHIRI_NIFA_3 == 0 & CHIRI_NIFA_4_fl == 0 & sum_vector_ID==3,  "ninf4ali+2other",
                                                                                                                                                                          ifelse(CHIRI_ADULTA ==0 & CHIRI_NINFA_4_alim == 1 & CHIRI_NIFA_3 == 0 & CHIRI_NIFA_4_fl == 0 & sum_vector_ID==4,  "ninf4ali+3other",
                                                                                                                                                                                 ifelse(CHIRI_ADULTA ==0 & CHIRI_NINFA_4_alim == 1 & CHIRI_NIFA_3 == 0 & CHIRI_NIFA_4_fl == 0 & sum_vector_ID==5,  "ninf4ali+4other",
                                                                                                                                                                                        ifelse(CHIRI_ADULTA ==0 & CHIRI_NINFA_4_alim == 0 & CHIRI_NIFA_3 == 1 & CHIRI_NIFA_4_fl == 0 & sum_vector_ID==2,  "ninf3+1other",
                                                                                                                                                                                               ifelse(CHIRI_ADULTA ==0 & CHIRI_NINFA_4_alim == 0 & CHIRI_NIFA_3 == 1 & CHIRI_NIFA_4_fl == 0 & sum_vector_ID==3,  "ninf3+2other",
                                                                                                                                                                                                      ifelse(CHIRI_ADULTA ==0 & CHIRI_NINFA_4_alim == 0 & CHIRI_NIFA_3 == 1 & CHIRI_NIFA_4_fl == 0 & sum_vector_ID==4,  "ninf3+3other",
                                                                                                                                                                                                             ifelse(CHIRI_ADULTA ==0 & CHIRI_NINFA_4_alim == 0 & CHIRI_NIFA_3 == 1 & CHIRI_NIFA_4_fl == 0 & sum_vector_ID==5,  "ninf3+4other",
                                                                                                                                                                                                                    ifelse(CHIRI_ADULTA ==0 & CHIRI_NINFA_4_alim == 0 & CHIRI_NIFA_3 == 0 & CHIRI_NIFA_4_fl == 1 & sum_vector_ID==2,  "ninf4fl+1other",
                                                                                                                                                                                                                           ifelse(CHIRI_ADULTA ==0 & CHIRI_NINFA_4_alim == 0 & CHIRI_NIFA_3 == 0 & CHIRI_NIFA_4_fl == 1 & sum_vector_ID==3,  "ninf4fl+2other",
                                                                                                                                                                                                                                  ifelse(CHIRI_ADULTA ==0 & CHIRI_NINFA_4_alim == 0 & CHIRI_NIFA_3 == 0 & CHIRI_NIFA_4_fl == 1 & sum_vector_ID==4,  "ninf4fl+3other",
                                                                                                                                                                                                                                         ifelse(CHIRI_ADULTA ==0 & CHIRI_NINFA_4_alim == 0 & CHIRI_NIFA_3 == 1 & CHIRI_NIFA_4_fl == 1 & sum_vector_ID==4,  "ninf4fl+ninf3=2other",
                                                                                                                                                                                                                                                ifelse(CHIRI_ADULTA ==0 & CHIRI_NINFA_4_alim == 0 & CHIRI_NIFA_3 == 0 & CHIRI_NIFA_4_fl == 1 & sum_vector_ID==5,  "ninf4fl+4other",0))))))))))))))))))))))))))))))))



#categories for sign IDs (sumsignID up to 3)
data<- mutate(data, sign_cat=ifelse(CHIRI_FECAL_TRAILS== 1 & sum_sign_ID== 1, "solo fecal trails",
                                    ifelse(sum_sign_ID==60, "NS_NR",
                                           ifelse(CHIRI_FECAL_TRAILS==1 & (CHIRI_EGGS==1 | CHIRI_FECAL_TRAIL==1)& sum_sign_ID==2, "fecaltrails+1sign",
                                                  ifelse(CHIRI_FECAL_TRAILS==1 & CHIRI_EGGS==1 & CHIRI_FECAL_TRAIL==1 & sum_sign_ID==3, "all3signs",
                                                         ifelse(CHINCHE_CAMA_FECES==1 | MOUSE_FECES==1 | MOLD==1 & sum_sign_ID==1, "onlyincorrect(1)",
                                                                ifelse(CHINCHE_CAMA_FECES==1 | MOUSE_FECES==1 | MOLD==1 & sum_sign_ID==2, "onlyincorrect(2)",
                                                                       ifelse(CHINCHE_CAMA_FECES==1 | MOUSE_FECES==1 | MOLD==1 & sum_sign_ID==3, "onlyincorrect(3)",
                                                                              ifelse(CHIRI_FECAL_TRAILS==1 & (CHINCHE_CAMA_FECES==1 | MOUSE_FECES==1 | MOLD==1) & sum_sign_ID==2, "fecaltrails+1other",
                                                                                     ifelse(CHIRI_FECAL_TRAILS==1 & (CHINCHE_CAMA_FECES==1 | MOUSE_FECES==1 | MOLD==1) & sum_sign_ID==3, "fecaltrails+2other",
                                                                                            ifelse(CHIRI_EGGS==1 &(CHINCHE_CAMA_FECES==1 | MOUSE_FECES==1 | MOLD==1) & sum_sign_ID==2, "eggs+1other",
                                                                                                   ifelse(CHIRI_EGGS==1 &(CHINCHE_CAMA_FECES==1 | MOUSE_FECES==1 | MOLD==1) & sum_sign_ID==3, "eggs+2other",
                                                                                                          ifelse(CHIRI_FECAL_TRAIL==1 & (CHINCHE_CAMA_FECES==1 | MOUSE_FECES==1 | MOLD==1) & sum_sign_ID==2, "fecaltrail+1other",
                                                                                                                 ifelse(CHIRI_FECAL_TRAIL==1 & (CHINCHE_CAMA_FECES==1 | MOUSE_FECES==1 | MOLD==1) & sum_sign_ID==3, "fecaltrail+2other",
                                                                                                                        ifelse(CHIRI_EGGS==1 & sum_sign_ID==1, "solo eggs",
                                                                                                                               ifelse(CHIRI_FECAL_TRAIL==1 & sum_sign_ID==1, "solo fecal trail", 
                                                                                                                                      ifelse(CHIRI_EGGS==1 & CHIRI_FECAL_TRAIL==1 &sum_sign_ID==2, "fecaltrail+eggs", 0)))))))))))))))))                                                                                                                                                                                     





#create column that sums knowledge for vectors (score without subtraction)
data$sum_correct_vector=rowSums(data[,c("CHIRI_ADULTA","CHIRI_NIFA_3","CHIRI_NIFA_4_fl", "CHIRI_NINFA_4_alim")])
#create column that sums incorrect vector choices
data$sum_incorrect_vector=rowSums(data[,c("COREIDO","CHINCHE_CAMA","CHINCHE_HEDIONDA","CUCARACHA", "CHINCHE_CINTURON_AMARILLO","ESCARBAJO", "GORGOJO", "MOSCA")])
#create column with correct - incorrect scores
data$score_vectors<- (data$sum_correct_vector - data$sum_incorrect_vector)
#turn -40 into NA in vector score column and then make numeric
data["score_vectors"][data["score_vectors"]=="-40"]<-"NA"
data$score_vectors<- as.numeric(data$score_vectors)
#add 10 to each value so you can do median, etc
data$score_vectors_10<-data$score_vectors+10
#for IQR, median, range of vector scores
quantile(data$score_vectors_10, na.rm=TRUE)

#same as above, but for signs
data$sum_correct_sign=rowSums(data[,c("CHIRI_FECAL_TRAILS", "CHIRI_EGGS", "CHIRI_FECAL_TRAIL")])
data$sum_incorrect_sign=rowSums(data[,c("CHINCHE_CAMA_FECES", "MOUSE_FECES", "MOLD")])
data$score_signs <- (data$sum_correct_sign - data$sum_incorrect_sign)
#turn 60s into NA in sign score column and then make numeric
data["score_signs"][data["sum_sign_ID"]=="60" ]<-"NA"
data$score_signs<-as.numeric(data$score_signs)
#add 10 to each value so you can do median, etc
data$score_signs_10<-data$score_signs+10
#for IQR, median, range of sign scores
quantile(data$score_signs_10, na.rm=TRUE)


#to combine what would you do if you saw a chiri in your house and "other" in one new column
#Decirle a su esposo es biologo and Si encuentra solo una la mata,si encuentra varias acudiria al centro de salud in "other"
data<-mutate(data, QUE_HARIA_CHIRIS=ifelse(VIERA_CHIRI_CASA_QUE_HARIA == "atraparia_centro_salud", "atraparia centro salud",
                                         ifelse(VIERA_CHIRI_CASA_QUE_HARIA == "mataria", "mataria",
                                                ifelse(VIERA_CHIRI_CASA_QUE_HARIA == "buscaria_informacion", "buscaria informacion",
                                                       ifelse(VIERA_CHIRI_CASA_QUE_HARIA == "insecticida", "insecticida",
                                                              ifelse(VIERA_CHIRI_CASA_QUE_HARIA == "nada", "nada",
                                                                     ifelse(VIERA_CHIRI_CASA_QUE_HARIA == "NS_NR", "NS_NR",
                                                                            ifelse(VIERA_CHIRI_CASA_QUE_HARIA == "reportaria", "reportaria",
                                                                                   ifelse(VIERA_CHIRI_CASA_QUE_HARIA == "otro" & (VIERA_CHIRI_CASA_QUE_HARIA_OTRO == "Limpiar" | VIERA_CHIRI_CASA_QUE_HARIA_OTRO == "Limpiar la casa " | VIERA_CHIRI_CASA_QUE_HARIA_OTRO == "Limpieza"| VIERA_CHIRI_CASA_QUE_HARIA_OTRO =="Hecho agua caliente"), "limpiar",
     
                                                                                                                                                                               ifelse(VIERA_CHIRI_CASA_QUE_HARIA == "otro" & (VIERA_CHIRI_CASA_QUE_HARIA_OTRO == "Decirle a su esposo es biologo" | VIERA_CHIRI_CASA_QUE_HARIA_OTRO == "Si encuentra solo una la mata,si encuentra varias acudiria al centro de salud"), "Others", 0))))))))))
                                                
#to combine "what happens when you report chiris" and "other" in one new column
#other is "Le avisan si tiene alguna enfermedad y si son varias casas fumigan la cuadra"
data<- mutate(data, QUE_PASA_REPORTE = ifelse(SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA == "fumigan" |(SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA=="otro"& SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA_OTRO== "Vienen a matarlas "), "fumigan",
                                              ifelse(SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA == "inspector_revisa" | (SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA== "otro" & 	SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA_OTRO== "Vienen a ver mis corrales "), "Inspector revisa",
                                                     ifelse(SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA== "NS_NR", "NS_NR",
                                                            ifelse(SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA== "otro" & (SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA_OTRO== "Analizan la chirimacha" | SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA_OTRO== "Las analizan"), "las analizan",
                                                                 ifelse(SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA== "otro" & (SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA_OTRO== "Deberian fumigar pero no lo hacen" | SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA_OTRO=="No asen nada" | SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA_OTRO=="No hacen caso " | SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA_OTRO=="No hacen nada" | SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA_OTRO=="No hacen nada,no vienen" | SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA_OTRO=="No vienen,no hacen caso" | SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA_OTRO=="Hace tiempo llevo al chirimacha en una caja de fosforos y no vinieron."), "No hacen nada",
                                                                       ifelse(SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA== "otro" & (SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA_OTRO=="Demora  en venir el inspector" | SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA_OTRO== "Demoran en atender ,mucho tramite" | SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA_OTRO=="Demoran en venir" | SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA_OTRO== "Se tiene que insistir para que vengan a la casa"), "Demoran en venir",
                                                                             ifelse(SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA=="otro" & SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA_OTRO=="Le avisan si tiene alguna enfermedad y si son varias casas fumigan la cuadra", "otro", 0))))))))



#to extract one answer/word from the column for quien hablado chiri
data<- data%>%mutate(AGENTE_COMUNIDAD=grepl("agente_comunidad", QUIEN_HABLADO_CHIRI)*1)
data<- data%>%mutate(VECINO=grepl("vecino", QUIEN_HABLADO_CHIRI)*1)
data<- data%>%mutate(FAMILIAR=grepl("familiar", QUIEN_HABLADO_CHIRI)*1)
data<- data%>%mutate(PROFESIONAL_SALUD=grepl("profesional_salud", QUIEN_HABLADO_CHIRI)*1)
data<- data%>%mutate(OTRO=grepl("otro", QUIEN_HABLADO_CHIRI)*1)
data<- data%>%mutate(AMIGO=grepl("amigo", QUIEN_HABLADO_CHIRI)*1)


#to make new column for colegio category from quien hablado 
data<- mutate(data, COLEGIO=ifelse(OTRO==1 & (QUIEN_HABLADO_CHIRI_OTRO=="Colegio"| QUIEN_HABLADO_CHIRI_OTRO=="Colegio " | QUIEN_HABLADO_CHIRI_OTRO=="Colegio ,historietas hace 6 años" | QUIEN_HABLADO_CHIRI_OTRO== "En el colegio"), 1, 0))
#to make new column for others category from quien hablado
data<- mutate(data, OTRO_HABLADO=ifelse(OTRO==1 & (QUIEN_HABLADO_CHIRI_OTRO== "Cochabamba" | QUIEN_HABLADO_CHIRI_OTRO== "Recolector de basura" | QUIEN_HABLADO_CHIRI_OTRO=="Hace muchos anios"), 1, 0))
#to make new column for cuando fumigaron category from quien hablado
data<- mutate(data, CUANDO_FUMIGARON=ifelse(OTRO==1 & (QUIEN_HABLADO_CHIRI_OTRO=="Cuando fumigaron" | QUIEN_HABLADO_CHIRI_OTRO=="Hace mucho tiempo cuando vinieron a fumigar"), 1, 0))
#to make new column for gobierno regional category from quien hablado
data<- mutate(data, GOBIERNO_REGIONAL=ifelse(OTRO==1 & (QUIEN_HABLADO_CHIRI_OTRO=="En el gobierno regional" | QUIEN_HABLADO_CHIRI_OTRO== "Los del concejo"), 1, 0))
#to make new column for university category from quien hablado
data<- mutate(data, UNIVERSIDAD=ifelse(OTRO==1 & QUIEN_HABLADO_CHIRI_OTRO=="En la universidad ", 1, 0))
#to make new column for perifoneo category from quien hablado
data<- mutate(data, PERIFONEO=ifelse(OTRO==1 & (QUIEN_HABLADO_CHIRI_OTRO=="En perifoneo" | QUIEN_HABLADO_CHIRI_OTRO=="Perifoneo"), 1, 0))
#to make new column for amigo category from quien hablado
data<- mutate(data, NOTICIAS=ifelse(OTRO==1 & (QUIEN_HABLADO_CHIRI_OTRO=="Noticias" | QUIEN_HABLADO_CHIRI_OTRO== "Noticias " | QUIEN_HABLADO_CHIRI_OTRO== "Por las noticias " | QUIEN_HABLADO_CHIRI_OTRO== "Propaganda en la television hace 4 anios aproximado" | QUIEN_HABLADO_CHIRI_OTRO== "Radio"), 1, 0))
#to make new column for profesional salud from profesional salud column above plus others
data<- mutate(data, PROFESIONAL_SALUD_TOT=ifelse(PROFESIONAL_SALUD==1 | (QUIEN_HABLADO_CHIRI_OTRO== "Alguien que vino a su casa " | QUIEN_HABLADO_CHIRI_OTRO=="Hace mucho tiempo un grupo de extranjeros dieron charlas y tomaron muestra de sangre a los ninios de la I.E" | QUIEN_HABLADO_CHIRI_OTRO=="Inspector upch"), 1, 0))
#to delete profesional salud column
data$PROFESIONAL_SALUD<-NULL

#to extract one answer/word from the column for oido/visto chiri
data<- data%>%mutate(POSTA=grepl("posta", OIDO_VISTO_INFORMACION_CHIRI)*1)
data<- data%>%mutate(COLEGIO_UNIVERSIDAD_OIDO=grepl("colegio_universidad", OIDO_VISTO_INFORMACION_CHIRI)*1)
data<- data%>%mutate(REDES_SOCIALES=grepl("redes_sociales", OIDO_VISTO_INFORMACION_CHIRI)*1)
data<- data%>%mutate(MEDIO_COMUNICACION=grepl("medio_comunicacion", OIDO_VISTO_INFORMACION_CHIRI)*1)
data<- data%>%mutate(VECINDARIO=grepl("vecindario", OIDO_VISTO_INFORMACION_CHIRI)*1)
data<- data%>%mutate(OTRO_OIDO=grepl("otro", OIDO_VISTO_INFORMACION_CHIRI)*1)

#to make new column for categories like above for oido/visto chiri
data<- mutate(data, CASA_FAMILIAR=ifelse(OTRO_OIDO==1 & (OIDO_VISTO_INFORMACION_CHIRI_OTRO=="Casa" | OIDO_VISTO_INFORMACION_CHIRI_OTRO== "Casa de un familiar" | OIDO_VISTO_INFORMACION_CHIRI_OTRO=="Familiares" | OIDO_VISTO_INFORMACION_CHIRI_OTRO=="Y  en mi domicilio"), 1, 0))
data<- mutate(data, CALLE=ifelse(OTRO_OIDO==1 & (OIDO_VISTO_INFORMACION_CHIRI_OTRO=="En el mercado San Camilo hace años" | OIDO_VISTO_INFORMACION_CHIRI_OTRO=="En un mercado" | OIDO_VISTO_INFORMACION_CHIRI_OTRO=="Characato" | OIDO_VISTO_INFORMACION_CHIRI_OTRO=="En Cerro colorado " |OIDO_VISTO_INFORMACION_CHIRI_OTRO=="En la calle " |OIDO_VISTO_INFORMACION_CHIRI_OTRO=="En las calles perifoneo" | OIDO_VISTO_INFORMACION_CHIRI_OTRO=="En otro distrito" |OIDO_VISTO_INFORMACION_CHIRI_OTRO=="Hace 10 anios en Miraflores" |OIDO_VISTO_INFORMACION_CHIRI_OTRO=="Otro distrito"), 1, 0))
data<- mutate(data, OTRO_OIDO1=ifelse(OTRO_OIDO==1 & (OIDO_VISTO_INFORMACION_CHIRI_OTRO=="En la Ciudad de Ica,vio chirimachas y le hablo el personal de salud" |OIDO_VISTO_INFORMACION_CHIRI_OTRO=="En Nazca Ica hace años" | OIDO_VISTO_INFORMACION_CHIRI_OTRO=="Hace 10 anios un joven fue a donar sangre para un familiar y le dijeron que no podia porque tenia el mal de chagas" |OIDO_VISTO_INFORMACION_CHIRI_OTRO=="Hace tiempo cuando hicieron campaña" |OIDO_VISTO_INFORMACION_CHIRI_OTRO=="Por una capacitacion" |OIDO_VISTO_INFORMACION_CHIRI_OTRO=="En un documental"), 1, 0))
data<- mutate(data, REDES_SOCIALES_OIDO=ifelse(REDES_SOCIALES==1 | OTRO_OIDO==1 & (OIDO_VISTO_INFORMACION_CHIRI_OTRO=="Busca informacion" | OIDO_VISTO_INFORMACION_CHIRI_OTRO== "Internet"), 1, 0))
data<- mutate(data, MEDIO_COMUNICACION_OIDO=ifelse(MEDIO_COMUNICACION==1 | (OTRO_OIDO==1 & OIDO_VISTO_INFORMACION_CHIRI_OTRO=="En un periodico hace 3 anios"), 1, 0))
data$MEDIO_COMUNICACION<-NULL
data$REDES_SOCIALES<-NULL
data$OTRO_OIDO<-NULL



#to make new column with categories for where you find chiris
#NOT USING THIS CODE, DELETE LATER
data<- mutate(data, CAT_DONDE_BUSCA_CHIRI = ifelse(DONDE_BUSCA_CHIRI == "Acumulación de basura" | DONDE_BUSCA_CHIRI == "Basura" | DONDE_BUSCA_CHIRI =="Donde esta acumulados las cosas" | DONDE_BUSCA_CHIRI == "Cosas amontonadas" | DONDE_BUSCA_CHIRI == "Cosas guardadas mucho tiempo " | DONDE_BUSCA_CHIRI =="Donde hay cosas amontonadas" | DONDE_BUSCA_CHIRI =="Donde hay cosas guardadas" | DONDE_BUSCA_CHIRI =="En los depósitos donde hay cosas amontonadas" | DONDE_BUSCA_CHIRI == "Lugares amontonados ", "disorganization",
                                                   ifelse(DONDE_BUSCA_CHIRI == "Animales" | DONDE_BUSCA_CHIRI =="Animales " | DONDE_BUSCA_CHIRI =="Animales corrales cuyeros" | DONDE_BUSCA_CHIRI == "Animales desmontes" | DONDE_BUSCA_CHIRI =="Conejero " | DONDE_BUSCA_CHIRI =="Conejeros" | DONDE_BUSCA_CHIRI =="Conejos" | DONDE_BUSCA_CHIRI =="Corral de animales" | DONDE_BUSCA_CHIRI =="Corrales" | DONDE_BUSCA_CHIRI =="Corrales de animales" | DONDE_BUSCA_CHIRI =="Corrales de conejos, gallinas" | DONDE_BUSCA_CHIRI =="Corrales de cuyes y aves" |  DONDE_BUSCA_CHIRI =="Corrales donde no hey limpieza " | DONDE_BUSCA_CHIRI =="Donde ay animales" | DONDE_BUSCA_CHIRI =="Donde crían animales" | DONDE_BUSCA_CHIRI =="Donde esta los animales" | DONDE_BUSCA_CHIRI =="Donde estan los animales" | DONDE_BUSCA_CHIRI =="Donde están los animales" |  DONDE_BUSCA_CHIRI =="Donde estan los animales " |  DONDE_BUSCA_CHIRI =="Donde estan los conejos y pollos" |   DONDE_BUSCA_CHIRI =="Donde estan los conejos" | DONDE_BUSCA_CHIRI =="Donde hay animales" |  DONDE_BUSCA_CHIRI =="Donde hay animales " |  DONDE_BUSCA_CHIRI =="Donde hay gallinas" |  DONDE_BUSCA_CHIRI =="Donde los animales" |  DONDE_BUSCA_CHIRI =="Donde viven los animales" |  DONDE_BUSCA_CHIRI =="Donde viven los animales " |  DONDE_BUSCA_CHIRI =="Donde viven los animales cuyos gallinas" |  DONDE_BUSCA_CHIRI =="En corrales" |  DONDE_BUSCA_CHIRI =="En corrales " |  DONDE_BUSCA_CHIRI =="En el corral de cuyes" |  DONDE_BUSCA_CHIRI =="En el corral de los perros " |  DONDE_BUSCA_CHIRI =="En el corral de pollos" |  DONDE_BUSCA_CHIRI =="En el corral de pollos " |  DONDE_BUSCA_CHIRI =="En el corral del perro " |  DONDE_BUSCA_CHIRI =="En el cuy" |  DONDE_BUSCA_CHIRI =="En el cuyero" |  DONDE_BUSCA_CHIRI =="En el cuyero " |  DONDE_BUSCA_CHIRI =="En la casa del perro " |  DONDE_BUSCA_CHIRI =="En la perrera" |  DONDE_BUSCA_CHIRI =="En la perrera " |  DONDE_BUSCA_CHIRI =="En los animales" |  DONDE_BUSCA_CHIRI =="En los animales " |  DONDE_BUSCA_CHIRI =="En los conejeros" |  DONDE_BUSCA_CHIRI =="En los conejeros,corral de ovejas" |  DONDE_BUSCA_CHIRI =="En los corrales" |  DONDE_BUSCA_CHIRI =="En los corral" |  DONDE_BUSCA_CHIRI =="En los corrales " | DONDE_BUSCA_CHIRI =="En los cuyeros" |  DONDE_BUSCA_CHIRI =="En los cuyeros " | DONDE_BUSCA_CHIRI =="En los corrales basural" |  DONDE_BUSCA_CHIRI =="En los gallineros" |  DONDE_BUSCA_CHIRI =="Gellineros cuyeros" |  DONDE_BUSCA_CHIRI =="Los animales" |  DONDE_BUSCA_CHIRI =="Los corrales" | DONDE_BUSCA_CHIRI =="Patio de animales" |  DONDE_BUSCA_CHIRI =="Por donde estan los animales" |  DONDE_BUSCA_CHIRI =="Tienen animales corrales", "animales",
                                                          ifelse( DONDE_BUSCA_CHIRI =="Cosas viejas sucias" |  DONDE_BUSCA_CHIRI =="En la basura donde no hay limpieza " |  DONDE_BUSCA_CHIRI =="En los rincones donde  no se limpia" |  DONDE_BUSCA_CHIRI =="Lugares donde no se limpia" | DONDE_BUSCA_CHIRI =="Lugares donde no se limpian" | DONDE_BUSCA_CHIRI =="Lugares sucios", "lugares sucios",
                                                                  ifelse( DONDE_BUSCA_CHIRI =="Debajo de ladrillos" |  DONDE_BUSCA_CHIRI =="Debajo de los ladrillos" |  DONDE_BUSCA_CHIRI =="Debajo los ladrillos" |  DONDE_BUSCA_CHIRI =="En los ladrillos" |  DONDE_BUSCA_CHIRI =="En los ladrillos " |  DONDE_BUSCA_CHIRI =="Grietas de los ladrillos" |  DONDE_BUSCA_CHIRI =="Ladrillos" |  DONDE_BUSCA_CHIRI =="Ladrillos " |  DONDE_BUSCA_CHIRI =="Ladrillos pircados" |  DONDE_BUSCA_CHIRI =="Por los ladrillos", "ladrillos",
                                                                          ifelse( DONDE_BUSCA_CHIRI =="Debajo de los sillares" |  DONDE_BUSCA_CHIRI =="Debajo de sillares" |  DONDE_BUSCA_CHIRI =="Debajo del sillar" |  DONDE_BUSCA_CHIRI =="En el sillar" |  DONDE_BUSCA_CHIRI =="En los sillares" | DONDE_BUSCA_CHIRI =="En los sillares pircados" | DONDE_BUSCA_CHIRI =="Por los sillares" | DONDE_BUSCA_CHIRI =="Sillar" | DONDE_BUSCA_CHIRI =="Sillares", "sillares",
                                                                                  ifelse( DONDE_BUSCA_CHIRI =="Adobe" | DONDE_BUSCA_CHIRI =="En las pircas de adobe", "adobe",
                                                                                          ifelse( DONDE_BUSCA_CHIRI =="Cama " | DONDE_BUSCA_CHIRI =="Camas" | DONDE_BUSCA_CHIRI =="Debajo de cama" | DONDE_BUSCA_CHIRI =="Debajo de la cama" | DONDE_BUSCA_CHIRI =="Debajo de la cama " | DONDE_BUSCA_CHIRI =="Debajo de las camas" | DONDE_BUSCA_CHIRI =="En las camas" | DONDE_BUSCA_CHIRI =="Colchones" | DONDE_BUSCA_CHIRI =="Debajo de colchones " | DONDE_BUSCA_CHIRI =="Debajo de los colchones", "cama",
                                                                                                  ifelse(DONDE_BUSCA_CHIRI =="Debajo de la piedras" | DONDE_BUSCA_CHIRI == "Debajo de las piedras" | DONDE_BUSCA_CHIRI =="Debajo de piedras" | DONDE_BUSCA_CHIRI =="Debajo de piedras " |DONDE_BUSCA_CHIRI =="Donde Hay piedras" |DONDE_BUSCA_CHIRI =="En las piedras" | DONDE_BUSCA_CHIRI =="En las pircas de piedras" |DONDE_BUSCA_CHIRI =="Piedras" |DONDE_BUSCA_CHIRI =="Piedras ", "piedras",
                                                                                                         ifelse(DONDE_BUSCA_CHIRI =="No la buscaria" | DONDE_BUSCA_CHIRI =="No la buscaria porque no la conce" |  DONDE_BUSCA_CHIRI =="No sabe" |  DONDE_BUSCA_CHIRI =="No sabe " | DONDE_BUSCA_CHIRI =="Nose", "no sabe",
                                                                                                                ifelse( DONDE_BUSCA_CHIRI =="Cuarto" | DONDE_BUSCA_CHIRI =="Cuartos" | DONDE_BUSCA_CHIRI =="Cuartos " | DONDE_BUSCA_CHIRI =="Las habitaciones", "cuartos",
                                                                                                                        ifelse( DONDE_BUSCA_CHIRI =="Detras de cuadros" |  DONDE_BUSCA_CHIRI =="Detras de cuadros " |  DONDE_BUSCA_CHIRI =="Detras de muebles" |  DONDE_BUSCA_CHIRI =="Detrás de las cosas", "detras de cosas",
                                                                                                                                ifelse(DONDE_BUSCA_CHIRI =="En el patio" |DONDE_BUSCA_CHIRI =="Patio", "patio",
                                                                                                                                       ifelse(DONDE_BUSCA_CHIRI =="El techo" |DONDE_BUSCA_CHIRI =="En el techo" |DONDE_BUSCA_CHIRI =="En el techo " |DONDE_BUSCA_CHIRI =="En los techos" |DONDE_BUSCA_CHIRI =="Techos", "techos",
                                                                                                                                              ifelse())))))))))))))
              
#create a data frame 
data1<- as.data.frame(data[,c(1,12)])
#filter donde busca chiri and create columns for words that are filtered
data1$animal <- as.integer(grepl("animales|animal", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$ladrillo <- as.integer(grepl("ladrillo|ladrillos|ladrllos", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$sillar <- as.integer(grepl("sillares|sillar", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$piedra <- as.integer(grepl("piedras|piedra", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$adobe <- as.integer(grepl("adobe", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$cama <- as.integer(grepl("cama|camas|colchones", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$detras <- as.integer(grepl("detras|detrás", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$patio <- as.integer(grepl("patio", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$cuarto <- as.integer(grepl("cuarto|cuartos|habitacion|habitaciones|dormitorio", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$techo <- as.integer(grepl("techos|techo", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$tierra <- as.integer(grepl("tierra|chacra", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$grieta <- as.integer(grepl("grietas|grieta", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$rincon <- as.integer(grepl("rincon|rincones", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$humedo <- as.integer(grepl("humedo|humedos|humedad|húmedas|húmedos|humedas", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$oscuro <- as.integer(grepl("oscuro|oscuros|oscura|oscuras", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$esquina <- as.integer(grepl("esquina|esquinas", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$pared <- as.integer(grepl("pared|paredes", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$madera <- as.integer(grepl("maderas|madera", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$amontonada <- as.integer(grepl("amontonada|amontonadas|amontonados", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$sucio <- as.integer(grepl("sucio|sucios|sucias|sucia|suciedad", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$no_sabe <- as.integer(grepl("sabe|buscaria|nose", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$cuy <- as.integer(grepl("cuy|cuyes|cuyeros|cuyos", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$perro <- as.integer(grepl("perro|perros|perrera|perreras", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$conejo <- as.integer(grepl("conejo|conejos|conejero|conejeros", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$gallina <- as.integer(grepl("gallina|gallinas|gallineros|gellineros", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$corral <- as.integer(grepl("corral|corrales", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$pollo <- as.integer(grepl("pollo|pollos", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$rendija <- as.integer(grepl("rendija|rendijas", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$hueco <- as.integer(grepl("hueco|huecos", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$no_limpia <- as.integer(grepl("limpia|limpian|limpieza", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$jardin <- as.integer(grepl("jardin|jardines", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$acumulado <- as.integer(grepl("acumulados|acumulación", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$guardadas <- as.integer(grepl("guardadas", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$basura <- as.integer(grepl("basura", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$bloqueta<- as.integer(grepl("bloqueta|bloquetas", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$planta <- as.integer(grepl("planta|plantas", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$pirca <- as.integer(grepl("pirca|pircado", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$cajones <- as.integer(grepl("cajones", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$almacen <- as.integer(grepl("almacen|almacenes", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$papel <- as.integer(grepl("papel|papeles", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$cuadro <- as.integer(grepl("cuadro|cuadros", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$descampado <- as.integer(grepl("descampado|descampados", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$taller <- as.integer(grepl("taller", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$no_mueven <- as.integer(grepl("mueven|movemos", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$cocina <- as.integer(grepl("cocina", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$tabla <- as.integer(grepl("tabla|tablas", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$deposito <- as.integer(grepl("deposito|depósitos", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$baño <- as.integer(grepl("baño|baños", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$agua <- as.integer(grepl("agua|aguas", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))

















#download data to excel file
write.csv(data, file="C:/Users/zoeea/OneDrive/Documents/Immune/data from r.csv", row.names = FALSE)
