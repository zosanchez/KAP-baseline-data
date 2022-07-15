#KAP 


library (dplyr)
library(tidyverse)
library(lubridate)
library(tidyr)
library(readr)
#use this to read
data <- read.csv("Immune/KAP-baseline-data/immune_survey_23-05-2022.csv", header=TRUE, sep=";")
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

#gender distribution
table(data$SEXO)

#change age from chr to numeric
data$EDAD<- as.numeric(data$EDAD)

#making age groups. age groups based on range, IQR, median
data<-mutate(data, AGECLASS= ifelse(EDAD<=31,"18-31",
                                    ifelse(EDAD<=45,"32-45",
                                           ifelse(EDAD<=59, "46-59",
                                                  ifelse(EDAD<=100, "60-100", "0")))))

#making age groups for <=35 and >35
data<-mutate(data, AGE35=ifelse(EDAD<=35, "<35",
                                ifelse(EDAD>35, ">35", 0)))


#combine answers to ACTA_ECONOMICA and ACTA_ECONOMICA_OTRO to one column
data<-mutate(data, OCCUPATIONS=ifelse(ACT_ECONOMICA == "ama_casa", "ama_casa",
                                      ifelse(ACT_ECONOMICA == "estudiante", "estudiante",
                                             ifelse(ACT_ECONOMICA == "jubilado" | ACT_ECONOMICA == "otro" & ACT_ECONOMICA_OTRO == "Militar en retiro" | ACT_ECONOMICA == "sin_empleo", "other jobs",
                                                      ifelse(ACT_ECONOMICA == "trabajador_independiente" | ACT_ECONOMICA == "otro" & ACT_ECONOMICA_OTRO =="Directora de I.E(vivienda es I.E Inicial)" | ACT_ECONOMICA == "otro" & ACT_ECONOMICA_OTRO =="Docente pronoi local publico" | ACT_ECONOMICA == "otro" & ACT_ECONOMICA_OTRO =="que mas pue" | ACT_ECONOMICA == "otro" & ACT_ECONOMICA_OTRO =="Vivienda es tienda" | ACT_ECONOMICA == "otro" & ACT_ECONOMICA_OTRO =="Vivienda es Tienda", "trabajador_independiente",
                                                              ifelse(ACT_ECONOMICA == "trabajador_planilla", "trabajador_planilla", 0))))))

#collapse education level groups into categories of primary or less, secondary, tecnico/university or more. make one column that shows grouped answers
data<- mutate(data, EDUCATION_CATS=ifelse(NIVEL_ESTUDIOS == "ninguno" | NIVEL_ESTUDIOS == "primaria" | NIVEL_ESTUDIOS == "pre_escolar" | NIVEL_ESTUDIOS == "NS_NR", "primary or less",
                                          ifelse(NIVEL_ESTUDIOS == "secundaria", "secondary",
                                                 ifelse(NIVEL_ESTUDIOS == "tecnico" | NIVEL_ESTUDIOS == "universidad" | NIVEL_ESTUDIOS == "postgrado", "technical/univerity or more", 0))))

#to show answers Y/N/NS_NR for CHIRI TRANSMITIR ENFERMEDAD
table(data$CHIRI_TRANSMITIR_ENFERMEDAD)

#to show answers for que enfermedades transmite chiri
table(data$QUE_ENFERMEDADES_TRANSMITE_CHIRI)

#to make a column for "other" answers to question about what disease chirimachas transmit.
#"crecimiento de organos" and "enfermedad del corazón/pecho" could = Chagas, they might not have known the name 
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




#combine knowledge that chirimachas transmit a disease, what disease, and answers for others in one column
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

#collapse categories of what diseases are transmitted by chiris into correct/incorrect/NS_NR. these are out of those who said yes to chiri transmitir enfermedad. NS_NR refers to those who said yes chiris transmit disease, but didn't give an answer for what disease
data<- mutate(data, DISEASE_TRANSMIT_CATS=ifelse(CHIRI_TRANSMITIR_ENFERMEDAD == "si" & (KNOWLEDGE_OF_DISEASE_TRANSMISSION == "Chagas" | KNOWLEDGE_OF_DISEASE_TRANSMISSION == "Enfermedad peligrosa" | KNOWLEDGE_OF_DISEASE_TRANSMISSION == "Crecimiento de organos" | KNOWLEDGE_OF_DISEASE_TRANSMISSION == "Enfermedad del corazón/pecho"), "correct",
                                                 ifelse(CHIRI_TRANSMITIR_ENFERMEDAD == "si" & (KNOWLEDGE_OF_DISEASE_TRANSMISSION == "Tuberculosis" | KNOWLEDGE_OF_DISEASE_TRANSMISSION == "Toxoplasma" | KNOWLEDGE_OF_DISEASE_TRANSMISSION == "Paludismo" | KNOWLEDGE_OF_DISEASE_TRANSMISSION == "Lesmidiasis" | KNOWLEDGE_OF_DISEASE_TRANSMISSION == "Infección" | KNOWLEDGE_OF_DISEASE_TRANSMISSION == "Dengue" | KNOWLEDGE_OF_DISEASE_TRANSMISSION == "Alergia" | KNOWLEDGE_OF_DISEASE_TRANSMISSION == "Enfermedad del estomago" | KNOWLEDGE_OF_DISEASE_TRANSMISSION == "Fiebre y otro" | KNOWLEDGE_OF_DISEASE_TRANSMISSION == "Solo fiebre" | KNOWLEDGE_OF_DISEASE_TRANSMISSION == "Enfermedades de la piel"), "incorrect",
                                                        ifelse(CHIRI_TRANSMITIR_ENFERMEDAD == "si" & (KNOWLEDGE_OF_DISEASE_TRANSMISSION == "Si, pero no se acuerda" | KNOWLEDGE_OF_DISEASE_TRANSMISSION == "Si, pero no sabe/responde"), "NS_NR", 0))))



#to create a new data base just with the variables you are going to use
#df<- select(data, CUALES_PLACA_SON_CHIRI)

#to extract one answer/word from CUALES_PLACA_SON_CHIRI column. New column is created and 1 is given if extracted answer is present
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

#to enter "NS_NR" in all columns created above when NS_NR column has a 1
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

#to change NS_NR to 10 for all possible vector columns (above) and then change to numeric for scores
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


#to extract one answer/word from SABES_SENIALES_CHIRI column. New column is created and 1 is given if extracted answer is present
data<- data%>%mutate(NS_NR_SENIALES=grepl("NS_NR", SABES_SENIALES_CHIRI)*1)
data<- data%>%mutate(CHIRI_FECAL_TRAILS=grepl("a", SABES_SENIALES_CHIRI)*1)
data<- data%>%mutate(CHINCHE_CAMA_FECES=grepl("b", SABES_SENIALES_CHIRI)*1)
data<- data%>%mutate(MOUSE_FECES=grepl("c", SABES_SENIALES_CHIRI)*1)
data<- data%>%mutate(MOLD=grepl("d", SABES_SENIALES_CHIRI)*1)
data<- data%>%mutate(CHIRI_EGGS=grepl("e", SABES_SENIALES_CHIRI)*1)
data<- data%>%mutate(CHIRI_FECAL_TRAIL=grepl("f", SABES_SENIALES_CHIRI)*1)

#to enter "NS_NR" in all columns created above when NS_NR column has a 1
data$CHINCHE_CAMA_FECES[data$NS_NR_SENIALES==1]<- "NS_NR"
data$MOUSE_FECES[data$NS_NR_SENIALES==1]<- "NS_NR"
data$MOLD[data$NS_NR_SENIALES==1]<- "NS_NR"
data$CHIRI_EGGS[data$NS_NR_SENIALES==1]<- "NS_NR"
data$CHIRI_FECAL_TRAIL[data$NS_NR_SENIALES==1]<- "NS_NR"
data$CHIRI_FECAL_TRAILS[data$NS_NR_SENIALES==1]<- "NS_NR"

#to change NS_NR to 10 for all possible signs columns (above) and then change to numeric for scores
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

#create column that sums all rows from vector columns. to see how many options participants chose
data$sum_vector_ID=rowSums(data[,c("CHIRI_ADULTA","CHIRI_NIFA_3","CHIRI_NIFA_4_fl", "CHIRI_NINFA_4_alim","COREIDO","CHINCHE_CAMA","CHINCHE_HEDIONDA","CUCARACHA", "CHINCHE_CINTURON_AMARILLO","ESCARBAJO", "GORGOJO", "MOSCA")])

#create column that sums all rows from sign columns. to see how many options participants chose
data$sum_sign_ID=rowSums(data[,c("CHIRI_FECAL_TRAILS","CHINCHE_CAMA_FECES","MOUSE_FECES", "MOLD","CHIRI_EGGS","CHIRI_FECAL_TRAIL")])


#create a new column that contains categories of answers to the CUALES_PLACA_SON_CHIRI question. (sumvectorID up to 5 = the highest number of insects selected amongst the participants is 5) 
data<-mutate(data, vector_cat=ifelse(CHIRI_ADULTA ==1 & sum_vector_ID==1,"solo adulta", 
                                     ifelse(sum_vector_ID==120, "NS_NR",
                                            ifelse(CHIRI_NIFA_3 == 1 & sum_vector_ID==1, "solo ninf3",
                                                   ifelse(CHIRI_NIFA_4_fl == 1 & sum_vector_ID==1, "solo ninf4 fl", 
                                                          ifelse(CHIRI_NINFA_4_alim == 1 & sum_vector_ID==1, "solo ninf4 ali", 
                                                                 ifelse(CHIRI_ADULTA ==1 & (CHIRI_NINFA_4_alim == 1 | CHIRI_NIFA_3 == 1 |CHIRI_NIFA_4_fl == 1) & sum_vector_ID==2,  "adult+1ninf",
                                                                        ifelse(CHIRI_ADULTA ==1 & (CHIRI_NINFA_4_alim == 1 | CHIRI_NIFA_3 == 1 |CHIRI_NIFA_4_fl == 1) & sum_vector_ID==3,  "adult+2ninf",
                                                                               ifelse(CHIRI_ADULTA ==1 & (CHIRI_NINFA_4_alim == 1 | CHIRI_NIFA_3 == 1 |CHIRI_NIFA_4_fl == 1) & sum_vector_ID==4,  "adult+3ninf",
                                                                                      ifelse(CHIRI_ADULTA ==1 & (CHIRI_NINFA_4_alim == 1 & CHIRI_NIFA_3 == 1 & CHIRI_NIFA_4_fl == 1) & sum_vector_ID==5,  "adult+3ninf+1other",
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
                                                                                                                                                                                                                                                ifelse(CHIRI_ADULTA ==0 & CHIRI_NINFA_4_alim == 0 & CHIRI_NIFA_3 == 0 & CHIRI_NIFA_4_fl == 1 & sum_vector_ID==5,  "ninf4fl+4other",
                                                                                                                                                                                                                                                       ifelse(CHIRI_ADULTA ==1 & (CHIRI_NINFA_4_alim == 1 | CHIRI_NIFA_3 == 1 |CHIRI_NIFA_4_fl == 1) & sum_vector_ID==5,  "adult+2ninf+2other", 0)))))))))))))))))))))))))))))))))
#create a new column for insect ID answer categories of Adult correct, at least one nymph correct, all 4 correct, and only incorrect
data<- mutate(data, INSECT_CATS=ifelse(vector_cat == "solo adulta" | vector_cat == "adult+1ninf" | vector_cat == "adult+2ninf" | vector_cat == "adult+3ninf+1other" | vector_cat == "adult+1other" | vector_cat == "adult+2other" | vector_cat == "adult+3other" | vector_cat == "adult+2ninf+2other", "adult correct",
                                       ifelse(vector_cat == "solo ninf3" | vector_cat == "solo ninf4 fl" | vector_cat == "solo ninf4 ali" | vector_cat == "solo 2ninf" | vector_cat == "solo 3ninf" | vector_cat == "ninf4ali+1other" | vector_cat == "ninf4ali+2other" | vector_cat == "ninf4ali+3other" | vector_cat == "ninf4ali+4other" | vector_cat == "ninf3+1other" | vector_cat == "ninf3+2other" | vector_cat == "ninf3+3other" | vector_cat == "ninf3+4other" | vector_cat == "ninf4fl+1other" | vector_cat == "ninf4fl+2other" | vector_cat == "ninf4fl+3other" | vector_cat == "ninf4fl+ninf3=2other" | vector_cat == "ninf4fl+4other", "any nymph correct", 
                                              ifelse(vector_cat == "incorrect only(1)" | vector_cat == "incorrect only(2)" | vector_cat == "incorrect only(3)" | vector_cat == "incorrect only(4)", "incorrect only",
                                                     ifelse(vector_cat == "adult+3ninf", "all 4 correct",
                                                            ifelse(vector_cat == "NS_NR", "NS_NR", 0))))))


#create a new column that contains categories of answers to the SABES_SENIALES CHIRI question. (sumvectorID up to 3 = the highest number of signs selected amongst the participants is 3) 
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


#create a new column for sign ID answer categories of only incorrect, all 3 correct, 1 correct, combination of 2 correct
data<- mutate(data, SIGN_CATS=ifelse(sign_cat == "onlyincorrect(1)" | sign_cat == "onlyincorrect(2)" | sign_cat == "onlyincorrect(3)", "only incorrect",
                                     ifelse(sign_cat == "all3signs", "all 3 correct",
                                            ifelse(sign_cat == "solo fecal trails" | sign_cat == "fecaltrails+1other" | sign_cat == "fecaltrails+2other" | sign_cat == "eggs+1other" | sign_cat == "eggs+2other" | sign_cat == "fecaltrail+1other" | sign_cat == "fecaltrail+2other" | sign_cat == "solo eggs" | sign_cat == "solo fecal trail", "1 correct",
                                                   ifelse(sign_cat == "fecaltrails+1sign" | sign_cat == "fecaltrail+eggs", "2 correct",
                                                          ifelse(sign_cat == "NS_NR", "NS_NR", 0))))))


#create column that sums knowledge for vectors (score without subtraction for incorrect answers)
data$sum_correct_vector=rowSums(data[,c("CHIRI_ADULTA","CHIRI_NIFA_3","CHIRI_NIFA_4_fl", "CHIRI_NINFA_4_alim")])
#create column that sums incorrect vector choices
data$sum_incorrect_vector=rowSums(data[,c("COREIDO","CHINCHE_CAMA","CHINCHE_HEDIONDA","CUCARACHA", "CHINCHE_CINTURON_AMARILLO","ESCARBAJO", "GORGOJO", "MOSCA")])
#create column with correct - incorrect scores for insect ID
data$score_vectors<- (data$sum_correct_vector - data$sum_incorrect_vector)
#turn score of -40 into NA in vector score column and then make numeric
data["score_vectors"][data["score_vectors"]=="-40"]<-"NA"
data$score_vectors<- as.numeric(data$score_vectors)
#add 10 to each value so you can do median, etc
data$score_vectors_10<-data$score_vectors+10
#for IQR, median, range of vector scores
quantile(data$score_vectors_10, na.rm=TRUE)

#same as above (scores), but for signs
data$sum_correct_sign=rowSums(data[,c("CHIRI_FECAL_TRAILS", "CHIRI_EGGS", "CHIRI_FECAL_TRAIL")])
data$sum_incorrect_sign=rowSums(data[,c("CHINCHE_CAMA_FECES", "MOUSE_FECES", "MOLD")])
data$score_signs <- (data$sum_correct_sign - data$sum_incorrect_sign)
#turn score of 60 into NA in sign score column and then make numeric
data["score_signs"][data["sum_sign_ID"]=="60" ]<-"NA"
data$score_signs<-as.numeric(data$score_signs)
#add 10 to each value so you can do median, etc
data$score_signs_10<-data$score_signs+10
#for IQR, median, range of sign scores
quantile(data$score_signs_10, na.rm=TRUE)

#to view answers about if they've seen chirimiachas or signs in the last year, if they reported it, who they reported to, and if they came to confirm
table(data$ULTIMO_ANIO_VIO_CHIRI)
table(data$REPORTO_DENUNCIO_CHIRI)
table(data$AQUIEN_REPORTO)
table(data$VINIERON_CONFIRMAR_PRESENCIA_INSECTO)

#view answers to what would you do if you saw chiri in  house
table(data$VIERA_CHIRI_CASA_QUE_HARIA)
#to combine and categorize given answers for what would you do if you saw a chiri in your house and "other" answers in one new column
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

#view answers for what would you do if you suspect you have chirimachas in your house
table(data$SOSPECHAS_CHIRI_CASA_QUE_HARIA)

#make categories for what would you do if you suspect chirimachas in your house
data<- mutate(data, QUE_HARIA_SOSPECHAS_CS = ifelse(SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO == "Acudir al centro de salud" | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Acudiria al centro de salud" | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Avisar al centro de salud" | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Informo al cs" | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Ir al centro de salud" | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Iria al centro de salud" | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Llamaria a centro de salud" | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Llamaria al centro de salud" | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Llamaria al Centro de Salud" | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Llamaria al Centro de Salud " | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Reportaria a c3ntro de salud" | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Voy al centro de salud", 1, 0))
data<- mutate(data, QUE_HARIA_SOSPECHAS_PS = ifelse(SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO == "Acudiria ala posta" | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Avisaria a la posta" | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Avisaria a la posta " | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Llamar ala posta" | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Llamaria a alguien de la posta" | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Llamaria a la posta" | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Llamaria ala posta", 1, 0))
data<- mutate(data, QUE_HARIA_SOSPECHAS_LIMP_BUSC = ifelse(SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO == "Buscar" | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Buscar hacer limpieza" | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Buscaria " | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Buscaría en las paredes y ladrillos" | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Buscaría en lugares donde hay cosas amontanadas" | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Buscaria haciendo limpieza" | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Buscaria limpiando" | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Buscaria y haria limpieza" | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Haria limpieza" | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Haría limpieza" | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Haría limpieza " | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Haria limpieza de la casa" | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Haría limpieza en toda mi casa" | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="La buscaria y fumigaria" | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Limpiar" | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Limpiar la casa " | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Limpiar mi casa " | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Limpiar y buscar patio " | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Limpiar y buscarla" | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Limpiaria" | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Limpiaria " | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Limpiaria y fumigaria" | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Limpieza" | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Limpieza " | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Limpieza de la casa" | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Limpieza de toda la casa" | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Limpieza profunda " | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Limpio la casa" | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Mover todas las cosas" | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Mueve las cosas" | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Muevo todas las cosas" | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Revision de la casa", 1, 0))
data<- mutate(data, QUE_HARIA_SOSPECHAS_OTRO = ifelse(SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO == "Hecharia cal" | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Llamaria al inspector sanitario" | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Llamaria al senasa" | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="No abrir las ventanas" | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="La buscaria y fumigaria" | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Limpiaria y fumigaria" | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Llamaria a inspector sanitario", 1, 0))
data<- mutate(data, QUE_HARIA_SOSPECHAS_REPORT = ifelse(SOSPECHAS_CHIRI_CASA_QUE_HARIA == "reportaria" | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO == "La reporto" | SOSPECHAS_CHIRI_CASA_QUE_HARIA_OTRO =="Reportaria al concejo", 1, 0))
     
#answers to what happens when you report chiris or signs of chiris in your house
table(data$SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA)
#to combine and categorize answers for "what happens when you report chiris" and "other" answers in one new column
#other is "Le avisan si tiene alguna enfermedad y si son varias casas fumigan la cuadra"
data<- mutate(data, QUE_PASA_REPORTE = ifelse(SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA == "fumigan" |(SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA=="otro"& SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA_OTRO== "Vienen a matarlas "), "fumigan",
                                              ifelse(SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA == "inspector_revisa" | (SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA== "otro" & 	SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA_OTRO== "Vienen a ver mis corrales "), "Inspector revisa",
                                                     ifelse(SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA== "NS_NR", "NS_NR",
                                                            ifelse(SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA== "otro" & (SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA_OTRO== "Analizan la chirimacha" | SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA_OTRO== "Las analizan"), "las analizan",
                                                                 ifelse(SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA== "otro" & (SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA_OTRO== "Deberian fumigar pero no lo hacen" | SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA_OTRO=="No asen nada" | SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA_OTRO=="No hacen caso " | SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA_OTRO=="No hacen nada" | SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA_OTRO=="No hacen nada,no vienen" | SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA_OTRO=="No vienen,no hacen caso" | SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA_OTRO=="Hace tiempo llevo al chirimacha en una caja de fosforos y no vinieron."), "No hacen nada",
                                                                       ifelse(SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA== "otro" & (SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA_OTRO=="Demora  en venir el inspector" | SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA_OTRO== "Demoran en atender ,mucho tramite" | SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA_OTRO=="Demoran en venir" | SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA_OTRO== "Se tiene que insistir para que vengan a la casa"), "Demoran en venir",
                                                                             ifelse(SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA=="otro" & SABE_QUE_PASA_DENUNCIA_REPORTE_CHIRI_CASA_OTRO=="Le avisan si tiene alguna enfermedad y si son varias casas fumigan la cuadra", "otro", 0))))))))

#collapse answers in que pasa reporte to categories: correct answer, no hacen nada, NS_NR
data<- mutate(data, REPORTE_CATS=ifelse(QUE_PASA_REPORTE == "fumigan" | QUE_PASA_REPORTE == "Inspector revisa" | QUE_PASA_REPORTE == "las analizan" | QUE_PASA_REPORTE == "Demoran en venir" | QUE_PASA_REPORTE == "otro", "correct",
                                        ifelse(QUE_PASA_REPORTE == "No hacen nada", "no hacen nada",
                                               ifelse(QUE_PASA_REPORTE == "NS_NR", "NS_NR", 0))))

#view answer to has someone talked to you or have you seen or heard info about chiri
table(data$HABLADO_OIDO_VISTO_CHIRI)

#to extract one answer/word from the column for quien hablado chiri and make new column for each extracted word. 1 is given if extracted answer is present for participant
data<- data%>%mutate(AGENTE_COMUNIDAD=grepl("agente_comunidad", QUIEN_HABLADO_CHIRI)*1)
data<- data%>%mutate(VECINO=grepl("vecino", QUIEN_HABLADO_CHIRI)*1)
data<- data%>%mutate(FAMILIAR=grepl("familiar", QUIEN_HABLADO_CHIRI)*1)
data<- data%>%mutate(PROFESIONAL_SALUD=grepl("profesional_salud", QUIEN_HABLADO_CHIRI)*1)
data<- data%>%mutate(OTRO=grepl("otro", QUIEN_HABLADO_CHIRI)*1)
data<- data%>%mutate(AMIGO=grepl("amigo", QUIEN_HABLADO_CHIRI)*1)


#to make new column for colegio category from quien hablado chiri otro
data<- mutate(data, COLEGIO=ifelse(OTRO==1 & (QUIEN_HABLADO_CHIRI_OTRO=="Colegio"| QUIEN_HABLADO_CHIRI_OTRO=="Colegio " | QUIEN_HABLADO_CHIRI_OTRO=="Colegio ,historietas hace 6 años" | QUIEN_HABLADO_CHIRI_OTRO== "En el colegio"), 1, 0))
#to make new column for others category from quien hablado chiri otro
data<- mutate(data, OTRO_HABLADO=ifelse(OTRO==1 & (QUIEN_HABLADO_CHIRI_OTRO== "Cochabamba" | QUIEN_HABLADO_CHIRI_OTRO== "Recolector de basura" | QUIEN_HABLADO_CHIRI_OTRO=="Hace muchos anios"), 1, 0))
#to make new column for cuando fumigaron category from quien hablado chiri otro
data<- mutate(data, CUANDO_FUMIGARON=ifelse(OTRO==1 & (QUIEN_HABLADO_CHIRI_OTRO=="Cuando fumigaron" | QUIEN_HABLADO_CHIRI_OTRO=="Hace mucho tiempo cuando vinieron a fumigar"), 1, 0))
#to make new column for gobierno regional category from quien hablado chiri otro
data<- mutate(data, GOBIERNO_REGIONAL=ifelse(OTRO==1 & (QUIEN_HABLADO_CHIRI_OTRO=="En el gobierno regional" | QUIEN_HABLADO_CHIRI_OTRO== "Los del concejo"), 1, 0))
#to make new column for university category from quien hablado chiri otro
data<- mutate(data, UNIVERSIDAD=ifelse(OTRO==1 & QUIEN_HABLADO_CHIRI_OTRO=="En la universidad ", 1, 0))
#to make new column for perifoneo category from quien hablado chiri otro
data<- mutate(data, PERIFONEO=ifelse(OTRO==1 & (QUIEN_HABLADO_CHIRI_OTRO=="En perifoneo" | QUIEN_HABLADO_CHIRI_OTRO=="Perifoneo"), 1, 0))
#to make new column for noticias category from quien hablado chiri otro
data<- mutate(data, NOTICIAS=ifelse(OTRO==1 & (QUIEN_HABLADO_CHIRI_OTRO=="Noticias" | QUIEN_HABLADO_CHIRI_OTRO== "Noticias " | QUIEN_HABLADO_CHIRI_OTRO== "Por las noticias " | QUIEN_HABLADO_CHIRI_OTRO== "Propaganda en la television hace 4 anios aproximado" | QUIEN_HABLADO_CHIRI_OTRO== "Radio"), 1, 0))
#to make new column for profesional salud from profesional salud column above plus quien hablado chiri otro
data<- mutate(data, PROFESIONAL_SALUD_TOT=ifelse(PROFESIONAL_SALUD==1 | (QUIEN_HABLADO_CHIRI_OTRO== "Alguien que vino a su casa " | QUIEN_HABLADO_CHIRI_OTRO=="Hace mucho tiempo un grupo de extranjeros dieron charlas y tomaron muestra de sangre a los ninios de la I.E" | QUIEN_HABLADO_CHIRI_OTRO=="Inspector upch"), 1, 0))
#to delete original profesional salud column created from quien hablado chiri
data$PROFESIONAL_SALUD<-NULL

#change NA to 0 in profesional_salud_tot
data$PROFESIONAL_SALUD_TOT[is.na(data$PROFESIONAL_SALUD_TOT)] <- 0

#collapse answers above to categories of profesional salud, vecino/familiar, other. *check to see if agente comunidad should be with profesional salud or with others
data<- mutate(data, QUIEN_HABLADO_CATS=ifelse(VECINO == 1 | FAMILIAR == 1 |(VECINO == 1 & FAMILIAR == 1),"vecino_familiar",
                                              ifelse(PROFESIONAL_SALUD_TOT == 1, "profesional salud",
                                                     ifelse(COLEGIO == 1 | OTRO_HABLADO == 1 | CUANDO_FUMIGARON == 1 | GOBIERNO_REGIONAL == 1 | PERIFONEO == 1 | NOTICIAS == 1 | UNIVERSIDAD == 1 | AMIGO == 1 | AGENTE_COMUNIDAD == 1, "other", 0))))

#collapse answers above to categories of profesional salud, vecino/familiar, other. *check to see if agente comunidad should be with profesional salud or with others
#data<- mutate(data, QUIEN_HABLADO_CATS=ifelse(VECINO == 1 | FAMILIAR == 1 |(VECINO == 1 & FAMILIAR == 1),"vecino_familiar", 0))
#data<- mutate(data, QUIEN_HABLADO_CATS=ifelse(PROFESIONAL_SALUD_TOT == 1 | (is.na(PROFESIONAL_SALUD_TOT)== FALSE) ,"profesional salud", 0))

#data<- mutate(data, QUIEN_HABLADO_CATS=ifelse(is.na(PROFESIONAL_SALUD_TOT) == TRUE, 0 , 1))
#ifelse(PROFESIONAL_SALUD_TOT == 0 | is.na(PROFESIONAL_SALUD_TOT), "profesional salud",
                                                 #    ifelse(COLEGIO == 1 | OTRO_HABLADO == 1 | CUANDO_FUMIGARON == 1 | GOBIERNO_REGIONAL == 1 | PERIFONEO == 1 | NOTICIAS == 1 | UNIVERSIDAD == 1 | AMIGO == 1 | AGENTE_COMUNIDAD == 1, "other", 0))))


#to extract one answer/word from the column for oido/visto chiri and make new column for each extracted word. 1 is given if extracted answer is present for participant
data<- data%>%mutate(POSTA=grepl("posta", OIDO_VISTO_INFORMACION_CHIRI)*1)
data<- data%>%mutate(COLEGIO_UNIVERSIDAD_OIDO=grepl("colegio_universidad", OIDO_VISTO_INFORMACION_CHIRI)*1)
data<- data%>%mutate(REDES_SOCIALES=grepl("redes_sociales", OIDO_VISTO_INFORMACION_CHIRI)*1)
data<- data%>%mutate(MEDIO_COMUNICACION=grepl("medio_comunicacion", OIDO_VISTO_INFORMACION_CHIRI)*1)
data<- data%>%mutate(VECINDARIO=grepl("vecindario", OIDO_VISTO_INFORMACION_CHIRI)*1)
data<- data%>%mutate(OTRO_OIDO=grepl("otro", OIDO_VISTO_INFORMACION_CHIRI)*1)

#to make new column for categories from oidi visto informacion chiri otro like above for oido/visto chiri
data<- mutate(data, CASA_FAMILIAR=ifelse(OTRO_OIDO==1 & (OIDO_VISTO_INFORMACION_CHIRI_OTRO=="Casa" | OIDO_VISTO_INFORMACION_CHIRI_OTRO== "Casa de un familiar" | OIDO_VISTO_INFORMACION_CHIRI_OTRO=="Familiares" | OIDO_VISTO_INFORMACION_CHIRI_OTRO=="Y  en mi domicilio"), 1, 0))
data<- mutate(data, CALLE=ifelse(OTRO_OIDO==1 & (OIDO_VISTO_INFORMACION_CHIRI_OTRO=="En el mercado San Camilo hace años" | OIDO_VISTO_INFORMACION_CHIRI_OTRO=="En un mercado" | OIDO_VISTO_INFORMACION_CHIRI_OTRO=="Characato" | OIDO_VISTO_INFORMACION_CHIRI_OTRO=="En Cerro colorado " |OIDO_VISTO_INFORMACION_CHIRI_OTRO=="En la calle " |OIDO_VISTO_INFORMACION_CHIRI_OTRO=="En las calles perifoneo" | OIDO_VISTO_INFORMACION_CHIRI_OTRO=="En otro distrito" |OIDO_VISTO_INFORMACION_CHIRI_OTRO=="Hace 10 anios en Miraflores" |OIDO_VISTO_INFORMACION_CHIRI_OTRO=="Otro distrito"), 1, 0))
data<- mutate(data, OTRO_OIDO1=ifelse(OTRO_OIDO==1 & (OIDO_VISTO_INFORMACION_CHIRI_OTRO=="En la Ciudad de Ica,vio chirimachas y le hablo el personal de salud" |OIDO_VISTO_INFORMACION_CHIRI_OTRO=="En Nazca Ica hace años" | OIDO_VISTO_INFORMACION_CHIRI_OTRO=="Hace 10 anios un joven fue a donar sangre para un familiar y le dijeron que no podia porque tenia el mal de chagas" |OIDO_VISTO_INFORMACION_CHIRI_OTRO=="Hace tiempo cuando hicieron campaña" |OIDO_VISTO_INFORMACION_CHIRI_OTRO=="Por una capacitacion" |OIDO_VISTO_INFORMACION_CHIRI_OTRO=="En un documental"), 1, 0))
data<- mutate(data, REDES_SOCIALES_OIDO=ifelse(REDES_SOCIALES==1 | OTRO_OIDO==1 & (OIDO_VISTO_INFORMACION_CHIRI_OTRO=="Busca informacion" | OIDO_VISTO_INFORMACION_CHIRI_OTRO== "Internet"), 1, 0))
data<- mutate(data, MEDIO_COMUNICACION_OIDO=ifelse(MEDIO_COMUNICACION==1 | (OTRO_OIDO==1 & OIDO_VISTO_INFORMACION_CHIRI_OTRO=="En un periodico hace 3 anios"), 1, 0))
data$MEDIO_COMUNICACION<-NULL
data$REDES_SOCIALES<-NULL
data$OTRO_OIDO<-NULL


#create a new data frame to more easily view work being done below
data1<- as.data.frame(data[,c(1,12)])
#filter donde busca chiri and create columns for keywords that are filtered. 1 is given for participants that answer filtered keyword
data1$animal <- as.integer(grepl("animales|animal", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$ladrillo <- as.integer(grepl("ladrillo|ladrillos|ladrllos|kadrillos", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$sillar <- as.integer(grepl("sillares|sillar", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$piedra <- as.integer(grepl("piedras|piedra|pidras", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$adobe <- as.integer(grepl("adobe", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$cama <- as.integer(grepl("cama|camas|colchones", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$detras <- as.integer(grepl("detras|detrás", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$patio <- as.integer(grepl("patio", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$cuarto <- as.integer(grepl("cuarto|cuartos|habitacion|habitaciones|dormitorio|vivienda", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$techo <- as.integer(grepl("techos|techo", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$tierra <- as.integer(grepl("tierra|chacra", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$grieta <- as.integer(grepl("grietas|grieta|grietad", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$rincon <- as.integer(grepl("rincon|rincones", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$humedo <- as.integer(grepl("humedo|humedos|humedad|húmedas|húmedos|humedas", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$oscuro <- as.integer(grepl("oscuro|oscuros|oscura|oscuras|luz|oscuridad|oacuros", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
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
data1$pirca <- as.integer(grepl("pirca|pircado|pircas|pircados", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$cajones <- as.integer(grepl("cajones", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$almacen <- as.integer(grepl("almacen|almacenes", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$papel <- as.integer(grepl("papel|papeles", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$cuadro <- as.integer(grepl("cuadro|cuadros", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$descampado <- as.integer(grepl("descampado|descampados", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$taller <- as.integer(grepl("taller", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$no_mueven <- as.integer(grepl("mueven|movemos|mue", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$cocina <- as.integer(grepl("cocina", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$tabla <- as.integer(grepl("tabla|tablas", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$deposito <- as.integer(grepl("deposito|depósitos", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$baño <- as.integer(grepl("baño|baños", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$agua <- as.integer(grepl("agua|aguas", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$casa <- as.integer(grepl("casa|casas", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$ventana <- as.integer(grepl("ventanas", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$alfombra <- as.integer(grepl("alfombras", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$acampado <- as.integer(grepl("acampados", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$escombro <- as.integer(grepl("escombros|desmontes", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$mueble <- as.integer(grepl("mueble|muebles", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$escondido <- as.integer(grepl("escondidos|ocultas|esconden", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$seco <- as.integer(grepl("secos", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$agujero <- as.integer(grepl("agujeros", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$abandonado <- as.integer(grepl("abandonado|abandonada", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$no_estucar <- as.integer(grepl("estucar|estucadas", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$cochera <- as.integer(grepl("cochera", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$comoda <- as.integer(grepl("comodas", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$aves <- as.integer(grepl("aves", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$viejo <- as.integer(grepl("viejas", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$ropero <- as.integer(grepl("roperos", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$oveja <- as.integer(grepl("ovejas", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$estera <- as.integer(grepl("esteras", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$pasto <- as.integer(grepl("pasto", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$granja <- as.integer(grepl("granjas", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$grasa <- as.integer(grepl("grasa", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$establo <- as.integer(grepl("establo", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$piso <- as.integer(grepl("piso", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$desagues <- as.integer(grepl("desgues|desagüe", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))
data1$trapos <- as.integer(grepl("trapos", data$DONDE_BUSCA_CHIRI, ignore.case = TRUE))


#to make columns containing filtered words above. basically condensing columns from above
data1$animales <- ifelse(data1$animal == 1 | data1$cuy == 1 | data1$perro == 1 | data1$conejo == 1 | data1$gallina == 1 | data1$corral == 1 | data1$pollo == 1 | data1$aves == 1 | data1$oveja == 1, 1, 0)
data1$disorganizacion <- ifelse(data1$acumulado == 1 | data1$amontonada == 1 | data1$basura == 1 | data1$guardadas ==1 | data1$no_mueven == 1 | data1$viejo == 1 | data1$abandonado == 1, 1, 0)
data1$tierras <- ifelse(data1$tierra == 1 | data1$jardin == 1 | data1$pasto == 1, 1, 0)
data1$grietas <- ifelse(data1$grieta == 1 | data1$rendija == 1 | data1$hueco == 1 | data1$agujero == 1 | data1$no_estucar == 1, 1, 0)
data1$sucios <- ifelse(data1$sucio == 1 | data1$no_limpia == 1 | data1$grasa == 1, 1, 0)
data1$esquinas <- ifelse(data1$esquina == 1 | data1$rincon == 1, 1, 0)
data1$NS <- ifelse(data1$no_sabe == 1, 1, 0)
data1$ladrillos <- ifelse(data1$ladrillo == 1, 1, 0)
data1$sillares <- ifelse(data1$sillar == 1, 1, 0)
data1$piedras <- ifelse(data1$piedra == 1, 1, 0)
data1$adobe <- ifelse(data1$adobe == 1, 1, 0)
data1$bloquetas <- ifelse(data1$bloqueta == 1, 1, 0)
data1$maderas <- ifelse(data1$madera == 1, 1, 0)
data1$detras_cosas <- ifelse(data1$detras == 1, 1, 0)
data1$en_casa <- ifelse(data1$tabla ==1 | data1$cajones == 1 | data1$pared == 1 | data1$baño == 1 | data1$cocina == 1 | data1$cuadro == 1 | data1$papel == 1 | data1$casa == 1 | data1$ropero == 1 | data1$comoda == 1 | data1$estera == 1 | data1$alfombra == 1 | data1$desagues == 1 | data1$trapos == 1 | data1$ventana == 1 | data1$mueble == 1 | data1$piso == 1, 1, 0)
data1$humedos <- ifelse(data1$humedo == 1, 1, 0)
data1$oscuros <- ifelse(data1$oscuro == 1, 1, 0)
data1$patios <- ifelse(data1$patio == 1, 1, 0)
data1$techos <- ifelse(data1$techo ==1, 1, 0)
data1$cuartos <- ifelse(data1$cuarto ==1, 1, 0)
data1$fuera_de_casa <- ifelse(data1$planta == 1 | data1$almacen == 1 | data1$descampado == 1 | data1$taller == 1 | data1$deposito == 1 | data1$agua == 1 | data1$granja == 1 | data1$establo == 1 | data1$escombro == 1 | data1$acampado == 1, 1, 0)
data1$escondidos <- ifelse(data1$escondido == 1, 1, 0)
data1$secos <- ifelse(data1$seco == 1, 1, 0)
data1$camas <- ifelse(data1$cama == 1, 1, 0)
data1$pircas <- ifelse(data1$pirca == 1, 1, 0)

#create a new column that collapses the categories from above into groups of "correct"(aligns with what the ministry of health tells people to look for/looks for), "incorrect"(could be accurate, but not what the ministry stresses), and ?
#This should place responses that contain the answer in the groups that have that (ex: response = "corrales de animales, lugares secos, debajo de cama" will go in the correct column bc it contains corrales de animales, which is "correct")
data1<- mutate(data1, DONDE_BUSCA_CATS=ifelse(animales == 1 | disorganizacion == 1 | patios == 1 | techos == 1 | cuartos == 1, "correct-ministry", "other"))


#view answers to do you know if chiris have been found in your zone recently and how did you hear about it
table(data$RECIENTEMENTE_CHIRI_ZONA)
table(data$COMO_ENTERADO)
#either heard from neighbors or they found them
table(data$COMO_ENTERADO_OTRO)

#answers to questions about seeing ministerio de salud or ucph in neighborhood, if they knocked on the door, how long it has been, if they let them in/why not
table(data$ESTE_ANIO_HA_VISTO_PERSONAL_SALUD)
table(data$ULTIMO_ANIO_PERSONAL_SALUD_TOCO_PUERTA_CASA)
table(data$HACE_CUANTO_TIEMPO_VISITARON)
table(data$PERMITIO_INSPECTOR_INGRESE_CASA)
table(data$RAZON_NO_PERMITIO_INSPECTOR_INGRESE_CASA)
table(data$RAZON_NO_PERMITIO_INSPECTOR_INGRESE_CASA_OTRO)

#answers to questions about using nearby bodegas
table(data$VA_BODEGAS_CERCANAS)
table(data$FRECUENCIA_COMPRA_BODEGA)

#see if theres connection between going to bodegas a lot and seeing alerta chiri flyers/posters
table(data$VA_BODEGAS_CERCANAS, data$HA_VISTO_IMAGEN_MOSTRADA)
table(data$VA_BODEGAS_CERCANAS, data$VIO_IMAGEN_1)

#view answers to hace uso app
table(data$HACE_USO_APP)

#to extract one answer/word from hace uso app column. New column is created and 1 is given if extracted answer is present
data<- data%>%mutate(FACEBOOK=grepl("facebook", HACE_USO_APP)*1)
data<- data%>%mutate(INSTAGRAM=grepl("instagram", HACE_USO_APP)*1)
data<- data%>%mutate(WHATSAPP=grepl("whatsapp", HACE_USO_APP)*1)
data<- data%>%mutate(TIKTOK=grepl("tiktok", HACE_USO_APP)*1)
data<- data%>%mutate(NINGUNA=grepl("ninguna_anteriores", HACE_USO_APP)*1)

#view answers to ha visto imagen mostrada
table(data$HA_VISTO_IMAGEN_MOSTRADA)

#view answers to donde los ha visto
table(data$VIO_IMAGEN_1)

#view answers to donde los ha visto OTRO
table(data$VIO_IMAGEN_1_OTRO)
table(data$VIO_IMAGEN_2)
table(data$VIO_IMAGEN_2_OTRO)
table(data$VIO_IMAGEN_3)
table(data$VIO_IMAGEN_3_OTRO)

#group otro answers and make new column for each answer category
#for vio_otro_cs one person gave two answers that go into this category (vio_imagen_1 and vio_imagen_2) so table(data$VIO_OTRO_CS) shows 10, but there are actually 11
data<- mutate(data, VIO_OTRO_CS=ifelse(VIO_IMAGEN_1_OTRO == "C.s san Martín de socabaya "  | VIO_IMAGEN_1_OTRO == "C.s tiabaya" | VIO_IMAGEN_1_OTRO == "C.s. ampliación paucarpata " | VIO_IMAGEN_1_OTRO == "C.s. ciudad blanca" | VIO_IMAGEN_1_OTRO == "C.s. sabandia" | VIO_IMAGEN_1_OTRO == "C.s. Tiabaya" | VIO_IMAGEN_1_OTRO == "Centro de .Salud San Martin de Socabaya" | VIO_IMAGEN_1_OTRO == "Cs" | VIO_IMAGEN_1_OTRO == "Un afiche en un CS de Camana" | VIO_IMAGEN_1_OTRO == "Posta de San martin  cs.ciudad mi trabajo" | data$VIO_IMAGEN_2_OTRO == "Centro de Salud lara", 1, 0))
#for vio_otro_ps one person gave two answers that go into this category (vio_imagen_1 and vio_imagen_2) so table(data$VIO_OTRO_PS) shows 26, but there are actually 27 
data<- mutate(data, VIO_OTRO_PS=ifelse(VIO_IMAGEN_1_OTRO == "En la posta" | VIO_IMAGEN_1_OTRO == "P.s Villa San Juan" | VIO_IMAGEN_1_OTRO == "P.s. pampa de camarones" | VIO_IMAGEN_1_OTRO == "P.s. sabandia" | VIO_IMAGEN_1_OTRO == "P.s. sabandia " | VIO_IMAGEN_1_OTRO == "P.s. sachaca" | VIO_IMAGEN_1_OTRO == "P.s. san Juan " | VIO_IMAGEN_1_OTRO == "P.S. villa jesus" | VIO_IMAGEN_1_OTRO == "P.s. villa san juan" | VIO_IMAGEN_1_OTRO == "P.s. Villa San Juan" | VIO_IMAGEN_1_OTRO == "Posta" | VIO_IMAGEN_1_OTRO == "Posta de San martin  cs.ciudad mi trabajo" | VIO_IMAGEN_1_OTRO == "Posta Francisco Bolognesi" | VIO_IMAGEN_1_OTRO == "Socabaya ps lara" | VIO_IMAGEN_2_OTRO == "P.s. 4 de octubre " | VIO_IMAGEN_2_OTRO == "Posta", 1, 0))
data<- mutate(data, VIO_OTRO_OTRO=ifelse(VIO_IMAGEN_1_OTRO == "En la television" | VIO_IMAGEN_1_OTRO == "En la televisión " | VIO_IMAGEN_1_OTRO == "Mercado Israel " | VIO_IMAGEN_1_OTRO == "No se acuerda el lugar exacto" | VIO_IMAGEN_1_OTRO == "Noticieros" | VIO_IMAGEN_1_OTRO == "Pegado en el gobierno regional" | VIO_IMAGEN_1_OTRO == "Pegado en un poste" | VIO_IMAGEN_1_OTRO == "Personal de salud " | VIO_IMAGEN_1_OTRO == "Por Socabaya pegado en un poste ,cuando visitaba un amigo" | VIO_IMAGEN_1_OTRO == "Television"| VIO_IMAGEN_1_OTRO == "Un Familiar le mostro un volante"  | VIO_IMAGEN_2_OTRO == "En los postes" | VIO_IMAGEN_3_OTRO == "En el cruce 3 de octubre", 1, 0 ))





#download data to excel file
write.csv(data, file="C:/Users/zoeea/OneDrive/Documents/Immune/data from r.csv", row.names = FALSE)
