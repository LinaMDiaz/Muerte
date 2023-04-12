
##################################################################################################
########### Cargue de información  y transformación de los datos #################################
###################################################################################################

## cleaning workspace
rm(list=ls())

## Leer los datos de defunciones

library("tidyverse")
library("readxl")
library("mgcv")
library("zoo")
library("lubridate")
library("dplyr")
library(tidyr)
library(lubridate)
library(here)
library(EpiWeek)
library(ggplot2)
library(epiR)
library(scales)
library(zoo)
library(hrbrthemes)
library(tidyverse)



SAVE <- F

if (!dir.exists("datos")) {dir.create("datos")}

#################################################################################################################################
############################ Procesamiento datos mortalidad por todas las causas ################################################
#################################################################################################################################

## datos defunciones todas las causas


def2020<-read.csv("MORTALIDAD_VALLE2020-2022/Valle_resnf_2020.csv", sep=";", row.names=NULL)
def2021<-read.csv("MORTALIDAD_VALLE2020-2022/Valle_resf_2021.csv", sep=";", row.names=NULL)
def2015<-read.csv("MORTALIDAD2015-2019/Valle_resNF_2015.csv", sep=";", row.names=NULL,fileEncoding="latin1")
def2016<-read.csv("MORTALIDAD2015-2019/Valle_resNF_2016.csv", sep=";", row.names=NULL,fileEncoding="latin1")
def2017<-read.csv("MORTALIDAD2015-2019/Valle_resNF_2017.csv", sep=";", row.names=NULL,fileEncoding="latin1")
def2018<-read.csv("MORTALIDAD2015-2019/Valle_resNF_2018.csv", sep=";", row.names=NULL,fileEncoding="latin1")
def2019<-read.csv("MORTALIDAD2015-2019/Valle_resNF_2019.csv", sep=";", row.names=NULL,fileEncoding="latin1")

## agregar año a dataframes

def2015$ANO<- 2015
def2016$ANO<- 2016
def2017$ANO<- 2017
def2018$ANO<- 2018
def2019$ANO<- 2019
def2020$ANO<- 2020
def2021$ANO<- 2021

##### Unir tablas de mortalidad de los diferentes años, en una sola BD

def2015<-mutate_at(def2015, vars(COD_LOCA,LOCALOCUHE,COD_INST,NRO_DOCM), as.character)
def2016<- mutate_at(def2016, vars(COD_LOCA,LOCALOCUHE,COD_INST), as.character)
def2017<- mutate_at(def2017, vars(COD_LOCA,LOCALOCUHE,COD_INST,NRO_DOCM), as.character)
def2018<- mutate_at(def2018, vars(COD_LOCA,LOCALOCUHE,COD_INST,NRO_DOCM), as.character)
def2019<- mutate_at(def2019, vars(COD_LOCA,LOCALOCUHE,COD_INST,NRO_DOCM), as.character)
def2020<- mutate_at(def2020, vars(COD_LOCA,LOCALOCUHE,COD_INST,NRO_DOCM), as.character)
def2021<- mutate_at(def2021, vars(COD_LOCA,LOCALOCUHE,COD_INST,NRO_DOCM), as.character)


dfmortalidad<- bind_rows(def2015,def2016,def2017,def2018,def2019,def2020,def2021)

######### Calcular semana de defunción ######################

dfmortalidad$FECHA_DEF<-as.POSIXct(dfmortalidad$FECHA_DEF, format="%d/%m/%Y")

###### Calculo de semana epidemiologica empezando desde el primer domingo del Mes según el estandar del CDC

options(epiyear.start.day = "Sun")
dfmortalidad$SemanaEpi<-epiweek(dfmortalidad$FECHA_DEF)

##### Crear una nueva columna para el codigo de municipio en los datos de mortalidad por año

Create_CodMuni <- function(base_def) {
  base_def$COD_DEPTO<-76
  base_def$COD_MUNIC1<- sprintf("%03d",base_def$CODMUNRE)
  base_def$COD_MUNICIPIO <- paste0(base_def$COD_DEPTO,base_def$COD_MUNIC1)
  return(base_def)
}

dfmortalidad<-Create_CodMuni(dfmortalidad)

########## recodificar variables

oldvalues <- c(1, 2)
newvalues <- factor(c("Femenino","Masculino")) 

dfmortalidad$Sexo <- newvalues[ match(dfmortalidad$SEXO, oldvalues)]

oldvalues <- c(1,2,3,4,5,6,7)
newvalues <- factor(c("Menor de 1 año", "1 a 4 años","5 a 14 años","15 a 44 años","45 a 64 años","65 y más años","Edad desconocida")) 

dfmortalidad$GrupoEdad <- newvalues[ match(dfmortalidad$GRU_ED2, oldvalues)]

oldvalues <- c(1,2,3,4,5,6,9)
newvalues <- factor(c("Indígena", "Room","Raizal","Palenquero de San Basilio","Negro(a),mulato(a),afrocolombiano(a)","Ninguna de las anteriores","Sin información"))

dfmortalidad$IDPERTET <- newvalues[ match(dfmortalidad$IDPERTET, oldvalues)]


oldvalues <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,99)
newvalues <- factor(c("Preescolar", "Básica primaria","Secundaria","Media","Media","Normalista","Técnica", "Técnica","Profesional","Especialización","Maestría", "Doctorado","Ninguno","Sin información"))

dfmortalidad$NIVEL_EDU <- newvalues[ match(dfmortalidad$NIVEL_EDU, oldvalues)]

oldvalues <- c(1,2,3,4,5,9)
newvalues <- factor(c("Contributivo", "Subsidiado","Excepción","Especial","No asegurado","Sin información"))

dfmortalidad$SEG_SOCIAL <- newvalues[ match(dfmortalidad$SEG_SOCIAL, oldvalues)]

oldvalues <- c(1,2,3,4,5,6,9)
newvalues <- factor(c("No casado(a) con covivencia mayor a 2 años", "No casado(a) convivencia menor a 2 años","Separado(a)","Viudo(a)","Soltero(a)","Casado(a)", "Sin información"))

dfmortalidad$EST_CIVIL<-newvalues[ match(dfmortalidad$EST_CIVIL, oldvalues)]


oldvalues <- c(1,2,3)
newvalues <- factor(c("Natural", "Violenta","En estudio"))

dfmortalidad$PMAN_MUER<-newvalues[match(dfmortalidad$PMAN_MUER, oldvalues)]


saveRDS(dfmortalidad, file = "datos/dfmortalidad.RDS")

############################################################################################################################################
#######################################Procesamiento Datos mortalidad COVID-19 ##############################################################
############################################################################################################################################

defCOVID2020_2022<-read.csv("MORTALIDADCOVID2020-2022/Mortalidad_Valle_COVID_2020_2022.csv", sep=";", row.names=NULL,fileEncoding="latin1")

defCOVID2020_2022$fec_def<-as.POSIXct(defCOVID2020_2022$fec_def, format="%d/%m/%Y")

defCOVID2020_2022$ANO<- year(as.Date(defCOVID2020_2022$fec_def))

defCOVID2020_2022$GrupoEdad <- cut(defCOVID2020_2022$edad, c(-1, 4, 14,44,64, Inf), labels = c("1 a 4 años", "5 a 14 años", "15 a 44 años", "45 a 64 años", "65 y más años"))

oldvalues <- c("F","M")
newvalues <- factor(c("Femenino","Masculino")) 

defCOVID2020_2022$Sexo <- newvalues[ match(defCOVID2020_2022$sexo, oldvalues)]


saveRDS(defCOVID2020_2022, file = "datos/defCOVID2020_2022.RDS")


############################################################################################################################################
########################################Bases de datos confirmados COVID-19 ###############################################################
##############################################################################################################################################

CASOSCOVID2020<-read.csv("MORTALIDADCOVID2020-2022/CASOS_COVID2020.csv", sep=";", row.names=NULL,fileEncoding="latin1")
CASOSCOVID2021<-read.csv("MORTALIDADCOVID2020-2022/CASOS_COVID2021.csv", sep=";", row.names=NULL,fileEncoding="latin1")

Casoscovidunion<- bind_rows(CASOSCOVID2020,CASOSCOVID2021)

Casoscovidunion$Fecha.de.notificacion<-as.POSIXct(Casoscovidunion$Fecha.de.notificacion, format="%d/%m/%Y")
Casoscovidunion$Fecha.de.consulta<-as.POSIXct(Casoscovidunion$Fecha.de.consulta, format="%d/%m/%Y")
Casoscovidunion$Fecha.Inicio.Sintomas<-as.POSIXct(Casoscovidunion$Fecha.Inicio.Sintomas, format="%d/%m/%Y")

saveRDS(Casoscovidunion, file = "datos/Casoscovidunion.RDS")

#################################################################################################################################################
########## ##########################################Datos Población por año DANE ####################################################################
################################################################################################################################################

pobla<-read.csv('POBLACION_DANE/Datos_Poblacionmunicipios.csv',header=T,fileEncoding = "Latin1",check.names = F,sep=";")

poblavalle<-dplyr::filter(pobla,DP==76)

#### utilizar proyecciones de población total para los años 2015-2021

poblavalle<-poblavalle%>%filter(`AÑO`>= 2015 & `AÑO`<= 2022, `ÁREA GEOGRÁFICA` == "Total")

names(poblavalle)[5] <- "ANO"

saveRDS(poblavalle, file = "datos/poblavalle.RDS")

##################################################################################################################################################
################################################  Datos población DANE valle por sexo y edad #####################################################
#################################################################################################################################################

PobDANEEDAD<-read.csv("POBLACION_DANE/PoblacionDANE_SEXOEDAD.csv", sep=";", row.names=NULL,fileEncoding="latin1")

PobDANEEDAD$GrupoEdad <- cut(PobDANEEDAD$edad, c(-1, 4, 14,44,64, Inf), labels = c("1 a 4 años", "5 a 14 años", "15 a 44 años", "45 a 64 años", "65 y más años"))

saveRDS(PobDANEEDAD, file = "datos/pobDANEEDAD.RDS")


######################################################################################################################################################
######################################### Datos sociodemograficos ##################################################################################
###################################################################################################################################################3
#### Carga de datos


analf<-read.csv("DATOS_DANE/Analfab.csv", header=T,sep=";")
bajologred<-read.csv("DATOS_DANE/bajologroeduc.csv", header=T,sep=";")
barrerasalud<-read.csv("DATOS_DANE/barrerasaccesosalud.csv", header=T,sep=";")
barrerascuidinf<-read.csv("DATOS_DANE/barrerascuidadoinfancia.csv", header=T,sep=";")
Hacicrit<-read.csv("DATOS_DANE/Hacinamientocritico.csv", header=T,sep=";")
ipm<-read.csv("DATOS_DANE/IPM.csv", header=T,sep=";")
matpared<-read.csv("DATOS_DANE/materialinadecuadoparedes.csv", header=T,sep=";")
matpisos<-read.csv("DATOS_DANE/Materialinadecuadopisos.csv", header=T,sep=";")

###### Codigos de municipios

cod_mun<-read.csv("DATOS_DANE/Cod_municipios.csv", header=T,sep=";")
