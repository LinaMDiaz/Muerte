
## Libraries
library(dplyr)
library(gsubfn)

source("01-Descriptivas_mortalidad.R")

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



############### Asignar los nombres a las bases de datos

df_list <- c("analf", "bajologred","barrerasalud","barrerascuidinf","Hacicrit","ipm","matpared","matpisos")

nombres <- c("analf", "bajologred","barrerasalud","barrerascuidinf","Hacicrit","ipm","matpared","matpisos")

# Iterar a través de una lista de data frames

for (i in 1:length(df_list)) {
  # Get the data frame
  df <- get(df_list[i])
  # Create a new column and fill it with a letter
  
  df[["informa"]] <- nombres[i]
  # Store the modified data frame back in its original variable
  assign(df_list[i], df)
}

############################# Filtra las categorias de privación en cada base de datos y luego las une #######

df_list <- list(analf,bajologred,barrerasalud,barrerascuidinf,Hacicrit,ipm,matpared,matpisos)


final_df <- data.frame()

# Iterate through the list of data frames
for (df in df_list) {
  # Apply a condition
  
  df<- replace(df, df=='', NA)
  df=df[complete.cases(df), ] 
  filtered_df <- df %>% filter(Tipo=="Privacion")
  
  
  # Append the filtered rows to the final data frame
  final_df <- rbind(final_df, filtered_df)
}



# use your folder location instead of "~/"
#write.csv(x = dataframe.list[[i]], file = paste0("~/", "dataframe_for_city_", city,".csv")) 

#### Traer información de los Codigos de municipios del Valle del Cauca

filter_and_cbind <- function(value) {
  cbind(cod_mun,final_df[ final_df[["informa"]] == value, c("Tipo", "por", "informa")])
}


filter_values <- c("analf", "bajologred","barrerasalud","barrerascuidinf","Hacicrit","ipm","matpared","matpisos")

# Apply the function to each value in the list
filtered_dfs <- lapply(filter_values, function(value) {
  filter_and_cbind(value)
})


# Combinando los resultados de la lista en un data frame
final_df2 <- do.call(rbind,filtered_dfs)

#### Reemplazar "," por punto y descartar %

final_df2$por <- gsub("%", "", final_df2$por)

# replace comma with decimal point
final_df2$por <- gsubfn(",", ".", final_df2$por)


##### Variables demográficas por columna

final_dfsp<- tidyr:: spread(final_df2, key =informa, value = por)

names(final_dfsp)[2] <- "COD_MUNICIPIO"

df_SOCIODEMO<-final_dfsp%>%left_join(Tasas_ANO, by = c("COD_MUNICIPIO"))

df_SOCIODEMO<-df_SOCIODEMO%>% select(Descripcion.Municipio,analf,bajologred,barrerasalud,barrerascuidinf,Hacicrit,ipm,matpared,matpisos,Tasa2015,Tasa2016,Tasa2017,Tasa2018,Tasa2019,Tasa2020,Tasa2021)

colnames(df_SOCIODEMO)[colnames(df_SOCIODEMO) == "analf"] <- "Analfabetismo"
colnames(df_SOCIODEMO)[colnames(df_SOCIODEMO) == "bajologred"] <- "Bajologroeducativo"
colnames(df_SOCIODEMO)[colnames(df_SOCIODEMO) == "barrerasalud"] <- "Barrerasdesalud"
colnames(df_SOCIODEMO)[colnames(df_SOCIODEMO) == "barrerascuidinf"] <- "Cuidadoprimerainfancia"
colnames(df_SOCIODEMO)[colnames(df_SOCIODEMO) ==  "Hacicrit"] <- "Hacinamientocrítico"
colnames(df_SOCIODEMO)[colnames(df_SOCIODEMO) ==  "ipm"] <- "Pobrezamultidimensional"
colnames(df_SOCIODEMO)[colnames(df_SOCIODEMO) ==  "matpared"] <- "Materialdeparedes"
colnames(df_SOCIODEMO)[colnames(df_SOCIODEMO) ==  "matpisos"] <- "Materialdepisos"



####### Analisis Factorial multiple ###############

#### Descripción de los datos

df_SOCIODEMO$Analfabetismo <- as.numeric(df_SOCIODEMO$Analfabetismo)
df_SOCIODEMO$Bajologroeducativo<- as.numeric(df_SOCIODEMO$Bajologroeducativo)
df_SOCIODEMO$Barrerasdesalud<- as.numeric(df_SOCIODEMO$Barrerasdesalud)
df_SOCIODEMO$Cuidadoprimerainfancia <- as.numeric(df_SOCIODEMO$Cuidadoprimerainfancia)
df_SOCIODEMO$Hacinamientocrítico<- as.numeric(df_SOCIODEMO$Hacinamientocrítico)
df_SOCIODEMO$Pobrezamultidimensional <- as.numeric(df_SOCIODEMO$Pobrezamultidimensional)
df_SOCIODEMO$Materialdeparedes<- as.numeric(df_SOCIODEMO$`Materialdeparedes`)
df_SOCIODEMO$Materialdepisos<- as.numeric(df_SOCIODEMO$Materialdepisos)

##### Mapa de Calor sociodemográficas

df_SOCIODEMO1<-df_SOCIODEMO%>% select(Descripcion.Municipio,Analfabetismo,Bajologroeducativo,Barrerasdesalud,Cuidadoprimerainfancia,Hacinamientocrítico,Pobrezamultidimensional,Materialdeparedes,Materialdepisos)
rownames(df_SOCIODEMO1) <- df_SOCIODEMO1$Descripcion.Municipio
df_SOCIODEMO1<-df_SOCIODEMO1%>% select(Analfabetismo,Bajologroeducativo,Barrerasdesalud,Cuidadoprimerainfancia,Hacinamientocrítico,Pobrezamultidimensional,Materialdeparedes,Materialdepisos)


install.packages('pheatmap') # if not installed already
library(pheatmap)
pheatmap(df_SOCIODEMO1,annotation_row = df_SOCIODEMO1, display_numbers = T)


##### Mapa de calor tasas de mortalidad 
df_SOCIODEMO2<-df_SOCIODEMO%>% select(Descripcion.Municipio,Tasa2015,Tasa2016,Tasa2017,Tasa2018,Tasa2019,Tasa2020,Tasa2021)
rownames(df_SOCIODEMO2) <- df_SOCIODEMO2$Descripcion.Municipio
df_SOCIODEMO2<-df_SOCIODEMO2%>% select(Tasa2015,Tasa2016,Tasa2017,Tasa2018,Tasa2019,Tasa2020,Tasa2021)
library(pheatmap)
pheatmap(df_SOCIODEMO2,annotation_row = df_SOCIODEMO2, display_numbers = T)

describe(df_SOCIODEMO2)

#########

str(df_SOCIODEMO)

library(psych)
describe(df_SOCIODEMO)
library(xts)
library(zoo)
library(PerformanceAnalytics)
chart.Correlation(df_SOCIODEMO, histogram=TRUE, pch=20)

####

cor.plot(cor(df_SOCIODEMO),
         main="Mapa de correlaciones", 
         diag=F,
         show.legend = TRUE,cex.axis =0.87,las=2) 




#### Análisis de cluster para las variables

scaled_data <- scale(df_SOCIODEMO)

wss <- c()
for (i in 1:10) {
  kmeans_model <- kmeans(scaled_data, centers = i, nstart = 25)
  wss[i] <- kmeans_model$tot.withinss
}
plot(1:10, wss, type = "b", xlab = "Número de clusters", ylab = "Total del error cuadrado medio")

kmeans_model <- kmeans(scaled_data, centers = 3, nstart = 25)

summary(kmeans_model)
library(cluster)
clusplot(scaled_data, kmeans_model$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)




