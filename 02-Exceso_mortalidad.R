
#######################################################################
######## Estimando el exceso de mortalidad #########################
#############################################################################

source("00-Obtener_datos_mortalidad.R")

library("dplyr")
library("tidyverse")
library("readxl")
library("mgcv")


########## Se une la tabla de mortalidad con la información organizada por años con los datos de población por municipio

#### Se hace inicialmente la prueba con Cali pero se espera automatizar para los demás municipios

my.df.long<- dfmortalidad%>% group_by(COD_MUNICIPIO,ANO,SemanaEpi)%>%summarise(muertes=n())


my.df.long$COD_MUNICIPIO<-as.numeric(my.df.long$COD_MUNICIPIO)

df <- my.df.long%>%left_join(poblavalle, by = c("COD_MUNICIPIO", "ANO")) %>% 
  mutate(Poblacion=Total)
names(df)[names(df) == "MPNOM"] <- "Municipio"


df <- df %>% 
  filter(!(COD_MUNICIPIO==76999))


#######################Agrupar la mortalidad por semana ###########################################

# Extender el data frame por en columnas por "Semana"

dfmortalidad_gropmuni<- spread(df,SemanaEpi,muertes)


#######

df_menos <- df %>%
  filter(!(Municipio %in% c("Buenaventura", "Candelaria","Cartago","Guadalajara de Buga","Palmira", "Jamundí","Tuluá","Yumbo","Cali")))

df_filtered <- df %>%
  filter(Municipio %in% c("Buenaventura", "Candelaria","Cartago","Guadalajara de Buga","Palmira", "Jamundí","Tuluá","Yumbo"))


my.df.cali <- filter(df, Municipio=="Cali")



############################## 


### menos frecuentes
ggplot(data =df_menos, aes(x=SemanaEpi,y=muertes,colour=factor(ANO))) +
  geom_line() +
  scale_color_brewer(palette = "BrBG") +
  facet_wrap(.~Municipio)

## Graficando muertes por semana en cada municipio(otros)
ggplot(data =df_filtered, aes(x=SemanaEpi,y=muertes,colour=factor(ANO))) +
  geom_line() +
  scale_color_brewer(palette = "BrBG") +
  facet_wrap(.~Municipio)


##### Graficando muertes de cali
ggplot(data =my.df.cali, aes(x=SemanaEpi,y=muertes,colour=factor(ANO))) +
  geom_line() 



####### Descriptivos de mortalidad media, coeficiente de variación #########

options(OutDec = ",")

mydata_summary <- df %>%filter(! (ANO==2020 |ANO==2021))%>%
  group_by(Municipio) %>%
  summarise(mean_count = mean(muertes),
            var_count = var(muertes),cv=var_count/mean_count)

write.csv(mydata_summary, "D:/MaestriaEpidem/Semestre IV/Trabajo_investigación/Proyecto_tesis/DATOS_MORTALIDAD/mydata.csv")

# reset the decimal separator to a period
options(OutDec = ".")


#################### Calcular el exceso de mortalidad por cada municipio #################################

muni <- unique(df$COD_MUNICIPIO)
n_muni<-length(muni)

i <- 1
for (i in 1:n_muni){
  cat("analysing", muni[i],"\n")  
  my.muni <- muni[i]


## pop size
my.pop <- poblavalle[poblavalle$COD_MUNICIPIO==76036 ,] 
names(my.pop)[names(my.pop) == "MPNOM"] <- "Municipio"

## compute population for each week by interpolating 
## and extrapolating

## dataframe

df <- subset(my.df.long,COD_MUNICIPIO==76036) %>% 
  left_join(my.pop, by = c("COD_MUNICIPIO", "ANO")) %>% 
  mutate(Poblacion=Total)

df$tiempo <- seq(1, nrow(df))


## create exposure (person-weeks)
df$exposicion <- df$Poblacion * 7 / 365.25

## remove last few observations
df1 <- df %>% 
  filter(!(ANO==2020| ANO==2021))

df.mean <- df %>% 
  filter(! (ANO==2020 |ANO==2021)) %>% 
  group_by(SemanaEpi) %>% 
  summarise(deaM=mean(muertes))

## fitting gam (neg binomial)
fit.gam <- gam(muertes ~ factor(SemanaEpi) + s(tiempo),
               family=nb(), offset= log(exposicion),
               data=df)

fit.poiss<-gam(muertes ~ factor(SemanaEpi) + s(tiempo),
               family=poisson(), offset= log(exposicion),
               data=df)

pchisq(2 * (logLik(fit.gam) - logLik(fit.poiss)), df = 1, lower.tail = FALSE)


#######


## predictions (with extrapolation)
pre.data <- df

pre.gam <- predict.gam(fit.gam,newdata = pre.data, type = "response") *df$exposicion

## save excess
pre.data$muertes_gam <- pre.gam


pre.data <- pre.data %>%
    mutate(excess_gam = muertes - muertes_gam,
           tasa_gam = excess_gam / exposicion * 1e3, Pscore1=((muertes - muertes_gam)/muertes))


df2 <- df %>%
  left_join(df.mean,by = "SemanaEpi") %>%
  mutate(excess_mean = muertes - deaM,
         tasa_mean = excess_mean / exposicion * 1e3,Pscore2=((muertes - deaM)/muertes))

my.res <- pre.data %>% 
  select(COD_MUNICIPIO,Municipio,ANO,SemanaEpi,excess_gam,tasa_gam,Pscore1,tiempo) %>% 
  left_join(df2 %>% 
              select(COD_MUNICIPIO,ANO,SemanaEpi,excess_mean,tasa_mean,Pscore2),
            by = c("COD_MUNICIPIO", "ANO", "SemanaEpi")) %>% 
  filter(ANO==2015|ANO==2016|ANO==2017|ANO==2018|ANO==2019| ANO==2020 |ANO==2021)

if (i == 1) df.mortality <- my.res 
if (i != 1) df.mortality <- df.mortality %>% bind_rows(my.res)

}

## Gráficos adicionales

## muertes (GAM vs media historica)

plot(df2$tiempo,df2$muertes)
lines(df2$tiempo,df2$deaM,col=2,lwd=2)
lines(df$tiempo,pre.gam,col=4,lwd=2)

## Exceso de muertes (GAM vs mean)
plot(df2$tiempo,pre.data$excess_gam, main="")
lines(df2$tiempo,df2$excess_mean,col=3,lwd=2)

## excess rates (GAM vs mean)
plot(pre.data$tiempo,pre.data$tasa_gam,t="l",lwd=2)
lines(df2$tiempo,df2$tasa_mean,t="l",col=2,lwd=2)



###########
## plotting (1 panel)


df.mortality %>% filter(ANO==2021)%>% 
  ggplot(aes(x=SemanaEpi,y=tasa_gam,group=Municipio,color=Municipio))+
  geom_line()

if (SAVE) ggsave("figs/1_Excess1panel.pdf",width=12,height = 8)

## Graficando (42 paneles)
df.mortality %>% filter(ANO==2021)%>% 
  # select(-rate_gam,-rate_mean) %>% 
  select(-excess_gam,-excess_mean,-Pscore1,-Pscore2) %>% 
  pivot_longer(cols=c(-SemanaEpi,-Municipio, -ANO,-COD_MUNICIPIO)) %>% 
  ggplot(aes(x=SemanaEpi,y=value,group=name,color=name)) +
  geom_line() +
  facet_wrap(.~Municipio)
if (SAVE) ggsave("figs/1_Excess10panels.pdf",width=12,height = 8)



## saving excess mortality rate data
save(df.mortality,
     file="./dataOUT/mortalityData.Rdata")





