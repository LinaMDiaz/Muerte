######################################################################################################
##################### Estadisticas de mortalidad ####################################################
#########################################################################################################

source("00-Obtener_datos_mortalidad.R")

################### Análisis descriptivos Tasas de mortalidad por municipio y por año ######################################


############# Descripción variables sociodemograficas de mortalidad #############


#### Tabla 1 documento word

df_totalm<- dfmortalidad%>% group_by(ANO)%>%summarise(muertes=n())

df_Pvalle<- poblavalle%>%group_by(ANO)%>%summarize(Poblacion=sum(Total))



df_covid<-defCOVID2020_2022%>%group_by(ANO)%>%summarize(muertescovid=n())

df_Tvalle<-df_totalm%>%left_join(df_Pvalle, by = c( "ANO"))
df_Tvalle<-df_Tvalle%>%left_join(df_covid, by = c( "ANO"))

df_Tvalle<- df_Tvalle %>%
  mutate(Tasa= (muertes/Poblacion)*1000)

df_Tvalle<- df_Tvalle %>%
  mutate(Tasacovid= (muertescovid/Poblacion)*1000)

######## Figura 1 ############ 

muertesotras<- dfmortalidad%>% group_by(FECHA_DEF)%>%summarise(casos=n())

ggplot() +
  theme_bw() +
  geom_histogram(muertesotras, mapping = aes(x =as.Date(FECHA_DEF), weight = casos), binwidth = 1, fill = "#738ca6", colour = "grey", size = 0.1) +
  scale_x_date(breaks = date_breaks("3 month"),limits = as.Date(c("2015-01-01","2021-12-31")),labels = date_format("%b %Y"), name = "Fecha") +
  scale_y_continuous(limits = c(0,200), name = "Número de muertes por todas las causas") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



##### Mortalidad COVID-19 

######### Figura 2 #######

mortalcovid<- defCOVID2020_2022%>% group_by(fec_def)%>%summarise(casos=n())

ggplot() +
  theme_bw() +
  geom_histogram(mortalcovid, mapping = aes(x =as.Date(fec_def), weight = casos), binwidth = 1, fill = "#738ca6", colour = "grey", size = 0.1) +
  scale_x_date(breaks = date_breaks("2 weeks"), labels = date_format("%b %Y"), 
               name = "Date") +
  scale_y_continuous(limits = c(0,90), name = "Número de muertes por COVID-19") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



############ Piramides poblacionales mortalidad Valle ##################

options(scipen=999)
piram<- dfmortalidad%>% group_by(ANO,Sexo,GrupoEdad)%>%summarise(casos=n())

piram<-piram %>% group_by(ANO,Sexo) %>% mutate(prop = casos/sum(casos)*100)

PobDANE1<- PobDANEEDAD%>% group_by(GrupoEdad)%>%summarize(Femenino=sum(Mujer),Masculino=sum(Hombre))

PobDANE1G <- gather(PobDANE1, key = "Sexo", value = "Pobladesag",Femenino:Masculino)


piram<-piram%>%left_join(PobDANE1G, by = c("Sexo","GrupoEdad"))

piram<-piram %>% mutate(Tasa= round((casos/Pobladesag)*1000,2))



########## Figura 3 #################

piram<- piram%>% mutate(casos = ifelse(Sexo=="Masculino", -casos, casos))



popPy1 <- ggplot(data = piram, 
                 mapping = aes(
                   x = GrupoEdad, 
                   y = ifelse(Sexo == "Masculino",  yes = -prop, no = prop), 
                   fill = Sexo,
                   label=paste(round(prop, 1), "%", sep="")
                 )) +
  geom_bar(stat = "identity") +
  #geom_text( aes(label = TotalCount, TotalCount = TotalCount + 0.05)) +
  geom_text(hjust=ifelse(test =piram$Sexo == "Masculino",  yes = 1.1, no = -0.1), size=6, colour="#505050") +
  #  scale_y_continuous(limits=c(0,max(appArr$Count)*1.7)) +
  # The 1.1 at the end is a buffer so there is space for the labels on each side
  scale_y_continuous(labels = abs, limits = max(piram$prop) * c(-1,1) * 1.1) +
  # Custom colours
  scale_fill_manual(values=as.vector(c("#d23f67","#505050"))) +
  # Remove the axis labels and the fill label from the legend - these are unnecessary for a Population Pyramid
  labs(
    x = "",
    y = "",
    fill=""
  ) +
  coord_flip() +
  # Remove the grid and the scale
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.x=element_blank(), 
    axis.text.y=element_text( size=20),
    strip.text.x=element_text(size=24),
    legend.position="bottom",
    legend.text=element_text(size=20))+ 
  geom_col()+facet_wrap(.~ANO)


#### Figura 4 ########
piram%>% ggplot(aes(GrupoEdad,Tasa, fill =Sexo)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(facets = vars(ANO)) +
  geom_text(aes(label = Tasa), position = position_dodge(width = 0.9), vjust = -0.5, size=3) + 
  labs("Tasas de mortalidad",
       subtitle = "") +
  theme(legend.position = "top")+
   ylim(0, 80)+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
scale_fill_manual(values=as.vector(c("#d23f67","#505050"))) 

####### Tabla mortalidad  todas las causas variables demográficas por año ################

library(stats)


table_Sex <- xtabs(~Sexo+ANO, data=dfmortalidad)
write.csv(addmargins(table_Sex),"table_Sex.csv")
summary(table_Sex )

Prop_sex<-proportions(table_Sex, "ANO")


table_Grupoedad <- xtabs(~GrupoEdad+ANO, data=dfmortalidad)
write.csv(addmargins(table_Grupoedad),"table_Grupoedad.csv")
summary(table_Grupoedad)

Prop_edad<-proportions(table_Grupoedad, "ANO")



table_Etnico <- xtabs(~IDPERTET+ANO, data=dfmortalidad)
write.csv(addmargins(table_Etnico),"table_Etnico.csv")
summary(table_Etnico)
#fisher.test(table_Etnico)

Prop_edad<-proportions(table_Grupoedad, "ANO")



table_Edu<- xtabs(~NIVEL_EDU+ANO, data=dfmortalidad)
write.csv(addmargins(table_Edu),"table_Edu.csv")
summary(table_Edu)

Prop_edad<-proportions(table_Edu, "ANO")

########################################################################

options(scipen=999)
piram<- defCOVID2020_2022%>% group_by(ANO,Sexo,GrupoEdad)%>%summarise(casos=n())

piram<-piram %>% group_by(ANO,Sexo) %>% mutate(prop = casos/sum(casos)*100)

PobDANE1<- PobDANEEDAD%>% group_by(GrupoEdad)%>%summarize(Femenino=sum(Mujer),Masculino=sum(Hombre))

PobDANE1G <- gather(PobDANE1, key = "Sexo", value = "Pobladesag",Femenino:Masculino)


piram<-piram%>%left_join(PobDANE1G, by = c("Sexo","GrupoEdad"))

piram<-piram %>% mutate(Tasa= round((casos/Pobladesag)*1000,2))



########## Figura 5 #################

#piram<- piram%>% mutate(casos = ifelse(Sexo=="Masculino", -casos, casos))

popPy1 <- ggplot(data = piram, 
                 mapping = aes(
                   x = GrupoEdad, 
                   y = ifelse(Sexo == "Masculino",  yes = -prop, no = prop), 
                   fill = Sexo,
                   label=paste(round(prop, 1), "%", sep="")
                 )) +
  geom_bar(stat = "identity") +
  #geom_text( aes(label = TotalCount, TotalCount = TotalCount + 0.05)) +
  geom_text(hjust=ifelse(test =piram$Sexo == "Masculino",  yes = 1.1, no = -0.1), size=3.8, colour="#505050") +
  #  scale_y_continuous(limits=c(0,max(appArr$Count)*1.7)) +
  # The 1.1 at the end is a buffer so there is space for the labels on each side
  scale_y_continuous(labels = abs, limits = max(piram$prop) * c(-1,1) * 1.1) +
  # Custom colours
  scale_fill_manual(values=as.vector(c("#d23f67","#505050"))) +
  # Remove the axis labels and the fill label from the legend - these are unnecessary for a Population Pyramid
  labs(
    x = "",
    y = "",
    fill=""
  ) +
  coord_flip() +
  # Remove the grid and the scale
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.x=element_blank(), 
    axis.text.y=element_text( size=20),
    strip.text.x=element_text(size=20),
    legend.position="bottom",
    legend.text=element_text(size=20))+ 
  geom_col()+facet_wrap(.~ANO)


#### Figura 6 ########
piram%>% ggplot(aes(GrupoEdad,Tasa, fill =Sexo)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(facets = vars(ANO)) +
  geom_text(aes(label = Tasa), position = position_dodge(width = 0.9), vjust = -0.5, size=3.3) + 
  labs("Tasas de mortalidad",
       subtitle = "") +
  theme(legend.position = "top")+
  ylim(0,18)+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_fill_manual(values=as.vector(c("#d23f67","#505050"))) 


###### Tabla mortalidad  por variables demográficas y comorbilidades por año ################

library(stats)

table_Sex <- xtabs(~Sexo+ANO, data=defCOVID2020_2022)
#write.csv(addmargins(table_Sex),"table_Sexcovid.csv")
summary(table_Sex )


table_Grupoedad <- xtabs(~GrupoEdad+ANO, data=defCOVID2020_2022)
#write.csv(addmargins(table_Grupoedad),"table_Grupoedadcovid.csv")
summary(table_Grupoedad)

table_Regimen <- xtabs(~Regimen+ANO, data=defCOVID2020_2022)
#write.csv(addmargins(table_Regimen),"Regimencovid.csv")
summary(table_Regimen)

lugar_fallec<- xtabs(~lug_ser_fallece+ANO, data=defCOVID2020_2022)
#write.csv(addmargins(lugar_fallec),"lug_ser_fallececovid.csv")
summary(lugar_fallec)

momento_dx<- xtabs(~momento_dx+ANO, data=defCOVID2020_2022)
#write.csv(addmargins(momento_dx),"momento_dx_covid.csv")
summary(momento_dx)

hta<- xtabs(~hta+ANO, data=defCOVID2020_2022)
#write.csv(addmargins(hta),"hta_covid.csv")
summary(hta)

enf_cardiovas<- xtabs(~enf_cardiovas+ANO, data=defCOVID2020_2022)
#write.csv(addmargins(enf_cardiovas),"enf_cardiovas.csv")
summary(enf_cardiovas)

enf_cerebrovas<- xtabs(~enf_cerebrovas+ANO, data=defCOVID2020_2022)
#write.csv(addmargins(enf_cerebrovas),"enf_cerebrovas.csv")
summary(enf_cerebrovas)

diabetes<- xtabs(~diabetes+ANO, data=defCOVID2020_2022)
#write.csv(addmargins(diabetes),"diabetes.csv")
summary(diabetes)

cancer<- xtabs(~cancer+ANO, data=defCOVID2020_2022)
#write.csv(addmargins(diabetes),"cancer.csv")
summary(cancer)

epoc<- xtabs(~epoc+ANO, data=defCOVID2020_2022)
#write.csv(addmargins(epoc),"epoc.csv")
summary(epoc)

asma<- xtabs(~asma+ANO, data=defCOVID2020_2022)
#write.csv(addmargins(asma),"asma.csv")
summary(asma)

enf_renal<- xtabs(~enf_renal+ANO, data=defCOVID2020_2022)
#write.csv(addmargins(enf_renal),"enf_renal.csv")
summary(enf_renal)

dislipidemia<- xtabs(~dislipidemia+ANO, data=defCOVID2020_2022)
#write.csv(addmargins(dislipidemia),"dislipidemia.csv")
summary(dislipidemia)

vih<- xtabs(~vih+ANO, data=defCOVID2020_2022)
#write.csv(addmargins(vih),"vih.csv")
summary(vih)

autoinmune<- xtabs(~autoinmune+ANO, data=defCOVID2020_2022)
#write.csv(addmargins(autoinmune),"autoinmune.csv")
summary(autoinmune)

enf_huerf<- xtabs(~enf_huerf+ANO, data=defCOVID2020_2022)
#write.csv(addmargins(enf_huerf),"enf_huerf.csv")
summary(enf_huerf)

hipotiroidismo<- xtabs(~hipotiroidismo+ANO, data=defCOVID2020_2022)
#write.csv(addmargins(hipotiroidismo),"hipotiroidismo.csv")
summary(hipotiroidismo)

con_corticoide<- xtabs(~con_corticoide+ANO, data=defCOVID2020_2022)
#write.csv(addmargins(con_corticoide),"con_corticoide.csv")
summary(con_corticoide)
 
mal_nutr<- xtabs(~mal_nutr+ANO, data=defCOVID2020_2022)
#write.csv(addmargins(mal_nutr),"mal_nutr.csv")
summary(con_corticoide)

fumador<- xtabs(~fumador+ANO, data=defCOVID2020_2022)
#write.csv(addmargins(fumador),"fumador.csv")
summary(fumador)
 

#################  Capitulo 7.3 Calculo de tasas de mortalidad por año y municipio #################

df_muni<- dfmortalidad%>% group_by(COD_MUNICIPIO,ANO)%>%summarise(muertes=n())

df_muni$COD_MUNICIPIO<-as.numeric(df_muni$COD_MUNICIPIO)

df_muni<-df_muni%>%left_join(poblavalle, by = c("COD_MUNICIPIO", "ANO")) %>% 
  mutate(Poblacion=Total)

df_muni$Poblacion<-as.numeric(df_muni$Poblacion)

df_muni<- df_muni %>%
  mutate(Tasa= (muertes/ Poblacion)*100)

subset_fmuni <- df_muni[, c("COD_MUNICIPIO","MPNOM","ANO","Tasa")]

subset_muerte <- df_muni[, c("COD_MUNICIPIO","MPNOM","ANO","muertes")]

Tasas_ANO<- tidyr:: spread(subset_fmuni, key =ANO, value = Tasa)

colnames(Tasas_ANO)[3:9] <- c("Tasa2015", "Tasa2016","Tasa2017","Tasa2018","Tasa2019","Tasa2020","Tasa2021")
saveRDS(Tasas_ANO, file = "datos/Tasas_ANO.RDS")

muertes_ANO<- tidyr:: spread(subset_muerte, key =ANO, value = muertes)

colnames(muertes_ANO)[3:9] <- c("Muertes2015", "Muertes2016","Muertes2017","Muertes2018","Muertes2019","Muertes2020","Muertes2021")



