# 1 Primeros pasos

## 1.1 Instalar librerías de IPUMS


#install.packages ("ipumsr")


## 1.2 Activar librerías necesarias


library(ipumsr)
library(dplyr)
library(ggplot2)


## 1.3 Crear directorio 

setwd("D:\\Google Drive\\Albert Esteve i Anna Turu\\13_curs_ALAP_des_2024\\materials_classe_ALAP")


## 1.4 Importar datos

ddi<-read_ipums_ddi("ipumsi_00154.xml")
data<-read_ipums_micro(ddi)


# 2 Individuos

## 2.1 Número de registros (individuos) que hay en cada muestra


data %>% 
  group_by(SAMPLE=as_factor(SAMPLE)) %>% 
  summarize(n=n())


## 2.2 Población total de cada país usando los pesos


data %>% 
  group_by(SAMPLE=as_factor(SAMPLE)) %>% 
  summarize(n=sum(PERWT))

## 2.3 Frecuencia de la variable ‘relación con el/la jefe/a del hogar’


data %>% 
  group_by(
    SAMPLE=as_factor(SAMPLE),
    RELATE=as_factor(RELATE)) %>%
  summarize(n=sum(PERWT)) %>% 
  mutate(pct=n/sum(n))


## 2.4 Proporción de mujeres de 30 a 39 años que está en unión consensual


data %>%
  filter(SEX==2 & (AGE2==21 | AGE2==22)) %>% 
  group_by(
    SAMPLE=as_factor(SAMPLE),
    CONSENS=as_factor(CONSENS)) %>%
  summarize(n=sum(PERWT)) %>% 
  mutate(pct=n/sum(n))



## 2.5 Distribución de personas de 30 a 39 años por estado civil y sexo


data %>%
  filter(AGE2==21 | AGE2==22) %>% 
  group_by(
    SAMPLE=as_factor(SAMPLE),
    MARST=as_factor(MARST),
    SEX=as_factor(SEX)) %>%
  summarize(n=sum(PERWT)) %>% 
  mutate(pct=n/sum(n))



## 2.6 Proporción de individuos por sexo y edad quinquenal que convive con la pareja


#table(data$SPLOC,useNA = "always")

data26<-data %>% 
  mutate(SPLOC2=case_when(SPLOC== 0 ~ 0,
                          SPLOC>0 ~ 1))

data26 %>%
  group_by(
    SAMPLE=as_factor(SAMPLE),
    SEX=as_factor(SEX),
    AGE2=as_factor(AGE2)) %>%
  summarize(conv_pareja=weighted.mean(SPLOC2,PERWT)) 


## 2.7 Proporción de individuos por sexo y edad quinquenal que convive con la madre


#table(data$MOMLOC,useNA = "always")

data27<-data %>% 
  mutate(MOMLOC2=case_when(MOMLOC== 0 ~ 0,
                           MOMLOC>0 ~ 1))

data27 %>%
  group_by(
    SAMPLE=as_factor(SAMPLE),
    SEX=as_factor(SEX),
    AGE2=as_factor(AGE2)) %>%
  summarize(conv_madre=weighted.mean(MOMLOC2,PERWT)) 


## 2.8 Proporción de individuos por sexo y edad quinquenal que convive con el padre


#table(data$POPLOC,useNA = "always")

data28<-data %>% 
  mutate(POPLOC2=case_when(POPLOC== 0 ~ 0,
                           POPLOC>0 ~ 1))

data28 %>%
  group_by(
    SAMPLE=as_factor(SAMPLE),
    SEX=as_factor(SEX),
    AGE2=as_factor(AGE2)) %>%
  summarize(conv_padre=weighted.mean(POPLOC2,PERWT)) 


## 2.9 Tipología de hogar donde viven los individuos de cada país


data %>%
  group_by(
    SAMPLE=as_factor(SAMPLE),
    HHTYPE=as_factor(HHTYPE)) %>% 
  summarise(n=sum(PERWT)) %>% 
  mutate(pct=n/sum(n)*100) %>% 
  arrange(SAMPLE,desc(pct)) 


# 3 Hogares

## 3.1 Número de hogares únicos que hay en cada muestra


hogares<-data %>% 
  filter(PERNUM==1)

hogares %>% 
  group_by(SAMPLE=as_factor(SAMPLE)) %>% 
  summarise(num_hogares=n())%>% 
  ungroup()



## 3.2 Número de hogares únicos usando los pesos


hogares %>% 
  group_by(SAMPLE=as_factor(SAMPLE)) %>% 
  summarise(num_hogares=sum(HHWT))%>% 
  ungroup()


## 3.3 Calcular la distribución de hogares por tipo de hogar


hogares %>%
  group_by(
    SAMPLE=as_factor(SAMPLE),
    HHTYPE=as_factor(HHTYPE)) %>% 
  summarise(n=sum(PERWT)) %>% 
  mutate(pct=n/sum(n)*100) %>% 
  arrange(SAMPLE,desc(pct)) %>% 
  ungroup()


## 3.4 Calcular el tamaño medio del hogar de cada muestra


hogares %>% 
  group_by(
    SAMPLE=as_factor(SAMPLE)) %>% 
  summarise(weighted.mean(PERSONS,HHWT)) %>% 
  ungroup()


# 4 Interacción individuos-hogares

## 4.1 Identificar los hogares en los que conviven personas de 10 años o menos y calcular la proporción de hogares de estas características


#table(data$AGE,useNA="always")

data410<-data %>% 
  mutate(men_10=case_when(AGE<= 10 ~ 1,
                          .default = 0))

table(data410$men_10,useNA="always")

data411<-data410 %>% 
  group_by(SAMPLE,SERIAL) %>% 
  mutate(sum_men_10_hog=sum(men_10)) %>% 
  ungroup()

data411<-data411 %>% 
  mutate(d_men_10=case_when(sum_men_10_hog>0 ~1,
                            .default=0)) %>% 
  ungroup()

data411 %>% 
  filter(PERNUM==1) %>% 
  group_by(as_factor(SAMPLE)) %>% 
  summarise(prop=weighted.mean(d_men_10,HHWT))%>% 
  ungroup()



## 4.2 Identificar los hogares en los que conviven personas de 10 años o menos y de 70 años y más. Calcular la proporción de hogares con personas de 10 años o menos que conviven también con personas de 70 años y más


data420<-data %>% 
  mutate(men_10=case_when(AGE<= 10 ~ 1,
                          .default = 0),
         may_70=case_when(AGE>=70 ~1,
                          .default = 0))

data421<-data420 %>% 
  group_by(SAMPLE,SERIAL) %>% 
  mutate(sum_men_10_hog=sum(men_10),
         sum_may_70_hog=sum(may_70)) %>% 
  ungroup()

data422<-data421 %>% 
  filter(sum_men_10_hog>0) %>% 
  mutate(d_men_10_may_70=case_when(sum_may_70_hog>0 ~1,
                                   .default=0)) %>% 
  ungroup()

table(data422$d_men_10_may_70,useNA="always")

data422 %>% 
  filter(PERNUM==1) %>% 
  group_by(as_factor(SAMPLE)) %>% 
  summarise(prop=weighted.mean(d_men_10_may_70,HHWT))%>% 
  ungroup()


## 4.3 Identificar los hogares en los que viven personas no emparentadas con el/la jefe/a del hogar y calcular la proporción de personas que viven en este tipo de hogar y la proporción de hogares con estas características.


#table(data$RELATE)
#table(as_factor(data$RELATE))

data430<-data %>% 
  mutate(non_rel=case_when(RELATE==5 ~ 1,
                           .default = 0))

table(data430$non_rel)

data431<-data430 %>% 
  group_by(SAMPLE,SERIAL) %>% 
  mutate(sum_non_rel=sum(non_rel))%>% 
  ungroup()

data431<-data431 %>% 
  mutate(d_non_rel=case_when(sum_non_rel>0 ~1,
                             .default=0))

table(data431$d_non_rel,useNA="always")

data431 %>% 
  filter(PERNUM==1) %>% 
  group_by(as_factor(SAMPLE)) %>% 
  summarise(n=sum(HHWT)) %>% 
  mutate(pct=n/sum(n)*100)%>% 
  ungroup()

# 5 Visualización de datos

## 5.1 Estructura de la población por edad, sexo. Realizar una pirámide para cada muestra.

data41<-data %>%
  group_by(SAMPLE,SEX,AGE) %>% 
  filter(AGE!=999) %>% 
  summarise(value=sum(PERWT)) %>% 
  mutate(value2=case_when(SEX==1~-value,
                          SEX==2~value))%>% 
  ungroup()

head(data41)

options(scipen=999)

ggplot(data41, aes(x = AGE, y = value2, fill= factor(SEX))) + 
  geom_bar(stat = "identity",width=1,alpha=.8)+  
  facet_wrap(~as_factor(SAMPLE),scales="free",ncol=3)+
  geom_hline(yintercept=0,linetype="dashed")+
  labs(x="",y="")+
  coord_flip() +
  scale_y_continuous(labels=abs)+
  scale_x_continuous(limits=c(0,100),breaks=seq(0,100,10),expand=c(0,0))+
  scale_fill_manual(values = c("#3a1293",'#006d2c'),name="") + 
  theme_bw()+
  theme(legend.position="",
        panel.grid.minor=element_blank())


## 5.2 Estructura de la población por edad, sexo y relación con la persona principal. Realizar una pirámide para cada muestra.

#table(as_factor(data$RELATE))

data42<-data %>%
  group_by(SAMPLE,SEX,AGE,RELATE) %>% 
  filter(AGE!=999) %>% 
  summarise(value=sum(PERWT)) %>% 
  mutate(value2=case_when(SEX==1~-value,
                          SEX==2~value))%>% 
  ungroup()

head(data42)

options(scipen=999)

ggplot(data42, aes(x = AGE, y = value2, fill= as_factor(RELATE))) + 
  geom_bar(stat = "identity",width=2)+ 
  facet_wrap(~as_factor(SAMPLE),scales="free",ncol=3)+
  geom_hline(yintercept=0,linetype="dashed")+
  labs(x="",y="")+
  coord_flip() +
  scale_y_continuous(labels=abs)+
  #scale_x_continuous(expand=c(0,0))+
  scale_x_continuous(limits=c(0,100),breaks=seq(0,100,10),expand=c(0,0))+
  scale_fill_manual(values = c('#91cf60','#fc8d59','#fee08b','#d73027','#d9ef8b','#1a9850')) + 
  theme_bw()+
  theme(legend.position="bottom",
        legend.title=element_blank(),
        panel.grid.minor=element_blank())
