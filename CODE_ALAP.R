
library(tidyverse)     # Data manipulation and visualization
library(sf)            # Spatial data manipulation
library(giscoR)        # Retrieve GISCO data
library(RColorBrewer)  # Color palettes for the map
library(readxl)        # Read excel files into R


## Leer la base de datos CORESIDENCE


load("CORESIDENCE_DATABASE_2024.RData")

# Extraer el conjunto de datos nacionales de la lista
NATIONAL_DB <- CORESIDENCE_DB[["NATIONAL"]]

## Filtrar casos de Latinoamérica

LA <- NATIONAL_DB |>
  filter(T3 == 1, C3 == "LATIN-AMERICA") |>
  select(1:12, "HS17") |>
  filter(T1 >= 2000)


## Recuperar Datos Espaciales de Gisco

# Retrieve spatial data for countries of the world
world <- gisco_get_countries(resolution = "03", epsg = 4326)

# Rename column to match with statistical data
colnames(world)[3] <- "C1"

## Unir datos espaciales y estadísticos

world <- world |>
  left_join(LA, by = "C1") |>
  filter(C3 == "LATIN-AMERICA")

# Cambie el nombre de la columna correspondiente para mayor claridad
colnames(world)[16] <- "indicator"

## Calcular deciles
deciles <- quantile(world$indicator, probs = seq(0, 1, by = 0.1), na.rm = TRUE)

## Categorizar datos por deciles

world <- world |>
  mutate(indicator2 = cut(indicator,
                          breaks = quantile(indicator, probs = seq(0, 1, 0.1), na.rm = TRUE),
                          include.lowest = TRUE,
                          labels = FALSE),
         labels = case_when(
           indicator2 == 1 ~ paste("<=", round(deciles[2], 2), sep = ""),
           indicator2 == 2 ~ paste("(", round(deciles[2], 2), "-", 
                                   round(deciles[3], 2), "]", sep = ""),
           indicator2 == 3 ~ paste("(", round(deciles[3], 2), "-", 
                                   round(deciles[4], 2), "]", sep = ""),
           indicator2 == 4 ~ paste("(", round(deciles[4], 2), "-", 
                                   round(deciles[5], 2), "]", sep = ""),
           indicator2 == 5 ~ paste("(", round(deciles[5], 2), "-", 
                                   round(deciles[6], 2), "]", sep = ""),
           indicator2 == 6 ~ paste("(", round(deciles[6], 2), "-", 
                                   round(deciles[7], 2), "]", sep = ""),
           indicator2 == 7 ~ paste("(", round(deciles[7], 2), "-", 
                                   round(deciles[8], 2), "]", sep = ""),
           indicator2 == 8 ~ paste("(", round(deciles[8], 2), "-", 
                                   round(deciles[9], 2), "]", sep = ""),
           indicator2 == 9 ~ paste("(", round(deciles[9], 2), "-", 
                                   round(deciles[10], 2), "]", sep = ""),
           indicator2 == 10 ~ paste(">=", round(deciles[10], 2), sep = "")),
         labels = fct_relevel(labels,
                              paste("<=", round(deciles[2], 2), sep = ""),
                              paste("(", round(deciles[2], 2), "-", 
                                    round(deciles[3], 2), "]", sep = ""),
                              paste("(", round(deciles[3], 2), "-", 
                                    round(deciles[4], 2), "]", sep = ""),
                              paste("(", round(deciles[4], 2), "-", 
                                    round(deciles[5], 2), "]", sep = ""),
                              paste("(", round(deciles[5], 2), "-", 
                                    round(deciles[6], 2), "]", sep = ""),
                              paste("(", round(deciles[6], 2), "-", 
                                    round(deciles[7], 2), "]", sep = ""),
                              paste("(", round(deciles[7], 2), "-", 
                                    round(deciles[8], 2), "]", sep = ""),
                              paste("(", round(deciles[8], 2), "-", 
                                    round(deciles[9], 2), "]", sep = ""),
                              paste("(", round(deciles[9], 2), "-", 
                                    round(deciles[10], 2), "]", sep = ""),
                              paste(">=", round(deciles[10], 2), sep = "")))


## Crear una paleta de colores

myColors <- rev(brewer.pal(10, "Spectral"))

# Visualice su paleta de colores y obtenga los nombres hexadecimales de ellos.
barplot(rep(length(myColors),length(myColors)),col=c(myColors));myColors


## Visualice los datos en un mapa


worldmap <- ggplot() + 
  geom_sf(data = world, aes(fill = labels), colour = "black", linewidth = 0.075) +
  scale_fill_manual(values = c(myColors),
                    guide = guide_legend(direction = "horizontal",
                                         nrow = 1,
                                         keyheight = 0.5,
                                         keywidth = 4,
                                         label.position = "bottom")) +
  theme_void() +
  annotate("text", x = -73, y = 35, label = "Average household size (deciles), 
           Latin-American countries", fontface = "bold", size = 5) +
  labs(caption = "\nData: CORESIDENCE Database | 
       Elaboration: Juan Galeano for Escuela ALAP") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.caption = element_text(vjust = 0.5, size = 10, colour = "black"),
        plot.margin = unit(c(0.5, 0, 0.5, 0), "cm"),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.grid.major = element_line(color = "white", linewidth = 0.15, linetype = 2),
        panel.grid.minor = element_line(color = "white", linewidth = 0.1, linetype = 1))

worldmap


## Guardar el mapa como un archivo de imagen

ggsave("map_1.png", 
       scale = 1, 
       dpi = 300,     
       height =7.5,    
       width = 17)

# Ejercicio 2: Tamaño medio de hogares por región, Latinoamérica
  
## Cargar Datos Espaciales
  
WORLDCO <- st_read("G:\\Shared drives\\CORESIDENCE\\WP4_RESEARCH OUTPUT\\4_5_Workshops\\ALAP_2024\\JUAN", "A1_WORLD_SUBNATIONAL_CORESIDENCE_SIMP")
colnames(WORLDCO)[4] <- "C5"


## Extraer el conjunto de datos armonizados de la base de datos CORESIDENCE

SUBNATHA_DB <- CORESIDENCE_DB[["SUBNATIONAL_HARMONIZED"]]

data <- SUBNATHA_DB |>
  filter(T3 == 1, C4 == "LATIN-AMERICA") |>
  select(1:14, 32) |>
  filter(T1 >= 2000)

colnames(data)[15] <- "indicator"


## Categorizar datos por deciles

deciles <- quantile(data$indicator, probs = seq(0, 1, by = .1), na.rm = TRUE)

data <- data |> mutate(
  indicator2 = cut(indicator,
                   breaks = quantile(indicator, probs = seq(0, 1, 0.1), na.rm = TRUE),
                   include.lowest = TRUE,
                   labels = FALSE),
  labels = case_when(
    indicator2 == 1 ~ paste("<=", round(deciles[2], 2), sep = ""),
    indicator2 == 2 ~ paste("(", round(deciles[2], 2), "-", round(deciles[3], 2), "]", sep = ""),
    indicator2 == 3 ~ paste("(", round(deciles[3], 2), "-", round(deciles[4], 2), "]", sep = ""),
    indicator2 == 4 ~ paste("(", round(deciles[4], 2), "-", round(deciles[5], 2), "]", sep = ""),
    indicator2 == 5 ~ paste("(", round(deciles[5], 2), "-", round(deciles[6], 2), "]", sep = ""),
    indicator2 == 6 ~ paste("(", round(deciles[6], 2), "-", round(deciles[7], 2), "]", sep = ""),
    indicator2 == 7 ~ paste("(", round(deciles[7], 2), "-", round(deciles[8], 2), "]", sep = ""),
    indicator2 == 8 ~ paste("(", round(deciles[8], 2), "-", round(deciles[9], 2), "]", sep = ""),
    indicator2 == 9 ~ paste("(", round(deciles[9], 2), "-", round(deciles[10], 2), "]", sep = ""),
    indicator2 == 10 ~ paste(">=", round(deciles[10], 2), sep = "")
  ),
  labels = fct_relevel(labels,
                       paste("<=", round(deciles[2], 2), sep = ""),
                       paste("(", round(deciles[2], 2), "-", round(deciles[3], 2), "]", sep = ""),
                       paste("(", round(deciles[3], 2), "-", round(deciles[4], 2), "]", sep = ""),
                       paste("(", round(deciles[4], 2), "-", round(deciles[5], 2), "]", sep = ""),
                       paste("(", round(deciles[5], 2), "-", round(deciles[6], 2), "]", sep = ""),
                       paste("(", round(deciles[6], 2), "-", round(deciles[7], 2), "]", sep = ""),
                       paste("(", round(deciles[7], 2), "-", round(deciles[8], 2), "]", sep = ""),
                       paste("(", round(deciles[8], 2), "-", round(deciles[9], 2), "]", sep = ""),
                       paste("(", round(deciles[9], 2), "-", round(deciles[10], 2), "]", sep = ""),
                       paste(">=", round(deciles[10], 2), sep = "")
  )
)


## Unir datos estadísticos y espaciales

WORLDCO1 <- left_join(WORLDCO, data, by = "C5") |>
  filter(C4 == "LATIN-AMERICA")


## Visualice los datos en un mapa

worldmap <- ggplot() + 
  geom_sf(data = WORLDCO1, aes(fill = labels), colour = "black", linewidth = .075) +
  scale_fill_manual(values = rev(brewer.pal(10, "Spectral")),
                    guide = guide_legend(direction = "horizontal",
                                         nrow = 1,
                                         keyheight = .5,
                                         keywidth = 4,
                                         label.position = "bottom")) +
  theme_void() +
  annotate("text", x = -73, y = 35, label = "Average household size (deciles), 
           Latin-American regions", fontface = "bold", size = 5) +
  labs(caption = "\nData: CORESIDENCE Database | 
       Elaboration: Juan Galeano for Escuela ALAP") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.caption = element_text(vjust = 0.5, size = 10, colour = "black"),
        plot.margin = unit(c(0.5, 0, 0.5, 0), "cm"),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.grid.major = element_line(color = "white", linewidth = 0.15, linetype = 2),
        panel.grid.minor = element_line(color = "white", linewidth = 0.1, linetype = 1))

worldmap


## Guarde el mapa como un archivo de imagen

ggsave("map_2.png", 
       scale = 1, 
       dpi = 300, 
       height = 7.5, 
       width = 17)


  
# Ejercicio 3: Diagrama de Caja para el tamaño medio de los hogares por regiones en cada país de latinoamérica
  
## Crear un tema para los gráficos
theme_alap<-list(theme(plot.title = element_text(lineheight=1, size=15, face="bold"), 
                       plot.subtitle = element_text(lineheight=1, size=12, face="bold"), 
                       plot.caption = element_text(lineheight=1, size=13, hjust=1),
                       legend.title = element_blank (), 
                       legend.text = element_text(colour="black", size = 15), 
                       legend.position="bottom", 
                       legend.background = element_rect(fill=NA, colour = NA), 
                       legend.key.size = unit(1.5, 'lines'), 
                       legend.key = element_rect(colour = NA, fill = NA), 
                       axis.title.x = element_blank (), 
                       axis.text.x  = element_text(angle = 0,vjust=0.5, size=15,colour="black"), 
                       axis.title.y = element_text(vjust=0.5, size=15,colour="black"), 
                       axis.text.y  = element_text(vjust=0.5, size=15,colour="black"),
                       strip.text = element_text(size=15, face="bold"),
                       plot.background =  element_rect(fill = "white"), 
                       panel.grid.major=element_line(colour="#E6E6E6",linewidth=.5), 
                       panel.grid.minor=element_line(colour="#E6E6E6",linewidth=.15), 
                       panel.border = element_rect(colour = "#585858", fill=NA, linewidth=.75),
                       panel.background =element_rect(fill ="#FFFFFF", colour = "#FFFFFF")))



## Ordenar los países respecto a la mediana y calcular el tamaño medio de los hogares en cada país

data <- data |>
  mutate(C2 = fct_reorder(C2, indicator, .fun = 'median')) |>
  mutate(mean = mean(indicator), .by = C2)


## Calcular deciles sobre la media


deciles <- quantile(data$mean, probs = seq(0, 1, by = .1), na.rm = TRUE)

data<-data|>mutate(indicator3=cut(mean,
                                  breaks = quantile(mean, probs = seq(0, 1, 0.1), na.rm = TRUE),
                                  include.lowest = TRUE,
                                  labels = FALSE),
                   labels_mean= case_when(
                     indicator3 == 1 ~ paste("<=",round(deciles[2],2),sep=""),
                     indicator3 == 2 ~ paste("(",round(deciles[2],2),"-",
                                             round(deciles[3],2),"]",sep=""),
                     indicator3 == 3 ~ paste("(",round(deciles[3],2),"-",
                                             round(deciles[4],2),"]",sep=""),
                     indicator3 == 4 ~ paste("(",round(deciles[4],2),"-",
                                             round(deciles[5],2),"]",sep=""),
                     indicator3 == 5 ~ paste("(",round(deciles[5],2),"-",
                                             round(deciles[6],2),"]",sep=""),
                     indicator3 == 6 ~ paste("(",round(deciles[6],2),"-",
                                             round(deciles[7],2),"]",sep=""),
                     indicator3 == 7 ~ paste("(",round(deciles[7],2),"-",
                                             round(deciles[8],2),"]",sep=""),
                     indicator3 == 8 ~ paste("(",round(deciles[8],2),"-",
                                             round(deciles[9],2),"]",sep=""),
                     indicator3 == 9 ~ paste("(",round(deciles[9],2),"-",
                                             round(deciles[10],2),"]",sep=""),
                     indicator3 == 10 ~  paste(">=",round(deciles[10],2),sep="")),
                   labels_mean= fct_relevel(labels_mean,
                                            paste("<=",round(deciles[2],2),sep=""),
                                            paste("(",round(deciles[2],2),"-",
                                                  round(deciles[3],2),"]",sep=""),
                                            paste("(",round(deciles[3],2),"-",
                                                  round(deciles[4],2),"]",sep=""),
                                            paste("(",round(deciles[4],2),"-",
                                                  round(deciles[5],2),"]",sep=""),
                                            paste("(",round(deciles[5],2),"-",
                                                  round(deciles[6],2),"]",sep=""),
                                            paste("(",round(deciles[6],2),"-",
                                                  round(deciles[7],2),"]",sep=""),
                                            paste("(",round(deciles[7],2),"-",
                                                  round(deciles[8],2),"]",sep=""),
                                            paste("(",round(deciles[8],2),"-",
                                                  round(deciles[9],2),"]",sep=""),
                                            paste("(",round(deciles[9],2),"-",
                                                  round(deciles[10],2),"]",sep=""),
                                            paste(">=",round(deciles[10],2),sep=""))
)



## Adjuntar colores al factor

names(myColors) <- levels(data$labels_mean)


## Visualice los datos en un diagrama de caja

boxplot<-ggplot(data, 
                aes(C2, indicator, fill=labels_mean))+ 
  geom_boxplot(alpha=1,
               width=.4,
               outlier.colour = "red")+
  scale_fill_manual (name = "Average\nHH size",
                     values = c(myColors),
                     guide = guide_legend(direction = "horizontal",
                                          nrow = 1,
                                          keywidth=4.5,
                                          label.position = "none"))+
  theme_alap+
  theme(legend.position = "none",
        axis.title.y = element_blank())+
  coord_flip();boxplot

## Guarde el diagrama de caja como un archivo de imagen


ggsave("fig_1.png", 
       scale = 1, 
       dpi = 300,     
       height =12.5,    
       width = 11.75)


  
# Ejercicio 3.1: Organizar dos Gráficos en un _grid_
  

  
## Crear un _grid_ de objetos _ggplot_ 

worldmap2 <- ggplot() + 
  geom_sf(data = WORLDCO1, aes(fill = labels), colour = "black", linewidth = .075) +
  scale_fill_manual(values = rev(brewer.pal(10, "Spectral")),
                    guide = guide_legend(direction = "horizontal",
                                         nrow = 1,
                                         keyheight = .5,
                                         keywidth = 4,
                                         label.position = "bottom")) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.caption = element_text(vjust = 0.5, size = 10, colour = "black"),
        plot.margin = unit(c(0.5, 0, 0.5, 0), "cm"),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.grid.major = element_line(color = "white", linewidth = 0.15, linetype = 2),
        panel.grid.minor = element_line(color = "white", linewidth = 0.1, linetype = 1))

worldmap2


## Organice una cuadrícula con los dos objetos

library(gridExtra)
map_box<-grid.arrange(worldmap2, boxplot,
                      ncol = 2, nrow = 1)

map_box


## Guarde la imagen de la cuadrícula ordenada como un archivo de imagen

ggsave("fig_2.png", 
       plot=map_box,
       scale = 1, 
       dpi = 300,     
       height =12.5,    
       width = 22.75)


  
# Ejercicio 4: Hogares unipersonales en México
  
## Crear un objeto espacial de México por regiones

mex_shp <- WORLDCO |>
  filter(CNTRY_NAME == "Mexico")


## Crear un marco de datos con datos mexicanos

mex_data <- SUBNATHA_DB |>
  filter(C2 == "Mexico") |>
  select(1:14, "HS01")


## Calcular _pretty_breaks_

mex_data<-mex_data|>
  mutate(HS01_c=if_else(HS01<0.03,"<3%",
                        if_else(HS01<0.06,"3-6%",
                                if_else(HS01<0.09,"6-9%",
                                        if_else(HS01<0.12,"9-12%", "12-16%")))),
         HS01_c=fct_relevel(HS01_c,"<3%",
                            "3-6%",
                            "6-9%",
                            "9-12%",
                            "12-16%"))


## Unir datos espaciales y estadísticos

mexico_shp2 <- mex_shp |>
  left_join(mex_data, by = "C5") |>
  mutate(T1 = as.factor(T1))


## Crea una paleta de colores usando RColorBrewer

myColors <- c(brewer.pal(5, "YlOrRd"))


## Trazar un mapa coroplético facetado

world <- gisco_get_countries(resolution = "03", epsg = 4326)
Map1 <-ggplot()+
  geom_sf(data= world, fill = "#bdbdbd",
          colour = "#bdbdbd",linewidth=.05) +
  geom_sf(data=mexico_shp2, 
          aes(fill = HS01_c),colour = "Black",linewidth=.05) + 
  scale_fill_manual(name="Unipersonal households(%)",
                    values=myColors,
                    guide = guide_legend(direction = "horizontal",
                                         title.position = "top",
                                         nrow = 1,
                                         keyheight=.5,
                                         keywidth=4,
                                         label.position = "bottom"))+
  facet_wrap(~T1)+
  coord_sf(
    xlim = c(-118.37075, -86.70731 ),
    ylim = c(14.53455, 32.71878 ))+
  
  labs(caption="Elaboration: Juan Galeano for Escuela ALAP\nData: CORESIDENCE Database")+
  theme_light()+
  theme(axis.title.x = element_blank (),
        axis.title.y = element_blank (),
        axis.text.x = element_blank (),
        axis.text.y = element_blank (),
        strip.text = element_text(size=18, face="bold"), 
        legend.position = "inside", 
        legend.position.inside = c(.80, 0.075),
        legend.text = element_text(colour="black", size = 15),
        legend.title= element_text(colour="black", size = 15),
        axis.ticks = element_blank())

Map1


## Guardar el mapa facetado como un archivo de imagen

ggsave("map_3.png", 
       scale = 1,
       height = 8,
       width=12, 
       dpi = 300) 

# Ejercicio 4.1: Hogares unipersonales en México geofacet

library(geofacet)

mex_data<-mex_data|>
  mutate(name_official=C6, 
         name_official=ifelse(name_official=="Distrito Federal", "Ciudad de México", name_official))


GRID_MX<-mx_state_grid3

GRID_MX$name<-with(GRID_MX,if_else(name=="Ciudad de México", "CDMX",
                                   if_else(name=="Baja California Sur", "Baja California S",name)))

df <- merge(mex_data, GRID_MX, by = "name_official", all.x = TRUE)

## Trazar un gráfico de lineas por estados con geogacet

df %>% 
  ggplot()+
  geom_smooth(stat = "smooth",
              aes((T1), HS01),
              colour="red",se=FALSE, linewidth=.75) +
  # coord_flip() +
  scale_y_continuous(labels = scales::percent)+
  facet_geo(~ name,grid = GRID_MX) +
  theme_light()+theme(legend.position = "none", 
                      axis.text.x =element_text(angle = 90 ), 
                      axis.title.x =element_blank(),
                      axis.title.y =element_blank(),
                      strip.text = element_text(face="bold"))


ggsave("G:\\Shared drives\\CORESIDENCE\\WP4_RESEARCH OUTPUT\\4_5_Workshops\\ALAP_2024\\JUAN/map_4.png", 
       scale = 1,
       height = 8,
       width=13, 
       dpi = 300)          



  
# Ejercicio 5: Configuraciones de convivencia los hogares, Uruguay 1963-2011
  
## Cargar y preparar los datos
  


load("CORESIDENCE_DATABASE_LA_2024.Rda")

# Extraer los datos de edades individuales de IPUMS de la base de datos GLAD
ipums <- CODBLA[["IPUMS"]]


## Extrar los datos disponibles de Uruguay

ury <- ipums |> filter(CNTRY == "Uruguay") |> select(1:14)


## Dividir el marco de datos en una lista por muestras

ury_list <- split(ury, f = ury$SAMPLE)


## Calcular la distribución relativa de los arreglos de vida por sexo y edades individuales para cada uno de los elementos de la lista

list1 <- lapply(ury_list, function(df) {
  df <- df |>
    mutate(SEX = unclass(SEX),
           AGE2 = if_else(AGE < 80, AGE, 80)) |>
    filter(SEX != 9) |>
    group_by(SAMPLE, SEX, AGE2, LA, LAQ) |>
    summarise(pop = sum(POPW)) |>
    ungroup()
  
  df <- df |>
    mutate(n_rel = pop / sum(pop), .by = c(SEX, AGE2))
})


## Combine los elementos de lista en un único marco de datos

final <- data.table::rbindlist(list1)

# Agregar información del año desde la columna SAMPLE
final <- final |> mutate(YEAR = as.numeric(substr(SAMPLE, 5, 8)))


## Leer archivo excel con la clasificación de los arreglos de vida


GELAI_AGRUPACION <- read_excel("GELAI_AGRUPACION.xlsx", 
                               sheet = "GELAI_TO_LA")

# Crea un marco de datos con distintas combinaciones de LA y LAQ
LAQ <- GELAI_AGRUPACION |> distinct(LA, LAQ)


## Crear un marco de datos con años, edades individuales y sexo

unique_ages <- data.frame(
  YEAR = rep(unique(final$YEAR), each = 81 * 2),
  AGE2 = rep(0:80, length(unique(final$YEAR)) * 2),
  SEX = rep(rep(c(1, 2), each = 81), length(unique(final$YEAR)))
)

# Realizar una unión cruzada entre edades únicas y LAQ
cross_join <- unique_ages %>%
  mutate(key = 1) %>%
  inner_join(LAQ %>% mutate(key = 1), by = "key") %>%
  select(-key)

# Fusionar datos unidos de forma cruzada con el conjunto de datos final
final <- cross_join %>%
  left_join(final, by = c("YEAR", "SEX", "AGE2", "LA", "LAQ"))

# Reemplazar NA en variables cruciales
final$SAMPLE <- paste("URY_", final$YEAR, "_IPUMS", sep = "")
final$pop[is.na(final$pop)] <- 0
final$n_rel[is.na(final$n_rel)] <- 0


## Recodificar y renivelar variables

final <- final |>
  mutate(SEX = as.factor(SEX),
         SEX = fct_recode(SEX, Men = "1", Women = "2"),
         LAQ = as.factor(LAQ),
         LAQ = fct_relevel(LAQ,
                           "Alone",
                           "With single parent",
                           "With single parent extended",
                           "With single parent extended composite",
                           "With parents",
                           "With parents extended",
                           "With parents extended composite",
                           "With partner",
                           "With partner extended",
                           "With partner extended composite",
                           "With partner and children",
                           "With partner and children extended",
                           "With partner and children extended composite",
                           "With children",
                           "With children extended",
                           "With children extended composite",
                           "Extended",
                           "Extended composite",
                           "Non-relative"))


## Crea una paleta de colores personalizada

library(colorspace)
library(RColorBrewer)

pal <- "Set3"
my_colors <- c(
  brewer.pal(8, pal)[1],
  brewer.pal(8, pal)[2], darken(brewer.pal(8, pal)[2], 0.25), darken(brewer.pal(8, pal)[2], 0.5),
  brewer.pal(8, pal)[3], darken(brewer.pal(8, pal)[3], 0.25), darken(brewer.pal(8, pal)[3], 0.5),
  brewer.pal(8, pal)[4], darken(brewer.pal(8, pal)[4], 0.25), darken(brewer.pal(8, pal)[4], 0.5),
  brewer.pal(8, pal)[5], darken(brewer.pal(8, pal)[5], 0.25), darken(brewer.pal(8, pal)[5], 0.5),
  brewer.pal(8, pal)[6], darken(brewer.pal(8, pal)[6], 0.25), darken(brewer.pal(8, pal)[6], 0.5),
  brewer.pal(8, pal)[7], darken(brewer.pal(8, pal)[7], 0.25),
  brewer.pal(8, pal)[8]
)

## Crear una gráfico de area por edades simples y sexo 

# Plot first and last available years
LA_URY<-ggplot(final |> filter(YEAR %in% c(1963, 2011)), aes(x = AGE2, y = n_rel, fill = LAQ)) +
  stat_smooth(
    geom = "area", position = "stack", method = "loess", span = 1 / 4
  ) +
  scale_fill_manual(values = my_colors) +
  scale_x_continuous(limits = c(0, 80), breaks = seq(0, 80, 5)) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "\nAge",
    y = "Percentage\n"
  ) +
  facet_grid(YEAR ~ SEX) +
  guides(fill = guide_legend(nrow = 7)) +
  theme_light() +
  theme(
    legend.position = "bottom",
    plot.caption = element_text(size = 13, colour = "black"),
    axis.text.x = element_text(size = 13, colour = "black"),
    axis.text.y = element_text(size = 13, colour = "black")
  )

LA_URY


## Guardar el mapa facetado como un archivo de imagen

ggsave("fig_3.png", 
       scale = 1,
       height = 10,
       width=12, 
       dpi = 300) 
