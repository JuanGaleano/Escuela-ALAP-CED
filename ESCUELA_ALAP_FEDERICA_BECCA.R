# Escuela ALAP: Familia, parentesco y hogares en América Latina y el Caribe
# Introducción a la base de datos CORESIDENCE
# Autor: Federica Becca | fbecca@ced.uab.es
# Fecha: 09/12/2024

# Libraries
library(tidyverse)
library(dplyr)
library(haven)

# Establecer directorio de trabajo
setwd()

# 1. Cargar datos
load("CORESIDENCE_DATABASE_2024.RData")
NATIONAL_DB <- CORESIDENCE_DB[["NATIONAL"]]
CODEBOOK <- CORESIDENCE_DB[["CODEBOOK"]]

# 2. Ejercicio I: Disponibilidad de muestras en América Latina y el Caribe

## 2.1 Filtrar y preparar datos
DISP <- NATIONAL_DB |>
    filter(C3 == "LATIN-AMERICA") |>
    select(1:12) |>
    mutate(S2_new = ifelse(C2 == "Dominican Republic" & T1 == 2002, "IPUMS+DHS",
        ifelse(C2 == "Colombia" & T1 == 2005, "IPUMS+DHS", S2)))

## 2.2 Representación gráfica
A <- ggplot(DISP, aes(x = T1, y = C2)) +
    geom_line() +
    geom_point(aes(color = S2_new), size = 3.5, alpha = 0.9) +
    scale_colour_manual(values = c("#DA627D", "#118F5E", "#219ebc", "#F38B53"), name = NULL) +
    scale_y_discrete(limits = rev) +
    scale_x_continuous(breaks = seq(1960, 2020, 10), limits = c(1960, 2020)) +
    labs(x = "Año", y = "") +
    theme_bw(base_size = 15) +
    theme(legend.position = "bottom")
A 

ggsave("I_disponibilidad.png", plot = A, scale = 1, dpi = 500, height = 10, width = 9)

# 3. Ejercicio II (a): Tendencias en el tamaño medio de los hogares

## 3.1 Filtrar y preparar datos
LAC <- NATIONAL_DB |>
    filter(C3 == "LATIN-AMERICA") |>
    group_by(C2, S2) |>
    mutate(C1_T1 = ifelse(T1 == min(T1), C1, NA)) |>
    arrange(T1, HS17)

## 3.2 Representación gráfica
B <- ggplot(LAC, aes(T1, HS17, group = interaction(C2, S2))) +
    geom_line(aes(col = C2), linewidth = 0.4, show.legend = FALSE) +
    geom_point(aes(shape = S2, colour = C2), size = 1.4, alpha = 0.7, show.legend = c(color = FALSE, shape = TRUE)) +
    scale_shape_manual(values = c(8, 16, 17), name = "Fuente") +
    scale_x_continuous(breaks = seq(1960, 2020, 10), limits = c(1960, 2020)) +
    geom_label(aes(label = C1_T1), size = 2.5, col = 'white', vjust = -0.6, label.padding = unit(0.001, "lines"), label.size = 0.2) +
    geom_text(aes(label = C1_T1), check_overlap = TRUE, size = 3, vjust = -0.6, col = "black") +
    labs(y = "Tamaño medio del hogar", x = '') +
    facet_wrap(~C4) +
    theme_bw(base_size = 15) +
    theme(legend.position = 'bottom', axis.text.x = element_text(angle = 45, hjust = 1))
B

ggsave("II_trend_tamaño.png", plot = B, scale = 1, dpi = 500, height = 8, width = 12)

# 4. Ejercicio II (b): Tendencias en la proporción de hijos/as de la persona de referencia en el hogar

## 4.1 Filtrar y preparar datos
LAC2 <- NATIONAL_DB |>
    filter(C3 == "LATIN-AMERICA") |>
    mutate(alpha = ifelse(C2 %in% c("Mexico", "Costa Rica", "Brazil", 
        "Colombia", "Uruguay", "Dominican Republic") & S2 == "IPUMS", "1", "0"),
        C0_new = ifelse(alpha == "1", C0, NA),
        prop_ch = HR03 / HS17,
        ch = "Proporción de hijos/as en el hogar")

## 4.2 Representación gráfica
C <- ggplot(LAC2, aes(x = T1, y = prop_ch, group = interaction(C2, S2), label = C2)) +
    geom_line(aes(alpha = alpha), linewidth = 0.6, show.legend = FALSE, color = "#118F5E") +
    scale_alpha_manual(values = c(0.3, 1)) +
    scale_x_continuous(breaks = seq(1960, 2020, 10), limits = c(1960, 2020)) +
    scale_y_continuous(breaks = seq(0.3, 0.6, 0.05), limits = c(0.28, 0.6)) +
    geom_label(aes(label = C0_new), color = "white", label.padding = unit(0.1, "lines"), size = 2.5) +
    geom_text(aes(label = C0_new), size = 3.2, check_overlap = TRUE) +
    facet_wrap(~ch) +
    labs(x = "", y = "Proporción") +
    theme_bw(base_size = 15)
C

ggsave("II_trend_hij.png", plot = C, scale = 1, dpi = 500, height = 10, width = 10)


# 5. Ejercicio III (a): Distribución de hogares por tamaño

## 5.1. Filtrar y preparar datos
SIZE <- NATIONAL_DB |>
    filter(C3 == "LATIN-AMERICA", S2 == "IPUMS") |>
    group_by(C2) |>
    filter(T1 == max(T1) | T1 == min(T1),
        C2 %in% c("Mexico","Costa Rica","Brazil","Colombia")) |>
    dplyr::select(C2, C3, C4, S2, T1, HS01:HS11) |>
    pivot_longer(c(HS01:HS11), names_to = "NUM_MEMB_CAT", values_to = "VALOR") |>
    mutate(size = as.factor(case_when(
        NUM_MEMB_CAT == "HS01" ~ "1",
        NUM_MEMB_CAT == "HS02" ~ "2",
        NUM_MEMB_CAT == "HS03" ~ "3",
        NUM_MEMB_CAT == "HS04" ~ "4",
        NUM_MEMB_CAT == "HS05" ~ "5",
        NUM_MEMB_CAT == "HS06" ~ "6",
        NUM_MEMB_CAT == "HS07" ~ "7",
        NUM_MEMB_CAT == "HS08" ~ "8",
        NUM_MEMB_CAT == "HS09" ~ "9",
        NUM_MEMB_CAT == "HS10" ~ "10",
        NUM_MEMB_CAT == "HS11" ~ "11+",
        TRUE ~ NA_character_
    ))) |>
    group_by(C2) |>
    mutate(TX = ifelse(T1 == max(T1), "max", "min")) |>
    pivot_wider(names_from = "TX", values_from = "VALOR") |>
    ungroup()

# Cambio el orden de los niveles para tener los tamaños en orden ascendente
SIZE$size <- factor(SIZE$size, 
    levels=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11+"))


## 5.2. Representación gráfica: geom_col()
D <- ggplot(SIZE, aes(x = size)) +
    geom_col(aes(y = min), width = 0.7, alpha = 0.3, position = position_dodge(width = 0), fill = "#49b2cc") +
    geom_col(aes(y = max), width = 0.4, alpha = 0.8, position = position_dodge(width = 0), fill = "#219ebc") +
    labs(x = "Tamaño del hogar", y = "Proporción") +
    facet_wrap(~C2, nrow = 3) +
    theme_bw(base_size = 17)

D

ggsave("III_distribucion_tamaño.png", plot = D, scale = 1, dpi = 500, height = 10, width = 12)


# 6. Ejercicio III (b): Distribución de hogares por tipología

## 6.1. Filtrar y preparar datos
TIPO <- NATIONAL_DB |> 
    filter(
        C3 == "LATIN-AMERICA", 
        S2 == "IPUMS", 
        C2 %in% c("Nicaragua", "Chile", "Uruguay", "Colombia")) |> 
    group_by(C2) |> 
    filter(T1 == max(T1) | T1 == min(T1)) |> 
    select(C2, T1, HT20:HT24) |> 
    pivot_longer(c(HT20:HT24), names_to = "TYPE_CAT", values_to= "VALOR") |> 
    mutate(
        # Asignar nombres descriptivos a los tipos de hogar
        type = case_when(
            TYPE_CAT == "HT20" ~ "Unipersonal",
            TYPE_CAT == "HT21" ~ "Nuclear",
            TYPE_CAT == "HT22" ~ "Stem",
            TYPE_CAT == "HT23" ~ "Other Family",
            TYPE_CAT == "HT24" ~ "Non-Family",
            TRUE ~ NA_character_),
        type = factor(type, levels = c("Unipersonal", "Nuclear", "Stem", "Other Family", "Non-Family"))) |> 
    group_by(C2) |> 
    mutate(TX = ifelse(T1 == max(T1), "max", "min")) |> 
    pivot_wider(
        names_from = "TX", 
        values_from = "VALOR") |> 
    ungroup()

# revertir el orden de las tipologías
TIPO$type <- factor(TIPO$type, levels = rev(unique(TIPO$type)))

## 6.2. Representación gráfica: geom_col()
E <- ggplot(TIPO, aes(x = type)) +
    geom_col(aes(y = min), width = 0.7, alpha = 0.5, fill = "#67b99a") +
    geom_col(aes(y = max), width = 0.3, alpha = 0.8, fill = "#14746f") +
    scale_y_continuous(breaks = seq(0, 0.6, 0.2), limits = c(0, 0.7), expand = c(0, 0)) +
    coord_flip() +
    labs(x = "Tipología", y = "Proporción") +
    facet_wrap(~C2, nrow = 2) +
    theme_light(base_size = 17)
E

ggsave("III_distribucion_tipo.png", plot = E, scale = 1, dpi = 500, height = 10, width = 14)

# 7. Ejercicio IV: Correlación entre dos indicadores clave

## 7.1. Filtrar y preparar datos
COR <- NATIONAL_DB |>
    filter(C3 == "LATIN-AMERICA") |>
    group_by(C2) |> 
    filter(T1==max(T1), 
        !is.na(D1))

## 7.2. Representación gráfica: geom_point() + geom_smooth()

F <- ggplot(COR, aes(x = D1, y = HS17)) +
    geom_point(aes(col = C4), alpha = .2, size = 8) +
    geom_point(aes(col = C4), alpha = .9, size = 8, shape = 21, show.legend = TRUE) +
    geom_text(aes(label=C0), size=2.5)+
    geom_smooth(method = "lm", col = '#383e42', alpha = .2, linewidth=.6, linetype="longdash") +
    scale_colour_manual(values = c("#36B98A", "#DCA925", "#CD583E"), name = NULL) +
    scale_y_continuous(breaks = seq(2,5,1), limits = c(2,5.5))+
    scale_x_continuous(breaks = seq(0.5, .9, 0.1), limits = c(.5,.9))+
    labs(x = "HDI", y = "Tamaño medio de los hogares",
        title="Relación entre el HDI y el tamaño medio de los hogares\npor país y subregión (muestra más reciente)") +
    theme_bw(base_size = 13) +
    theme(legend.position = c(0.8, 0.8))
F

ggsave("IV_correlacion.png", plot = F, scale = 1, dpi = 500, height = 8, width = 10)

