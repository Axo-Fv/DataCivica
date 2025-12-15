
# Libraries
# ----------------------------------------------------------------------------- 
setwd("~/")
library("foreign")
library("dplyr")
library("tidyverse")
library("ggplot2")
# ----------------------------------------------------------------------------- 


# Datasets
# ----------------------------------------------------------------------------- 
# Read < dbf > file
main23_dataset <- read.dbf("DEFUN23.dbf")
main24_dataset <- read.dbf("DEFUN24.dbf")
geo_dataset <- read.dbf("CATEMLDE24.dbf")
ocup_dataset <- read.dbf("OCUPACIONES.dbf")
def1_dataset <- read.dbf("CATMINDE.dbf")
def2_dataset <- read.dbf("LISTAMEX.dbf")
# def3_dataset <- read.dbf("COD_ADICIO.dbf") # No contiene X85 Y09
parent_dataset <- read.dbf("PARENTESCO.dbf")
paises_dataset <- read.dbf("PAISES.dbf")
# ----------------------------------------------------------------------------- 

# https://www.inegi.org.mx/app/ageeml/#
estados_mx <- c(
  "AGS", "BC", "BCS", "CAMP", "COA", "COL", "CHIA", "CHIH", "CDMX", "DUR",
  "GTO", "GRO", "HGO", "JAL", "EDOMEX", "MICH", "MOR", "NAY", "NL", "OAX",
  "PUE", "QUE", "QRO", "SLP", "SIN", "SON", "TAB", "TAM", "TLAX", "VER",
  "YUC", "ZAC", "DESC"
  )

# Preprocesing
# ----------------------------------------------------------------------------- 
# Constraint to murders only
# Sobre la víctima: SEXO, CONINDIG, NACIONALID, eDAD, OCUPACION, EDO_CIVIL, 
# EMBARAZO, TLOC_RESID, ESCOLARID, DERECHOHAB
# Sobre el autor: EDO_CIVIL, PAR_AGRE, VIO_FAM, 
# Sobre el hecho: ENT_OCURR, CAUSA_DEF, COD_ADICIO, LISTA_MEX, MES_OCURR, HORAS
# Sobre el contexto: NATVIOLE, LUGAR_OCUR


cleaning <- function(data){
  data <- data %>% 
    select(SEXO, CONINDIG, NACIONALID, EDAD, OCUPACION, EDO_CIVIL, DERECHOHAB,
           EMBARAZO, TLOC_RESID, ESCOLARIDA, EDO_CIVIL, PAR_AGRE, VIO_FAMI,
           ENT_OCURR, CAUSA_DEF, COD_ADICIO, LISTA_MEX, MES_OCURR, HORAS,
           NATVIOLE, LUGAR_OCUR, TIPO_DEFUN) %>% 
    filter(TIPO_DEFUN == 2 & SEXO == 2 & NATVIOLE == 1) %>% 
    mutate(EDAD = case_when(EDAD %% 1000 == 998 ~ NA_real_,
                            TRUE ~ EDAD - 4000)) %>% 
    filter(EDAD > 0)
  
  return(data)
  
}

main23_dataset <- cleaning(main23_dataset)
main24_dataset <- cleaning(main24_dataset)

# Defunción con violencia
# < NATVIOLE = 1 >

# Data exploration
View(main_dataset)
glimpse(main_dataset)
names(main_dataset)

main_dataset %>% 
  select(ENT_OCURR) %>% 
  count(ENT_OCURR) %>% 
  arrange(desc(n)) %>% 
  View()



# Desriptive statistics

# Hipótesis : el partido al frente de la presidencia se asume como un
# gobierno feminista que cuida y protege a las mujeres

# ----------------------------------------------------------------------------- 
#

# Nuevas variables
#   - Esados gobernados por partido político
# PRI : COAH, DUR
# VERD : SLP
# MC : JAL, NL
# PAN : AGS, GTO, QUE, CHIH
# MORENA : !PAN & !MC & !VER & !PRI
#   - LUGAR_OCUR -> LUGAR_PUB = {0,1}
# 


# Partidos políticos en México
main24_dataset <- main24_dataset %>%
  mutate(
    ENT_OCURR = as.integer(as.character(ENT_OCURR)),
    PARTIDO = case_when(
      ENT_OCURR %in% c(5, 10)           ~ "PRI",
      ENT_OCURR == 24                   ~ "VERDE",
      ENT_OCURR %in% c(14, 19)          ~ "MC",
      ENT_OCURR %in% c(1, 8, 11, 22)    ~ "PAN",
      TRUE                              ~ "MORENA"
    )
  )



# 
main_dataset %>% 
  group_by(PARTIDO) %>% 
  summarise(mean = mean(EDAD),n = n())

# -----------------------------------------------------------------------------
# ¿Cuál es la edad promedio de las mujeres que son víctima de feminicio?
desc_stats1 %>% 
  group_by(NACIONALID) %>% 
  summarise(mean = mean(EDAD),n = n())

desc_stats1 %>% 
  group_by(NACESP_CVE) %>% 
  summarise(mean = mean(EDAD),n = n())

desc_stats1 <- main24_dataset %>% 
  group_by(ENT_OCURR) %>% 
  summarise(EDAD_PROM = mean(EDAD), STD = sd(EDAD), OBS = n())

# Plots
# -----------------------------------------------------------------------------
# ggplot(main_dataset$ENTIDAD, aes(y = main_dataset$PARTIDO)) +
#   geom_bar(aes(fill = main_dataset$NACIONALID), 
#            position = position_stack(reverse = TRUE)) + 
#   theme(legend.position = "top") +
#   labs(title = "Feminicios registrados por partidos políticos",
#        x = "Feminicidios registrados", y = "Partido polítco", 
#        fill = "Nacionalidad", labels("Mexicana", "Extranjera", "NE"))

# Escenario antes y después del cambio de presidencia.
main24_dataset <- main24_dataset %>% 
  mutate(PRESIDENCIA = case_when(
    MES_OCURR < 10 ~ "Previo",
    TRUE ~ "Posterior"
  ))
ggplot(main24_dataset, aes(x = EDAD, fill = PAR_AGRE)) +
  geom_bar(position = "dodge") +
  # geom_bar(position = position_stack(reverse = TRUE)) + 
  # scale_fill_manual(
  #   labels = c(
  #     "1" = "Nacional",
  #     "2" = "Extranjera",
  #     "9" = "NE"
  #   )
  # ) +
  theme(legend.position = "top") +
  # labs(
    # title = "Feminicidios registrados por entidad federativa",
    # y = "Partido político",
    # x = "Feminicidios",
    # fill = "Elección 2024"
  # )
# ----------------------------------------------------------------------------- 
# Wordcloud
# install.packages("ggwordcloud")
library("ggwordcloud")
library("scales")

plot_wordc <- main24_dataset %>%
  count(CAUSA_DEF) %>%
  filter(!is.na(CAUSA_DEF)) %>%
  ggplot(aes(label = CAUSA_DEF, size = n, color = n)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 50) +
  scale_color_viridis_c(option = "turbo") +
  theme_minimal()

ggsave("CausaDef-2024.jpg", plot_wordc, width = 10, height = 8, dpi = 300,
       bg = "white")

# ----------------------------------------------------------------------------- 
# Heatmap
heat_data <- main24_dataset %>%
  group_by(ENT_OCURR) %>% 
  count(CAUSA_DEF, sort = TRUE)

estados_mx <- tibble(
  estado  = 1:33,
  LUGAR = c(
    "AGS", "BC", "BCS", "CAMP", "CHIA", "CHIH", "CDMX", "COA", "COL", "DUR", 
    "EDOMX", "GTO", "GRO", "HGO", "JAL", "MICH", "MOR", "NAY", "NL", "OAX", 
    "PUE", "QUE", "QRO", "SLP", "SIN", "SON", "TAB", "TAM", "TLAX", "VER", 
    "YUC", "ZAC", "DESC"
  )
)

heat_data <- heat_data %>%
  left_join(estados_mx, by = c("ENT_OCURR" = "estado")) %>%
  mutate(
    LUGAR = factor(LUGAR)
  )

plot_heat <- ggplot(heat_data, aes(x = LUGAR, y = CAUSA_DEF, fill = n)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "plasma") +
  theme_minimal() +
  labs(
    # title = "Mapa de calor por causa y estado",
    x = "Estado",
    y = "Causa de defunción",
    fill = "Total"
  ) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )


ggsave("DefDistro-2024.png", plot_heat, width = 14, height = 10, dpi = 300,
       bg = "white")
