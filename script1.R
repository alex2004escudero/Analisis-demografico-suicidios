library(rio)
library(tidyverse)
library(readxl)
library(ggplot2)
library(plotly)
library(writexl)
library(gridExtra)
library(scales)
library(ggrepel)

paleta1 <- c("#A1EDD2", "#87E06C", "#F0E845", "#F4AA1E", "#E11710")

#enlace del ine a los datos:  https://www.ine.es/dynt3/inebase/index.htm?padre=8965&capsel=8967

años<-c("13","14","15","16","17","18","19","20")

df<-c()

n<-length(años)

k<-1


 for (k in 1:n){    #lo mejor es probar si con un k funciona x si hay un error
  
  #setwd("") NO HACE FALTA SI ESTÁ EN EL PROYECTO
  nombre <- paste("Datos/suicidios_20",años[k],".csv",sep="")
  datos <- read.csv(file = nombre, sep = ";", dec = ".", header = T)
  datos<-datos[,-2]
  df <- rbind(df, datos)
  
}

rm(datos)


#Creamos los dataframes sin los datos totales
df_sin_total <- df[df$Medio.empleado != "Total"& df$Sexo != "A" & df$Edad != "Todas las edades",]


#Creamos los dataframes de los datos totales
df_total <- df[df$Medio.empleado == "Total"& df$Sexo == "A" & df$Edad == "Todas las edades",]

# Agrupaciones de metodo

df_sin_total4<-df_sin_total %>% 
  mutate(metodo_agrupado = case_when(Medio.empleado == "X60" | Medio.empleado == "X61" | Medio.empleado == "X62" |
                                       Medio.empleado == "X63" | Medio.empleado == "X64" | Medio.empleado == "X65" |
                                       Medio.empleado == "X66" | Medio.empleado == "X67" | Medio.empleado == "X68" |
                                       Medio.empleado == "X69" ~ "Envenenamiento" ,
                                     Medio.empleado == "X70" | Medio.empleado == "X71" ~ "Asfixia",
                                     Medio.empleado == "X72" | Medio.empleado == "X73" | Medio.empleado == "X74" ~ "Disparo",
                                     Medio.empleado == "X75" | Medio.empleado == "X76" | Medio.empleado == "X77" ~ "Exposicion calor",
                                     Medio.empleado == "X78" ~ "Corte",
                                     Medio.empleado == "X80" ~ "Precipitacion",
                                     Medio.empleado == "X81" | Medio.empleado == "X82"~ "Vehiculos",
                                     Medio.empleado == "X79" | Medio.empleado == "X83" | Medio.empleado == "X84"~ "Otros"))
# 


df_nuevo <- data.frame()

 for (k in 1:n){    
   
  año <- paste("20",años[k],sep="")
  
  total_metodo_por_años <- df_sin_total4 %>% 
    filter(Año == año) %>% 
    group_by(Sexo, metodo_agrupado) %>% 
    summarize(suma = sum(Total)) %>% 
    mutate(años = año)
  
  df_nuevo <- rbind(df_nuevo, total_metodo_por_años)
  
 }
  
#  Agrupacion de edades
  
  
  df_total_edades<- df_sin_total4 %>% 
    mutate(edad_agrupada = case_when(Edad == "0_15" | Edad == "15_29" ~ "joven" ,
                                     Edad == "30_39" | Edad == "40_44" | Edad == "45_49" | Edad == "50_54" | Edad == "55_59" | Edad == "60_64"  ~ "adulto",
                                     Edad == "65_69" | Edad == "70_74" | Edad == "75_79" | Edad == "80_84" | Edad == "85_89" | Edad == "90_94" | Edad == "95_mas" ~ "anciano"))
  
  # for para hacer las probs y graph 1

  nuevo_df <- df_total_edades %>% 
    group_by(edad_agrupada, metodo_agrupado) %>%
    summarize (suma = sum(Total))
  
  nuevo_df2 <- nuevo_df %>% 
    group_by(metodo_agrupado) %>% 
    summarize (suma2 = sum(suma))
  
  nombres_grupos <- nuevo_df2$metodo_agrupado
  
  totales <- nuevo_df2$suma2
  
  n2 <-length(nombres_grupos)
  
  nuevo_df3 <- data.frame()
  
  for (k in 1:n2){
  
    
    df5 <- nuevo_df %>% 
      filter (metodo_agrupado == nombres_grupos[k]) %>% 
      mutate(prob = round(suma/totales[k], 2))
      
      
    nuevo_df3 <- rbind(nuevo_df3, df5)
    
  }
  
  valor<-  format(nuevo_df3$prob, nsmall = 1, decimal.mark = ".")
  nuevo_df3$prob2 <- sub("^0","", valor)
  
  # graf 1
  
  graph1 <- ggplot(nuevo_df3, aes(x = edad_agrupada , y = prob, fill = edad_agrupada)) +
    geom_bar (position = "dodge", stat= "identity") +
    theme(
      legend.position="none",
      plot.title = element_text(size=14),
      panel.grid = element_blank(),
    ) +
    theme_grey() +
    scale_fill_gradientn(colors = paleta1) +
    geom_text(aes(label = prob2), color = "black", size = 3.5, position = position_nudge(x = 0, y = 0.05)) +
    ggtitle("Probabilidades del grupo de edad por metodo empleado") +
    labs (fill = "Grupos de edad") +
    xlab ("Grupos de edad") +
    ylab("Probabilidades") + 
    facet_wrap(~metodo_agrupado ,ncol = 4) 
  
  # ahora mapa de calor (graf 3)
  
  
  df_edad_años <- df_total_edades %>% 
    group_by(Edad, Año) %>% 
    summarize(Total = sum(Total))
  
  mapa_calor <- ggplot(df_edad_años, aes(x = as.factor(Año), y = Edad, fill = Total)) +
    geom_tile(color = "white",
              lwd = 1.5,
              linetype = 1) +
    labs(title = "Distribución de los suicidios por edad",
         x = "Años",
         y = "Edades") + 
    scale_fill_gradientn(colors = paleta1) +
    geom_text(aes(label = Total), color = "black", size = 4) +
    theme_minimal()
  
  # y con su variacion porcentual
  
  graph2 <- read.csv(file = "./datos/df_edad_años.csv", sep = ";", dec = ".", header = T)
  
  graph2$variacion.porc <- as.numeric(graph2$variacion.porc)
  
  graph2$variacion.porc <- round(graph2$variacion.porc,2)
  
  graph2$variacion.porc2 <- percent(graph2$variacion.porc) 
  
  
  mapa_calor2 <- ggplot(graph2, aes(x = as.factor(Intervalo), y = Edad, fill = variacion.porc)) +
    geom_tile(color = "white",
              lwd = 1.5,
              linetype = 1) +
    labs(title = "Variación porcentual de los suicidios por edad",
         x = "Años",
         y = "Edades") + 
    scale_fill_gradientn(colors = paleta1, name = "Variación") +
    geom_text(aes(label = variacion.porc2), color = "black", size = 4) +
    theme_minimal()
  
  grid.arrange(mapa_calor, mapa_calor2, nrow = 1)
  

  
  # Gráfico de pirámide de suicidios con intervalos mas chicos (ultimo grafico)
  
  df_intervalos_pequeños <- read.csv(file = "./datos/df_intervalos.csv", sep = ";", dec = ".", header = T)
  
  totales_suicidios2 <- df_intervalos_pequeños %>% 
    group_by (Sexo) %>% 
    summarize (total = sum(Total))
  
  suicidios_2020 <- data.frame()
  
  for (k in 1:length(totales_suicidios2$total)) {
    
    suicidios_1 <- df_intervalos_pequeños %>% 
      filter (Sexo == totales_suicidios2$Sexo[k]) %>% 
      mutate (prob = round(Total/totales_suicidios2$total[k],3))
    
    
    suicidios_2020 <- rbind(suicidios_2020, suicidios_1)
    
  }
  
  suicidios_2020$prob2 <- percent(suicidios_2020$prob)
  
  suicidios_2020$prob <- suicidios_2020$prob * 100
  
  suicidios_2020$Edad <-  gsub("_mas", " o más",suicidios_2020$Edad)
  

  piramide_suicidio <- ggplot(suicidios_2020, aes (x = Edad, y = prob, fill = Sexo)) +
    geom_col(data = subset(suicidios_2020, Sexo == "H") %>% 
               mutate (prob = - prob), 
             fill = "blue", width = 0.7) +
    geom_col(data = subset(suicidios_2020, Sexo == "M"), 
             fill = "pink", width = 0.7) +
    coord_flip() +
    theme_minimal() +
    labs (title = "Pirámide de distribución de los suicidios por edad en 2020",
          x = "Grupos de edad",
          y = "Hombres                     Mujeres") + 
    theme(plot.title = element_text(hjust = 0.42),
          axis.title.x = element_text(hjust = 0.43)) +
    scale_y_continuous(breaks = seq(-12, 12, by = 2), 
                       labels = paste0(c(seq(-12, 0, by = 2)*-1, seq(2, 12, by = 2)), "%"))
  
  piramide_suicidio

  # Ahora la pirámide de poblacion con nuevos intervalos
  
  mujer_2020 <- read.csv(file = "./datos/mujer_2020.csv", sep = ";", dec = ".", header = T)
  
  mujer_2020$Edad <- as.integer(mujer_2020$Edad)
  
  mujer_2020 <- mujer_2020 %>% 
    mutate(edad_agrupada = case_when(Edad == 0:4  ~ "0_04",
                                     Edad == 5:9  ~ "05_09" ,
                                     Edad == 10:14 ~ "10_14",
                                     Edad == 15:19 ~ "15_19",
                                     Edad == 20:24 ~ "20_24",
                                     Edad == 25:29 ~ "25_29",
                                     Edad == 30:34 ~ "30_34",
                                     Edad == 35:39 ~ "35_39",
                                     Edad == 40:44 ~ "40_44", 
                                     Edad == 45:49 ~ "45_49",
                                     Edad == 50:54 ~ "50_54",
                                     Edad == 55:59 ~ "55_59",
                                     Edad == 60:64 ~ "60_64",
                                     Edad == 65:69 ~ "65_69",
                                     Edad == 70:74 ~ "70_74",
                                     Edad == 75:79 ~ "75_79",
                                     Edad == 80:84 ~ "80_84",
                                     Edad == 85:89 ~ "85_89",
                                     Edad == 90:94 ~ "90_94",
                                     Edad == 95 | Edad == 96 | Edad == 97 | Edad == 98 | Edad == 99 | Edad == 100 | 
                                       Edad == 101 | Edad == 102 | Edad == 103 | Edad == 104 | Edad == 105 ~ "95 o mas"))
  
  
  hombre_2020 <- read.csv(file = "./datos/hombre_2020.csv", sep = ";", dec = ".", header = T)
  
  hombre_2020$Edad <- as.integer(hombre_2020$Edad)
  
  hombre_2020 <- hombre_2020 %>% 
    mutate(edad_agrupada = case_when(Edad == 0:4  ~ "0_04",
                                     Edad == 5:9  ~ "05_09",
                                     Edad == 10:14 ~ "10_14",
                                     Edad == 15:19 ~ "15_19",
                                     Edad == 20:24 ~ "20_24",
                                     Edad == 25:29 ~ "25_29",
                                     Edad == 30:34 ~ "30_34",
                                     Edad == 35:39 ~ "35_39",
                                     Edad == 40:44 ~ "40_44", 
                                     Edad == 45:49 ~ "45_49",
                                     Edad == 50:54 ~ "50_54",
                                     Edad == 55:59 ~ "55_59",
                                     Edad == 60:64 ~ "60_64",
                                     Edad == 65:69 ~ "65_69",
                                     Edad == 70:74 ~ "70_74",
                                     Edad == 75:79 ~ "75_79",
                                     Edad == 80:84 ~ "80_84",
                                     Edad == 85:89 ~ "85_89",
                                     Edad == 90:94 ~ "90_94",
                                     Edad == 95 | Edad == 96 | Edad == 97 | Edad == 98 | Edad == 99 | Edad == 100 | 
                                       Edad == 101 | Edad == 102 | Edad == 103 | Edad == 104 | Edad == 105 ~ "95 o mas"))
  
  poblacion_2020 <- rbind(hombre_2020, mujer_2020)
  
  poblacion_2020 <- poblacion_2020[,-1]
  
  
  # Calculo de probabilidades 
  
  poblacion_2020 <- poblacion_2020 %>% 
    group_by (edad_agrupada, Sexo) %>% 
    summarize (total = sum(Total))
  
  totales_poblacion <- poblacion_2020 %>% 
    group_by(Sexo) %>% 
    summarize (total = sum(total))
  
  poblacion_2020_final <- data.frame()
  
  
  for (k in 1:length(totales_poblacion$total)) {
    
    poblacion_1 <- poblacion_2020 %>% 
      filter (Sexo == totales_poblacion$Sexo[k]) %>% 
      mutate (prob = round(total/totales_poblacion$total[k],3))
    
    
    poblacion_2020_final <- rbind(poblacion_2020_final, poblacion_1)
    
  }
  
  poblacion_2020_final$prob2 <- percent(poblacion_2020_final$prob)
  
  poblacion_2020_final$prob <- poblacion_2020_final$prob * 100
  
  piramide_poblacion <- ggplot(poblacion_2020_final, aes (x = edad_agrupada, y = prob, fill = Sexo)) +
    geom_col(data = subset(poblacion_2020_final, Sexo == "H") %>% 
               mutate (prob = - prob), 
             fill = "blue", width = 0.7) +
    geom_col(data = subset(poblacion_2020_final, Sexo == "M"), 
             fill = "pink", width = 0.7) +
    coord_flip() +
    theme_minimal() +
    labs (title = "Pirámide poblacional española en 2020",
          x = "Grupos de edad",
          y = "Hombres                     Mujeres") + 
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.x = element_text(hjust = 0.51)) +
    scale_y_continuous(breaks = seq(-16, 16, by = 2), 
                       labels = paste0(c(seq(-16, 0, by = 2)*-1, seq(2, 16, by = 2)), "%"))
  
  piramide_poblacion

  grid.arrange(piramide_suicidio, piramide_poblacion, nrow = 2)    
    
  
  # PARTE JOSEMI
  
  df_edad_metodo <- df_total_edades %>% 
    group_by(edad_agrupada, metodo_agrupado) %>% 
    summarize(Suma = sum(Total))
  
  df_edad_metodo <- df_edad_metodo %>% mutate(edad_agrupada = case_when(edad_agrupada == "adulto" ~ "Adulto",
                                                               edad_agrupada == "anciano" ~ "Anciano",
                                                               edad_agrupada == "joven" ~"Joven"))
  
  
  #saco los porcentajes para el gráfico de sectores
  totales_edad <- df_edad_metodo %>% 
    group_by (edad_agrupada) %>%
    summarise(Suma = sum(Suma))
  
  prop_edades <- data.frame()
  
  for (k in 1:length(totales_edad$Suma)) {
    
    prop_1 <- df_edad_metodo %>% 
      filter (edad_agrupada == totales_edad$edad_agrupada[k]) %>% 
      mutate (Proporcion = round(Suma/totales_edad$Suma[k],3)) %>% 
      mutate (Porcentaje = Proporcion * 100)
  
    prop_edades <- rbind(prop_edades, prop_1)
    
  }

  prop_edades$Porcentaje <- percent(prop_edades$Proporcion)
  
  # Gráfico general, igual que antes pero sin diferenciacion entre edades
  
  total_suicidios <- sum(df_edad_metodo$Suma)
  
  proptotal <- df_edad_metodo %>% 
    group_by(metodo_agrupado) %>% 
    summarize (Suma = sum(Suma)) %>% 
    mutate (Proporcion = Suma/total_suicidios) %>% 
    mutate (edad_agrupada = "Todas") %>% 
    mutate (Porcentaje = Proporcion * 100)
  
  proptotal$Porcentaje <- percent(round(proptotal$Proporcion,2))
  
  # Graficos con un facet wrap a ver si así sale mejor el grid.arrange
  
  proporciones_edad <- rbind(prop_edades, proptotal)
  
  grafico_sectores <- ggplot(proporciones_edad, aes(x = "", y = Proporcion, fill = metodo_agrupado)) +
    geom_col(width = 1, color = 1) +
    labs(title= "Proporción del empleo de cada método por grupos de edad")+
    theme (plot.title = element_text(hjust = 2)) +
    coord_polar(theta = "y")+
    geom_label_repel(data = subset(proporciones_edad, Proporcion >= 0.009),
                     aes(y = Proporcion, label = Porcentaje),
                     position = position_stack(vjust = 0.5),
                     min.segment.length = 5,
                     size = 3, 
                     show.legend = FALSE) +
    guides(fill = guide_legend(title = "Método")) +
    scale_fill_manual(values = c("#E11710", "#F97A32", "#F4AA1E", "#E6E241", 
                                 "#CDEA68", "#87E06C", "#A1EDD2", "#FAF0FB")) +
    facet_wrap(~edad_agrupada ,nrow = 2) +
    theme_void()
  
  grafico_sectores
  
  #grid. arrange final
  
  grid.arrange(graph1, grafico_sectores,
               layout_matrix = rbind(c(1,1,2,2),c(1,1,2,2)))

  