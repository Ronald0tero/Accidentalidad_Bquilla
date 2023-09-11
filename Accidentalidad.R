library(tidyverse)
library(janitor)
library(plotly)
library(zoo)
library(hrbrthemes)
library(streamgraph)
library(viridis)


Acci_Barranquilla <- read_delim("Mi carpeta/SQL_Rstudio_Ronaldo/Proyectos R/Accidentalidad/Accidentalidad_en_Barranquilla.csv", 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% clean_names() %>% 
  mutate(dia_accidente = ifelse(dia_accidente == "Lun", "Lunes", dia_accidente),
         dia_accidente = ifelse(dia_accidente == "Mar", "Martes", dia_accidente),
         dia_accidente = ifelse(dia_accidente == "Mié", "Miercoles", dia_accidente),
         dia_accidente = ifelse(dia_accidente == "Jue", "Jueves", dia_accidente),
         dia_accidente = ifelse(dia_accidente == "Vie", "Viernes", dia_accidente),
         dia_accidente = ifelse(dia_accidente == "Sáb", "Sabado", dia_accidente),
         dia_accidente = ifelse(dia_accidente == "Dom", "Domingo", dia_accidente)) %>% rename( direccion_accidente = sitio_exacto_accidente)

glimpse(Acci_Barranquilla)

unique(Acci_Barranquilla$direccion_accidente)

cambios = c('CALLE'='CLL','CLLE'='CLL','calle'='CLL', "CON"= " ", "con"= " ","clle"= "CLL","cll"= "CLL","clle"= "CLL","Calle"= "CLL","CL"= "CLL","cl"="CLL", "CLLL" = "Calle", "CalleL"= "Calle","cCalle" = "Calle",
            "Cr" = "Carrera","CARRERA" = "Carrera","cr" = "Carrera","Cra" = "Carrera","CR" = "Carrera","cr" = "","CRA" = "Carrera","cra" = "Carrera","crra" = "Carrera","CAR" = "Carrera","CarreraA" = "Carrera","Carreraa" = "Carrera")

Acci_Barranquilla$direccion_accidente <-  str_replace_all(Acci_Barranquilla$direccion_accidente, cambios)

#accidentes desde 2015 - 2023

accidentes_año <- Acci_Barranquilla %>% select(ano_accidente, gravedad_accidente, clase_accidente) %>% 
                                 group_by(ano_accidente) %>% 
                                 summarise(cantidad_accidentes = n())

acc_año_g <- ggplot(accidentes_año, aes(x = ano_accidente, y = cantidad_accidentes))+
                    geom_line(color="#69b3a2")+
                    geom_point(shape=21, color="black", fill="#69b3a2", size=3) +
                    theme_ipsum() +
                    ggtitle("Accidentes 2015 - 2023")

ggplotly(acc_año_g)


#porcentaje de la gravedad de los accidentes

heridos <- Acci_Barranquilla %>% select(gravedad_accidente) %>% 
  group_by(gravedad_accidente) %>% 
  summarise(cantidad = n(), porcentaje = (cantidad/43205 * 100))

heridos_g <- ggplot(heridos, aes(x = "", y = porcentaje , fill = gravedad_accidente))+
  geom_bar(stat = "identity", color = "white")+
  theme_ipsum()
  #coord_polar(theta="y")+ #grafica circular o pi
  #geom_text(aes(label=percent(porcentaje/100)),
  #          position=position_stack(vjust=0.5),color="white",size=6)+
  #coord_polar(theta = "y")+
  #scale_fill_manual(values=c("salmon","steelblue","orange"))+
  #theme_void()+
  #labs(title="Gráfico de Pie")

ggplotly(heridos_g)

#nuemero de accidentes por gravedad en los años
num_heridos_año <- Acci_Barranquilla %>% select(ano_accidente, gravedad_accidente) %>% 
                                         group_by(ano_accidente, gravedad_accidente) %>% 
                                         summarise( n = n())

acc_her_año_g <- ggplot(num_heridos_año, aes(x = ano_accidente, y = n , color = gravedad_accidente))+
  geom_line()+
  geom_point()+
  theme_ipsum()
ggplotly(acc_her_año_g)

# accidentes por clase 
num_clase_año <- Acci_Barranquilla %>% select(ano_accidente, clase_accidente) %>% 
  group_by(ano_accidente, clase_accidente) %>% 
  summarise( n = n())

acc_clase_año_g <- ggplot(num_clase_año, aes(x = ano_accidente, y = n , color = clase_accidente))+
  geom_line()+
  geom_point()+
  theme_ipsum()

ggplotly(acc_clase_año_g)

#Cantidad de accidentes por mes del año 2022
 

can_acc_mes_22 <- Acci_Barranquilla %>% 
                  select(ano_accidente, mes_accidente) %>%
                  group_by(ano_accidente, mes_accidente) %>% 
                  summarise(cantidad = n()) %>% 
                  filter(ano_accidente == 2022) %>% 
                  arrange(mes_accidente)
#ordenar meses del año
can_acc_mes_22$mes_accidente <- factor(can_acc_mes_22$mes_accidente, levels = c("enero","febrero","marzo","abril","mayo","junio","julio","agosto","septiembre","octubre","noviembre","diciembre")) 

can_acc_mes_22_g <- ggplot(can_acc_mes_22, aes( x = mes_accidente, y = cantidad, fill = mes_accidente))+
  geom_bar(stat = "identity", width=0.5)+
  theme(legend.position="none")+
  theme_ipsum() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position="none",
    axis.text.x = element_text(angle = 80, hjust=1))+
  xlab("")+
  ylab("Cantidad")

ggplotly(can_acc_mes_22_g)

#numero de accidentes por dias de la semana 

acc_sem <- Acci_Barranquilla %>% select(ano_accidente, dia_accidente) %>% 
                                 summarise( cantidad = n()) 
acc_sem$dia_accidente <- factor(acc_sem$dia_accidente, levels = c("Lunes","Martes","Miercoles","Jueves","Viernes","Sabado","Domingo"))

acc_sem_g <- ggplot(acc_sem, aes(x = dia_accidente, y = cantidad , fill = dia_accidente))+
                      geom_bar(stat = "identity", width=0.5)+
  theme(legend.position="none")+
  theme_ipsum()+
  coord_flip() 

ggplotly(acc_sem_g)

#numero de accidentes en dias de la semana por año(desde el año 2015 - 2023)

acc_sem_año <- Acci_Barranquilla %>% select(ano_accidente, dia_accidente) %>% 
  group_by(ano_accidente, dia_accidente) %>% 
  summarise( cantidad = n())
#ordenar los dias de la semana
acc_sem_año$dia_accidente <- factor(acc_sem_año$dia_accidente, levels = c("Lunes","Martes","Miercoles","Jueves","Viernes","Sabado","Domingo")) 

acc_sem_año_g <- ggplot(acc_sem_año, aes(x = ano_accidente, y = cantidad , color = dia_accidente))+
  geom_line()+
  geom_point()+
  theme_ipsum()
#facet_wrap(~dia_accidente, scale="free_y")

ggplotly(acc_sem_año_g) 

acc_sem_año2 <- Acci_Barranquilla %>% select(ano_accidente, dia_accidente) %>% 
  group_by(ano_accidente, dia_accidente) %>% 
  summarise( cantidad = n())
#ordenar los dias de la semana
 

acc_sem_año_g2 <- ggplot(acc_sem_año2, aes(x = ano_accidente, y = cantidad , fill = dia_accidente, text = dia_accidente))+
  geom_area()+
  theme(legend.position="none") +
  ggtitle("Popularity of American names in the previous 30 years") +
  theme_ipsum()
  #facet_wrap(~dia_accidente, scale="free_y")

ggplotly(acc_sem_año_g2) 

#mapa




fig <- Acci_Barranquilla
fig <- fig %>%
  plot_ly(
    lat = ~cant_muertos_en_sitio_accidente,
    lon = ~cant_heridos_en_sitio_accidente,
    marker = list(color = "fuchsia"),
    type = 'scattermapbox' )
fig <- fig %>%
  layout(
    mapbox = list(
      style = 'open-street-map',
      zoom =10,
      center = list(lon = -74.8062, lat = 10.9919))) 

fig
