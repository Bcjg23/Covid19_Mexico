# Paquetes utiles
library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)
library(readr)
library(tidyverse)
library(lubridate)


#Variables auxiliares
path_img <-'~/github/Covid19_Mexico/imagenes/'
caption_text <- 'Fuente: Elaboración propia con datos de Data Repository by Johns Hopkins CSSE\n @Bcjg23 | @GutzCarlos | @czammar'


# Cargamos datos de serie de tiempo
# Fuente: 2019 Novel Coronavirus COVID-19 (2019-nCoV) Data Repository by Johns Hopkins CSSE

urlfile="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
df <-read_csv(url(urlfile))


# Datos para graficas
## Reordenamos los datos para las graficas
df <- df %>% select(-c(Lat,Long,`Province/State`)) %>% group_by(`Country/Region`) %>% summarise_all(funs(sum))
## Alargamos la tabla por Pais
df <-gather(df, `1/22/20`:`3/25/20`,key='Fecha',value=ConfirmedCases)
## Renombramos columna
df <- rename(df, Country=`Country/Region` )
## Transformamos la columna al formato de fecha adecuado
df <- df %>% mutate( date = as.Date(df$Fecha, format = "%m/%d/%Y"))
df$Date <-  mdy(df$Fecha)
## Paises para del comparativo
df <- df %>% filter(Country %in% c("Mexico","Colombia","Chile","Argentina","Brazil","Peru","Ecuador"))

## Filtro de fechas
df <- df %>% filter(Date>='2020-03-15')

## Etiquetas para el final de las series de tiempo
d_ends <- df %>% group_by(Country) %>% top_n(1, ConfirmedCases) %>% pull(ConfirmedCases)

##
fecha_actualizacion <- str_sub(max(df$ConfirmedCases), end = -1)


#
ggplot(df, aes(x = Date, y = ConfirmedCases,color = Country)) + 
  geom_line(size = 1,alpha=0.4) +
  geom_point(size = 1)+
  #scale_y_continuous(sec.axis = sec_axis(~ ., breaks = d_ends)) + # Coloca numero de casos totales al final de la serie (lo quite por overplotting)
  theme_ipsum()  +
  labs(title="Número de casos confirmados de COVID-19 en paises\n de LATAM por día", 
       subtitle=paste0("Total en estos paises: ", as.character(sum(df$ConfirmedCases)), "\nFecha de corte: ", df$Date[nrow(df)]),
       caption=caption_text,
       x="",
       y="") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 1, size =11 ),
        plot.caption = element_text(size = 11),
        strip.text = element_text(size = 14),
        panel.spacing.x = unit(3, "lines"),
        text = element_text(family = "Arial Narrow"),
        axis.text.x = element_text(size = 12, angle = 90, hjust = 1),
        axis.text.y = element_text(size = 12))


# Usamos ggplotly para hacerlo interactivo
#p <- ggplotly(p)

ggsave(filename = paste0(path_img, "08_casos_acumulados_latam.png"),
       width = 10, height = 8, dpi = 100)