#Link para tablas (PDF)
#https://www.gob.mx/salud/documentos/coronavirus-covid-19-comunicado-tecnico-diario-238449
#Link para Comunicado Tecnico
#https://www.gob.mx/salud/documentos/informacion-internacional-y-nacional-sobre-nuevo-coronavirus-2019-ncov

setwd("~/Documents/covid19_mx")
path_in <- '~/Documents/covid19_mx/Data/'
path_out <-'~/Documents/covid19_mx/Output/'
path_img <-'~/Documents/covid19_mx/Images/'

library(tidyverse)
library(ggplot2)
#library(dplyr)
library(hrbrthemes)


# Confirmados por fecha a nivel estatal
confirmados <- read.csv('confirmados_20_03_2020.csv', encoding="UTF-8", stringsAsFactors=FALSE)
confirmados$fecha_de_corte <- as.Date(confirmados$fecha_de_corte, format = "%d/%m/%Y" )
confirmados$fecha_de_inicio_de_sintomas <- as.Date(confirmados$fecha_de_inicio_de_sintomas, format = "%d/%m/%Y" )
confirmados$fecha_de_llegada_a_mexico <- as.Date(confirmados$fecha_de_llegada_a_mexico, format = "%d/%m/%Y" )

confirmados_nacional <- confirmados %>% group_by(fecha_de_corte) %>% summarise(nuevos_casos = n())
confirmados_nacional <- confirmados_nacional %>% mutate(confirmados = cumsum(nuevos_casos))
fecha_actualizacion <- str_sub(max(confirmados_nacional$fecha_de_corte), end = -1)

###########  Casos Acumulados
ggplot(confirmados_nacional, 
       aes(x = fecha_de_corte,
           y = confirmados,
           label = confirmados)) +
  geom_line(color="red") + geom_point(shape=21, color="gray", fill="#c0392b", size=6) +
  geom_text(vjust = 0, nudge_y = 6) + scale_x_date(date_breaks = "1 day") +
  theme_ipsum()  + 
  labs(title='Número de casos confirmados de COVID-19 en México', 
       subtitle=paste0("Fecha de corte: ", fecha_actualizacion),
       caption='Elaboración propia con datos de la Secretaría de Salud',
       x="",
       y="") +
  theme(plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size =15 ),
        plot.caption = element_text(size = 12),
        strip.text = element_text(size = 14),
        panel.spacing.x = unit(3, "lines"),
        text = element_text(family = "Arial Narrow"),
        axis.text.x = element_text(size = 12, angle = 90, hjust = 1),
        axis.text.y = element_text(size = 10))
ggsave(filename = paste0(path_img, "01_acumulados_", str_replace_all(fecha_actualizacion, "-", "_"), ".png"),
      width = 10, height = 8, dpi = 100)



###########  Nuevos casos
ggplot(confirmados_nacional, 
       aes(x = fecha_de_corte,
           y = nuevos_casos,
           label = nuevos_casos)) +
  geom_line(color="red") + geom_point(shape=21, color="gray", fill="#c0392b", size=6) +
  geom_text(vjust = 0, nudge_y = 1.5) + scale_x_date(date_breaks = "1 day") +
  theme_ipsum()  + 
  labs(title="Número de *nuevos* casos confirmados de COVID-19 en México por día", 
       subtitle=paste0("Fecha de corte: ", fecha_actualizacion,"\nTotal: ", as.character(sum(confirmados_nacional$nuevos_casos))),
       caption="Elaboración propia con datos de la Secretaría de Salud",
       x="",
       y="") +
  theme(plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size =15 ),
        plot.caption = element_text(size = 12),
        strip.text = element_text(size = 14),
        panel.spacing.x = unit(3, "lines"),
        text = element_text(family = "Arial Narrow"),
        axis.text.x = element_text(size = 12, angle = 90, hjust = 1),
        axis.text.y = element_text(size = 10))
ggsave(filename = paste0(path_img, "02_nuevos_", str_replace_all(fecha_actualizacion, "-", "_"), ".png"),
      width = 10, height = 8, dpi = 100)


###########  Distribucion de Edad (casos totales)
confirmados_sexo <- round(prop.table(table(confirmados$sexo))*100,2)
confirmados_sexo <- as.data.frame(cbind(c('F','M'), confirmados_sexo))
colnames(confirmados_sexo) <- c('categoria', 'valor')
confirmados_sexo$valor <- as.numeric(as.character(droplevels(confirmados_sexo$valor)))

# Compute label position
confirmados_sexo$label <- paste0(confirmados_sexo$categoria, "\n", confirmados_sexo$valor,"%")
# Compute the bottom of each rectangle
confirmados_sexo$ymax <- cumsum(confirmados_sexo$valor)
# Compute the bottom of each rectangle
confirmados_sexo$ymin <- c(0, head(confirmados_sexo$ymax, n=-1))

# Compute label position
confirmados_sexo$label_position <- (confirmados_sexo$ymax + confirmados_sexo$ymin) / 2

ggplot(confirmados_sexo, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=2, fill=categoria)) +
  geom_rect() +
  geom_text(x=2, aes(y=label_position, label=label, color=categoria), size=6) +
  scale_fill_manual(values = c("red", "steelblue")) +
  scale_color_manual(values = c("black", "black")) +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  labs(title="Distribucion por sexo", 
       subtitle=paste0("Fecha de corte: ", fecha_actualizacion,"\nTotal: ", as.character(sum(confirmados_nacional$nuevos_casos))),
       caption="Elaboración propia con datos de la Secretaría de Salud",
       x="",
       y="") +
  theme(plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size =15 ),
        plot.caption = element_text(size = 12),
        strip.text = element_text(size = 14),
        panel.spacing.x = unit(3, "lines"),
        text = element_text(family = "Arial Narrow"),
        #axis.text.x = element_text(size = 12, angle = 90, hjust = 1),
        #axis.text.y = element_text(size = 10)
        legend.position = "none")
ggsave(filename = paste0(path_img, "03_total_por_sexo", str_replace_all(fecha_actualizacion, "-", "_"), ".png"),
       width = 10, height = 8, dpi = 100)


###########  Distribucion de Edad (casos totales)
bks <- c(0,19, 44,54, 64, 74, 84, Inf)
lbs <- c('0-19','20-44', '45-54', '55-64', '65-74', '75-84', '>84')
confirmados$bins_edad <- cut(confirmados$edad, breaks = bks, labels = lbs, include.lowest=TRUE)

confirmados_edad <- confirmados %>% group_by(bins_edad) %>% summarise(confirmados = n())

ggplot(confirmados_edad,
       aes(x=bins_edad,
           y=confirmados)) +
  geom_bar(stat="identity", fill="#45b39d", alpha=.6, width=.4) +
  coord_flip() +
  geom_text(aes(label=confirmados), hjust=-0.2, size=3.5) +
  theme_ipsum(axis = 'X')+
  labs(title="Distribucion por rango de edad", 
       subtitle=paste0("Fecha de corte: ", fecha_actualizacion,"\nTotal: ", as.character(sum(confirmados_nacional$nuevos_casos))),
       caption="Elaboración propia con datos de la Secretaría de Salud",
       x="",
       y="") +
  theme(plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size =15 ),
        plot.caption = element_text(size = 12),
        strip.text = element_text(size = 14),
        panel.spacing.x = unit(3, "lines"),
        text = element_text(family = "Arial Narrow"),
        axis.text.x = element_text(size = 12, hjust = 1, color = 'white'),
        axis.text.y = element_text(size = 10))
ggsave(filename = paste0(path_img, "04_total_por_edad_", str_replace_all(fecha_actualizacion, "-", "_"), ".png"),
       width = 10, height = 8, dpi = 100)


####### Propuesta de Gráfia
####### Piramde de casos confirmados

confirmados_edad_sexo <- confirmados %>% group_by(bins_edad, sexo) %>% summarise(confirmados = n())

ggplot(data = confirmados_edad_sexo, 
       mapping = aes(x = bins_edad, fill = reorder(sexo, desc(sexo)), 
                     y = ifelse(test = sexo %in% c("M"), 
                                yes = -confirmados, no = confirmados))) + 
  geom_bar(stat = "identity", colour=alpha("grey90",.6), width=2 ,position=position_dodge(width=0)) +
  scale_y_continuous(labels = abs, limits = max(confirmados_edad_sexo$confirmados) * c(-1,1)) +
  #scale_fill_manual(values=c(alpha("#607d8b",.9), NA, alpha("#ff5722",.9), NA )) +
  scale_fill_manual(labels = c("Hombres", "Mujeres"), values=c(alpha("#607d8b",.9),alpha("#ff5722",.9))) +
  theme_ipsum(axis = 'X')+
  labs(title="Distribucion por rango de edad", 
       subtitle=paste0("Fecha de corte: ", fecha_actualizacion,"\nTotal: ", as.character(sum(confirmados_nacional$nuevos_casos))),
       caption="Elaboración propia con datos de la Secretaría de Salud",
       x="",
       y="") +
  theme(plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size =15 ),
        plot.caption = element_text(size = 12),
        strip.text = element_text(size = 14),
        legend.title = element_blank(),
        panel.spacing.x = unit(3, "lines"),
        text = element_text(family = "Arial Narrow"),
        axis.text.x = element_text(size = 12, hjust = 1, color = 'white'),
        axis.text.y = element_text(size = 10))+
  geom_shadowtext(aes(y = 0, 
                      label = bins_edad),
                  size=3, colour="white", fontface = "bold") +
  # annotate("text", x=1:7, y=0 ,label=as.character(levels(confirmados_edad_sexo$bins_edad)), color="black", size=3.5)+
  coord_flip()



################  Distribucion de casos totales por Estado ###################

confirmados_estado <- confirmados %>% group_by(estado) %>% summarise(confirmados=n())
confirmados_estado <- arrange(confirmados_estado, desc(confirmados))

ggplot(confirmados_estado,
       aes(x=reorder(estado, confirmados), ### Aqui sólo hay que añadir la funcion reorder, el primer argmento es la variable que vas a aocupar en el eje x y el segundo es la variable por la que quieres ordenar
           y=confirmados)) +
  geom_bar(stat="identity", fill="#45b39d", alpha=.6, width=.4) +
  coord_flip() +
  geom_text(aes(label=confirmados), hjust=-0.2, size=3.5) +
  theme_ipsum(axis = 'X')+
  labs(title="Distribucion por estado", 
       subtitle=paste0("Fecha de corte: ", fecha_actualizacion,"\nTotal: ", as.character(sum(confirmados_nacional$nuevos_casos))),
       caption="Elaboración propia con datos de la Secretaría de Salud",
       x="",
       y="") +
  theme(plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size =15 ),
        plot.caption = element_text(size = 12),
        strip.text = element_text(size = 14),
        panel.spacing.x = unit(3, "lines"),
        text = element_text(family = "Arial Narrow"),
        axis.text.x = element_text(size = 12, hjust = 1, color = 'white'),
        axis.text.y = element_text(size = 10))
ggsave(filename = paste0(path_img, "05_total_por_estado_", str_replace_all(fecha_actualizacion, "-", "_"), ".png"),
       width = 10, height = 8, dpi = 100)


ggplot(confirmados_estado, aes(x=reorder(estado, confirmados), y=confirmados, label =confirmados)) + ##### Mismo truco que en la gráfica anterior
  geom_segment( aes(xend=estado, yend=0), color = "#45b39d") +
  geom_point( size = 4, color = "#45b39d") +
  coord_flip() +
  geom_text(vjust = 0.5, nudge_y = 0.5) +
  theme_ipsum(axis = 'X') +
  labs(title="Distribucion por estado", 
       subtitle=paste0("Fecha de corte: ", fecha_actualizacion,"\nTotal: ", as.character(sum(confirmados_nacional$nuevos_casos))),
       caption="Elaboración propia con datos de la Secretaría de Salud",
       x="",
       y="") +
  theme(plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size =15 ),
        plot.caption = element_text(size = 12),
        strip.text = element_text(size = 14),
        panel.spacing.x = unit(3, "lines"),
        text = element_text(family = "Arial Narrow"),
        axis.text.x = element_text(size = 12, hjust = 1, color = 'white'),
        axis.text.y = element_text(size = 10))
#ggsave(filename = paste0(path_img, "05_total_por_estado_", str_replace_all(fecha_actualizacion, "-", "_"), ".png"),
#       width = 10, height = 8, dpi = 100)







###########  Distribusiones de Sexo (en el tiempo)   --- NO ME SALE --
confirmados_sexo <- confirmados %>% group_by(fecha_de_corte) %>% count(sexo) %>% as.data.frame()
confirmados_sexo <- confirmados_sexo %>% group_by(sexo) %>% mutate(acumm = cumsum(n))
#confirmados_sexo[nrow(confirmados_sexo),]=c("2020-02-29",'M',0,3)
#confirmados_sexo[nrow(confirmados_sexo),]=c("2020-03-01",'M',0,3)

#######  Es necesario tener un panel balanceado. la tabla confirmados sexo,
#######  sólo tienen datos cuando hay un nuevo confirmado,
#######  motivo por el cual hay saltos en las fechas. 
#######  La grafica debe mantenar cosntante los valores anteriores para los dias
#######  que no hay nuevos confirmados

dt1<-data.frame(fecha_de_corte=seq(min(confirmados_sexo$fecha_de_corte), max(confirmados_sexo$fecha_de_corte), by=1), sexo="F") ## Lista completa de fechas para mujetes (de la fecha minima hasta la fecha maxima con saltos de un dia)
dt2<-data.frame(fecha_de_corte=seq(min(confirmados_sexo$fecha_de_corte), max(confirmados_sexo$fecha_de_corte), by=1), sexo="M") ## Lista completa de fechas para hombre (de la fecha minima hasta la fecha maxima con saltos de un dia)
dt<-rbind(dt1, dt2) ## Union de lineas de tiempo completas
dt<-merge(dt, confirmados_sexo, by.x=c("fecha_de_corte","sexo"), by.y=c("fecha_de_corte","sexo"), all.x=T) ### merge con los datos confirmados por d{ia}
dt$n[is.na(dt$n)]<-0 ### a las fechas donde nu hay nuevos confirmados se les asigna cero
dt <- dt %>% group_by(sexo) %>% mutate(acumm = cumsum(n)) ### calculo de valores acumulados contabilizando los ceros
confirmados_sexo<-dt ### reasignando
rm(dt) ### eliminar data frame que ya no se ocupara


ggplot(data = confirmados_sexo,
       aes(x=fecha_de_corte, 
           y=acumm, 
           fill = sexo,
           label=acumm)) +
  geom_bar(position="stack", stat="identity") +
  scale_x_date(date_breaks = "1 day") +
  #geom_col(aes(fill = sexo), position = "dodge") +
  #geom_text(aes(label = acumm), position = position_dodge(0.9)) +
  theme_ipsum() +
  labs(title="Número de casos confirmados de COVID-19 en México por día", 
       subtitle=paste0("Fecha de corte: ", fecha_actualizacion,"\nTotal: ", as.character(sum(confirmados_nacional$nuevos_casos))),
       caption="Elaboración propia con datos de la Secretaría de Salud",
       x="",
       y="") +
  theme(plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size =15 ),
        plot.caption = element_text(size = 12),
        strip.text = element_text(size = 14),
        panel.spacing.x = unit(3, "lines"),
        text = element_text(family = "Arial Narrow"),
        axis.text.x = element_text(size = 12, angle = 90, hjust = 1),
        axis.text.y = element_text(size = 10))


#### Otro tipo de gráfico que te pude ser de utilidad

ggplot(confirmados_sexo, aes(x=fecha_de_corte, y=acumm, fill=sexo)) + 
  geom_area()+   scale_x_date(date_breaks = "1 day") + theme_ipsum() +
  scale_fill_manual(values=c("#bf2027", "#324a5e"))+
  # scale_fill_manual(values=rev(colsg1))+
  labs(title="Número de casos confirmados de COVID-19 en México por día", 
       subtitle=paste0("Fecha de corte: ", fecha_actualizacion,"\nTotal: ", as.character(sum(confirmados_nacional$nuevos_casos))),
       caption="Elaboración propia con datos de la Secretaría de Salud",
       x="",
       y="") +
  theme(plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size =15 ),
        plot.caption = element_text(size = 12),
        strip.text = element_text(size = 14),
        panel.spacing.x = unit(3, "lines"),
        text = element_text(family = "Arial Narrow"),
        axis.text.x = element_text(size = 12, angle = 90, hjust = 1),
        axis.text.y = element_text(size = 10))

    
    
    
    
    
    
    
    
    
    
  
  
  
  
  
  
    scale_y_continuous(name="Miles de Pesos ($)", labels = scales::comma)+
  theme(
    axis.text.y = element_text(size=10),
    axis.text.x = element_text(size=10, angle = 90 , hjust = 1),
    #axis.title.y=element_text(size=16, face="bold"),
    axis.title.y=element_blank(),
    #axis.title.x=element_text(size=18, face="bold"),
    plot.title = element_text(size=16, hjust = 0.5, face="bold"),
    plot.caption = element_text(hjust = 0 , size=8, face="italic"),
    axis.title.x=element_blank(),
    panel.grid.major.y = element_line(size=.4,
                                      linetype="dotted",
                                      color="#f96d15"),
    axis.line = element_line(colour = "gray10", 
                             size = .8, linetype = "solid"),
    panel.background = element_rect(fill = alpha("#dddddd",.1),
                                    colour = NULL),
    legend.position="bottom"
  )+
  scale_x_continuous(breaks=c(min(Agricultura$Anio):max(Agricultura$Anio)), expand=c(0, 0.2))+
  labs(title=paste0('Valor de la producción ganadera ',' (miles de pesos)'),y = "miles de pesos",  x="Año",
       caption=paste0("\nFuente: Elaboración propia con datos de la Secretaría de Agricultura y Desarrollo Rural (SIAP, 2019) para los municipios de\n",
                      paste0(munInter$NOM_MUN, collapse=", "),'\nValores a precios de julio de 2018 (INPC, base segunda quincena de julio 2018).'))+
  guides(fill=guide_legend("Especies"))

