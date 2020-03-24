confirmados<-confirmados_hoy[,1:9]
names(confirmados)<-c("estado","n_caso", "sexo", "edad", "fecha_de_inicio_de_sintomas", "identificacion_de_covid19", "procedencia", "fecha_de_llegada_a_mexico", "fecha_de_corte")


confirmados$fecha_de_corte <- as.Date(confirmados$fecha_de_corte, format = "%d/%m/%Y" )
confirmados$fecha_de_inicio_de_sintomas <- as.Date(confirmados$fecha_de_inicio_de_sintomas, format = "%d/%m/%Y" )
confirmados$fecha_de_llegada_a_mexico <- as.Date(confirmados$fecha_de_llegada_a_mexico, format = "%d/%m/%Y" )

confirmados_nacional <- confirmados %>% group_by(fecha_de_corte) %>% summarise(nuevos_casos = n())
confirmados_nacional <- confirmados_nacional %>% mutate(confirmados = cumsum(nuevos_casos))
fecha_actualizacion <- str_sub(max(confirmados_nacional$fecha_de_corte), end = -1)



plt_Conf<-ggplotly(ggplot(confirmados_nacional, 
                        aes(x = fecha_de_corte,
                            y = confirmados,
                            label = confirmados)) +
  geom_line(color="red") + geom_point(shape=21, color="gray", fill="#c0392b", size=2) +
  geom_text(color="white", vjust = 0, nudge_y = 15, nudge_x = -.5) + scale_x_date(date_breaks = "2 day") +
  theme_ipsum()  + 
  labs(title='Número de casos confirmados de \nCOVID-19 en México', 
       subtitle=paste0("Fecha de corte: ", fecha_actualizacion),
       caption='Elaboración propia con datos de la Secretaría de Salud',
       x="",
       y="") +
  theme(panel.background = element_rect(fill =alpha("black", 0.35),
                                        colour="black"),
        plot.background = element_rect(fill = alpha("black", 0.35), colour = "black"),
        plot.title = element_text(color="white",size = 11, face = "bold", hjust=0.5),
        plot.subtitle = element_text(color="white",size =10 ),
        plot.caption = element_text(color="white",size = 8),
        strip.text = element_text(color="white",size = 8),
        panel.spacing.x = unit(3, "lines"),
        plot.margin = margin(.1, 0, 0, 0, "cm"),
        text = element_text(color="white",family = "Arial Narrow"),
        axis.text.x = element_text(color="white",size = 8, angle = 90, hjust = 1),
        axis.text.y = element_text(color="white",size = 10))) %>% layout(width=350, height=350)



###########  Nuevos casos
plt_New<-ggplotly(ggplot(confirmados_nacional, 
       aes(x = fecha_de_corte,
           y = nuevos_casos,
           label = nuevos_casos)) +
  geom_line(color="red") + geom_point(shape=21, color="gray", fill="#c0392b", size=2) +
  geom_text(color="white", vjust = 0, nudge_y = 5, nudge_x = .5 ) + scale_x_date(date_breaks = "2 day") +
  theme_ipsum()  + 
  labs(title="Número de *nuevos* casos confirmados de \nCOVID-19 en México por día", 
       subtitle=paste0("Fecha de corte: ", fecha_actualizacion,"\nTotal: ", as.character(sum(confirmados_nacional$nuevos_casos))),
       caption="Elaboración propia con datos de la Secretaría de Salud",
       x="",
       y="") +
  theme(panel.background = element_rect(fill =alpha("black", 0.35),
                                        colour="black"),
        plot.background = element_rect(fill = alpha("black", 0.35), colour = "black"),
        plot.title = element_text(color="white", size = 11, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color="white", size =10 ),
        plot.caption = element_text(color="white", size = 8),
        strip.text = element_text(color="white", size = 8),
        panel.spacing.x = unit(3, "lines"),
        plot.margin = margin(.1, 0, 0, 0, "cm"),
        text = element_text(color="white", family = "Arial Narrow"),
        axis.text.x = element_text(color="white", size = 8, angle = 90, hjust = 1),
        axis.text.y = element_text(color="white", size = 10))) %>% layout(width=350, height=350)


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

plt_Gender<-ggplotly(ggplot(confirmados_sexo, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=2, fill=categoria)) +
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
  theme(panel.background = element_rect(fill =alpha("black", 0.35),
                                        colour="black"),
        plot.background = element_rect(fill = alpha("black", 0.35), colour = "black"),
        plot.title = element_text(color="white", size = 20, face = "bold"),
        plot.subtitle = element_text(color="white", size =15 ),
        plot.caption = element_text(color="white", size = 12),
        strip.text = element_text(color="white", size = 14),
        panel.spacing.x = unit(3, "lines"),
        text = element_text(color="white", family = "Arial Narrow"),
        #axis.text.x = element_text(color="white", size = 12, angle = 90, hjust = 1),
        #axis.text.y = element_text(color="white", size = 10)
        legend.position = "none")) %>% layout(width=350, height=350)


###########  Distribucion de Edad (casos totales)
bks <- c(0,19, 44,54, 64, 74, 84, Inf)
lbs <- c('0-19','20-44', '45-54', '55-64', '65-74', '75-84', '>84')
confirmados$bins_edad <- cut(confirmados$edad, breaks = bks, labels = lbs, include.lowest=TRUE)


confirmados_edad <- confirmados %>% group_by(bins_edad) %>% summarise(confirmados = n())

plt_Age<-ggplotly(ggplot(confirmados_edad,
       aes(x=bins_edad,
           y=confirmados)) +
  geom_bar(stat="identity", fill="#45b39d", alpha=.6, width=.4) +
  coord_flip() +
  geom_text(color="white", aes(label=confirmados), hjust=-0.2, size=3.5) +
  theme_ipsum(axis = 'X')+
  labs(title="Distribucion por rango de edad", 
       subtitle=paste0("Fecha de corte: ", fecha_actualizacion,"\nTotal: ", as.character(sum(confirmados_nacional$nuevos_casos))),
       caption="Elaboración propia con datos de la Secretaría de Salud",
       x="",
       y="") +
  theme(panel.background = element_rect(fill =alpha("black", 0.35),
                                        colour="black"),
        plot.background = element_rect(fill = alpha("black", 0.35), colour = "black"),
        plot.title = element_text(color="white", size = 11, face = "bold", hjust=0.5),
        plot.subtitle = element_text(color="white", size =10 ),
        plot.caption = element_text(color="white", size = 8),
        strip.text = element_text(color="white", size = 10),
        panel.spacing.x = unit(3, "lines"),
        text = element_text(color="white", family = "Arial Narrow"),
        axis.text.x = element_text(size = 8, hjust = 1, color = 'white'),
        axis.text.y = element_text(color="white", size = 10))) %>% layout(width=350, height=350)


####### Propuesta de Gráfia
####### Piramde de casos confirmados

confirmados_edad_sexo <- confirmados %>% group_by(bins_edad, sexo) %>% summarise(confirmados = n())

plt_PirC<-ggplotly(ggplot(data = confirmados_edad_sexo, 
       mapping = aes(x = bins_edad, fill = reorder(sexo, desc(sexo)), 
                     y = ifelse(test = sexo %in% c("M"), 
                                yes = -confirmados, no = confirmados))) + 
  geom_bar(stat = "identity", colour=alpha("grey90",.6), width=2 ,position=position_dodge(width=0)) +
  scale_y_continuous(labels = abs, limits = max(confirmados_edad_sexo$confirmados) * c(-1,1)) +
  scale_fill_manual(labels = c("Hombres", "Mujeres"), values=c(alpha("#607d8b",.9),alpha("#ff5722",.9))) +
  theme_ipsum(axis = 'X')+
  labs(title="Distribucion por rango de edad", 
       subtitle=paste0("Fecha de corte: ", fecha_actualizacion,"\nTotal: ", as.character(sum(confirmados_nacional$nuevos_casos))),
       caption="Elaboración propia con datos de la Secretaría de Salud",
       x="",
       y="") +
  theme(panel.background = element_rect(fill =alpha("black", 0.35),
                                        colour="black"),
        plot.background = element_rect(fill = alpha("black", 0.35), colour = "black"),
        plot.title = element_text(color="white", size = 12, face = "bold"),
        plot.subtitle = element_text(color="white", size =11 ),
        plot.caption = element_text(color="white", size = 10),
        strip.text = element_text(color="white", size = 10),
        legend.title = element_blank(),
        legend.position = "top",
        panel.spacing.x = unit(3, "lines"),
        plot.margin = margin(.1, 0, 0, 0, "cm"),
        text = element_text(color="white", family = "Arial Narrow"),
        axis.text.x = element_text(size = 8, hjust = 1, color = 'white'),
        axis.text.y = element_text(color="white", size = 10))+
  annotate("text", x=1:7, y=0 ,label=as.character(levels(confirmados_edad_sexo$bins_edad)), color="white", size=3.5)+
  coord_flip()) %>% layout(width=350, height=350)


####### Propuesta de Gráfia
####### Piramde de casos sospechosos
sospechosos_hoy$bins_edad <- cut(sospechosos_hoy$Edad, breaks = bks, labels = lbs, include.lowest=TRUE)
sospechosos_edad_sexo <- sospechosos_hoy %>% group_by(bins_edad, Sexo) %>% summarise(sospechosos = n())
sospechosos_edad_sexo<-sospechosos_edad_sexo[!is.na(sospechosos_edad_sexo$bins_edad),]

plt_PirS<-ggplotly(ggplot(data = sospechosos_edad_sexo, 
                         mapping = aes(x = bins_edad, fill = reorder(Sexo, desc(Sexo)), 
                                       y = ifelse(test = Sexo %in% c("M"), 
                                                  yes = -sospechosos, no = sospechosos))) + 
                    geom_bar(stat = "identity", colour=alpha("grey90",.6), width=2 ,position=position_dodge(width=0)) +
                    scale_y_continuous(labels = abs, limits = max(sospechosos_edad_sexo$sospechosos) * c(-1,1)) +
                    scale_fill_manual(labels = c("Hombres", "Mujeres"), values=c(alpha("#607d8b",.9),alpha("#ff5722",.9))) +
                    theme_ipsum(axis = 'X')+
                    labs(title="Distribucion por rango de edad", 
                         subtitle=paste0("Fecha de corte: ", fecha_actualizacion,"\nTotal: ", as.character(sum(sospechosos_hoy$casos))),
                         caption="Elaboración propia con datos de la Secretaría de Salud",
                         x="",
                         y="") +
                    theme(panel.background = element_rect(fill =alpha("black", 0.35),
                                                          colour="black"),
                          plot.background = element_rect(fill = alpha("black", 0.35), colour = "black"),
                          plot.title = element_text(color="white", size = 12, face = "bold"),
                          plot.subtitle = element_text(color="white", size =11 ),
                          plot.caption = element_text(color="white", size = 10),
                          strip.text = element_text(color="white", size = 10),
                          legend.title = element_blank(),
                          legend.position = "top",
                          panel.spacing.x = unit(3, "lines"),
                          plot.margin = margin(.1, 0, 0, 0, "cm"),
                          text = element_text(color="white", family = "Arial Narrow"),
                          axis.text.x = element_text(size = 8, hjust = 1, color = 'white'),
                          axis.text.y = element_text(color="white", size = 10))+
                    annotate("text", x=1:7, y=0 ,label=as.character(levels(sospechosos_edad_sexo$bins_edad)), color="white", size=3.5)+
                    coord_flip()) %>% layout(width=350, height=350)



################  Distribucion de casos totales por Estado ###################

confirmados_estado <- confirmados %>% group_by(estado) %>% summarise(confirmados=n())
confirmados_estado <- arrange(confirmados_estado, desc(confirmados))

plt_ConfEst<-ggplotly(ggplot(confirmados_estado,
       aes(x=reorder(estado, confirmados), ### Aqui sólo hay que añadir la funcion reorder, el primer argmento es la variable que vas a aocupar en el eje x y el segundo es la variable por la que quieres ordenar
           y=confirmados)) +
  geom_bar(stat="identity", fill="#45b39d", alpha=.6, width=.8) +
  coord_flip() +
  geom_text(color="white", aes(label=confirmados), hjust=-0.2, size=2.5) +
  theme_ipsum(axis = 'X')+
  labs(title="Distribucion por estado", 
       subtitle=paste0("Fecha de corte: ", fecha_actualizacion,"\nTotal: ", as.character(sum(confirmados_nacional$nuevos_casos))),
       caption="Elaboración propia con datos de la Secretaría de Salud",
       x="",
       y="") +
  theme(panel.background = element_rect(fill =alpha("black", 0.35),
                                        colour="black"),
        plot.background = element_rect(fill = alpha("black", 0.35), colour = "black"),
        plot.title = element_text(color="white", size = 12, face = "bold", hjust=0.5),
        plot.subtitle = element_text(color="white", size =11 ),
        plot.caption = element_text(color="white", size = 10),
        strip.text = element_text(color="white", size = 11),
        plot.margin = margin(.1, 0, 0, 0, "cm"),
        panel.spacing.x = unit(3, "lines"),
        text = element_text(color="white", family = "Arial Narrow"),
        axis.text.x = element_text( size = 8, hjust = 1, color = 'white'),
        axis.text.y = element_text(color="white", size = 7))) %>% layout(width=350, height=350)


ggplot(confirmados_estado, aes(x=reorder(estado, confirmados), y=confirmados, label =confirmados)) + ##### Mismo truco que en la gráfica anterior
  geom_segment( aes(xend=estado, yend=0), color = "#45b39d") +
  geom_point( size = 4, color = "#45b39d") +
  coord_flip() +
  geom_text(color="white", vjust = 0.5, nudge_y = 0.5) +
  theme_ipsum(axis = 'X') +
  labs(title="Distribucion por estado", 
       subtitle=paste0("Fecha de corte: ", fecha_actualizacion,"\nTotal: ", as.character(sum(confirmados_nacional$nuevos_casos))),
       caption="Elaboración propia con datos de la Secretaría de Salud",
       x="",
       y="") +
  theme(panel.background = element_rect(fill =alpha("black", 0.35),
                                        colour="black"),
        plot.background = element_rect(fill = alpha("black", 0.35), colour = "black"),
        plot.title = element_text(color="white", size = 20, face = "bold"),
        plot.subtitle = element_text(color="white", size =15 ),
        plot.caption = element_text(color="white", size = 12),
        strip.text = element_text(color="white", size = 14),
        panel.spacing.x = unit(3, "lines"),
        text = element_text(color="white", family = "Arial Narrow"),
        axis.text.x = element_text(size = 12, hjust = 1, color = 'white'),
        axis.text.y = element_text(color="white", size = 10))
#ggsave(filename = paste0(path_img, "05_total_por_estado_", str_replace_all(fecha_actualizacion, "-", "_"), ".png"),
#       width = 10, height = 8, dpi = 100)

################  Distribucion de casos sospechosos totales por Estado ###################

sospechosos_estado <- sospechosos_hoy %>% group_by(Estado) %>% summarise(sospechosos=n())
sospechosos_estado <- arrange(sospechosos_estado, desc(sospechosos))

plt_SospEst<-ggplotly(ggplot(sospechosos_estado,
                             aes(x=reorder(Estado, sospechosos), ### Aqui sólo hay que añadir la funcion reorder, el primer argmento es la variable que vas a aocupar en el eje x y el segundo es la variable por la que quieres ordenar
                                 y=sospechosos)) +
                        geom_bar(stat="identity", fill="#45b39d", alpha=.6, width=.9) +
                        coord_flip() +
                        geom_text(color="white", aes(label=sospechosos), hjust=-0.2, size=2.8, nudge_y = (sospechosos_estado$sospechosos*.15)) +
                        theme_ipsum(axis = 'X')+
                        labs(title="Distribucion por estado", 
                             subtitle=paste0("Fecha de corte: ", fecha_actualizacion,"\nTotal: ", as.character(sum(sospechosos_hoy$casos))),
                             caption="Elaboración propia con datos de la Secretaría de Salud",
                             x="",
                             y="") +
                        theme(panel.background = element_rect(fill =alpha("black", 0.35),
                                                              colour="black"),
                              plot.background = element_rect(fill = alpha("black", 0.35), colour = "black"),
                              plot.title = element_text(color="white", size = 12, face = "bold", hjust=0.5),
                              plot.subtitle = element_text(color="white", size =11 ),
                              plot.caption = element_text(color="white", size = 10),
                              strip.text = element_text(color="white", size = 11),
                              plot.margin = margin(.1, 0, 0, 0, "cm"),
                              panel.spacing.x = unit(3, "lines"),
                              text = element_text(color="white", family = "Arial Narrow"),
                              axis.text.x = element_text( size = 8, hjust = 1, color = 'white'),
                              axis.text.y = element_text(color="white", size = 7))) %>% layout(width=350, height=350)


sospechosos_pais <- sospechosos_hoy %>% group_by(Procedencia) %>% summarise(sospechosos=n())
sospechosos_pais <- arrange(sospechosos_pais, desc(sospechosos))
sospechosos_pais$Procedencia[sospechosos_pais$Procedencia==""]<-"No Identificado"

plt_SospPais<-ggplotly(ggplot(sospechosos_pais,
                             aes(x=reorder(Procedencia, sospechosos), ### Aqui sólo hay que añadir la funcion reorder, el primer argmento es la variable que vas a aocupar en el eje x y el segundo es la variable por la que quieres ordenar
                                 y=sospechosos)) +
                        geom_bar(stat="identity", fill="#e07619", alpha=.8, width=.8) +
                        coord_flip() +
                        geom_text(color="white", aes(label=sospechosos), hjust=-0.2, size=4, nudge_y = -(sospechosos_pais$sospechosos/2)) +
                        theme_ipsum(axis = 'X')+
                        labs(title="Distribucion por País \nde arribo", 
                             subtitle=paste0("Fecha de corte: ", fecha_actualizacion,"\nTotal: ", as.character(sum(sospechosos_hoy$casos))),
                             caption="Elaboración propia con datos de la Secretaría de Salud",
                             x="",
                             y="") +
                        theme(panel.background = element_rect(fill =alpha("black", 0.35),
                                                              colour="black"),
                              plot.background = element_rect(fill = alpha("black", 0.35), colour = "black"),
                              plot.title = element_text(color="white", size = 12, face = "bold", hjust=0.5),
                              plot.subtitle = element_text(color="white", size =11 ),
                              plot.caption = element_text(color="white", size = 10),
                              strip.text = element_text(color="white", size = 11),
                              plot.margin = margin(.1, 0, 0, 0, "cm"),
                              panel.spacing.x = unit(3, "lines"),
                              text = element_text(color="white", family = "Arial Narrow"),
                              axis.text.x = element_text( size = 8, hjust = 1, color = 'white'),
                              axis.text.y = element_text(color="white", size = 10))) %>% layout(width=350, height=350)

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


plt_CumGender1<-ggplotly(ggplot(data = confirmados_sexo,
       aes(x=fecha_de_corte, 
           y=acumm, 
           fill = sexo,
           label=acumm)) +
  geom_bar(position="stack", stat="identity") +
  scale_x_date(date_breaks = "2 day") +
  #geom_col(aes(fill = sexo), position = "dodge") +
  #geom_text(color="white", aes(label = acumm), position = position_dodge(0.9)) +
  theme_ipsum() +
  labs(title="Número de casos confirmados de\n COVID-19 en México por día", 
       subtitle=paste0("Fecha de corte: ", fecha_actualizacion,"\nTotal: ", as.character(sum(confirmados_nacional$nuevos_casos))),
       caption="Elaboración propia con datos de la Secretaría de Salud",
       x="",
       y="") +
  theme(panel.background = element_rect(fill =alpha("black", 0.35),
                                        colour="black"),
        plot.background = element_rect(fill = alpha("black", 0.35), colour = "black"),
        plot.title = element_text(color="white", size = 12, face = "bold"),
        plot.subtitle = element_text(color="white", size =11 ),
        plot.caption = element_text(color="white", size = 10),
        strip.text = element_text(color="white", size = 10),
        panel.spacing.x = unit(3, "lines"),
        plot.margin = margin(.1, 0, 0, 0, "cm"),
        text = element_text(color="white", family = "Arial Narrow"),
        axis.text.x = element_text(color="white", size = 8, angle = 90, hjust = 1),
        axis.text.y = element_text(color="white", size = 10))) %>% layout(width=350, height=350)


#### Otro tipo de gráfico que te pude ser de utilidad

plt_CumGender2<-ggplotly(ggplot(confirmados_sexo, aes(x=fecha_de_corte, y=acumm, fill=sexo)) + 
  geom_area()+   scale_x_date(date_breaks = "2 day") + theme_ipsum() +
  scale_fill_manual(values=c("#bf2027", "#324a5e"))+
  # scale_fill_manual(values=rev(colsg1))+
  labs(title="Número de casos confirmados de \nCOVID-19 en México por día", 
       subtitle=paste0("Fecha de corte: ", fecha_actualizacion,"\nTotal: ", as.character(sum(confirmados_nacional$nuevos_casos))),
       caption="Elaboración propia con datos de la Secretaría de Salud",
       x="",
       y="") +
  theme(plot.title = element_text(color="white", size = 12, face = "bold"),
        plot.subtitle = element_text(color="white", size =11 ),
        plot.caption = element_text(color="white", size = 10),
        strip.text = element_text(color="white", size = 10),
        panel.spacing.x = unit(3, "lines"),
        text = element_text(color="white", family = "Arial Narrow"),
        axis.text.x = element_text(color="white", size = 8, angle = 90, hjust = 1),
        axis.text.y = element_text(color="white", size = 10))) %>% layout(width=350, height=350)


