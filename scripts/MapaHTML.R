rm(list=ls())
library(shiny)
library(shinyWidgets)
library(shinythemes) 
library(digest)
library(haven)
library(reshape2)
library(DT)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(taRifx)
library(rapport)
library(leaftime)
library(leaflet.minicharts)
library(leafpop)
library(lubridate)
library(covidMex) #### Datos Covid19
 require(geos_combine)
 library(rgdal)
 library(sf)
 library(sp)
 library(rgeos)
 library(scales)
 library(shadowtext)
library(stringr)


if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(leaftime)) install.packages("leaftime", repos = "http://cran.us.r-project.org")
if(!require(leaflet.minicharts)) install.packages("leaflet.minicharts", repos = "http://cran.us.r-project.org")



dir_base<-"C:/Users/gutie/Dropbox/Documentos Insumos/Covid19" ##### SfBook
setwd(dir_base)
estados<-readOGR("shp/Estados", "ESTADOS", stringsAsFactors = FALSE, encoding = "UTF-8")
estados<-spTransform(estados, CRS("+proj=longlat +datum=WGS84"))
clv_estado<-read.csv("claves_estado.csv", encoding = "UTF-8")
names(clv_estado)<-c("Entidad", "id_estado")
wms<-"http://gaia.inegi.org.mx/NLB/tunnel/wms/wms61?"

sospechosos_hoy <-getData(type = "suspect", source = "Serendipia")
confirmados_hoy <- getData(type = "confirmed")
sospechosos_old <- getData(type = "suspect", date = "18/03/2020",  source = "Serendipia")
confirmados_old <- getData(type = "confirmed", date = "18/03/2020")

sospechosos_hoy<-sospechosos_hoy[!is.na(sospechosos_hoy$Estado),]
sospechosos_old<-sospechosos_old[!is.na(sospechosos_old$Estado),]
confirmados_hoy<-confirmados_hoy[!is.na(confirmados_hoy$ent),]
confirmados_old<-confirmados_old[!is.na(confirmados_old$ent),]

sospechosos_hoy<-merge(sospechosos_hoy, clv_estado, by.x="Estado", by.y="Entidad", all.x=T)
sospechosos_old<-merge(sospechosos_old, clv_estado, by.x="Estado", by.y="Entidad", all.x=T)
confirmados_hoy<-merge(confirmados_hoy, clv_estado, by.x="ent", by.y="Entidad", all.x=T)
confirmados_old<-merge(confirmados_old, clv_estado, by.x="ent", by.y="Entidad", all.x=T)

sospechosos_hoy$casos<-1
sospechosos_old$casos<-1
confirmados_old$casos<-1
confirmados_hoy$casos<-1
sospechosos_hoy$gEdad<-cut(sospechosos_hoy$Edad,
                           c(0,14,24,29,35,45,60,Inf),
                           labels = c("0-14", "15-24", "25-29","29-35", "36-45", "46-60", "60-100"),
                           include.lowest = T)
confirmados_hoy$gEdad<-cut(confirmados_hoy$edad,
                           c(0,14,24,29,35,45,60,Inf),
                           labels = c("0-14", "15-24", "25-29","29-35", "36-45", "46-60", "60-100"),
                           include.lowest = T)



mSospechosos_hoy<-sospechosos_hoy %>%
  group_by(id_estado) %>%
  summarise(casos_S_m=sum(casos[Sexo=="M"], na.rm=T),
            casos_S_f=sum(casos[Sexo=="F"], na.rm=T),
            casos_S=sum(casos, na.rm=T))

mConfirmados_hoy<-confirmados_hoy %>%
  group_by(id_estado) %>%
  summarise(casos_C_m=sum(casos[sexo=="M"], na.rm=T),
            casos_C_f=sum(casos[sexo=="F"], na.rm=T),
            casos_C=sum(casos, na.rm=T))

estados<- merge(estados, mSospechosos_hoy, by.x="ID_ESTADO", by.y="id_estado", all.x=T )
estados<- merge(estados, mConfirmados_hoy, by.x="ID_ESTADO", by.y="id_estado", all.x=T )
for(var in c("casos_S","casos_S_f", "casos_S_m", "casos_C", "casos_C_f", "casos_C_m")){
  estados[[var]][is.na(estados[[var]])]<-0
}


mSospechosos_hoy<-sospechosos_hoy %>%
  group_by(id_estado, Sexo) %>%
  summarise(casos_S_m=sum(casos[Sexo=="M"], na.rm=T),
            casos_S_f=sum(casos[Sexo=="F"], na.rm=T),
            casos_S=sum(casos, na.rm=T))
mSospechosos_hoy<-data.frame(mSospechosos_hoy)
mSospechosos_hoy<-dcast(mSospechosos_hoy, id_estado~Sexo, value.var="casos_S")
mSospechosos_hoy<-merge(data.frame(estados)[1], mSospechosos_hoy, by.x="ID_ESTADO", by.y="id_estado", all.x=T)
mSospechosos_hoy$F[is.na(mSospechosos_hoy$F)]<-0
mSospechosos_hoy$M[is.na(mSospechosos_hoy$M)]<-0
mSospechosos_hoy<-melt(mSospechosos_hoy, i=c("ID_ESTADO"))
names(mSospechosos_hoy)<-c("id_estado", "Sexo", "casos_S")

mConfirmados_hoy<-confirmados_hoy %>%
  group_by(id_estado, sexo) %>%
  summarise(casos_C_m=sum(casos[sexo=="M"], na.rm=T),
            casos_C_f=sum(casos[sexo=="F"], na.rm=T),
            casos_C=sum(casos, na.rm=T))
mConfirmados_hoy<-data.frame(mConfirmados_hoy)
mConfirmados_hoy<-dcast(mConfirmados_hoy, id_estado~sexo, value.var="casos_C")
mConfirmados_hoy<-merge(data.frame(estados)[1], mConfirmados_hoy, by.x="ID_ESTADO", by.y="id_estado", all.x=T)
mConfirmados_hoy$F[is.na(mConfirmados_hoy$F)]<-0
mConfirmados_hoy$M[is.na(mConfirmados_hoy$M)]<-0
mConfirmados_hoy<-melt(mConfirmados_hoy, i=c("ID_ESTADO"))
names(mConfirmados_hoy)<-c("id_estado", "Sexo", "casos_C")


piram_Sospechosos<-sospechosos_hoy %>% 
  group_by(gEdad, Sexo, id_estado ) %>%
  summarise(casos_S=sum(casos, na.rm=T))
piram_Confirmados<-confirmados_hoy %>% 
  group_by(gEdad, sexo, id_estado ) %>%
  summarise(casos_C=sum(casos, na.rm=T))


pirS<-list()
t=0
for(i in estados$ID_ESTADO){
  t=t+1
  p<- ggplot(data = piram_Sospechosos[piram_Sospechosos$id_estado==i,], 
             mapping = aes(x = gEdad, fill = reorder(Sexo, desc(Sexo)), 
                           y = ifelse(test = Sexo %in% c("M"), 
                                      yes = -casos_S, no = casos_S))) + 
    geom_bar(stat = "identity", colour=alpha("grey90",.6), width=2 ,position=position_dodge(width=0)) +
    scale_y_continuous(labels = abs, limits = max(piram_Sospechosos$casos_S) * c(-1,1)) +
    #scale_fill_manual(values=c(alpha("#607d8b",.9), NA, alpha("#ff5722",.9), NA )) +
    scale_fill_manual(labels = c("Hombres", "Mujeres"), values=c(alpha("#607d8b",.9),alpha("#ff5722",.9))) +
    theme(panel.background = element_rect(fill = "black",
                                          colour="black"),
          plot.background = element_rect(fill = "black", colour = "black"),
          axis.ticks = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.position = "top",
          legend.title = element_blank(),
          legend.background = element_rect(fill = "black"),
          legend.text= element_text(size=10, color = "white"),
          plot.caption = element_blank(),
          axis.ticks.y = element_blank(), axis.text.y = element_blank(),
          axis.text.x = element_text(size=3.5, color="white"),
          axis.title.x = element_blank()) +
    annotate("text", x=1:7, y=0 ,label=as.character(levels(piram_Sospechosos$gEdad)), color="white", size=3.5)+
    coord_flip()
  pirS[[t]]<-p
}

pirC<-list()
t=0
for(i in estados$ID_ESTADO){
  t=t+1
  p<- ggplot(data = piram_Confirmados[piram_Confirmados$id_estado==i,], 
             mapping = aes(x = gEdad, fill = reorder(sexo, desc(sexo)), 
                           y = ifelse(test = sexo %in% c("M"), 
                                      yes = -casos_C, no = casos_C))) + 
    geom_bar(stat = "identity", colour=alpha("grey90",.6), width=2 ,position=position_dodge(width=0)) +
    scale_y_continuous(labels = abs, limits = max(piram_Confirmados$casos_C) * c(-1,1)) +
    #scale_fill_manual(values=c(alpha("#607d8b",.9), NA, alpha("#ff5722",.9), NA )) +
    scale_fill_manual(labels = c("Hombres", "Mujeres"), values=c(alpha("#607d8b",.9),alpha("#ff5722",.9))) +
    theme(panel.background = element_rect(fill = "black",
                                          colour="black"),
          plot.background = element_rect(fill = "black", colour = "black"),
          axis.ticks = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.position = "top",
          legend.title = element_blank(),
          legend.background = element_rect(fill = "black"),
          legend.text= element_text(size=10, color = "white"),
          plot.caption = element_blank(),
          axis.ticks.y = element_blank(), axis.text.y = element_blank(),
          axis.text.x = element_text(size=3.5, color="white"),
          axis.title.x = element_blank()) +
    annotate("text", x=1:7, y=0 ,label=as.character(levels(piram_Sospechosos$gEdad)), color="white", size=3.5)+
    coord_flip()
  pirC[[t]]<-p
}


par(bg="black")

pltS<-list()
t=0
for(i in estados$ID_ESTADO){
  t=t+1
  p<- ggplot(mSospechosos_hoy[mSospechosos_hoy$id_estado==i,], aes(x="", y=casos_S, fill=as.character(1:nrow(mSospechosos_hoy[mSospechosos_hoy$id_estado==i,]))))+
    geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + 
    theme(axis.text.x=element_blank())+
    # geom_text(aes(y = pOcupada/3 + c(0, cumsum(pOcupada)[-length(pOcupada)]), 
    #               label = percent(pOcupada)), size=12)+
    geom_shadowtext(aes(y = casos_S/3 + c(0, cumsum(casos_S)[-length(casos_S)]), 
                        label = percent(casos_S/sum(casos_S))),
                    size=3, colour="white", fontface = "bold")+
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.border = element_blank(),
      panel.grid=element_blank(),
      panel.background = element_rect(fill = "black", colour = "black"),
      axis.ticks = element_blank(),
      plot.caption = element_blank(),
      plot.background = element_rect(fill = "black", colour = "black", color="black"),
      legend.background = element_rect(fill = "black"),
      legend.title = element_blank(),
      legend.text= element_text(size=10, color = "white"),
      legend.position = "top"
    )+
    # labs(fill="Sexo")+
    scale_fill_manual(labels = c("Mujeres", "Hombres"),values = c("#f56c42", "#5b8ea3"))
  pltS[[t]]<-p
}

pltC<-list()
t=0
for(i in estados$ID_ESTADO){
  t=t+1
  p<- ggplot(mConfirmados_hoy[mConfirmados_hoy$id_estado==i,], aes(x="", y=casos_C, fill=as.character(1:nrow(mConfirmados_hoy[mConfirmados_hoy$id_estado==i,]))))+
    geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + 
    theme(axis.text.x=element_blank())+
    # geom_text(aes(y = pOcupada/3 + c(0, cumsum(pOcupada)[-length(pOcupada)]), 
    #               label = percent(pOcupada)), size=12)+
    geom_shadowtext(aes(y = casos_C/3 + c(0, cumsum(casos_C)[-length(casos_C)]), 
                        label = percent(casos_C/sum(casos_C))),
                    size=3, colour="white", fontface = "bold")+
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.border = element_blank(),
      panel.grid=element_blank(),
      panel.background = element_rect(fill = "black", colour = "black"),
      axis.ticks = element_blank(),
      plot.caption = element_blank(),
      plot.background = element_rect(fill = "black", colour = "black", color="black"),
      legend.background = element_rect(fill = "black"),
      legend.title = element_blank(),
      legend.text= element_text(size=10, color = "white"),
      legend.position = "top"
    )+
    # labs(fill="Sexo")+
    scale_fill_manual(labels = c("Mujeres", "Hombres"),values = c("#f56c42", "#5b8ea3"))
  pltC[[t]]<-p
}



labelsS <- sprintf(
  "Estado: <strong>%s<br><strong>%d Casos sospechosos<br>Mujeres: %d <br>Hombres: %d",
  estados$NOM_ENT, estados$casos_S, estados$casos_S_f, estados$casos_S_m
) %>% lapply(htmltools::HTML)

labelsC <- sprintf(
  "Estado: <strong>%s<br><strong>%d Casos confirmados<br>Mujeres: %d<br>Hombres: %d",
  estados$NOM_ENT, estados$casos_C, estados$casos_C_f, estados$casos_C_m
) %>% lapply(htmltools::HTML)

labelsS <-paste0(labelsS,"<br>", popupGraph(pltS, type = "svg", width=175, height=175),
                 "<br>", popupGraph(pirS, type = "svg", width=175, height=175))%>% lapply(htmltools::HTML)
labelsC <-paste0(labelsC,"<br>", popupGraph(pltC, type = "svg", width=175, height=175),
                 "<br>", popupGraph(pirC, type = "svg", width=175, height=175))%>% lapply(htmltools::HTML)%>% lapply(htmltools::HTML)


coord<-data.frame(coordinates(estados))


map<-leaflet() %>% 
  # addWMSTiles(
  #   wms,
  #   layers = "MGE",
  #   options = WMSTileOptions(format = "image/png", transparent = TRUE),
  #   group="INEGI") %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group="Dark") %>%
  # addTiles(group = "OSM (default)") %>%
  addProviderTiles(providers$Stamen.Toner, group="Toner") 

map<- map %>%
  addPolygons(
    data=estados,
    fill=TRUE,
    stroke=TRUE,
    color = "White", weight = .5, smoothFactor = 0.5,
    opacity = 0.7, fillOpacity = 0.3,
    fillColor = ~colorQuantile("YlOrRd", estados$casos_S)(estados$casos_S),
    highlightOptions = highlightOptions(
      color = "White", weight = 1,
      bringToFront = TRUE
    ),
    label = labelsS,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px", background="black", "color"="white"),
      textsize = "15px",
      direction = "auto",
      opacity = .85),
    group="Sospechosos"
  ) %>%
  addPolygons(
    data=estados,
    fill=TRUE,
    stroke=TRUE,
    color = "White", weight = .5, smoothFactor = 0.5,
    opacity = 0.7, fillOpacity = 0.3,
    fillColor = ~colorQuantile("YlOrRd", estados$casos_C)(estados$casos_C),
    highlightOptions = highlightOptions(
      color = "White", weight = 1,
      bringToFront = TRUE
    ),
    label = labelsC,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px", background="black", "color"="white"),
      textsize = "15px",
      direction = "auto",
      opacity=.85),
    # popup = leafpop::popupGraph(plt, type = "svg", width=150, height=150),
    group="Confirmados"
  ) %>%
  # addMinicharts(
  #   coord$X1, coord$X2,
  #   type = "pie",
  #   chartdata = data.frame(estados)[, c("casos_S_m", "casos_S_f")],
  #   colorPalette = c("#f56c42", "#5b8ea3"),
  #   width = 15
  #   , transitionTime = 0
  # ) %>%
  addLegend(pal = colorQuantile("YlOrRd", estados$casos_C),
            values =  estados$casos_C , opacity = 0.7, title = "Cuartiles",
            position = "bottomleft",
            group="Confirmados") %>%
  addLegend(pal = colorQuantile("YlOrRd", estados$casos_S),
            values =  estados$casos_S , opacity = 0.7, title = "Cuartiles",
            position = "bottomleft",
            group="Sospechosos") %>%
  addLayersControl(baseGroups = c("Dark", "Toner"),
                 overlayGroups = c("Sospechosos","Confirmados"),
                 options = layersControlOptions(collapsed = FALSE)) %>%
  addMiniMap(
    tiles = providers$Stamen.Toner,
    toggleDisplay = TRUE) %>%
  addEasyButton(easyButton(
    icon="fa-globe", title="Zoom to Level 8",
    onClick=JS("function(btn, map){ map.setZoom(8); }"))) %>%
  hideGroup("Sospechosos")
  # addMeasure(
  #   position = "bottomleft",
  #   primaryLengthUnit = "meters",
  #   primaryAreaUnit = "sqmeters",
  #   activeColor = "#3D535D",
  #   completedColor = "#7D4479")

 saveWidget(map, file="map.html")





sospechosos<-data.frame()
i=0
for(t in seq(as.Date("2020-03-15"), Sys.Date(), by=1)){
  i=i+1
  tmp<- getData(type = "suspect", date = format(as.Date.numeric(t, origin="1970-01-01"), "%d/%m/%Y"),  source = "Serendipia")
  tmp$f_Reporte<-format(as.Date.numeric(t, origin="1970-01-01"), "%d/%m/%Y")
  if(i>1){names(tmp)<-names(sospechosos)}
  sospechosos<-rbind(sospechosos, tmp)
}


confirmados<-data.frame()
i=0
for(t in seq(as.Date("2020-03-16"), Sys.Date(), by=1)){
  i=i+1
  tmp<- getData(type = "confirmed", date = format(as.Date.numeric(t, origin="1970-01-01"), "%d/%m/%Y"))
  tmp$f_Reporte<-format(as.Date.numeric(t, origin="1970-01-01"), "%d/%m/%Y")
  if(i>1){names(tmp)<-names(confirmados)}
  confirmados<-rbind(confirmados, tmp)
}


