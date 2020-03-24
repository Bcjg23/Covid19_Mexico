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
library(rgdal)
library(sf)
library(sp)
library(rgeos)
library(scales)
library(shadowtext)
library(plotly)
# library(tidyverse)
library(stringr)
library(hrbrthemes)



if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(leaftime)) install.packages("leaftime", repos = "http://cran.us.r-project.org")
if(!require(leaflet.minicharts)) install.packages("leaflet.minicharts", repos = "http://cran.us.r-project.org")


dir_base<-"/Users/carlosgutz/Dropbox/Documentos Insumos/Covid19" ##### MacPro
dir_base<-"C:/Users/gutie/Dropbox/Documentos Insumos/Covid19" ##### SfBook
# setwd(dir_base)
estados<-readOGR("data/shp/Estados", "ESTADOS", stringsAsFactors = FALSE, encoding = "UTF-8")
estados<-spTransform(estados, CRS("+proj=longlat +datum=WGS84"))
clv_estado<-read.csv("claves_estado.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
confirmados <- read.csv('confirmados_20_03_2020.csv', encoding="UTF-8", stringsAsFactors=FALSE)
names(clv_estado)<-c("Entidad", "id_estado")
wms<-"http://gaia.inegi.org.mx/NLB/tunnel/wms/wms61?"

for( file in c("maps.R",
               "graphs.R")){
  source(file)
}

ui <- navbarPage(theme = shinytheme("slate"), collapsible = TRUE,
                 "COVID-19 México", id="nav",
                 
                 tabPanel("Casos Confirmados",
                          div(class="outer",
                              tags$head(tags$style(
                                HTML('
                                #graphs1 {background-color: rgba(0,0,0,0.1);}
                                     #graphs2 {background-color: rgba(0,0,0,.1);}
                                     #titulo {background-color: rgba(0,0,0,0.1); border-color: rgba(0,0,0,0); margin: auto;}')
                              )),
                              leafletOutput("mymap", width="100%", height="1000px"),
                              absolutePanel(id = "graphs1", class = "panel panel-default",
                                            top = 100, left = 20, width = 350, fixed=TRUE,
                                            draggable = TRUE, height = "auto",
                                            plotlyOutput("plt_Conf"),
                                            plotlyOutput("plt_New")
                              ),
                              absolutePanel(id = "graphs2", class = "panel panel-default",
                                            top = 100, right = 20, width = 350, fixed=TRUE,
                                            draggable = TRUE, height = "auto",
                                            plotlyOutput("plt_ConfEst"),
                                            plotlyOutput("plt_Pir")
                              ),
                              absolutePanel(id = "titulo", class = "panel panel-default",
                                            top = 100, left="50%", fixed=F,
                                            draggable = F, height = "auto",
                                            h3(textOutput("reactive_case_count"), align = "center")
                              )
                          )
                 ),
                 tabPanel("Casos Sospechozos",
                          div(class="outer")
                          )
)

server = function(input, output) {
  output$mymap <- renderLeaflet({ 
    map
  })
  output$plt_Conf <- renderPlotly({plt_Conf })
  output$plt_New <- renderPlotly({plt_New })
  output$plt_ConfEst <- renderPlotly({plt_ConfEst})
  output$plt_Pir <- renderPlotly({plt_Pir})
  output$reactive_case_count <- renderText({
    paste0("Casos confirmados: ",prettyNum(sum(confirmados_hoy$casos), big.mark=","))
  })
  # output$plt_ConfEst <- renderPlotly({plt_ConfEst })
  }

shinyApp(ui, server)



