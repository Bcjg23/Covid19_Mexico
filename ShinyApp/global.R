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
# library(devtools)
# devtools::install_github("pablorm296/covidMex")
# library(covidMex) #### Datos Covid19
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