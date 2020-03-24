ui <- navbarPage(theme = shinytheme("slate"), collapsible = TRUE,
                 "COVID-19 México", id="nav",
                 
                 tabPanel("Casos Confirmados",
                          div(class="outer",
                              tags$head(tags$style(
                                HTML('
                                #graphs1 {background-color: rgba(0,0,0,0.1);}
                                     #graphs2 {background-color: rgba(0,0,0,.1);}
                                     #titulo {background-color: rgba(0,0,0,0.1); border-color: rgba(0,0,0,0); margin: auto;}
                                     #graphsS1 {background-color: rgba(0,0,0,0.1);}
                                     #graphsS2 {background-color: rgba(0,0,0,.1);}
                                     #tituloS {background-color: rgba(0,0,0,0.1); border-color: rgba(0,0,0,0); margin: auto;}')
                              )),
                              leafletOutput("map1", width="100%", height="1000px"),
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
                                            plotlyOutput("plt_PirC")
                              ),
                              absolutePanel(id = "titulo", class = "panel panel-default",
                                            top = 100, left="50%", fixed=F,
                                            draggable = F, height = "auto",
                                            h3(textOutput("cuenta_confirmados"), align = "center")
                              )
                          )
                 ),
                 tabPanel("Casos Sospechozos",
                          div(class="outer",
                              leafletOutput("map2", width="100%", height="1000px"),
                              absolutePanel(id = "graphsS1", class = "panel panel-default",
                                            top = 100, left = 20, width = 350, fixed=TRUE,
                                            draggable = TRUE, height = "auto",
                                            plotlyOutput("plt_SospEst"),
                                            plotlyOutput("plt_SospPais")
                                            ),
                              absolutePanel(id = "graphsS2", class = "panel panel-default",
                                            top = 100, right = 20, width = 350, fixed=TRUE,
                                            draggable = TRUE, height = "auto",
                                            plotlyOutput("plt_PirS")),
                              absolutePanel(id = "tituloS", class = "panel panel-default",
                                            top = 100, left="50%", fixed=F,
                                            draggable = F, height = "auto",
                                            h3(textOutput("cuenta_sospechosos"), align = "center"))
                              )
                 )#,
                 # tabPanel("Acerca de este sitio",
                 #          tags$div(
                 #            tags$h4("1.- Objetivo"),
                 #            tags$h6("El Covid19 es un tema de salud pública y como tal queremos ayudar a todos los interesados en el uso, distribución y análisis de los datos que genera la Secretaría de Salud."),
                 #            tags$br(),
                 #            tags$h4("2.- Información"),
                 #            tags$br(),
                 #            tags$p("Siguiendo las propuestas de varios actores de la sociedad civil, ponemos a disposición publica los siguientes contenidos:"),
                 #            tags$br(),
                 #            tags$img(src = "www/folder.png", width = "15px", height = "15px"),
                 #            tags$h6("data:"),
                 #            tags$br(),
                 #            tags$p("Datos oficiales de casos sospechosos y confirmados publicados por la Secretaría de Salud en formatos csv desde el 17 de Marzo de 2020."),
                 #            tags$br(),
                 #            tags$img(src = "www/folder.png", width = "15px", height = "15px"),
                 #            tags$h6("output:"),
                 #            tags$br(),
                 #            tags$ul(
                 #              tags$li("Confirmados: Información públicada de los casos confirmados hasta el último corte, con la fecha en que cada uno de los casos fue publicado. Esta base es actualizada diariamente de manera manual para incluir los nuevos casos que da a conocer la Secretaría de Salud. Debido a esto podemos incurrir en errores propios o por parte de la misma Secretaría de Salud."),
                 #              tags$li("De hecho, como menciona ",tags$a(href="https://github.com/guzmart/covid19_mex","@guzmart_")," en un ejercicio similar, y corroborado por nosotros, los datos publicados el 20 de Marzo de 2020 toman como un nuevo caso a un paciente ya considerado en la información del día anterior. Ver más información acerca de errores en la sección 4."),
                 #              tags$li("Acumulados: Datos acumulados de los casos confirmados, sospechosos, negativos, recuperados, fallecidos y portadores obtenidos de los Comunicados Técnicos Diarios de la Secretaría de Salud.")
                 #            ),
                 #            tags$br(),
                 #            tags$img(src = "www/folder.png", width = "15px", height = "15px"),h6("Pdf_SS:"),
                 #            tags$br(),
                 #            tags$p("Originales de las Tablas de casos positivos (desde el 17 ed Marzo) y sospechosos (desde el 20 de Marzo) que publica la Secretaría de Salud"),
                 #            tags$a(href="https://www.gob.mx/salud/documentos/coronavirus-covid-19-comunicado-tecnico-diario-238449/","aqui"),
                 #            tags$br(),
                 #            tags$h4("3.- Análisis"),
                 #            tags$br(),
                 #            tags$br(),
                 #            tags$h4("4.- Errores de fuente"),
                 #            tags$br(),
                 #            tags$h6("Tabla de positivos"),
                 #            tags$br(),
                 #            tags$p("Las tablas de la Secretaria de Salud reportan diariamente las tablas de casos positivos en formato pdf, resaltando en color azul los casos nuevos. En esta sección se documentan errores en las versiones de las tablas."),
                 #            tags$br(),
                 #            tags$ul(
                 #              tags$li("En la tabla del 20.03.2020 se marco el caso 163 como caso nuevo, sin embargo este caso esta confirmado en la tabla del 19.03.2020 (caso 149)"),
                 #              tags$li("La tabla del 21-03-2020 no reporta en azul, es decir como casos nuevos, los casos 226, 227, 228, 229, pero estos casos no se encuentran en la tabla del 20.03.2020. Además, en la tabla del 20.03.2020 se reporta el caso 225, que desaparece en la tabla del 21.03.2020.")
                 #            ),
                 #            tags$br(),
                 #            tags$h8("Actualizacion de casos"),tags$br()
                 #            tags$ul(
                 #              tags$li("En la tabla del 20.03.2020 se actualizó el número de caso 67"),
                 #              tags$li("En la tabla 21.03.2020 se actualizó el número de caso 64"),
                 #              tags$li("La tabla del 23.03.2020 se actualizaron los países de residencia de los casos 167, 143, 311 y la edad del caso 311.")
                 #            )
                 #         )
                 # )
)
