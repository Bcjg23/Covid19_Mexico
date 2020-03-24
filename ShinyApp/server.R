server <- function(input, output, session) {
  output$map1 <- renderLeaflet({ 
    mapC
  })
  output$map2 <- renderLeaflet({ 
    mapS
  })
  output$plt_Conf <- renderPlotly({plt_Conf })
  output$plt_New <- renderPlotly({plt_New })
  output$plt_ConfEst <- renderPlotly({plt_ConfEst})
  output$plt_PirC <- renderPlotly({plt_PirC})
  output$plt_SospPais <- renderPlotly({plt_SospPais})
  output$plt_SospEst <- renderPlotly({plt_SospEst})
  output$plt_PirS <- renderPlotly({plt_PirS})
  
  
  output$cuenta_confirmados <- renderText({
    paste0("Casos confirmados al ",format(max(confirmados$fecha_de_corte), "%d/%m/%Y"),": ",prettyNum(sum(confirmados_hoy$casos), big.mark=","))
  })
  output$cuenta_sospechosos <- renderText({
    paste0("Casos sospechosos al ",format(max(confirmados$fecha_de_corte), "%d/%m/%Y"),": ",prettyNum(sum(sospechosos_hoy$casos), big.mark=","))
  })
  # output$plt_ConfEst <- renderPlotly({plt_ConfEst })
}
