#PREPARAR DATOS
library(readr)
library(reshape2)
library(dplyr)
library(shinyWidgets)
library(plotly)
#library(leaflet)
#library(sf)
library(rgdal)
library(spatialEco)
library(shiny)
library(tmap)

data(World)
consolidado <- read.csv("consolidado.csv")
temp <- consolidado %>% group_by(ESTADO, PAIS, Lat, Long) %>% summarise(confirmed = max(confirmed, na.rm = T))
temp <- data.frame(temp[,c("Lat","Long", "PAIS" ,"confirmed")])
temp <- subset(temp, !c(is.na(Long) | Long == 0))
coordinates(temp)= ~Long+Lat
proj4string(temp) = CRS(sf::st_crs(4326)[[2]]) 
pts <- data.frame(point.in.poly(temp, World)@data)
temp <- pts %>% group_by(PAIS) %>% summarise(confirmed = max(confirmed))
pts2 <- inner_join(temp, pts, by = c("PAIS", "confirmed")) 
pts2 <- pts2[,c("continent", "PAIS")]
consolidado <- full_join(consolidado, pts2, by = "PAIS")
colnames(consolidado)[9]<-"CONTINENT"
consolidado$CONTINENT <- as.character(consolidado$CONTINENT)

ui <- navbarPage(title = "Dashboard COVID-19",
                tabPanel("Temporal",
                         tabPanel("Comportamiento temporal",
                                  sidebarLayout(
                                    sidebarPanel(width = 3,
                                                 fluidRow(      
                                                   column(
                                                     width = 12,  offset = 0,
                                                     tags$h3("Seleccionar filtro"),
                                                     panel(
                                                       pickerInput(
                                                         label = "Continente(s):",
                                                         inputId = "continente",
                                                         choices = unique(consolidado$CONTINENT),
                                                         options = list(`actions-box` = TRUE,
                                                                        title = "None selected"),
                                                         multiple = T,
                                                         selected = "North America"
                                                       ),
                                                       selectizeGroupUI(
                                                         inline=FALSE,
                                                         id = "my-filters",
                                                         params = list(
                                                           PAIS = list(inputId = "PAIS", title = "Pais(es):"),
                                                           ESTADO = list(inputId = "ESTADO", title = "Estado(s):")
                                                         )
                                                       ), status = "primary"
                                                     ),
                                                   )
                                                 ),
                                                 fluidRow(
                                                   column(width = 12,
                                                          checkboxGroupInput("variables", "Variable(s) a mostrar :",
                                                                             c("Numero de confirmados" = "confirmed",
                                                                               "Numero de recuperados" = "recovered",
                                                                               "Numero de fallecidos" = "deaths"), selected = "confirmed")
                                                   )
                                                 ),
                                                 fluidRow(
                                                   column(width = 12, offset = 0,
                                                          dateRangeInput("date", "Rango de fechas:",
                                                                         start = "2020-01-01",
                                                                         end   = "2021-11-24",
                                                                         separator = "a"),
                                                   )
                                                 )
                                    ),
                                    mainPanel(
                                      fluidRow(
                                        column(
                                          width = 12, offset = 0,
                                          plotlyOutput("plot1")
                                        )
                                      ),
                                      fluidRow(
                                        column(
                                          width = 11, offset = 0,
                                          tmapOutput("mapa_O1")
                                        ),
                                      ),
                                      br(),
                                      fluidRow(
                                        column(
                                          width = 12, offset = 0,
                                          dataTableOutput("tabla01")
                                        ) 
                                      )
                                    )
                                  )
                                  
                         )
                         ),
                tabPanel("Resumen", 
                         fluidPage(
                           p(strong("Boxplot para el porcentaje de casos recuperados de cada pais, agrupados por continente")),
                           fluidRow(
                             column(
                               width = 12, offset = 0,
                               plotlyOutput("plot02")
                               )
                             ),
                           p(strong("Boxplot para el porcentaje de mortalidad de cada pais, agrupados por continente")),
                           
                           fluidRow(
                             column(
                               width = 12, offset = 0,
                               plotlyOutput("plot03")
                             )
                           )
                           
                        )
                      )
                
                )





server <- function(input, output, session) {
  filtro_continente <- reactive({
    subset(consolidado, CONTINENT %in% input$continente)})
  res_mod <- callModule(
    module = selectizeGroupServer,
    id = "my-filters",
    data = filtro_continente,
    vars = c("PAIS", "ESTADO")
  )
  output$plot1 <- renderPlotly({
    table_f <- as.data.frame(res_mod())
    table_f$FECHA <- as.Date(table_f$FECHA, format = "%m/%d/%y")
    table <- table_f %>% subset(FECHA > input$date[1] & FECHA <input$date[2])
    table <- table %>% group_by(PAIS, FECHA) %>% summarise(confirmed = sum(confirmed, na.rm = T),
                                                           deaths = sum(deaths, na.rm = T),
                                                           recovered = sum(recovered, na.rm = T))
    table$FECHA <- as.Date(table$FECHA, format = "%m/%d/%y")
    if (length(input$variables)==1){
      
      table <- table[,c("PAIS", "FECHA", input$variables)]
      colnames(table) <- c("PAIS", "FECHA", "X1")
      plot1 <- plot_ly(data=table, x = ~ FECHA, y = ~ X1, type = "scatter", mode="lines", color= ~ PAIS)
      plot1 <- plot1 %>% layout(title = "",
                                xaxis = list(title = "Fecha"),
                                yaxis = list (title = paste0(input$variables, " cases")))
    }else{
      if(length(input$variables)==2){
        table <- table[,c("PAIS", "FECHA", input$variables)]
        colnames(table) <- c("PAIS", "FECHA", "X1", "X2")
        plot1<-plot_ly(data=table, x = ~ FECHA, y = ~ X1, type = "scatter", mode="lines", color= ~ PAIS)
        plot1 <- plot1 %>% layout(title = "",
                                  xaxis = list(title = "Fecha"),
                                  yaxis = list (title = paste0(input$variables[1], " Cases")))
        fplot1 <- plot1 %>% add_trace(y = ~X2, mode = 'lines+markers', name = paste0(input$variables[2], " cases"))
      }else{
        if(length(input$variables)==3){
          table <- table[,c("PAIS", "FECHA", input$variables)]
          colnames(table) <- c("PAIS", "FECHA", "X1", "X2", "X3")
          plot1 <- plot_ly(data=table, x = ~ FECHA, y = ~ X1, type = "scatter", mode="lines", color= ~ PAIS)
          plot1 <- plot1 %>% layout(title = "",
                                    xaxis = list(title = "Fecha"),
                                    yaxis = list (title = paste0(input$variables[1], " Cases")))
          fplot1 <- plot1 %>% add_trace(y = ~ X2, mode = 'lines+markers', name = paste0(input$variables[2], " cases"))
          fplot1 <- plot1 %>% add_trace(y = ~ X3, mode = 'markers', name = paste0(input$variables[3], " cases")) 
      }else{}}}
  })

  output$mapa_O1 <- renderTmap({
    table_f <- as.data.frame(res_mod())
    table_f$FECHA <- as.Date(table_f$FECHA, format = "%m/%d/%y")
    table_f <- table_f %>% subset(FECHA > input$date[1] & FECHA <input$date[2])
    table1 <- table_f %>% group_by(ESTADO, PAIS, Lat, Long) %>% summarise(confirmed = max(confirmed), 
                                                                         deaths = max(deaths),
                                                                         recovered = max(recovered))
    table <- data.frame(table1[,c("Lat","Long", "PAIS" ,"confirmed","deaths","recovered")])
    table <- subset(table, !c(Long==0 | is.na(Long)))
    coordinates(table)= ~Long+Lat
    proj4string(table) = CRS(sf::st_crs(4326)[[2]])
    ptsp <- data.frame(point.in.poly(table, World)@data)
    temp <- ptsp %>% group_by(PAIS) %>% summarise(confirmed = max(confirmed, na.rm = T))
    pts2 <- inner_join(temp, ptsp, by = c("PAIS", "confirmed"))
    pts2 <- na.omit(pts2[,c("iso_a3", "PAIS")])

    table2 <- table1 %>% 
      group_by(PAIS) %>% 
      summarise(confirmed = sum(confirmed, na.rm = T),
                deaths = sum(deaths, na.rm = T),
                recovered = sum(recovered, na.rm = T))
    pais <- inner_join(pts2, table2, by = "PAIS")
    
    mun_estado<- merge(World, pais, by.x ="iso_a3", by.y = "iso_a3" , all = FALSE)
    mun_estado$iso_a3 <-  NULL
    mapa_O1 <- tm_shape(mun_estado, name = "name") + tm_polygons(col = input$variables[1], 
                                                                 style = "cont", 
                                                                 title = paste0("Total ", input$variables[1]))
    mapa_O1
  })
  
  
  output$tabla01 <- renderDataTable({
    table_f <- as.data.frame(res_mod())
    table_f$FECHA <- as.Date(table_f$FECHA, format = "%m/%d/%y")
    table_f <- table_f %>% subset(FECHA > input$date[1] & FECHA <input$date[2])
    table1 <- table_f %>% group_by(ESTADO, PAIS, Lat, Long) %>% summarise(confirmed = max(confirmed), 
                                                                          deaths = max(deaths),
                                                                          recovered = max(recovered))
    table2 <- table1 %>% group_by(PAIS) %>% 
      summarise(confirmed = sum(confirmed, na.rm = T),
                deaths = sum(deaths, na.rm = T),
                recovered = sum(recovered, na.rm = T))
    
    table2$precuperados <- 100*(table2$recovered/table2$confirmed)
    table2$pmortalidad <- 100*(table2$deaths/table2$confirmed)
    colnames(table2) <- c("Pais", "Casos confirmados", "Total fallecidos", "Total recuperados", "Porcentaje de recuperados", "Porcentaje de mortalidad")
    table2
  })
  
  output$plot02 <- renderPlotly({
    table_f <- consolidado
    table_f <- subset(table_f, !c(is.na(CONTINENT)|CONTINENT=="Australia"))
    table1 <- table_f %>% group_by(CONTINENT,ESTADO, PAIS, Lat, Long) %>% 
      summarise(confirmed = max(confirmed), 
      deaths = max(deaths),
      recovered = max(recovered))
    table2 <- table1 %>% group_by(CONTINENT,PAIS) %>% 
      summarise(confirmed = sum(confirmed, na.rm = T),
                deaths = sum(deaths, na.rm = T),
                recovered = sum(recovered, na.rm = T))
    table2$precuperados <- 100*(table2$recovered/table2$confirmed)
    table2$pmortalidad <- 100*(table2$deaths/table2$confirmed)
    colnames(table2) <- c("Continente","Pais", "confirmados", "fallecidos", "recuperados", "Porcentajerecuperados", "Porcentajemortalidad")
    plot02 <- plot_ly(table2, y = ~Porcentajerecuperados, color = ~Continente, type = "box", text = ~Pais)
  })
  
  output$plot03 <- renderPlotly({
    table_f <- consolidado
    table_f <- subset(table_f, !c(is.na(CONTINENT)|CONTINENT=="Australia"))
    table1 <- table_f %>% group_by(CONTINENT,ESTADO, PAIS, Lat, Long) %>% 
      summarise(confirmed = max(confirmed), 
                deaths = max(deaths),
                recovered = max(recovered))
    table2 <- table1 %>% group_by(CONTINENT,PAIS) %>% 
      summarise(confirmed = sum(confirmed, na.rm = T),
                deaths = sum(deaths, na.rm = T),
                recovered = sum(recovered, na.rm = T))
    table2$precuperados <- 100*(table2$recovered/table2$confirmed)
    table2$pmortalidad <- 100*(table2$deaths/table2$confirmed)
    colnames(table2) <- c("Continente","Pais", "confirmados", "fallecidos", "recuperados", "Porcentajerecuperados", "Porcentajemortalidad")
    plot03 <- plot_ly(table2, y = ~Porcentajemortalidad, color = ~Continente, type = "box", text = ~Pais)
  })
  
}
shinyApp(ui = ui, server = server)
