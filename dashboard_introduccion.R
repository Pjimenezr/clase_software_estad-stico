library(shiny)
library(shinydashboard)
library(guaguas)
library(dplyr)
library(ggplot2)

ui <- dashboardPage(
  dashboardHeader(title = "Dashboard de nombres (guaguas)"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Visión general", tabName = "general",
               icon = icon("chart-bar")),
      menuItem("Detalle por nombre", tabName = "detalle",
               icon = icon("user"))
    ),
    sliderInput("anio_range",
                "Rango de años:",
                min = min(guaguas$anio),
                max = max(guaguas$anio),
                value = c(1990, 2020),
                step = 1),
    selectInput("sexo_dash",
                "Sexo:",
                choices = c("F", "M", "Ambos"),
                selected = "Ambos")
  ),
  dashboardBody(
    tabItems(
      #--- TAB 1: Visión general
      tabItem(tabName = "general",
              fluidRow(
                valueBoxOutput("box_total_n"),
                valueBoxOutput("box_total_nombres"),
                valueBoxOutput("box_anios")
              ),
              fluidRow(
                box(width = 6, title = "Top 10 nombres",
                    status = "primary", solidHeader = TRUE,
                    plotOutput("plot_top10_dash")),
                box(width = 6, title = "Distribución por año",
                    status = "primary", solidHeader = TRUE,
                    plotOutput("plot_series_dash"))
              )
      ),
      #--- TAB 2: Detalle por nombre
      tabItem(tabName = "detalle",
              fluidRow(
                box(width = 4, title = "Filtros nombre",
                    status = "info", solidHeader = TRUE,
                    textInput("nombre_detalle",
                              "Nombre a consultar:",
                              value = "Pablo"),
                    actionButton("btn_detalle",
                                 "Actualizar")
                ),
                box(width = 8, title = "Evolución del nombre",
                    status = "info", solidHeader = TRUE,
                    plotOutput("plot_detalle"))
              ),
              fluidRow(
                box(width = 12, title = "Tabla detalle",
                    status = "info", solidHeader = TRUE,
                    tableOutput("tabla_detalle"))
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Filtro global para todo el dashboard
  datos_filtrados_dash <- reactive({
    df <- guaguas %>%
      filter(anio >= input$anio_range[1],
             anio <= input$anio_range[2])
    
    if (input$sexo_dash != "Ambos") {
      df <- df %>% filter(sexo == input$sexo_dash)
    }
    df
  })
  
  #--- KPIs
  output$box_total_n <- renderValueBox({
    df <- datos_filtrados_dash()
    valueBox(
      value = format(sum(df$n), big.mark = "."),
      subtitle = "Total de inscripciones",
      icon = icon("baby"),
      color = "purple"
    )
  })
  
  output$box_total_nombres <- renderValueBox({
    df <- datos_filtrados_dash()
    valueBox(
      value = length(unique(df$nombre)),
      subtitle = "Nombres distintos",
      icon = icon("list"),
      color = "teal"
    )
  })
  
  output$box_anios <- renderValueBox({
    valueBox(
      value = paste(input$anio_range[1], "-", input$anio_range[2]),
      subtitle = "Rango de años",
      icon = icon("calendar-alt"),
      color = "blue"
    )
  })
  
  #--- Top 10 nombres
  output$plot_top10_dash <- renderPlot({
    df <- datos_filtrados_dash() %>%
      group_by(nombre) %>%
      summarise(total = sum(n), .groups = "drop") %>%
      arrange(desc(total)) %>%
      head(10)
    
    ggplot(df, aes(x = reorder(nombre, total), y = total)) +
      geom_col() +
      coord_flip() +
      labs(x = "Nombre", y = "Total inscripciones",
           title = "Top 10 nombres en el rango seleccionado") +
      theme_minimal()
  })
  
  #--- Serie de tiempo (total por año)
  output$plot_series_dash <- renderPlot({
    df <- datos_filtrados_dash() %>%
      group_by(anio) %>%
      summarise(total = sum(n), .groups = "drop")
    
    ggplot(df, aes(x = anio, y = total)) +
      geom_line() +
      geom_point() +
      theme_minimal() +
      labs(x = "Año", y = "Total inscripciones",
           title = "Total de inscripciones por año")
  })
  
  #--- Detalle por nombre (tab 2)
  datos_detalle <- eventReactive(input$btn_detalle, {
    datos_filtrados_dash() %>%
      filter(tolower(nombre) == tolower(input$nombre_detalle)) %>%
      group_by(anio, sexo) %>%
      summarise(total = sum(n), .groups = "drop")
  })
  
  output$plot_detalle <- renderPlot({
    df <- datos_detalle()
    req(nrow(df) > 0)
    
    ggplot(df, aes(x = anio, y = total, color = sexo)) +
      geom_line() +
      geom_point() +
      theme_minimal() +
      labs(x = "Año", y = "Inscripciones",
           title = paste("Evolución del nombre",
                         input$nombre_detalle))
  })
  
  output$tabla_detalle <- renderTable({
    datos_detalle()
  })
  
}

shinyApp(ui, server)
