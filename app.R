library(shiny)
library(shinydashboard)
library(tidyverse)

theme_set(theme_minimal())

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(
  ),
  dashboardSidebar(
    sidebarMenu(
      fileInput("base", "Faça o upload da base do seu histórico da Netflix. Nós não coletaremos os seus dados. O arquivo será deletado assim que a sessão for finalizada."),
      tags$p(
        class = "cursor",
        tags$a(
          href = "https://curso-r.com",
          "Criado pela",
          tags$br(),
          tags$img(src = "logo.png", width = "40%")
        ),
        tags$br(),
        tags$br(),
        tags$a(
          href = "https://github.com/curso-r/shinyNetflix",
          "Código fonte",
          tags$br(),
          tags$img(src = "https://image.flaticon.com/icons/svg/25/25231.svg", width = "15%")
        )
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    fluidRow(
      box(
        width = 12,
        plotOutput("grafico", height = "600px")
      )
    )
  )
)

server <- function(input, output, session) {
  
  netflix <- reactive({
    if (is.null(input$base))
      return(NULL)
    
    df <- read_csv(input$base$datapath) %>%
      mutate(
        Date = lubridate::mdy(Date),
        mes = lubridate::month(Date, label = TRUE),
        mes = lvls_revalue(mes, c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")),
        ano = lubridate::year(Date),
        mes_ano = Date,
        programa = str_remove(Title, ":.*")
      )
    
    lubridate::day(df$mes_ano) <- 1
    
    df
  })
  
  output$grafico <- renderPlot({
    
    req(netflix())
    
    p1 <- netflix() %>% 
      count(mes_ano) %>% 
      ggplot(aes(x = mes_ano, y = n)) +
      geom_line() +
      geom_smooth(se = FALSE, color = "#e50914") +
      labs(x = "Ano", y = "Frequência") +
      ggtitle("Frequência ao longo do tempo")
    
    p2 <- netflix() %>% 
      count(mes, ano) %>% 
      group_by(mes) %>% 
      summarise(freq_media = mean(n)) %>% 
      mutate(freq_media = round(freq_media)) %>% 
      ggplot(aes(x = mes, y = freq_media)) +
      geom_col(fill = "#e50914", color = "black") +
      geom_text(aes(label = freq_media, y = freq_media/2), color = "white") +
      labs(x = "Mês", y = "Frequência média") +
      ggtitle("Frequência média por mês")
    
    p3 <- netflix() %>% 
      count(ano) %>% 
      ggplot(aes(x = ano, y = n)) +
      geom_col(fill = "#e50914", color = "black") +
      geom_text(aes(label = n, y = n/2), color = "white") +
      labs(x = "Ano", y = "Frequência total") +
      scale_x_continuous(breaks = min(netflix()$ano):max(netflix()$ano)) +
      ggtitle("Frequência por ano")
    
    p4 <- netflix() %>% 
      count(programa) %>%
      top_n(10, n) %>% 
      mutate(programa = fct_reorder(programa, n)) %>% 
      ggplot(aes(x = programa, y = n)) +
      geom_col(fill = "#e50914", color = "black") +
      geom_text(aes(label = n, y = n/2), color = "white") +
      labs(x = "Série", y = "Número de vezes assistida(o)") +
      coord_flip() +
      ggtitle("Top 10 séries/filmes")
    
    patchwork::wrap_plots(p1, p2, p3, p4, ncol = 2)
  })
}

shinyApp(ui, server)
