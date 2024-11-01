# Carregar as bibliotecas necessárias
install.packages("shiny")
install.packages("ggplot2")
install.packages("readr")

# Carregar as bibliotecas necessárias
library(shiny)
library(ggplot2)
library(readr)

# Interface do usuário
ui <- fluidPage(
  titlePanel("Visualização de Total de Passageiros"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Este gráfico mostra a variação do total de passageiros ao longo das datas.")
    ),
    
    mainPanel(
      plotOutput("grafico_passageiros")
    )
  )
)

# Lógica do servidor
server <- function(input, output) {
  # Leitura do arquivo CSV usando o caminho fixo
  dados <- reactive({
    df <- read_csv("C:/Users/Kawl/Desktop/grafic_bus/doc/fator_de_rotatividade_29-11-2018.csv", 
                   locale = locale(encoding = "ISO-8859-1"), 
                   col_names = TRUE, 
                   show_col_types = FALSE)
    
    # Exibir as primeiras linhas e colunas disponíveis para verificar se a leitura foi bem-sucedida
    print(head(df))
    print(colnames(df))  # Exibe o nome das colunas no console
    
    # Retorna o data frame carregado
    df
  })
  
  # Renderizar o gráfico de Total Passageiros
  output$grafico_passageiros <- renderPlot({
    req(dados())
    
    dados_ajustados <- dados()
    
    # Checar se "Total Passageiros" existe nos dados e lançar mensagem se não estiver
    if (!"Total Passageiros" %in% colnames(dados_ajustados)) {
      stop("A coluna 'Total Passageiros' não foi encontrada. Verifique o nome das colunas.")
    }
    
    # Convertendo "Total Passageiros" para numérico
    dados_ajustados$`Total Passageiros` <- as.numeric(dados_ajustados$`Total Passageiros`)
    
    # Convertendo a coluna "Data Pesquisa" para o formato de data
    dados_ajustados$`Data Pesquisa` <- as.Date(dados_ajustados$`Data Pesquisa`, format = "%d/%m/%Y")
    
    # Gera o gráfico de linha para Total Passageiros ao longo do tempo
    ggplot(dados_ajustados, aes(x = `Data Pesquisa`, y = `Total Passageiros`)) +
      geom_line(color = "blue") +
      geom_point(color = "red") +
      labs(x = "Data", y = "Total de Passageiros", title = "Variação do Total de Passageiros ao Longo do Tempo") +
      theme_minimal()
  })
}

# Executa a aplicação Shiny
shinyApp(ui = ui, server = server)
