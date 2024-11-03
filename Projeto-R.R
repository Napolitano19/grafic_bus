# 1. Carregar Pacotes Necessários
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("lubridate", quietly = TRUE)) install.packages("lubridate")
if (!requireNamespace("leaflet", quietly = TRUE)) install.packages("leaflet")

# Carregar os pacotes
library(dplyr)
library(ggplot2)
library(lubridate)
library(leaflet)

# 2. Carregar o Dataset
fator_rotatividade <- read.csv("C:/Users/falca/Downloads/fator_de_rotatividade_29-11-2018.csv", sep=";", header=TRUE)

# Carregar os arquivos de movimentação
movimentacao_paths <- c(
  "C:/Users/falca/Downloads/movimentacao_sobe_desce_29-11-2018-004.csv",
  "C:/Users/falca/Downloads/movimentacao_sobe_desce_29-11-2018-003.csv",
  "C:/Users/falca/Downloads/movimentacao_sobe_desce_29-11-2018-002.csv",
  "C:/Users/falca/Downloads/movimentacao_sobe_desce_29-11-2018-001.csv"
)

# Carregar todos os movimentos em uma lista
movimentos <- lapply(movimentacao_paths, function(x) read.csv(x, sep=";", header=TRUE))

# Combinar todos os dataframes de movimentação em um único dataframe
movimentacao_total <- do.call(rbind, movimentos)

# 3. Limpeza e Preparação de Dados

# Remover valores faltantes
fator_rotatividade <- fator_rotatividade %>% na.omit()
movimentacao_total <- movimentacao_total %>% na.omit()

cat("Número de linhas após remoção de NA no fator de rotatividade:", nrow(fator_rotatividade), "\n")
cat("Número de linhas após remoção de NA na movimentação total:", nrow(movimentacao_total), "\n")

# Renomear as colunas para o fator de rotatividade
colnames(fator_rotatividade) <- c("Linha_SubLinha", "Servico", "Consorcio", "Data_Pesquisa", 
                                  "Dia_Semana", "Rota", "Hora_Viagem", "Total_Passageiros", 
                                  "Maior_Numero_Passageiro_Transportado", "Fator_Rotatividade")

# Limpar a coluna Fator_Rotatividade
fator_rotatividade$Fator_Rotatividade <- as.numeric(gsub(",", ".", trimws(fator_rotatividade$Fator_Rotatividade)))

# Verificar a conversão
if (any(is.na(fator_rotatividade$Fator_Rotatividade))) {
  warning("Existem valores NA na coluna 'Fator_Rotatividade' após a conversão.")
}

# Renomear as colunas para a movimentação total, se necessário
colnames(movimentacao_total) <- c("Linha_SubLinha", "Servico", "Consorcio", "Data_Pesquisa", 
                                  "Dia_Semana", "Rota", "Hora_Viagem", "Sequencia_do_PED", 
                                  "Endereco_do_PED", "Embarque", "Desembarque", "Saldo", 
                                  "Total_Passageiros", "Latitude", "Longitude")

# 4. Análise Exploratória
# Análise de Rotatividade Média por Linha/SubLinha e Rota
analise_rotatividade <- fator_rotatividade %>%
  group_by(Linha_SubLinha, Rota) %>%
  summarize(rotatividade_media = mean(Fator_Rotatividade, na.rm = TRUE), 
            media_passageiros = mean(Total_Passageiros, na.rm = TRUE),
            .groups = 'drop')

# Frequência de Passageiros por Linha/SubLinha e Dia da Semana
frequencia_passageiros <- fator_rotatividade %>%
  group_by(Linha_SubLinha, Dia_Semana) %>%
  summarize(total_passageiros = sum(Total_Passageiros, na.rm = TRUE), .groups = 'drop')

# 5. Criação de Visualizações

# 6. Gerar Relatório
# Para criar o relatório, abra o R Markdown no RStudio e copie este código.
