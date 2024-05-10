library(readr)
dados <- read_csv("banco/banco_final.csv")

#ANALISE 1  - Número de lançamentos a cada década por formato de lançamento:

# 1) Número de lançamentos a cada década por formato de lançamento;

### separando os lançamentos em décadas

install.packages("lubridate")
install.packages("dplyr")


library(lubridate)
library(dplyr)
library(tidyr)
library(stringr)
library(tidyverse)

# arrumando theme_estat

estat_colors <- c(
  "#A11D21", "#003366", "#CC9900",
  "#663333", "#FF6600", "#CC9966",
  "#999966", "#006606", "#008091", 
  "#041835", "#666666" )

theme_estat <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  
  return(
    list(
      theme,
      scale_fill_manual(values = estat_colors),
      scale_colour_manual(values = estat_colors)
    )
  )
}


# Converter para formato de data, especificando o formato
dados <- dados %>%
  mutate(date_aired = tryCatch(as.Date(date_aired, format = "%Y-%m-%d"), error = function(e) NA))

# Filtrar as linhas que foram convertidas com sucesso
dados <- dados %>%
  filter(!is.na(date_aired))

# Extrair o ano
dados <- dados %>%
  mutate(year = year(date_aired))

# Calcular a década
dados <- dados %>%
  mutate(decada = 10 * floor(year / 10))



# Agrupar por década e formato de lançamento, e contar o número de lançamentos em cada grupo

library(ggplot2)

banco1 <- dados %>%
  group_by(decada, format)

caminho_junior <- "resultados"

banco1$decada <- as.factor(banco1$decada)

banco1 <- banco1 %>%
  group_by(decada, format) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = round(freq / sum(freq) * 100,1)
  )
porcentagens <- str_c(banco1$freq_relativa, "%") %>% str_replace("
\\.", ",")
legendas <- str_squish(str_c(banco1$freq, " (", porcentagens, ")")
)

library(dplyr)

# Renomear a coluna "format" para "formato" e a variável "Movie" para "Filme"
banco1 <- banco1 %>%
  rename(formato = format)
 
# ALTERAR MOVIE PARA FILME 

ggplot(banco1) +
  aes(x = decada, y = freq, group = formato, colour = formato) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Década", y = "Frequência") +
  theme_estat() + scale_x_discrete(limits = unique(banco1$decada)) +
  scale_y_continuous(limits = c(0, 200))
  ggsave(filename = file.path(caminho_junior, "analise-1-trivariado.pdf"), width = 158, height = 93, units = "mm")


####################################################################################################
  
#ANALISE 2 - Variação da nota IMDB por temporada dos episódios:
  
banco2 <- dados 

banco2 <- subset(banco2, format == "Serie")  

banco2<- subset(banco2, season != "Special")

# Calculando a variância de 'imdb' agrupada por 'season'
variancia_por_season <- tapply(banco2$imdb, banco2$season, var)

# Visualizando os resultados
print(variancia_por_season)


# Calculando a média de 'imdb' agrupada por 'season'
media_por_season <- aggregate(imdb ~ season, data = banco2, FUN = mean)

# Visualizando os resultados
print(media_por_season)

# Calculando o desvio padrão das médias por temporada
desvio_padrao_por_season <- tapply(banco2$imdb, banco2$season, sd)


# Criando um novo data frame com os dados de variância e média por temporada
mini_dataframe <- data.frame(Season = names(variancia_por_season),
                             Variance = variancia_por_season,
                             Mean = media_por_season$imdb,
                             DesvioPadrao = desvio_padrao_por_season)

# Visualizando o novo data frame
print(mini_dataframe)

#Calculando a variância das médias

variancia_medias_por_season <- var(media_por_season$imdb)

print(variancia_medias_por_season)

#Calculando dados gerais

media_todas_temporadas <- mean(banco2$imdb)
variancia_geral <- var(banco2$imdb)
desvio_padrao_geral <- sd(dados$imdb)

#Agrupapando
dados_gerais <- data.frame(Media = media_todas_temporadas, Variance = variancia_geral, Desvio_Padrao = desvio_padrao_geral)


#Gráfico

ggplot(mini_dataframe) +
  aes(x=Season, y=Mean, group=1) +
  geom_line(size=1,colour="#A11D21") + geom_point(colour="#A11D21",
                                                  size=2) +
  labs(x="Temporada", y="Média IMDB") +
  theme_estat()
  ggsave(filename = file.path(caminho_junior, "analise-2-uni.pdf"), width = 158, height = 93, units = "mm")

  library(ggplot2)
  
  # Supondo que você já tenha um dataframe chamado banco2 com as colunas season e imdb
  
  banco2$season <- ifelse(banco2$season == 1, "Temporada 1",
                          ifelse(banco2$season == 2, "Temporada 2",
                                 ifelse(banco2$season == 3, "Temporada 3",
                                        ifelse(banco2$season == 4, "Temporada 4", "Outra Temporada"))))
  
  # Criar os intervalos imdb
  intervalos <- cut(banco2$imdb, breaks = c(0, 3, 5, 7, 9, 10), labels = c("0-2", "3-4", "5-6", "7-8", "9-10"))
  
  # Criar um novo dataframe com as colunas season e intervalo
  df_banco2 <- data.frame(intervalo = intervalos, season = banco2$season)
  
  # Exibir o dataframe resultante
  print(df_banco2)
  
  
  
  # Crie o gráfico utilizando ggplot
  ggplot(df_banco2, aes(x = intervalo)) +
    geom_bar(colour = "white", fill = "#A11D21",) +
    facet_grid(. ~ season) +
    labs(x = "Intervalo de IMDb", y = "Frequência") +
    theme_estat(
      strip.text = element_text(size=12),
      strip.background = element_rect(colour="black", fill="white")
    )
  
  # Salve o gráfico em um arquivo PDF
  ggsave(filename = file.path(caminho_junior, "analise-2-bivariado-facetgrid.pdf"), width = 200, height = 93, units = "mm")
  