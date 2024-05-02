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

  #quadro resumo
  
  print_quadro_resumo <- function(data, title="Medidas resumo da(o) [
nome da variável]", label="quad:quadro_resumo1")
  {
    data <- data %>%
      summarize(`Média` = round(mean(displ),2),
                `Desvio Padrão` = round(sd(displ),2),
                `Variância` = round(var(displ),2),
                `Mínimo` = round(min(displ),2),
                `1º Quartil` = round(quantile(displ, probs = .25),2),
                `Mediana` = round(quantile(displ, probs = .5),2),
                `3º Quartil` = round(quantile(displ, probs = .75),2),
                `Máximo` = round(max(displ),2)) %>%
      t() %>%
      as.data.frame() %>%
      rownames_to_column()
    latex <- str_c("\\begin{quadro}[H]
\t\\caption{", title, "}
\t\\centering
\t\\begin{adjustbox}{max width=\\textwidth}
\t\\begin{tabular}{", sep="")
    col_count <- ncol(data)
    row_count <- nrow(data)
    latex <- str_c(latex, "| l", "|", sep=" ")
    for (i in seq(2, col_count))
    {
      latex <- str_c(latex, "S", sep=" ")
    }
    latex <- str_c(latex, "|}\n\t\\toprule\n\t\t", sep="")
    if (col_count > 2)
    {
      32
      for (i in seq(1,col_count))
      {
        if (i == 1)
          latex <- str_c(latex, "\\textbf{Estatística}", sep="")
        else
          latex <- str_c(latex, " \\textbf{", data[1, i], "}", sep="")
        if (i < col_count)
          latex <- str_c(latex, "&", sep=" ")
        else
          latex <- str_c(latex, "\\\\\n", sep=" ")
      }
    }
    else
    {
      latex <- str_c(latex, "\\textbf{Estatística} & \\textbf{Valor}
\\\\\n", sep="")
    }
    latex <- str_c(latex, "\t\t\\midrule\n", sep="")
    if (col_count > 2)
      starting_number <- 2
    else
      starting_number <- 1
    for (i in seq(starting_number, row_count))
    {
      latex <- str_c(latex, "\t\t", str_flatten(t(data[i,]), collapse =
                                                  " & "), " \\\\\n")
    }
    latex <- str_c(latex, "\t\\bottomrule
\t\\end{tabular}
\t\\label{", label, "}
\t\\end{adjustbox}
\\end{quadro}", sep="")
    writeLines(latex)
  }
  
  
  
banco2 %>%
  group_by(imdb)
  print_quadro_resumo()
  