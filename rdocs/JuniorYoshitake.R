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


  banco2$season <- ifelse(banco2$season == 1, "Temporada 1",
                          ifelse(banco2$season == 2, "Temporada 2",
                                 ifelse(banco2$season == 3, "Temporada 3",
                                        ifelse(banco2$season == 4, "Temporada 4", "Outra Temporada"))))
  

  
  # Primeiro, converta a variável 'season' para fator e ordene as temporadas
  banco2$season <- factor(banco2$season, levels = sort(unique(banco2$season)))
  
  # Criar o boxplot com ggplot2
  ggplot(banco2) +
    aes(x = season, y = imdb) +
    geom_boxplot(fill = "#A11D21", width = 0.5) +
    stat_summary(
      fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
    ) +
    labs(x = "Temporada", y = "Notas IMDB") +
    theme_estat()  # Supondo que 'theme_estat' seja um tema personalizado, você pode substituí-lo por 'theme_minimal' ou seu tema preferido
  
  ggsave(filename = file.path(caminho_junior, "analise-2-boxplot.pdf"), width = 185, height = 93, units = "mm")
  
  
  
  ####################################################################################################
  
  #ANALISE 3 - Top 3 terrenos mais frequentes pela ativação da armadilha:
  
  dados3 <- read_csv("banco/banco_final.csv")
  
  dados3resumo <- data.frame(dados3$setting_terrain, dados3$trap_work_first)
  
  # Exibir o dataframe resultante
  print(dados3resumo)

  
  
  # Obter os valores únicos na coluna "setting_terrain"
  valores_unicos <- unique(dados3$setting_terrain)
  
  # Exibir os valores únicos
  print(valores_unicos)
  
  

  
  # Criar um fator com valores numéricos específicos para cada categoria
  dados3resumo$setting_terrain_numeric <- factor(dados3$setting_terrain, levels = c("Urban", "Coast", "Island", "Cave", "Desert", "Forest", "Swamp", "Ocean", "Rural", "Snow", "Jungle", "Mountain", "Moon", "Space", "Air"), labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15))
  
  # Converter o fator em valores numéricos
  dados3resumo$setting_terrain_numeric <- as.numeric(dados3$setting_terrain_numeric)
  
  # Exibir o resultado
  print(dados3resumo)
  
  # Tirando NAS
  resumo3 <- na.omit(dados3resumo)


  resumo3 <- resumo3 %>%
    rename(terreno = dados3.setting_terrain,
           trap = dados3.trap_work_first,
           numero = setting_terrain_numeric)

  # Calculamos a tabela de frequência dos números
  frequencia_numeros <- table(resumo3$numero)
  
  # Ordenamos a tabela de frequência em ordem decrescente e selecionamos os três números mais frequentes
  numeros_mais_frequentes <- names(head(sort(frequencia_numeros, decreasing = TRUE), 3))
  
  # Filtramos o dataframe original para incluir apenas os três números mais frequentes
  resumo3_filtrado <- resumo3[resumo3$numero %in% numeros_mais_frequentes, ]
  
  # Exibindo o dataframe filtrado
  print(resumo3_filtrado) 

  library(dplyr)
  
  # Suponha que você tenha um dataframe chamado "resumo3_filtrado" com a coluna "numero"
  
  resumo3_filtrado <- resumo3_filtrado %>%
    mutate(numero = case_when(
      numero == 1 ~ "Urban",
      numero == 6 ~ "Rural",
      numero == 9 ~ "Forest",
      TRUE ~ as.character(numero)  # Para manter os valores que não são 1, 6 ou 9
    ))
  
  
  #grafico
  
  library(ggplot2)
  library(dplyr)
  library(forcats) # para usar fct_reorder
  
  trans_drv <- resumo3_filtrado %>%
    group_by(numero, trap) %>%
    summarise(freq = n()) %>%
    mutate(
      freq_relativa = round(freq / sum(freq) * 100, 1)
    )
  
  porcentagens <- str_c(trans_drv$freq_relativa, "%") %>% str_replace("
\\.", ",")
  legendas <- str_squish(str_c(trans_drv$freq, " (", porcentagens, ")")
  )
  
  
  ggplot(trans_drv) +
    aes(
      x = fct_reorder(numero, freq, .desc = TRUE), 
      y = freq,
      fill = trap, 
      label = legendas
    ) +
    geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
    geom_text( 
      position = position_dodge(width = 0.9),
      vjust = -0.5, hjust = 0.5,
      size = 2.5
  
    ) +
    labs(x = "Terreno", y = "Frequência", fill = "Armadilha") +
    theme_estat()
  ggsave(filename = file.path(caminho_junior, "analise-3-colunas-bi-freq.pdf"), width = 158, height = 93, units = "mm")

  # separando para tabela
  
  library(dplyr)
  
  # Suponha que você tenha um dataframe chamado "dados" com a coluna "set_terrain"
  
  library(dplyr)
  
  # Suponha que você tenha um dataframe chamado "dados" com as colunas "setting_terrain" e "trap_work_first"
  
  # Calcular a contagem de ocorrências de cada terreno
  contagem_terrenos <- count(dados3, setting_terrain)
  
  # Selecionar os cinco terrenos mais frequentes
  cinco_terrenos_mais_frequentes <- contagem_terrenos %>%
    top_n(5, wt = n) %>%
    pull(setting_terrain)
  
  # Filtrar o dataframe original para incluir apenas os cinco terrenos mais frequentes
  mini3 <- dados3 %>%
    filter(setting_terrain %in% cinco_terrenos_mais_frequentes) %>%
    select(setting_terrain, trap_work_first)
  
  mini3 <- na.omit(mini3)
  
  # Exibir o mini dataframe
  print(mini3)
  
  library(dplyr)
  
  # Calcula a tabela de frequência para cada variável de setting_terrain e trap_work_first
  mini3freq <- mini3 %>%
    group_by(setting_terrain, trap_work_first) %>%
    summarise(frequencia = n()) %>%
    arrange(setting_terrain, trap_work_first)
  
  # Exibe a tabela de frequência
  print(mini3freq)
  
  
  # Remover linhas com NA na coluna "setting_terrain"
  dados3_sem_na <- na.omit(dados3$setting_terrain)
  
  # Calcular a frequência de cada variável após remover os NA
  frequencia_setting_terrain <- table(dados3_sem_na)
  
  # Ordenar os valores de frequência do mais frequente para o menos frequente
  frequencia_setting_terrain_ordenada <- sort(frequencia_setting_terrain, decreasing = TRUE)
  
  # Exibir a frequência de cada variável ordenada
  print(frequencia_setting_terrain_ordenada)
  
  
  #tabela nova
  
  dados3_clean <- dados3[!is.na(dados3$setting_terrain), ]
  
  cont2 <- count(dados3_clean, setting_terrain)

  # Instale o pacote dplyr se ainda não estiver instalado
  # install.packages("dplyr")
  
  # Carregar o pacote dplyr
  library(dplyr)
  
  # Data frame com valores NA em trap_work_first
  dados_na <- dados3 %>% filter(is.na(trap_work_first))
  
  # Data frame com valores não NA em trap_work_first
  dados_not_na <- dados3 %>% filter(!is.na(trap_work_first))

  
  # Instalar o pacote dplyr se ainda não estiver instalado
  # install.packages("dplyr")
  
  # Carregar o pacote dplyr
  library(dplyr)
  
  dados_summary <- dados3 %>%
    group_by(setting_terrain) %>%
    summarise(
      na_count = sum(is.na(trap_work_first)),
      not_na_count = sum(!is.na(trap_work_first))
    ) %>%
    mutate(total_count = na_count + not_na_count)
  
  # Verificar o resultado
  print(dados_summary)
  
  
# analise 4
  
 banco4 <- read_csv("banco/banco_final.csv")
 
 resumo4 <- data.frame(IMDB = banco4$imdb, engajamento = banco4$engagement)
 
 correlacao <- cor(resumo4$IMDB, resumo4$engajamento,method = "pearson",  use = "complete.obs")
 
 modelo <- lm(engagemento ~ IMDB, data = resumo4)
 
 mediaimdb <- mean(resumo4$IMDB)
 
 mediaeng <- mean(resumo4$engajamento)


 
 #grafico 4
 
 ggplot(resumo4) +
   aes(x = IMDB, y = engajamento) +
   geom_point(colour = "#A11D21", size = 3, alpha = 0.5) +
   geom_smooth(method = "lm", color = "#003366", se = FALSE) +
   labs(
     x = "Nota IMDB",
     y = "Engajamento"
   ) + scale_x_continuous(limits = c(2, NA)) +
   theme_estat()
 ggsave(filename = file.path(caminho_junior, "analise-4-disp-bi-freq.pdf"), width = 158, height = 93, units = "mm")
 
 
 #QUADRO 
 
 print_quadro_resumo <- function(data, var_name, title="Medidas resumo da(o) [nome da variável]", label="quad:quadro_resumo1")
 {
   var_name <- substitute(var_name)
   data <- data %>%
     summarize(`Média` = round(mean(!!sym(var_name)),2),
               `Desvio Padrão` = round(sd(!!sym(var_name)),2),
               `Variância` = round(var(!!sym(var_name)),2),
               `Mínimo` = round(min(!!sym(var_name)),2),
               `1º Quartil` = round(quantile(!!sym(var_name), probs = .25),2),
               `Mediana` = round(quantile(!!sym(var_name), probs = .5),2),
               `3º Quartil` = round(quantile(!!sym(var_name), probs = .75),2),
               `Máximo` = round(max(!!sym(var_name)),2)) %>%
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
   latex <- str_c(latex, "| l |\n", sep=" ")
   for (i in seq(2, col_count))
   {
     numCount <- data[i, -c(1)] %>%
       as.numeric() %>%
       {floor(log10(.)) + 1} %>%
       max()
     latex <- str_c(latex, "\t\t\tS[table-format = ", numCount ,".2]\n", sep="")
   }
   
   
   latex <- str_c(latex, "\t\t\t|}\n\t\\toprule\n\t\t", sep="")
   if (col_count > 2)
   {
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
     latex <- str_c(latex, "\\textbf{Estatística} & \\textbf{Valor} \\\\\n", sep="")  
   }
   
   latex <- str_c(latex, "\t\t\\midrule\n", sep="")
   
   if (col_count > 2)
     starting_number <- 2
   else
     starting_number <- 1
   
   for (i in seq(starting_number, row_count))
   {
     latex <- str_c(latex, "\t\t", str_flatten(t(data[i,]), collapse = " & "), " \\\\\n")
   }
   latex <- str_c(latex, "\t\\bottomrule
\t\\end{tabular}
\t\\label{", label, "}
\t\\end{adjustbox}
\\end{quadro}", sep="")
   
   writeLines(latex)
 }
 
 #
 
 resumo4 %>%
   print_quadro_resumo(var_name = IMDB)
 resumo4 %>%
   print_quadro_resumo(var_name = engajamento)
 
 variancia <- var(resumo4$engajamento)
 