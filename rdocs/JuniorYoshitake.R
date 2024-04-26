library(readr)
dados <- read_csv("banco/banco_final.csv")

# 1) Número de lançamentos a cada década por formato de lançamento;

### separando os lançamentos em décadas

install.packages("lubridate")
install.packages("dplyr")


library(lubridate)
library(dplyr)
library(tidyr)

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
resultado <- dados %>%
  group_by(decada, format) %>%
  summarise(num_lancamentos = n())

# Exibir o resultado
print(resultado)

banco1 <- resultado %>%
  pivot_wider(names_from = format, values_from = num_lancamentos, values_fill = 0)

names(banco1) <- sub("X", "", names(banco1))

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

#separando as decadas

#1960

banco_1960 <- banco1 %>%
  filter(decada == "1960")

#grafico 1960 ## ajustar variaveis##

classes <- mpg %>%
  filter(!is.na(class)) %>%
  count(class) %>%
  mutate(
    freq = n,
    relative_freq = round((freq / sum(freq)) * 100, 1),
    freq = gsub("\\.", ",", relative_freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )
ggplot(classes) +
  14
aes(x = fct_reorder(class, n, .desc=T), y = n, label = label) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) +
  labs(x = "manufacturer", y = "Frequência") +
  theme_estat()
ggsave("colunas-uni-freq.pdf", width = 158, height = 93, units = "mm"
)

#1970

banco_1970 <- banco1 %>%
  filter(decada == "1970")

#1980

banco_1980 <- banco1 %>%
  filter(decada == "1980")

#1990

banco_1990 <- banco1 %>%
  filter(decada == "1990")

#2000

banco_2000 <- banco1 %>%
  filter(decada == "2000")

#2010

banco_2010 <- banco1 %>%
  filter(decada == "2010")

#2020

banco_2020 <- banco1 %>%
  filter(decada == "2020")


