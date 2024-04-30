library(readr)
dados <- read_csv("banco/banco_final.csv")

# 1) Número de lançamentos a cada década por formato de lançamento;

### separando os lançamentos em décadas

install.packages("lubridate")
install.packages("dplyr")


library(lubridate)
library(dplyr)
library(tidyr)
library(stringr)


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


#grafico

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
ggplot(banco1) +
  aes(
    x = decada, y = freq,
    fill = format, label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding =
                                        0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = 0.35, hjust = -0.1,
    size = 3
  ) +
  labs(x = "Década", y = "Frequência") +
  theme_estat() + coord_flip() + scale_x_discrete(limits = unique(banco1$decada)) +
  scale_y_continuous(limits = c(0, 200)) 
#ggsave("colunas-bi-freq.pdf", width = 158, height = 93, units = "mm")
ggsave(filename = file.path(caminho_junior, "analise-1-colunas-bi-freq.pdf"), width = 158, height = 93, units = "mm")


#novo gráfico ADAPTAR PEDIR LUCAS

ggplot(mpg) +
  aes(x = cty, y = hwy) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Consumo em Cidade (milhas/galão)",
    y = "Consumo em Rodovias (milhas/galão)"
  ) +
  theme_estat()
ggsave("disp_uni.pdf", width = 158, height = 93, units = "mm")
