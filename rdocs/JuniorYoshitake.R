library(readr)
dados <- read_csv("banco/banco_final.csv")

# 1) Número de lançamentos a cada década por formato de lançamento;

### separando os lançamentos em décadas

install.packages("lubridate")
install.packages("dplyr")


library(lubridate)
library(dplyr)

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

