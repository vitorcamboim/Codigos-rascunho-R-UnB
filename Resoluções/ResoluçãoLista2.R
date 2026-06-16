# Questão 1

uber <- read.csv2("C:/Users/v4746/OneDrive/Desktop/UnB Matérias/Comp. em Estatística 2 R/Dados/uber.csv")

library(lubridate)

uber <- uber %>%
  mutate(data_hora = ymd_hms(paste(Ano, Mes, Dia, Horario))) %>%
  mutate(dia_semana = wday(data_hora, label = T))

uber %>%
  count(dia_semana)

uber %>%
  group_by(dia_semana) %>%
  summarise(preco_medio = mean(Preço, na.rm = T), temp_media = mean(Temperatura, na.rm = T))

# Questão 2

library(ggplot2)

uber %>%
  mutate(hora_dia = hour(data_hora)) %>%
  count(hora_dia) %>%
  ggplot(aes(reorder(hora_dia, n), n)) +
  geom_col()

names(uber)

uber <- uber %>%
  mutate(hora = hour(data_hora),
         periodo = case_when(
           hora >= 0 & hora < 6 ~ "madrugada",
           hora >= 6 & hora < 12 ~ "manha",
           hora >= 12 & hora < 18 ~ "tarde",
           hora >= 18 & hora <= 23 ~ "noite"
         ))

uber %>%
  count(periodo) %>%
  ggplot(aes(reorder(periodo, n), n)) +
  geom_col()

uber %>%
  group_by(periodo) %>%
  summarise(distancia_media = mean(Distância), mean(Preço, na.rm = T))
