# Questão 1

clientes <- read.csv2("C:/Users/v4746/OneDrive/Desktop/UnB Matérias/Comp. em Estatística 2 R/Dados/clientes.csv")

library(dplyr)
library(stringr)
library(forcats)
library(lubridate)
library(hms)

clientes %>%
  select(nome, segmento, valor, status)

clientes %>%
  filter(status == "ativo", valor > 300)

clientes %>%
  mutate(valor_desconto = valor * 0.9)

clientes %>%
  group_by(segmento) %>%
  summarise(valor_medio = mean(valor), nmr_clientes = n())

# Questão 2

clientes <- clientes %>%
  mutate(primeiro_nome = word(nome, 1))

clientes <- clientes %>%
  mutate(sobrenome = word(nome, -1))

clientes <- clientes %>%
  mutate(nome = str_to_upper(nome))

clientes %>%
  filter(str_detect(primeiro_nome, regex("a", ignore_case = TRUE)))

# Questão 3

clientes <- clientes %>%
  mutate(segmento = as_factor(segmento))

clientes %>%
  mutate(segmento = fct_reorder(segmento, valor, .fun = mean)) %>%
  group_by(segmento) %>%
  summarise(valor_medio = mean(valor, na.rm = TRUE))

library(ggplot2)

clientes %>%
  mutate(segmento = fct_infreq(segmento)) %>%
  ggplot(aes(segmento)) +
  geom_bar()

# Questão 4

clientes %>%
  mutate(data_cadastro = ymd(data_cadastro))

clientes <- clientes %>%
  mutate(ano = year(data_cadastro),
         mes = month(data_cadastro),
         dia = day(data_cadastro)
         )

clientes %>%
  mutate(dia_semana = wday(data_cadastro, label = TRUE))

clientes %>%
  mutate(mes = month(data_cadastro)) %>%
  count(mes)

# Questão 5

clientes <- clientes %>%
  mutate(horario_atendimento = as_hms(horario_atendimento))

clientes <- clientes %>%
  mutate(hora = hour(horario_atendimento),
         minuto = minute(horario_atendimento),
         segundo = second(horario_atendimento)
         )

clientes <- clientes %>%
  mutate(periodo = case_when(
    hora < 12 ~ "manha",
    hora >= 12 & hora < 18 ~ "tarde",
    hora >= 18 ~ "noite"
  ))

clientes %>%
  group_by(periodo) %>%
  summarise(mean(valor, na.rm = TRUE))

# Questão 6

clientes %>%
  mutate(data_hora = ymd_hms(paste(data_cadastro, horario_atendimento)))

clientes %>%
  group_by(segmento) %>%
  slice_max(valor, n = 1, with_ties = FALSE)

clientes %>%
  group_by(status, periodo) %>%
  summarise(quantidade_clientes = n(),
            valor_medio = mean(valor, na.rm = TRUE))
clientes %>%
  group_by(segmento) %>%
  summarise(valor_medio = mean(valor, na.rm = TRUE)) %>%
  ggplot(aes(reorder(segmento, valor_medio), valor_medio)) +
  geom_col()


         