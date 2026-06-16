# 1 a 3

simulacoes <- function(pessoas, n_sim = 10000, semente = NULL) {
  if (missing(pessoas)) {
    stop("É obrigatório informar o número de pessoas.")
  }
  
  count = 0
  set.seed(semente)
  for (i in 1:n_sim) {
    aniversario = sample(1:365, pessoas, replace = TRUE)
    
    if (any(duplicated(aniversario)) == TRUE) {
      count = count + 1
    }
  }
  probabilidade = count / n_sim
  probabilidade
}

# 4

library(dplyr)

banco <- airquality %>%
  split(.$Month)

radiacao_mes6 <- banco$`6`$Solar.R

library(tidyverse)

radiacao <- map(banco, "Solar.R")

media_radiacao <- map(radiacao, mean, na.rm = TRUE)

temperaturas <- map(banco, ~ max(.x$Temp, na.rm = TRUE) > 95)

quartis <- map(banco, ~ quantile(.x$Temp,
                      probs = c(0.25, 0.50, 0.75),
                      na.rm = TRUE))

quartis <- as.data.frame(quartis)



