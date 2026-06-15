exponencial <- function(dobras) {
  if (!is.numeric(dobras)) {
    stop("Digite um número de dobras ou altura válido!")
  }
  
  if (dobras < 0 || dobras %% 1 != 0) {
    stop("Digite um número inteiro positivo!")
  }
  
  altura_total <- 0.0001 * (2 ^ dobras)
  
  

  if (altura_total >= 384400000) {
    print("chegou na lua")
  } else if (altura_total > 40075000){
    print("maior que a circ da terra")
  } else if (altura_total > 203000) {
    print("maior que brasil pra goian")
  }
  
  
  list(
    k = dobras,
    altura_total = altura_total,
    mensagem = paste("Altura do papel após",
                      dobras,
                      "dobras foi de",
                      altura_total,
                      "metros")
  )
  
}



