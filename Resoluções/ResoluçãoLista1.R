# Questão 1

texto <- " A tecnologia molda silenciosamente a forma como percebemos o mundo e nos relacionamos com ele. O que antes era limitado por barreiras físicas hoje se expande em redes digitais que conectam pessoas, ideias e culturas em tempo real. Essa transformação não é apenas técnica, mas também profundamente humana, pois redefine hábitos, expectativas e até mesmo a maneira como pensamos. Ao mesmo tempo em que aproxima, também exige um novo tipo de consciência crítica sobre o que consumimos e compartilhamos.\nNesse contexto, a informação se tornou um dos recursos mais valiosos da atualidade. A facilidade de acesso ao conhecimento trouxe oportunidades inéditas de aprendizado, permitindo que qualquer pessoa, com um dispositivo conectado, explore conteúdos antes restritos a poucos. No entanto, essa abundância também traz desafios, como a disseminação de informações falsas e a dificuldade de distinguir fontes confiáveis. Assim, mais importante do que acessar dados é desenvolver a capacidade de interpretá-los com responsabilidade.\nOutro aspecto relevante é o impacto da tecnologia nas relações sociais. As interações digitais, embora práticas, podem reduzir a profundidade dos vínculos quando substituem completamente o contato presencial. Emoções complexas muitas vezes se perdem em mensagens rápidas, e a busca por validação em redes sociais pode gerar ansiedade e comparação constante. Por isso, torna-se essencial equilibrar o uso dessas ferramentas com momentos de conexão real, preservando a autenticidade das relações humanas.\nPor fim, é fundamental compreender que a tecnologia não é boa nem ruim por si só; tudo depende de como é utilizada. Ela pode ser uma ferramenta poderosa para o progresso, a educação e a inclusão, mas também pode amplificar desigualdades e problemas sociais se usada de forma irresponsável. Cabe a cada indivíduo e à sociedade como um todo construir uma relação consciente com esses recursos, garantindo que seu impacto seja positivo e sustentável ao longo do tempo."

library(stringr)

str_count(texto, "\\.")

texto_limpo <- str_replace_all(texto, "[[:punct:]\n]", "")

paragrafos <- str_split(texto_limpo, "\n")[[1]]
paragrafos <- str_trim(texto_limpo)

maisdetres <- str_extract_all(texto, "\\b\\w{4,}\\b")

maisdetres <- table(maisdetres)
maisdetres <- sort(maisdetres, decreasing = T)
maisdetres <- head(maisdetres, 6)
maisdetres

# Questão 2

imdb <- read.csv("C:/Users/v4746/OneDrive/Desktop/UnB Matérias/Comp. em Estatística 2 R/Dados/imdb.csv")

prop <- mean(imdb$orcamento >= 10000000, na.rm = T)

maisdenove <- str_extract_all(imdb$id_filme, "//b//w{10,}//b")
maisdenove

library(dplyr)

imdb %>%
  filter(str_length(id_filme) > 9) %>%
  count()

imdb %>%
  mutate(classificacao_nota = str_sub(nota_imdb, 1, 1))

imdb %>%
  filter(str_detect(elenco, "William Hurt")) %>%
  select(titulo)


