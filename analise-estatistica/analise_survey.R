# Importando as bibliotecas
if (!require('tidyverse')) install.packages('tidyverse')
library('tidyverse')

# Importando o dataset
respostas <- read.csv('respostas_formulario.csv')
head(respostas)
summary(respostas)

# Retirando respostas inválidas: marcou que utiliza as ferramentas de código (pergunta 6 = 'sim'),
# mas não selecionou ou escreveu nenhuma ferramenta (perguntas 7-11)

# Criando 5 dataframes diferentes: 1 com as respostas da caracterização da amostra e 1 para as
# respostas das 4 hipóteses

caracterizacao_amostra <- select(respostas, Quais.são.as.suas.áreas.de.atuação.,
                                 Quanto.tempo.de.experiência.profissional.você.possui.na.área.de.tecnologia.,
                                 Você.utiliza.ferramentas.de.IAs.conversacionais.e.de.geração.de.código.profissionalmente.,
                                 Para.quais.propósitos.você.utiliza.IAs.conversacionais.e.de.geração.de.código.profissionalmente.,
                                 Há.quanto.tempo.você.começou.a.utilizar.as.seguintes.ferramentas.de.IA...ChatGPT.,
                                 Há.quanto.tempo.você.começou.a.utilizar.as.seguintes.ferramentas.de.IA...Bing.Chat.,
                                 Há.quanto.tempo.você.começou.a.utilizar.as.seguintes.ferramentas.de.IA...GitHub.Copilot.,
                                 Há.quanto.tempo.você.começou.a.utilizar.as.seguintes.ferramentas.de.IA...Tabnine.,
                                 Caso.tenha.utilizado.IAs.que.não.foram.citadas.acima..informe.as.abaixo.e.indique.há.quando.tempo.você.começou.a.utilizá.las.,
                                 Qual.a.sua.frequência.média.de.uso.das.seguintes.ferramentas.de.IA.profissionalmente...ChatGPT.,
                                 Qual.a.sua.frequência.média.de.uso.das.seguintes.ferramentas.de.IA.profissionalmente...Bing.Chat.,
                                 Qual.a.sua.frequência.média.de.uso.das.seguintes.ferramentas.de.IA.profissionalmente...GitHub.Copilot.,
                                 Qual.a.sua.frequência.média.de.uso.das.seguintes.ferramentas.de.IA.profissionalmente...Tabnine.,
                                 Caso.tenha.utilizado.IAs.que.não.foram.citadas.acima..informe.as.abaixo.e.indique.a.frequência.de.uso.)
hipotese1_produtividade <- select(respostas, Complete.a.seguinte.frase.de.acordo.com.a.sua.percepção.e.experiência....O.uso.de.ferramentas.de.IA.afetou.a.minha.produtividade.profissional.........Resposta.)
hipotese2_satisfacao <- select(respostas, Complete.a.seguinte.frase.de.acordo.com.a.sua.percepção.e.experiência....O.uso.de.ferramentas.de.IA.afetou.a.minha.satisfação.profissional.........Resposta.)
hipotese3_cargos <- select(respostas, Qual.o.seu.sentimento.com.relação.a.modificações..em.razão.de.IAs..nos.cargos.da.área.da.tecnologia.)
hipotese4_futuro <- select(respostas, Qual.o.seu.sentimento.sobre.o.futuro.das.ferramentas.de.IA.conversacionais.e.de.geração.de.código.)

# Renomeando as colunas

colnames(hipotese1_produtividade)[1] = "Resposta"
colnames(hipotese2_satisfacao)[1] = "Resposta"
colnames(hipotese3_cargos)[1] = "Resposta"
colnames(hipotese4_futuro)[1] = "Resposta"


# Retirando respostas vazias

hipotese1_produtividade <- subset(hipotese1_produtividade,  Resposta != "")
hipotese2_satisfacao <- subset(hipotese2_satisfacao, Resposta != "")
hipotese3_cargos <- subset(hipotese3_cargos, Resposta != "")
hipotese4_futuro <- subset(hipotese4_futuro, Resposta != "")

# Transformando as respostas da escala Linkert para uma escala numérica de 1 a 5, de modo
# a facilitar a análise dos dados

hipotese1_produtividade["Resposta"][hipotese1_produtividade["Resposta"] == "Muito positivamente"] <- 5
hipotese1_produtividade["Resposta"][hipotese1_produtividade["Resposta"] == "Positivamente"] <- 4
hipotese1_produtividade["Resposta"][hipotese1_produtividade["Resposta"] == "Não afetou"] <- 3
hipotese1_produtividade["Resposta"][hipotese1_produtividade["Resposta"] == "Negativamente"] <- 2
hipotese1_produtividade["Resposta"][hipotese1_produtividade["Resposta"] == "Muito negativamente"] <- 1

hipotese1_produtividade <- transform(hipotese1_produtividade, Resposta = as.numeric(Resposta))

hipotese2_satisfacao["Resposta"][hipotese2_satisfacao["Resposta"] == "Muito positivamene"] <- 5
hipotese2_satisfacao["Resposta"][hipotese2_satisfacao["Resposta"] == "Positivamente"] <- 4
hipotese2_satisfacao["Resposta"][hipotese2_satisfacao["Resposta"] == "Não afetou"] <- 3
hipotese2_satisfacao["Resposta"][hipotese2_satisfacao["Resposta"] == "Negativamente"] <- 2
hipotese2_satisfacao["Resposta"][hipotese2_satisfacao["Resposta"] == "Muito negativamente"] <- 1

hipotese2_satisfacao <- transform(hipotese2_satisfacao, Resposta = as.numeric(Resposta))

hipotese3_cargos["Resposta"][hipotese3_cargos["Resposta"] == "Totalmente positivo - A utilização de IAs no mercado de trabalho será feita apenas de forma ferramental, haverá perda de emprego mínima e muitos novos cargos irão surgir no lugar."] <- 5
hipotese3_cargos["Resposta"][hipotese3_cargos["Resposta"] == "Parcialmente positivo - Alguns empregos podem deixar de existir, mas o mercado em geral se beneficiará com os novos cargos impulsionados por essas tecnologias."] <- 4
hipotese3_cargos["Resposta"][hipotese3_cargos["Resposta"] == "Neutro - Não tenho opinião a respeito."] <- 3
hipotese3_cargos["Resposta"][hipotese3_cargos["Resposta"] == "Parcialmente negativo - Apesar do uso de ferramentas de IA terem seus pontos positivos no trabalho, a perda de empregos consequente do uso dessas tecnologias torna a prática indesejável."] <- 2
hipotese3_cargos["Resposta"][hipotese3_cargos["Resposta"] == "Totalmente negativo - A substituição de cargos por IAs resultará em grande desemprego e será extremamente negativa para o ambiente profissional."] <- 1

hipotese3_cargos <- transform(hipotese3_cargos, Resposta = as.numeric(Resposta))

hipotese4_futuro["Resposta"][hipotese4_futuro["Resposta"] == "Totalmente otimista - Ferramentas de IAs são bastante promissoras e auxiliarão cada vez mais as pessoas em mais atividades pessoais e profissionais."] <- 5
hipotese4_futuro["Resposta"][hipotese4_futuro["Resposta"] == "Relativamente otimista - Ferramentas de IAs são promissoras, mas o nível de atenção e crédito que se dá a essas tecnologias atualmente não está alinhado com a realidade."] <- 4
hipotese4_futuro["Resposta"][hipotese4_futuro["Resposta"] == "Neutro - Não tenho opinião a respeito."] <- 3
hipotese4_futuro["Resposta"][hipotese4_futuro["Resposta"] == "Relativamente pessimista - Ferramentas de IAs têm certos benefícios, mas trazem impactos negativos para a sociedade que desencorajam o seu uso e desenvolvimento."] <- 2
hipotese4_futuro["Resposta"][hipotese4_futuro["Resposta"] == "Totalmente pessimista - Ferramentas de IAs são perigosas para a sociedade e devem ser regulamentadas para manterem-se limitadas a escopos mais restritos."] <- 1

hipotese4_futuro <- transform(hipotese4_futuro, Resposta = as.numeric(Resposta))

# Análise exploratória

# Comparando média, mediana e desvio padrao

tabela <- matrix(c(mean(hipotese1_produtividade$Resposta), median(hipotese1_produtividade$Resposta), sd(hipotese1_produtividade$Resposta),
                   mean(hipotese2_satisfacao$Resposta), median(hipotese2_satisfacao$Resposta), sd(hipotese2_satisfacao$Resposta),
                   mean(hipotese3_cargos$Resposta), median(hipotese3_cargos$Resposta), sd(hipotese3_cargos$Resposta),
                   mean(hipotese4_futuro$Resposta), median(hipotese4_futuro$Resposta), sd(hipotese4_futuro$Resposta)), ncol=3, byrow=TRUE)
colnames(tabela) <- c('Média','Mediana','Desvio padrão')
rownames(tabela) <- c('Hipótese 1 - produtividade','Hipótese 2 - satisfação','Hipótese 3 - cargos','Hipótese 4 - futuro')
tabela <- as.table(tabela)
tabela

# Gerando gráficos de barras
# ...

# Parâmetros gerais
confianca <- 0.85 # Confiança do teste
alpha <- 0.15 # Probabilidade do Erro Tipo 1
erro <- 0.15 # Erro máximo tolerável

z <- qnorm(confianca + (1 - confianca) / 2)

# Cálculo do tamanho mínimo da amostra - Hipótese 1 (produtividade)
hipotese1_variancia <- sd(hipotese1_produtividade$Resposta)
hipotese1_tamanho_minimo_amostra <- (z * hipotese1_variancia / erro) ** 2
hipotese1_tamanho_minimo_amostra

# Intervalo de confiança - Hipótese 1 (produtividade)
hipotese1_erro <- z * hipotese1_variancia / (length(hipotese1_produtividade) ** 0.5)
hipotese1_erro

hipotese1_media <- mean(hipotese1_produtividade$Resposta)

hipotese1_intervalo_confianca <- c(
  max(hipotese1_media - hipotese1_erro, 1),
  min(hipotese1_media + hipotese1_erro, 5)
)
hipotese1_intervalo_confianca

# Teste - Hipótese 1 (produtividade)
hipotese1_teste <- t.test(
  hipotese1_produtividade$Resposta,
  mu=3,
  conf.level=confianca,
  alternative="greater"
)
hipotese1_teste

if (hipotese1_teste$p.value >= alpha) {
  print("Hipótese 1: H0 não rejeitada")
} else {
  print("Hipótese 1: H0 rejeitada")
}

# Cálculo do tamanho mínimo da amostra - Hipótese 2 (satisfação)
hipotese2_variancia <- sd(hipotese2_satisfacao$Resposta)
hipotese2_tamanho_minimo_amostra <- (z * hipotese2_variancia / erro) ** 2
hipotese2_tamanho_minimo_amostra

# Intervalo de confiança - Hipótese 2 (satisfação)
hipotese2_erro <- z * hipotese2_variancia / (length(hipotese2_satisfacao) ** 0.5)
hipotese2_erro

hipotese2_media <- mean(hipotese2_satisfacao$Resposta)

hipotese2_intervalo_confianca <- c(
  max(hipotese2_media - hipotese2_erro, 1),
  min(hipotese2_media + hipotese2_erro, 5)
)
hipotese2_intervalo_confianca

# Teste - Hipótese 2 (satisfação)
hipotese2_teste <- t.test(
  hipotese2_satisfacao$Resposta,
  mu=3,
  conf.level=confianca,
  alternative="greater"
)
hipotese2_teste

if (hipotese2_teste$p.value >= alpha) {
  print("Hipótese 2: H0 não rejeitada")
} else {
  print("Hipótese 2: H0 rejeitada")
}

# Cálculo do tamanho mínimo da amostra - Hipótese 3 (cargos)
hipotese3_variancia <- sd(hipotese3_cargos$Resposta)
hipotese3_tamanho_minimo_amostra <- (z * hipotese3_variancia / erro) ** 2
hipotese3_tamanho_minimo_amostra

# Intervalo de confiança - Hipótese 3 (cargos)
hipotese3_variancia <- sd(hipotese3_cargos$Resposta)
hipotese3_tamanho_minimo_amostra <- (z * hipotese3_variancia / erro) ** 2
hipotese3_tamanho_minimo_amostra

# Intervalo de confiança - Hipótese 2 (satisfação)
hipotese3_erro <- z * hipotese3_variancia / (length(hipotese3_cargos) ** 0.5)
hipotese3_erro

hipotese3_media <- mean(hipotese3_cargos$Resposta)

hipotese3_intervalo_confianca <- c(
  max(hipotese3_media - hipotese3_erro, 1),
  min(hipotese3_media + hipotese3_erro, 5)
)
hipotese3_intervalo_confianca

# Teste - Hipótese 3 (cargos)
hipotese3_teste <- t.test(
  hipotese3_cargos$Resposta,
  mu=3,
  conf.level=confianca,
  alternative="greater"
)
hipotese3_teste

if (hipotese3_teste$p.value >= alpha) {
  print("Hipótese 3: H0 não rejeitada")
} else {
  print("Hipótese 3: H0 rejeitada")
}

# Cálculo do tamanho mínimo da amostra - Hipótese 4 (futuro)
hipotese4_variancia <- sd(hipotese4_futuro$Resposta)
hipotese4_tamanho_minimo_amostra <- (z * hipotese4_variancia / erro) ** 2
hipotese4_tamanho_minimo_amostra

# Intervalo de confiança - Hipótese 4 (futuro)
hipotese4_erro <- z * hipotese4_variancia / (length(hipotese4_futuro) ** 0.5)
hipotese4_erro

hipotese4_media <- mean(hipotese4_futuro$Resposta)

hipotese4_intervalo_confianca <- c(
  max(hipotese4_media - hipotese4_erro, 1),
  min(hipotese4_media + hipotese4_erro, 5)
)
hipotese4_intervalo_confianca

# Teste - Hipótese 4 (futuro)
hipotese4_teste <- t.test(
  hipotese4_futuro$Resposta,
  mu=3,
  conf.level=confianca,
  alternative="greater"
)
hipotese4_teste

if (hipotese4_teste$p.value >= alpha) {
  print("Hipótese 4: H0 não rejeitada")
} else {
  print("Hipótese 4: H0 rejeitada")
}
