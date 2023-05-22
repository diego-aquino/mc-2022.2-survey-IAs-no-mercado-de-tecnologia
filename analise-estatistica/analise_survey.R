# Importando as bibliotecas
if (!require('tidyverse'))
  install.packages('tidyverse')
library('tidyverse')
if (!require('gridExtra'))
  install.packages('gridExtra')
library(gridExtra)

library('ggplot2')

# Importando o dataset
respostas <- read.csv(paste(getwd(), 'questionario-final/respostas.csv', sep='/'))
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
rownames(tabela) <- c('Hipótese 1 - Produtividade','Hipótese 2 - Satisfação','Hipótese 3 - Cargos','Hipótese 4 - Futuro')
tabela <- as.table(tabela)
tabela

# Gerando gráficos de barras
# ...

#par(mfrow=c(2,2))

hipotese1_produtividade_freq <- as.data.frame(table(hipotese1_produtividade$Resposta))

hipotese1_produtividade_plot <- ggplot(hipotese1_produtividade_freq, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Respostas", y = "Quantidade", title = "Distribuição de respostas: hipótese 1")

hipotese2_satisfacao_freq <- as.data.frame(table(hipotese2_satisfacao$Resposta))

hipotese2_satisfacao_plot <- ggplot(hipotese2_satisfacao_freq, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Respostas", y = "Quantidade", title = "Distribuição de respostas: hipótese 2")

hipotese3_cargos_freq <- as.data.frame(table(hipotese3_cargos$Resposta))

hipotese3_cargos_plot <- ggplot(hipotese3_cargos_freq, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Respostas", y = "Quantidade", title = "Distribuição de respostas: hipótese 3")

hipotese4_futuro_freq <- as.data.frame(table(hipotese4_futuro$Resposta))

hipotese4_futuro_plot <- ggplot(hipotese4_futuro_freq, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Respostas", y = "Quantidade", title = "Distribuição de respostas: hipótese 4")

grid.arrange(hipotese1_produtividade_plot, hipotese2_satisfacao_plot, hipotese3_cargos_plot, hipotese4_futuro_plot, nrow = 2, ncol = 2)

# ______________________________________________________________________________

# Recursos gerais para os testes de hipótese
confianca <- 0.95 # Confiança do teste
alpha <- 0.05 # Probabilidade do Erro Tipo 1

z <- qnorm(confianca + (1 - confianca) / 2)

calcular_tamanho_minimo_amostra <- function(respostas, erro) {
  desvio_padrao <- sd(respostas)
  tamanho_minimo <- ceiling((z * desvio_padrao / erro) ** 2)
  tamanho_minimo
}

calcular_ic_likert <- function(respostas) {
  media <- mean(respostas)
  desvio_padrao <- sd(respostas)
  margem_erro <- z * desvio_padrao / (length(hipotese1_produtividade) ** 0.5)

  intervalo_confianca <- c(
    round(max(media - margem_erro, 1), digits=2),
    round(min(media + margem_erro, 5), digits=2)
  )
  intervalo_confianca
}

# ______________________________________________________________________________

# Hipótese 1: Impacto positivo na produtividade profissional

## Erro máximo tolerável para a variável "Impacto na produtividade profissional"
hipotese1_erro <- 0.25

## Tamanho mínimo da amostra
hipotese1_tamanho_minimo_amostra <- calcular_tamanho_minimo_amostra(
  hipotese1_produtividade$Resposta,
  hipotese1_erro
)

## Intervalo de confiança
hipotese1_media <- round(mean(hipotese1_produtividade$Resposta), digits=2)

hipotese1_ic <- calcular_ic_likert(
  hipotese1_produtividade$Resposta
)
hipotese1_margem_erro <- round(hipotese1_media - hipotese1_ic[1], digits=2)

## Teste de hipótese
hipotese1_teste <- t.test(
  hipotese1_produtividade$Resposta,
  mu=3,
  conf.level=confianca,
  alternative="greater"
)

# ______________________________________________________________________________

# Hipótese 2: Impacto positivo na satisfação profissional

## Erro máximo tolerável para a variável "Impacto na satisfação profissional"
hipotese2_erro <- 0.25

## Tamanho mínimo da amostra
hipotese2_tamanho_minimo_amostra <- calcular_tamanho_minimo_amostra(
  hipotese2_satisfacao$Resposta,
  hipotese2_erro
)

## Intervalo de confiança
hipotese2_media <- round(mean(hipotese2_satisfacao$Resposta), digits=2)

hipotese2_ic <- calcular_ic_likert(
  hipotese2_satisfacao$Resposta
)
hipotese2_margem_erro <- round(hipotese2_media - hipotese2_ic[1], digits=2)

## Teste de hipótese
hipotese2_teste <- t.test(
  hipotese2_satisfacao$Resposta,
  mu=3,
  conf.level=confianca,
  alternative="greater"
)

# ______________________________________________________________________________

# Hipótese 3: Sentimento com relação mudanças nos cargos

## Erro máximo tolerável para a variável "Sentimento com relação mudanças nos cargos"
hipotese3_erro <- 0.25

## Tamanho mínimo da amostra
hipotese3_tamanho_minimo_amostra <- calcular_tamanho_minimo_amostra(
  hipotese3_cargos$Resposta,
  hipotese3_erro
)

## Intervalo de confiança
hipotese3_media <- round(mean(hipotese3_cargos$Resposta), digits=2)

hipotese3_ic <- calcular_ic_likert(
  hipotese3_cargos$Resposta
)
hipotese3_margem_erro <- round(hipotese3_media - hipotese3_ic[1], digits=2)

## Teste de hipótese
hipotese3_teste <- t.test(
  hipotese3_cargos$Resposta,
  mu=3,
  conf.level=confianca,
  alternative="greater"
)

# ______________________________________________________________________________

# Hipótese 4: Sentimento com relação ao futuro das ferramentas

## Erro máximo tolerável para a variável "Sentimento com relação ao futuro das ferramentas"
hipotese4_erro <- 0.25

## Tamanho mínimo da amostra
hipotese4_tamanho_minimo_amostra <- calcular_tamanho_minimo_amostra(
  hipotese4_futuro$Resposta,
  hipotese4_erro
)

## Intervalo de confiança
hipotese4_media <- round(mean(hipotese4_futuro$Resposta), digits=2)

hipotese4_ic <- calcular_ic_likert(
  hipotese4_futuro$Resposta
)

hipotese4_margem_erro <- round(hipotese4_media - hipotese4_ic[1], digits=2)

## Teste de hipótese
hipotese4_teste <- t.test(
  hipotese4_futuro$Resposta,
  mu=3,
  conf.level=confianca,
  alternative="greater"
)

# ______________________________________________________________________________

# Resultados gerais

hipotese1_teste

writeLines(paste(
  "[Hipótese 1] Tamanho mínimo da amostra: ",
  hipotese1_tamanho_minimo_amostra,
  "\n",
  "[Hipótese 1] Intervalo de confiança: (",
  hipotese1_ic[1],
  ", ",
  hipotese1_ic[2],
  ")",
  "\n",
  "[Hipótese 1] Média: ",
  hipotese1_media,
  "\n",
  "[Hipótese 1] Margem de erro: ",
  hipotese1_margem_erro,
  "\n",
  "[Hipótese 1] Teste de hipótese - Valor p: ",
  hipotese1_teste$p.value,
  "\n",
  "[Hipótese 1] Teste de hipótese - Significância: ",
  alpha,
  "\n",
  "[Hipótese 1] Teste de hipótese - Resultado: ",
  if (hipotese1_teste$p.value >= alpha) {
    "H0 não rejeitada"
  } else {
    "H0 rejeitada"
  },
  sep=""))

hipotese2_teste

writeLines(paste(
  "[Hipótese 2] Tamanho mínimo da amostra: ",
  hipotese2_tamanho_minimo_amostra,
  "\n",
  "[Hipótese 2] Intervalo de confiança: (",
  hipotese2_ic[1],
  ", ",
  hipotese2_ic[2],
  ")",
  "\n",
  "[Hipótese 2] Média: ",
  hipotese2_media,
  "\n",
  "[Hipótese 2] Margem de erro: ",
  hipotese2_margem_erro,
  "\n",
  "[Hipótese 2] Teste de hipótese - Valor p: ",
  hipotese2_teste$p.value,
  "\n",
  "[Hipótese 2] Teste de hipótese - Significância: ",
  alpha,
  "\n",
  "[Hipótese 2] Teste de hipótese - Resultado: ",
  if (hipotese2_teste$p.value >= alpha) {
    "H0 não rejeitada"
  } else {
    "H0 rejeitada"
  },
  sep=""))

hipotese3_teste

writeLines(paste(
  "[Hipótese 3] Tamanho mínimo da amostra: ",
  hipotese3_tamanho_minimo_amostra,
  "\n",
  "[Hipótese 3] Intervalo de confiança: (",
  hipotese3_ic[1],
  ", ",
  hipotese3_ic[2],
  ")",
  "\n",
  "[Hipótese 3] Média: ",
  hipotese3_media,
  "\n",
  "[Hipótese 3] Margem de erro: ",
  hipotese3_margem_erro,
  "\n",
  "[Hipótese 3] Teste de hipótese - Valor p: ",
  hipotese3_teste$p.value,
  "\n",
  "[Hipótese 3] Teste de hipótese - Significância: ",
  alpha,
  "\n",
  "[Hipótese 3] Teste de hipótese - Resultado: ",
  if (hipotese3_teste$p.value >= alpha) {
    "H0 não rejeitada"
  } else {
    "H0 rejeitada"
  },
  sep=""))

hipotese4_teste

writeLines(paste(
  "[Hipótese 4] Tamanho mínimo da amostra: ",
  hipotese4_tamanho_minimo_amostra,
  "\n",
  "[Hipótese 4] Intervalo de confiança: (",
  hipotese4_ic[1],
  ", ",
  hipotese4_ic[2],
  ")",
  "\n",
  "[Hipótese 4] Média: ",
  hipotese4_media,
  "\n",
  "[Hipótese 4] Margem de erro: ",
  hipotese4_margem_erro,
  "\n",
  "[Hipótese 4] Teste de hipótese - Valor p: ",
  hipotese4_teste$p.value,
  "\n",
  "[Hipótese 4] Teste de hipótese - Significância: ",
  alpha,
  "\n",
  "[Hipótese 4] Teste de hipótese - Resultado: ",
  if (hipotese4_teste$p.value >= alpha) {
    "H0 não rejeitada"
  } else {
    "H0 rejeitada"
  },
  sep=""))

ic_resumo <- data.frame(
  variavel = c("1. Produtividade", "2. Satisfação", "3. Cargos", "4. Futuro"),
  valor = c(hipotese1_media, hipotese2_media, hipotese3_media, hipotese4_media),
  ic_inferior = c(hipotese1_ic[1], hipotese2_ic[1], hipotese3_ic[1], hipotese4_ic[1]),
  ic_superior = c(hipotese1_ic[2], hipotese2_ic[2], hipotese3_ic[2], hipotese4_ic[2])
)

ggplot(ic_resumo, aes(x = variavel, y = valor, fill = variavel)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = ic_inferior, ymax = ic_superior), width = 0.4, color = "black") +
  geom_text(aes(label = valor), vjust = -0.5, hjust = -0.25) +
  geom_text(aes(label = ic_inferior, y = ic_inferior), vjust = 1.5) +
  geom_text(aes(label = ic_superior, y = ic_superior), vjust = -0.5) +
  coord_cartesian(ylim = c(0, max(ic_resumo$valor) * 1.3)) +
  labs(title = "Intervalos de confiança", x = "Variável", y = "Valor", fill = "Variável")
