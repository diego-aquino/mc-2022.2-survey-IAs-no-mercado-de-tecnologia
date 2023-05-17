# Importando as bibliotecas
install.packages('tidyverse')
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
