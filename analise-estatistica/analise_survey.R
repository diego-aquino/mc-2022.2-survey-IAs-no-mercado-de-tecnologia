# Importando as bibliotecas
if (!require('tidyverse'))
  install.packages('tidyverse')
library('tidyverse')
if (!require('gridExtra'))
  install.packages('gridExtra')
library(gridExtra)
library(forcats)

library('ggplot2')

# Importando o dataset
respostas <- read.csv(paste(getwd(), 'questionario-final/respostas.csv', sep='/'))
head(respostas)
summary(respostas)

# ______________________________________________________________________________

# Retirando respostas inválidas:

# Apenas quem aceita o termo de consentimento e quem trabalha ou estuda na área de tecnologia

respostas <- subset(respostas, respostas[2] == 'Aceito participar' & respostas[3] == 'Sim')

# Retirando respostas em que o participante diz que usa as ferramentas, mas
# não indica nenhuma

respostas <- subset(respostas, (respostas[6] == 'Não') | (respostas[6] == 'Sim' & 
            (respostas[8] != 'Nunca utilizei' | respostas[9] != 'Nunca utilizei' | respostas[10] != 'Nunca utilizei' | respostas[11] != 'Nunca utilizei' | respostas[12] != "")))


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

# ______________________________________________________________________________

# Análise contextual

# Renomeando as colunas
colnames(caracterizacao_amostra)[1] = "Áreas de atuação"
colnames(caracterizacao_amostra)[2] = "Tempo de experiência"
colnames(caracterizacao_amostra)[3] = "Utiliza ferramentas de IA"
colnames(caracterizacao_amostra)[4] = "Propositos ferramentas"
colnames(caracterizacao_amostra)[5] = "Tempo de uso ChatGPT"
colnames(caracterizacao_amostra)[6] = "Tempo de uso Bing Chat"
colnames(caracterizacao_amostra)[7] = "Tempo de uso Github Copilot"
colnames(caracterizacao_amostra)[8] = "Tempo de uso Tabnine"
colnames(caracterizacao_amostra)[9] = "Tempo de uso outros"
colnames(caracterizacao_amostra)[10] = "Frequência ChatGPT"
colnames(caracterizacao_amostra)[11] = "Frequência Bing Chat"
colnames(caracterizacao_amostra)[12] = "Frequência Github Copilot"
colnames(caracterizacao_amostra)[13] = "Frequência Tabnine"
colnames(caracterizacao_amostra)[14] = "Frequência outros"

# ______________________________________________________________________________

# Áreas de atuação

#areas_names <- c("Gerência de Projetos", "Desenvolvimento de Software", "Infraestrutura e DevOps",
#          "Inteligência artificial", "Ciência de Dados", 
#          "Ensino e aprendizado (professor(a), monitor(a) ou estudante)")

#areas_count <- c(0, 0, 0, 0, 0, 0)

#for (r in caracterizacao_amostra$`Áreas de atuação`) {
#  split_r <- strsplit(r, ",")
#  for (r1 in split_r[[1]]) {
#    print(r1)
#  }
  #if (r == "Gerência de Projetos") {
  #  areas_count[1] = areas_count[1] + 1
  #} else if (r == "Desenvolvimento de Software") {
  #  areas_count
  #}
#}

# ______________________________________________________________________________

# Tempo de experiência
tempo_experiencia_freq <- as.data.frame(table(caracterizacao_amostra$`Tempo de experiência`))

tempo_experiencia_freq <- tempo_experiencia_freq %>% 
  add_row(Var1 = "Entre 6 e 8 anos", Freq=0,)

rows_order <- c("Menos que 2 anos", "Entre 2 e 4 anos", 
                "Entre 4 e 6 anos", "Entre 6 e 8 anos",
                "Mais que 8 anos")

tempo_experiencia_freq <- tempo_experiencia_freq %>%
  slice(match(rows_order, Var1))

tempo_experiencia_freq$Freq <- round(((tempo_experiencia_freq$Freq / sum(tempo_experiencia_freq$Freq)) * 100), 1)

tempo_experiencia_freq %>%
  mutate(Var1=factor(Var1, levels=Var1)) %>% 
  ggplot(aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") + theme_minimal() +
  geom_text(aes(label = Freq), vjust = 1.2, color = "white", size = 3) +
  labs(x = "", y = "%", title = "Tempo de experiência")

# ______________________________________________________________________________

# Utiliza ferramentas
utiliza_ferramentas_freq <- as.data.frame(table(caracterizacao_amostra$`Utiliza ferramentas de IA`))

utiliza_ferramentas_freq$Freq <- round(((utiliza_ferramentas_freq$Freq / sum(utiliza_ferramentas_freq$Freq)) * 100), 1)

ggplot(utiliza_ferramentas_freq, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") + theme_minimal() +
  geom_text(aes(label = Freq), vjust = 1.2, color = "white", size = 3) +
  labs(x = "", y = "%", title = "Utiliza ferramentas de IA")

# ______________________________________________________________________________

# Propositos de uso

# ______________________________________________________________________________

# Tempo de uso
rows_order <- c("Nunca utilizei", "Menos de 1 ano", 
                "Entre 1 e 2 anos", "Mais que 3 anos")

tempo_uso_chat_gpt_freq <- as.data.frame(table(caracterizacao_amostra$`Tempo de uso ChatGPT`))
tempo_uso_chat_gpt_freq <- subset(tempo_uso_chat_gpt_freq, Var1 != "")

tempo_uso_chat_gpt_freq <- tempo_uso_chat_gpt_freq %>%
  slice(match(rows_order, Var1))

tempo_uso_chat_gpt_freq$Freq <- round(((tempo_uso_chat_gpt_freq$Freq / sum(tempo_uso_chat_gpt_freq$Freq)) * 100), 1)

tempo_uso_chat_gpt_plot <- tempo_uso_chat_gpt_freq %>%
  mutate(Var1=factor(Var1, levels=Var1)) %>% 
  ggplot(aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") + theme_minimal() +
  geom_text(aes(label = Freq), vjust = -0.5, color = "black", size = 3) +
  labs(x = "", y = "%", title = "Tempo de uso - ChatGPT")

tempo_uso_bing_freq <- as.data.frame(table(caracterizacao_amostra$`Tempo de uso Bing Chat`))
tempo_uso_bing_freq <- subset(tempo_uso_bing_freq, Var1 != "")

tempo_uso_bing_freq <- tempo_uso_bing_freq %>%
  slice(match(rows_order, Var1))

tempo_uso_bing_freq$Freq <- round(((tempo_uso_bing_freq$Freq / sum(tempo_uso_bing_freq$Freq)) * 100), 1)

tempo_uso_bing_plot <- tempo_uso_bing_freq %>%
  mutate(Var1=factor(Var1, levels=Var1)) %>% 
  ggplot(aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") + theme_minimal() +
  geom_text(aes(label = Freq), vjust = -0.5, color = "black", size = 3) +
  labs(x = "", y = "%", title = "Tempo de uso - Bing Chat")

tempo_uso_copilot_freq <- as.data.frame(table(caracterizacao_amostra$`Tempo de uso Github Copilot`))
tempo_uso_copilot_freq <- subset(tempo_uso_copilot_freq, Var1 != "")

tempo_uso_copilot_freq <- tempo_uso_copilot_freq %>%
  slice(match(rows_order, Var1))

tempo_uso_copilot_freq$Freq <- round(((tempo_uso_copilot_freq$Freq / sum(tempo_uso_copilot_freq$Freq)) * 100), 1)

tempo_uso_copilot_plot <- tempo_uso_copilot_freq %>%
  mutate(Var1=factor(Var1, levels=Var1)) %>% 
  ggplot(aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") + theme_minimal() +
  geom_text(aes(label = Freq), vjust = -0.5, color = "black", size = 3) +
  labs(x = "", y = "%", title = "Tempo de uso - GitHub Copilot")

tempo_uso_tabnine_freq <- as.data.frame(table(caracterizacao_amostra$`Tempo de uso Tabnine`))
tempo_uso_tabnine_freq <- subset(tempo_uso_tabnine_freq, Var1 != "")

tempo_uso_tabnine_freq <- tempo_uso_tabnine_freq %>%
  slice(match(rows_order, Var1))

tempo_uso_tabnine_freq$Freq <- round(((tempo_uso_tabnine_freq$Freq / sum(tempo_uso_tabnine_freq$Freq)) * 100), 1)

tempo_uso_tabnine_plot <- tempo_uso_tabnine_freq %>%
  mutate(Var1=factor(Var1, levels=Var1)) %>% 
  ggplot(aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") + theme_minimal() +
  geom_text(aes(label = Freq), vjust = -0.5, color = "black", size = 3) +
  labs(x = "", y = "%", title = "Tempo de uso - Tabnine")

  grid.arrange(tempo_uso_chat_gpt_plot, tempo_uso_bing_plot, tempo_uso_copilot_plot, tempo_uso_tabnine_plot)

# ______________________________________________________________________________

# Frequência de uso

rows_order <- c("Nunca utilizei", "Raramente", "Mensalmente",
                "Semanalmente", "Diariamente")

frequencia_chat_gpt <- as.data.frame(table(caracterizacao_amostra$`Frequência ChatGPT`))
frequencia_chat_gpt <- subset(frequencia_chat_gpt, Var1 != "")

frequencia_chat_gpt <- frequencia_chat_gpt %>%
  slice(match(rows_order, Var1))

frequencia_chat_gpt$Freq <- round(((frequencia_chat_gpt$Freq / sum(frequencia_chat_gpt$Freq)) * 100), 1)

frequencia_chat_gpt_plot <- frequencia_chat_gpt %>%
  mutate(Var1=factor(Var1, levels=Var1)) %>% 
  ggplot(aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") + theme_minimal() +
  geom_text(aes(label = Freq), vjust = -0.5, color = "black", size = 3) +
  labs(x = "", y = "%", title = "Frequência - ChatGPT")

frequencia_bing <- as.data.frame(table(caracterizacao_amostra$`Frequência Bing Chat`))
frequencia_bing <- subset(frequencia_bing, Var1 != "")

frequencia_bing <- frequencia_bing %>%
  slice(match(rows_order, Var1))

frequencia_bing$Freq <- round(((frequencia_bing$Freq / sum(frequencia_bing$Freq)) * 100), 1)

frequencia_bing_plot <- frequencia_bing %>%
  mutate(Var1=factor(Var1, levels=Var1)) %>% 
  ggplot(aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") + theme_minimal() +
  geom_text(aes(label = Freq), vjust = -0.5, color = "black", size = 3) +
  labs(x = "Frequência", y = "Quantidade", title = "Frequência - Bing Chat")

frequencia_copilot <- as.data.frame(table(caracterizacao_amostra$`Frequência Github Copilot`))
frequencia_copilot <- subset(frequencia_copilot, Var1 != "")

frequencia_copilot <- frequencia_copilot %>%
  slice(match(rows_order, Var1))

frequencia_copilot$Freq <- round(((frequencia_copilot$Freq / sum(frequencia_copilot$Freq)) * 100), 1)

frequencia_copilot_plot <- frequencia_copilot %>%
  mutate(Var1=factor(Var1, levels=Var1)) %>% 
  ggplot(aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") + theme_minimal() +
  geom_text(aes(label = Freq), vjust = -0.5, color = "black", size = 3) +
  labs(x = "Frequência", y = "Quantidade", title = "Frequência - GitHub Copilot")

frequencia_tabnine <- as.data.frame(table(caracterizacao_amostra$`Frequência Tabnine`))
frequencia_tabnine <- subset(frequencia_tabnine, Var1 != "")

frequencia_tabnine <- frequencia_tabnine %>%
  slice(match(rows_order, Var1))

frequencia_tabnine$Freq <- round(((frequencia_tabnine$Freq / sum(frequencia_tabnine$Freq)) * 100), 1)

frequencia_tabnine_plot <- frequencia_tabnine %>%
  mutate(Var1=factor(Var1, levels=Var1)) %>% 
  ggplot(aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") + theme_minimal() +
  geom_text(aes(label = Freq), vjust = -0.5, color = "black", size = 3) +
  labs(x = "Frequência", y = "Quantidade", title = "Frequência - Tabnine")

grid.arrange(frequencia_chat_gpt_plot, frequencia_bing_plot, frequencia_copilot_plot, frequencia_tabnine_plot)


# ______________________________________________________________________________

# Análise exploratória

# Renomeando as colunas

colnames(hipotese1_produtividade)[1] = "Resposta"
colnames(hipotese2_satisfacao)[1] = "Resposta"
colnames(hipotese3_cargos)[1] = "Resposta"
colnames(hipotese4_futuro)[1] = "Resposta"

# Retirando respostas vazias das 2 primeiras hipóteses
# (apenas os participantes que usam as ferramentas respondem essas questões)

hipotese1_produtividade <- subset(hipotese1_produtividade,  Resposta != "")
hipotese2_satisfacao <- subset(hipotese2_satisfacao, Resposta != "")

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

# ______________________________________________________________________________

# Comparando média, mediana e desvio padrao

tabela <- matrix(c(mean(hipotese1_produtividade$Resposta), median(hipotese1_produtividade$Resposta), sd(hipotese1_produtividade$Resposta),
                   mean(hipotese2_satisfacao$Resposta), median(hipotese2_satisfacao$Resposta), sd(hipotese2_satisfacao$Resposta),
                   mean(hipotese3_cargos$Resposta), median(hipotese3_cargos$Resposta), sd(hipotese3_cargos$Resposta),
                   mean(hipotese4_futuro$Resposta), median(hipotese4_futuro$Resposta), sd(hipotese4_futuro$Resposta)), ncol=3, byrow=TRUE)
colnames(tabela) <- c('Média','Mediana','Desvio padrão')
rownames(tabela) <- c('Hipótese 1 - Produtividade','Hipótese 2 - Satisfação','Hipótese 3 - Cargos','Hipótese 4 - Futuro')
tabela <- as.table(tabela)
tabela

# ______________________________________________________________________________

# Gerando gráficos de barras

hipotese1_produtividade_freq <- as.data.frame(table(hipotese1_produtividade$Resposta))

hipotese1_produtividade_freq$Freq <- round(((hipotese1_produtividade_freq$Freq / sum(hipotese1_produtividade_freq$Freq)) * 100), 1)

hipotese1_produtividade_plot <- ggplot(hipotese1_produtividade_freq, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") + theme_minimal() +
  labs(x = "", y = "%", title = "Hipótese 1")

hipotese2_satisfacao_freq <- as.data.frame(table(hipotese2_satisfacao$Resposta))

hipotese2_satisfacao_freq$Freq <- round(((hipotese2_satisfacao_freq$Freq / sum(hipotese2_satisfacao_freq$Freq)) * 100), 1)

hipotese2_satisfacao_plot <- ggplot(hipotese2_satisfacao_freq, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") + theme_minimal() +
  labs(x = "", y = "%", title = "Hipótese 2")

hipotese3_cargos_freq <- as.data.frame(table(hipotese3_cargos$Resposta))

hipotese3_cargos_freq$Freq <- round(((hipotese3_cargos_freq$Freq / sum(hipotese3_cargos_freq$Freq)) * 100), 1)

hipotese3_cargos_plot <- ggplot(hipotese3_cargos_freq, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") + theme_minimal() +
  labs(x = "", y = "%", title = "Hipótese 3")

hipotese4_futuro_freq <- as.data.frame(table(hipotese4_futuro$Resposta))

hipotese4_futuro_freq$Freq <- round(((hipotese4_futuro_freq$Freq / sum(hipotese4_futuro_freq$Freq)) * 100), 1)

hipotese4_futuro_plot <- ggplot(hipotese4_futuro_freq, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") + theme_minimal() +
  labs(x = "", y = "%", title = "Hipótese 4")

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