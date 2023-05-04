# Title     : Trabalho individual de verificação suplementar
#             Análise Preditiva - MBA Business Analytics e Big Data
# Objective : Realizar análise dos dados e responder as questões propostas
# Created by: willyferreira
# Created on: 23/08/21

#-------------------------------------------------------------#
#----1.Instalação e leitura dos pacotes que serão utilizados----
#-------------------------------------------------------------#
install.packages('dplyr')
install.packages('openxlsx')
install.packages('stringr')
install.packages('car')
install.packages('corrplot')
install.packages("readxl")
install.packages('Rcpp')
install.packages('gmodels')
install.packages('rms')
install.packages('rpart')
install.packages('rpart.plot')
install.packages('caret')
install.packages('arules')
install.packages('rms')
install.packages('hmeasure')
install.packages('MLmetrics')
install.packages('rms')
install.packages('classifierplots')
install.packages('randomForest')
install.packages('hmeasure')
#
library(dplyr)
library(openxlsx)
library(stringr)
library(car)
library(corrplot)
library(readxl)
library(Rcpp)
library(gmodels)
library(rms)
library(rpart)
library(rpart.plot)
library(caret)
library(arules)
library(rms)
library(hmeasure)
library(MLmetrics)
library(rms)
library(classifierplots)
library(randomForest)
library(hmeasure)

#-------------------------------------------------------------#
#----2.Área para edições e impressão de variáveis----
#-------------------------------------------------------------#
rm(list = ls())

#-------------------------------------------------------------#
#----3.Declaração das variáveis----
#-------------------------------------------------------------#
arquivo_original <- 'D:/OneDrive/MBA/03 - Analise Preditiva/Trabalhos/Trabalho 02 - Exame Final/BSB T10 AP P1.xlsx'
cor_preenchimento_1 <- "deepskyblue1"
cor_preenchimento_2 <- "deepskyblue2"
cor_preenchimento_3 <- "deepskyblue3"
cor_preenchimento_4 <- "deepskyblue4"
cor_preenchimento_5 <- "gray"
cor_preenchimento_6 <- "red"

#-------------------------------------------------------------#
#----4.Leitura dos datas frame----
#-------------------------------------------------------------#
df_questoes1e2 <- read.xlsx(arquivo_original, #arquivo xlsx
                  sheet = 'questoes1e2', #nome da planilha
                  startRow = 1, #primeira linha a considerar para a importação
                  colNames = TRUE, #preservar nome das colunas
                  rowNames = FALSE, #linhas
                  detectDates = FALSE, #detectar datas
                  skipEmptyRows = TRUE, #pular linhas em branco
                  skipEmptyCols = TRUE, #If TRUE, empty columns are skipped.
                  rows = NULL, #A numeric vector specifying which rows in the Excel file to read. If NULL, all rows are read.
                  cols = NULL, #A numeric vector specifying which columns in the Excel file to read. If NULL, all columns are read.
                  check.names = FALSE, #logical. If TRUE then the names of the variables in the data frame are checked to ensure
                  #that they are syntactically valid variable names
                  sep.names = ".", #One character which substitutes blanks in column names. By default, "."
                  namedRegion = NULL, #A named region in the Workbook. If not NULL startRow, rows and cols parameters are ignored.
                  na.strings = "NA", #A character vector of strings which are to be interpreted as NA. Blank cells will be returned as NA.
                  fillMergedCells = FALSE) #If TRUE, the value in a merged cell is given to all cells within the merge.

str(df_questoes1e2)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
df_questoes3 <- read.xlsx(arquivo_original, #arquivo xlsx
                  sheet = 'questao3', #nome da planilha
                  startRow = 1, #primeira linha a considerar para a importação
                  colNames = TRUE, #preservar nome das colunas
                  rowNames = FALSE, #linhas
                  detectDates = FALSE, #detectar datas
                  skipEmptyRows = TRUE, #pular linhas em branco
                  skipEmptyCols = TRUE, #If TRUE, empty columns are skipped.
                  rows = NULL, #A numeric vector specifying which rows in the Excel file to read. If NULL, all rows are read.
                  cols = NULL, #A numeric vector specifying which columns in the Excel file to read. If NULL, all columns are read.
                  check.names = FALSE, #logical. If TRUE then the names of the variables in the data frame are checked to ensure
                  #that they are syntactically valid variable names
                  sep.names = ".", #One character which substitutes blanks in column names. By default, "."
                  namedRegion = NULL, #A named region in the Workbook. If not NULL startRow, rows and cols parameters are ignored.
                  na.strings = "NA", #A character vector of strings which are to be interpreted as NA. Blank cells will be returned as NA.
                  fillMergedCells = FALSE) #If TRUE, the value in a merged cell is given to all cells within the merge.

str(df_questoes3)

rm(arquivo_original)
#-------------------------------------------------------------#
#Criando um data frame para iniciar as análises e renomeando as variáveis

#cria o df para análises
df <- df_questoes1e2
str(df)
summary(df)
rm(df_questoes1e2)

#-------------------------------------------------------------#
#----5.Tratamento do data frame----
#-------------------------------------------------------------#
#renomeia as variáveis
names(df) <- c(
  'funcionario',
  'status',
  'idade',
  'estado_civil',
  'distancia_empresa',
  'tipo_residencia',
  'primeiro_emprego',
  'resultado_teste')

str(df)
#-------------------------------------------------------------#
#Transforma a variável alvo em factor

df <- df %>% mutate(
  status = factor(status)
  )

#-------------------------------------------------------------#
#----6.Análise Bivariada----
#-------------------------------------------------------------#
str(df)
#-------------------------------------------------------------#
#----6.1.Status x Idade----
CrossTable(
  df$idade,
  df$status,
  prop.chisq = FALSE,
  prop.t = FALSE,
  prop.c = FALSE
)

#-------------------------------------------------------------#
#----6.2.Status x Estado Civil----
CrossTable(
  df$estado_civil,
  df$status,
  prop.chisq = FALSE,
  prop.t = FALSE,
  prop.c = FALSE
)

#-------------------------------------------------------------#
#----6.3.Status x Distância Empresa----
CrossTable(
  df$distancia_empresa,
  df$status,
  prop.chisq = FALSE,
  prop.t = FALSE,
  prop.c = FALSE
)

#-------------------------------------------------------------#
#----6.4.Status x Tipo de Residência----
CrossTable(
  df$tipo_residencia,
  df$status,
  prop.chisq = FALSE,
  prop.t = FALSE,
  prop.c = FALSE
)

#-------------------------------------------------------------#
#----6.5.Status x Primeiro emprego----
CrossTable(
  df$primeiro_emprego,
  df$status,
  prop.chisq = FALSE,
  prop.t = FALSE,
  prop.c = FALSE
)

#-------------------------------------------------------------#
#----6.6.Status x Resultado Teste----
CrossTable(
  df$resultado_teste,
  df$status,
  prop.chisq = FALSE,
  prop.t = FALSE,
  prop.c = FALSE
)

#-------------------------------------------------------------#
#----7.Regressão Logística e criação do modelo----
#-------------------------------------------------------------#

#-------------------------------------------------------------#
#----7.1 Regressão Logística----
#-------------------------------------------------------------#
#cria data frame sem a variável Funcionario
df_regressao_logistica <- df[ , 2:8] #cria o data frame para trabalho
str(df_regressao_logistica) #estrutura do data frame
summary(df_regressao_logistica$status) #estrutura da variável status
CrossTable(df_regressao_logistica$status) #faz tabela com a variável status

names(df_regressao_logistica)

#transforma a variável status em numérica
df_regressao_logistica$status_reg<- ifelse(
  df_regressao_logistica$status == 'mau',
  1,
  0
  )

#elimina a variável status
df_regressao_logistica$status = NULL
str(df_regressao_logistica)
#-------------------------------------------------------------#
#Faz a regressão logística
#-------------------------------------------------------------#
reg_modelo <- glm(
  data = df_regressao_logistica,
  status_reg~.,
  family = binomial()
  )

#mostra o sumário do modelo e variáveis dummies
summary(reg_modelo)

#-------------------------------------------------------------#
#----7.2.Avalia a calibração do modelo - Mesma coisa questão B----
#-------------------------------------------------------------#
df_regressao_logistica_calib <- df_regressao_logistica
df_regressao_logistica_calib$PD <- round(
    predict(
      reg_modelo,
      df_regressao_logistica_calib,
      type = 'response'
    ), digits = 3
  )

#H0: O modelo está calibrado, confiar nas probs estimadas pela regressao
#Ha: O modelo não está calibrado, não confiar

val.prob(
  p = df_regressao_logistica_calib$PD,
  y = df_regressao_logistica_calib$status_reg,
  smooth = FALSE
)
########## p-value: 9.724192e-01
##########Aceitar H0 | #H0: O modelo está calibrado!

#-------------------------------------------------------------#
#----7.3.Avalia a capacidade de calibração do modelo----
#                     AUC -AUROC                              #
#-------------------------------------------------------------#
HMeasure(
    df_regressao_logistica_calib$status_reg,
    df_regressao_logistica_calib$PD
  )$metric
# AUC = 0.7211963

roc_plot(
  pred.prob = df_regressao_logistica_calib$PD,
  test.y = df_regressao_logistica_calib$status_reg
)
# AUC: 72.1196256901886

#-------------------------------------------------------------#
#----Questões B - Utilizam a regressão logística----
#-------------------------------------------------------------#

#-------------------------------------------------------------#
#----Questão B1----
#-------------------------------------------------------------#

#Cria um data frame apenas com o Funcionário A1
df_questaoB1 <- df_regressao_logistica [ 1, 1:7]

questaoB1_pmau <- predict(
  reg_modelo,
  newdata = df_questaoB1,
  type = 'response'
  )

questaoB1_pmau
questaoB1_pmau_3dig <- round(questaoB1_pmau, digits = 3)
questaoB1_pmau_3dig
questaoB1_pmau_percentual <-round((questaoB1_pmau * 100), digits = 3)
questaoB1_pmau_percentual
#-------------------------------------------------------------#
#----Questão B2----
#-------------------------------------------------------------#

#Cria um data frame apenas com o indivíduo da questão
df_questaoB2 <- data.frame(
  idade = '25a45',
  estado_civil = 'solt',
  distancia_empresa = 'média',
  tipo_residencia = 'propria',
  primeiro_emprego = 'sim',
  resultado_teste = 76
  )

questaoB2_pmau <- predict(
  reg_modelo,
  newdata = df_questaoB2,
  type = 'response'
)

questaoB2_pmau
questaoB2_pmau_3dig <- round(questaoB2_pmau, digits = 3)
questaoB2_pmau_3dig
questaoB2_pmau_percentual <- round((questaoB2_pmau * 100), digits = 3)
questaoB2_pmau_percentual
#-------------------------------------------------------------#
#----Questão B3----
#-------------------------------------------------------------#

#Cria um data frame para inclusão do pmau
df_questaoB3 <- df_regressao_logistica

#Cria o PD baseado no modelo de regressão
df_questaoB3$PD <- predict(
    reg_modelo,
    df_questaoB3,
    type = 'response'
    )

#Cria ponto de corte para calcular a taxa de erro
K50 <- ifelse(
  df_questaoB3$PD > 0.500,
  'mau',
  'bom'
  )

#Cria tabela para avaliar a taxa de erro
CrossTable(
  K50,
  df_questaoB3$status_reg,
  prop.t = FALSE,
  prop.chisq = FALSE
)

#soma das avaliações corretas dividido pelo total
accuracy_K50 <- (506 + 458) / 1448
accuracy_K50
accuracy_K50_3dig <- round(accuracy_K50, digits = 3)
accuracy_K50_3dig
accuracy_K50_percentual <- round((accuracy_K50 * 100), digits = 3)
accuracy_K50_percentual

#soma das avaliações incorretas dividido pelo total
tx_erro_K50 <- (251 + 233) / 1448
tx_erro_K50
tx_erro_K50_3dig <- round(tx_erro_K50, digits = 3)
tx_erro_K50_3dig
tx_erro_K50_percentual <- round((tx_erro_K50 * 100), digits = 3)
tx_erro_K50_percentual
#-------------------------------------------------------------#
#----Questão B4----
#-------------------------------------------------------------#
#necessário transformar variáveis qualitativas em factor para rodar
#RANDOM FOREST

df_questaoB4 <- df_regressao_logistica
names(df_questaoB4)

#Transforma as variáveis em factor
df_questaoB4 <- df_questaoB4 %>% mutate(
  idade = factor(idade),
  estado_civil = factor(estado_civil),
  distancia_empresa = factor(distancia_empresa),
  tipo_residencia = factor(tipo_residencia),
  primeiro_emprego = factor(primeiro_emprego),
  status_reg = factor(status_reg)
  )
str(df_questaoB4)

library(randomForest)
#Rodando o Random FOrest
set.seed(789) #set.seed definido no exercício
arvore_RF <- randomForest(
  data = df_questaoB4,
  status_reg~.,
  mtry = 4, #definido 4 variáveis conforme solicitado no exercício
  ntree = 400, #quantidade de árvores definida no exercício
  )

arvore_RF #OOB estimate of  error rate: 35.57%

#PREVISÃO
prev.arvore_RF <- predict(
  arvore_RF,
  newdata = df_questaoB4,
  type = 'prob'
)

#inclui pmau no data frame
head(prev.arvore_RF)
df_questaoB4$pmau <- prev.arvore_RF[ , 2]
df_questaoB4$pmau

prev.arvore_RF[,1]

#Imprime pmau do indivíduo A1
df_questaoB4[ 1, 8]
questaoB4_pmau <- df_questaoB4[ 1, 8]
questaoB4_pmau
questaoB4_pmau_3dig <- round(questaoB4_pmau, digits = 3)
questaoB4_pmau_3dig
questaoB4_pmau_percentual <- round((questaoB4_pmau * 100), digits = 3)
questaoB4_pmau_percentual

#-------------------------------------------------------------#
#----Questão B5----
#-------------------------------------------------------------#

#Saber o valor da área AUROC
round(#Arredondar o resultado para 3 casas decimais
  HMeasure(
  df_questaoB4$status_reg,
  prev.arvore_RF[ , 1],
  )$metric[[3]],
digits = 3
)

#Mostra gráfico das variáveis mais importantes
varImpPlot(arvore_RF)

#-------------------------------------------------------------#
#-------------------------------------------------------------#
#----8.Árvore de Classificação--------------------------------#
#-------------------------------------------------------------#
#-------------------------------------------------------------#

#cria um data frame sem a variável funcionário
df_arvore <- df[ , 2:8]
colnames(df_arvore)
str(df_arvore)
#cria uma tabela com a variável alvo
status_arvore <- table(df_arvore$status)
status_arvore

#seta o set.seed de acordo com o determinado no exercício
set.seed(123) #por conta do cross validation
#rodar a árvore
arvore_class <- rpart(
  data = df_arvore,
  status~.
)
arvore_class

#cria o plot da árvore
prp(
  arvore_class,
  extra = 104,
  type = 5,
  nn = F,
  fallen.leaves = T,
  branch.lty = 2,
  box.col = c('green', 'yellow')
)

#barplot importancia por variável
barplot(
  arvore_class$variable.importance,
  col = cor_preenchimento_3,
  las = 2
)
#-------------------------------------------------------------#
#----Questão A - Utilizam árvore----
#-------------------------------------------------------------#

#-------------------------------------------------------------#
#----Questão A1----
#-------------------------------------------------------------#

#cria o data frame com os dados do funcionário
df_questaoA1 <- data.frame(
  idade = '25a45',
  estado_civil = 'solt',
  distancia_empresa = 'média',
  tipo_residencia = 'propria',
  primeiro_emprego = 'não',
  resultado_teste = 82
)

str(df_questaoA1)

#faz o cálculo de pmau com o indivíduo do exercício
pmau.questaoA1 <- predict(
  arvore_class,
  newdata = df_questaoA1,
  type = 'prob'
  )

pmau.questaoA1
#> pmau.questaoA1
#bom        mau
#0.7204659  0.2795341

#-------------------------------------------------------------#
#----Questão A2----
#-------------------------------------------------------------#
df_questaoA2 <- df [ , -1]

#faz o cálculo de pmau usando a árvore
pred.arvore_class <- predict(
  arvore_class,
  newdata = df_questaoA2,
  type = 'prob'
)

head(pred.arvore_class, 10)
df_questaoA2$pbom <- round(pred.arvore_class[ , 1], digits = 3)
head(df_questaoA2$pbom)

ponto_corte <- 0.400
df_questaoA2$Klass40 <- ifelse(
  df_questaoA2$pbom > ponto_corte,
  'prev_bom',
  'prev_mau'
)

matriz_class_a2 <- table(
  df_questaoA2$Klass40,
  df_questaoA2$status,
  dnn = c('Classificação', 'Realidade')
  )
matriz_class_a2

tx_erro_Klass40 <- round((139 + 324) / 1448, digits = 3)
tx_erro_Klass40
accuracy_Klass40 <- 1.000 - tx_erro_Klass40
accuracy_Klass40

CrossTable(df_questaoA2$Klass40,
           df_questaoA2$status,
           digits = 3,
           prop.c = TRUE,
           prop.r = FALSE,
           prop.chisq = FALSE,
           prop.t = FALSE
           )

#Resposta
a2 <- round(139 / 739, digits = 3)
a2

#-------------------------------------------------------------#
#----Questão A3----
#-------------------------------------------------------------#

#Área AUROC
round(
    HMeasure(
      df_questaoA2$status,
      df_questaoA2$pbom
      )$metric,
    digits = 3
)
#Resposta: AUC 0.737

#-------------------------------------------------------------#
#----Questão A4----
#-------------------------------------------------------------#
df_arvore_200 <- df_arvore

set.seed(123) #por conta do cross validation
arvore_class_200 <- rpart(
  data = df_arvore_200,
  status~.,
  control = rpart.control(minbucket = 200)
  )

arvore_class_200

printcp(arvore_class_200)

prp(
  arvore_class_200,
  extra = 104,
  type = 5,
  nn = F,
  fallen.leaves = T,
  branch.lty = 2,
  box.col = c('green', 'yellow')
)

barplot(
  arvore_class_200$variable.importance,
  col = cor_preenchimento_3,
  las = 2
)

pred.arvore_class_200 <- predict(
  arvore_class_200,
  newdata = df_arvore,
  type = 'prob'
  )

head(pred.arvore_class_200, 10)
df_arvore_200$pbom <- round(pred.arvore_class_200[ , 1], digits = 3)
head(df_arvore_200$pbom)


#-------------------------------------------------------------#
#----Questões C - Utilizam outro data frame ----
#-------------------------------------------------------------#

#Leitura do data frame
df_3 <- df_questoes3
rm(df_questoes3)
summary(df_3)

#Análise bivariada por meio da variável alvo
#INCM
plot(
  EXPN ~ INCM,
  data = df_3,
  pch = 19,
  col = cor_preenchimento_1
)
abline((lm(EXPN ~ INCM, data = df_3)), lty = 2, lwd = 2, col = cor_preenchimento_6)
round(cor(df_3$EXPN, df_3$INCM), digits = 3)

#HIGH
plot(
  EXPN ~ HIGH,
  data = df_3,
  pch = 19,
  col = cor_preenchimento_1
)
abline((lm(EXPN ~ HIGH, data = df_3)), lty = 2, lwd = 2, col = cor_preenchimento_6)
round(cor(df_3$EXPN, df_3$HIGH), digits = 3)

#OVER
plot(
  EXPN ~ OVER,
  data = df_3,
  pch = 19,
  col = cor_preenchimento_1
)
abline((lm(EXPN ~ OVER, data = df_3)), lty = 2, lwd = 2, col = cor_preenchimento_6)
round(cor(df_3$EXPN, df_3$OVER), digits = 3)

#PHYD
plot(
  EXPN ~ PHYD,
  data = df_3,
  pch = 19,
  col = cor_preenchimento_1
)
abline((lm(EXPN ~ PHYD, data = df_3)), lty = 2, lwd = 2, col = cor_preenchimento_6)
round(cor(df_3$EXPN, df_3$PHYD), digits = 3)


#Regressão Múltipla ---
reg_mult <- lm(
  EXPN~.,
  data = df_3
  )
summary(reg_mult)


#-------------------------------------------------------------#
#----Questão C1----
#-------------------------------------------------------------#
#Aplicação do Akaike Information Criterion - AIC

step(reg_mult)  # Não houve necessidade de exclusão de variável conforme
#o resultado do teste
#HIGH | -12.97407
#-------------------------------------------------------------#

#Cria a variável com o valor estimado pelo modelo para a variavel alvo
df_3 <- df_3 %>% mutate(
  EXPN_previsao = fitted(reg_mult)
)

# Cria a variável com o valor estimado pelo modelo para a variavel alvo
df_3 <- df_3 %>% mutate(
  residuo_previsao = residuals(reg_mult)
)

#-------------------------------------------------------------#
#----Questão C2----
#-------------------------------------------------------------#
# Calcula o erro percentual
df_3 <- df_3 %>% mutate(
  erro_percentual = round((residuo_previsao / EXPN) * 100, digits = 3)
)


# Criando o gráfico de Erro Percentual
plot(
  df_3$erro_percentual,
  col = cor_preenchimento_4
)
grid()
abline(h = 0, lty = 2, lwd = 2, col = cor_preenchimento_6)

#-------------------------------------------------------------#
#----Questão C3----
#-------------------------------------------------------------#
#Analise de pontos influentes ---

# Gráfico de bolhas
influencePlot(
  model = reg_mult
)

# Gráfico
influenceIndexPlot (
  model = reg_mult,
  vars = c("Cook","Studentized","hat")
)

#Resposta: ponto 23
#23 -2.3433782 0.22442810 0.28896984

for (variavel in names(df_3)) {
  print(paste("---", variavel, "---"))
}
