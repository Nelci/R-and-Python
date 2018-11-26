setwd("~/OneDrive/posgraduacao/Especializacao Ciencias de Dados e Big Data/Programacao R e Python/")
insurance <- read.csv("insurance.csv",header=TRUE)
reglog = glm(factor(smoker) ~ ., data = insurance, family = 'binomial')
probpred = predict(reglog, data = insurance, type = 'response')
classificacao = ifelse(probpred > 0.5, 1, 0)
table(classificacao, insurance$smoker)


fnACC = function(insur){
  reglog = glm(factor(smoker) ~ ., data = insur, family = 'binomial')
  probpred = predict(reglog, data = insur, type = 'response')
  classificacao = ifelse(probpred > 0.5, 1, 0)
  return (table(classificacao, insur$smoker))
}

fnACC(insurance)




fnTreinamento = function(insur){
  reglog = glm(factor(smoker)~ ., data = insur, family = 'binomial')
  return(reglog)
}
fnTeste = function(insur,resultTreino){
  probpred = predict(resultTreino, data = insur, type = 'response')
  classificacao = ifelse(probpred[1:nrow(insur)] > 0.5, 1, 0)
  print(length(classificacao))
  print(length(insur$smoker))
  return(table(classificacao, insur$smoker))
}
fnAcc2 = function(insur) {
  return(fnTeste(insur, fnTreinamento(insur)))
}

fnAcc2(insurance)

insur70 <- insurance[sample(1:nrow(insurance), nrow(insurance)*0.70, replace=FALSE),]
insur30 <- insurance[sample(1:nrow(insurance), nrow(insurance)*0.30, replace=FALSE),]

resultListaTreinamento = fnTreinamento(insur70)
fnTeste(insur30, resultListaTreinamento) 
