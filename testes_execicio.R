setwd("~/OneDrive/posgraduacao/Especializacao Ciencias de Dados e Big Data/Programacao R e Python/")
insurance <- read.csv("insurance.csv",header=TRUE)
lml <- lm(charges ~ age + sex + bmi + children + smoker + region, data=insurance)
insur70 <- insurance[sample(1:nrow(insurance), nrow(insurance)*0.70, replace=FALSE),]
insur30 <- insurance[sample(1:nrow(insurance), nrow(insurance)*0.30, replace=FALSE),]
lmlage <- lm(charges ~ age + sex + bmi + children + smoker + region, data=insurance)
lml70age <- lm(charges ~ age + sex + bmi + children + smoker + region, data=insur70)

lmlfn = lmlage

fn = function(x){lmlfn[["coefficients"]][["(Intercept)"]]+lmlfn[["coefficients"]][["age"]]*x}
fne = function(a,c){c - fn(a)}
fnEQM = function(insur){
  errSum = 0.0
  for (row in 1:nrow(insur)) {
    age <- insur[row,'age']
    charges <- insur[row,'charges']
    error <- fne(age,charges)
    errSum = errSum + (error * error)
  }
  sqrt(errSum/nrow(insur))
}
eqmtotal = fnEQM(insurance)
lmlfn = lml70age
eqm30 = fnEQM(insur30)
summary(lml)
paste("EQM Total: ", eqmtotal , " | EQM 30: ", eqm30)
plot(lml)


