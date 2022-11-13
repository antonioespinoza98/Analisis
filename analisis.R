
# Leer la base de datos

base = read.csv("basededatos.csv")

str(base)
# Cambiar los factores

base$sustancia = factor(base$sustancia)
base$sujeto = factor(base$sujeto)

#continuo
base$tiempo1 = as.numeric(base$tiempo)
base$glucosa = as.numeric(base$glucosa)
# factor
base$tiempo = factor(base$tiempo)

# PRUEBA

base$sustancia1 = car::recode(base$sustancia, "'control' = 1; 'fibra' = 2; 'proteina' = 3; 'chocolate' = 4; 'cereal' = 5")

base$sustancia1
str(base)
###

table(base$sujeto)

# gráfico
library(lattice)

xyplot(glucosa~tiempo,group=sujeto,ylab="Glucosa",type="r",data = base)

beta0 = NULL
beta1 = NULL

ind=as.numeric(names(table(base$sujeto))) #esto es para hacerlos consecutivos

# options(contrasts=c("contr.sum","contr.poly"))
# options(contrasts=c("contr.treatment","contr.poly"))


for(i in 1:44) {
  mod=lm(glucosa ~ sustancia1, base[base$sujeto == ind[i],])
  beta0[i]=mod$coef[1]
  beta1[i]=mod$coef[2]
}



