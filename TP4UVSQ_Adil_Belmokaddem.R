######BELMOKADDEM Adil######
#TP4

#Analyse préliminaire
tab=read.table('usa_indicators.txt', header = TRUE, sep=";")
View(tab)
#Le nombre d'observations est plus petit que le nombre d'indicateurs, donc le modèle de est
#régression est compliqué.

#La variable EN.ATM.CO2E.KT représente contient la quantité d'émission de CO2 

Y=tab[,22]
years=tab[,1]
#View(years)
plot(years,Y,type='b')


#Le problème ici est que le modèle de régression peut être compliqué
scale(tab,center = FALSE)

#Régresion Ridge
#1.

library(MASS)
help(MASS)
help("lm.ridge")
#2.
resRidge1=lm.ridge(tab$EN.ATM.CO2E.KT~., data=tab[,2:110] , lambda=0)
resRidge2=lm.ridge(tab$EN.ATM.CO2E.KT~., data=tab , lambda=100)

coefRidge = coef(resRidge2)
View(as.double(coefRidge))
dim(as.numeric(coefRidge))

help("sort")
sorted=sort(coefRidge,decreasing=TRUE,coefRidge[1:5])
#indicateurs ne sont pas vraisemblables car si on voit la description des variables dans 
#le fichier usa_indicators_info.txt on verra que ça na aucun rapport avec l'emission du
#CO2.
#corelated= cor(tab)
res=lm("tab$EN.ATM.CO2E.KT~.", data=tab)
#attributes(res)
#summary(res)

#3.
res1=lm.ridge("tab$EN.ATM.CO2E.KT~.", data=tab , lambda=seq(0,100,0.01))
plot(res1$GCV)
#Affichage des différents coefficients en fonction de lambda
plot(res1)
#On constate qu'a un certain moment, les courbes n'évoluent pas et restent constante bien
#que lambda augumente.?????
#Le modèle a retenir est celui d'avant, émissoin du CO2.??????
#Valeur du paramètre de régularisation lambda??????
which.min(coefRidge)

#4.
X = as.matrix(tab)
X = t(X)


Yridge=t(as.matrix(X))%*%Y
dim (t(X))
dim (coefRidge)

View(Yridge)

# Régression Lasslo

#5.
library(lars)
#6.
help(lars)

Y=tab[,22]
resLasso=lars(as.matrix(tab),Y,type="lasso")
plot(resLasso)
plot(resLasso$lambda,type='b')

#7.
coefficient=predict.lars(resLasso,X,type="coefficients",mode="lambda",s=0)

#8.

a=predict.lars(resLasso,as.matrix(X),type="coefficients",mode="lambda",s=0.02)
b=predict.lars(resLasso,as.matrix(X),type="coefficients",mode="lambda",s=0.04)
c=predict.lars(resLasso,as.matrix(X),type="coefficients",mode="lambda",s=0.06)
