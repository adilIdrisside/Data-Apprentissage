############TP3############
#BELMOKADDEM Adil UVSQ 

#Exercice 1. Modèle de regression linéaire
help("read.table")
tab=read.table('UsCrime.txt', header = TRUE)

#On a 47 observations disponibles

plot(tab)

#a travers le plot on peut remarquer que certaines 
#variables sont liées, W & X on a une droite négative, les variables 
#U1 & U2 sont liées aussi, les variables sont liées aussi. EX1 & EX2
#se ressemblent vu qu'on a presque la même droite. Sinon pour la 
#variable S (sexe) on prend que 2 valeurs masculin ou féminin.

#Calcul de la matrice de corrélation
Y=tab
#A travers la fonction cor on regarde dans les colonnes la relation 
#de la variables avec chacune des autres variables, celle qui sont 
#quasiment nulles veut dire que la relation n'est pas importante,
#les autres qui sont importantes veut dire que la relation est importante

correlation=cor(tab)
#Y=(tab,1)
res=lm('R~.',data=tab)
plot(res)

#1.
#la fonction print ressemble a plot
print(res)
#summary donne l'état de la variable
sum=summary(res)
#attributes
att=attributes(res)

#Le modèle obtenu ici est la somme des coefficients estimés

#2.modèle globale
#On mesure la qualité de la prédiction d'une régression linéaire
#avec R-squared. On trouve le coefficient de détermination avec la 
#fonction attributes(res), on le coefficient multiple et adjusté qui
#sont tropproche de 1, ce qui signifie que le pouvoir prédictif de 
#ce modèle de régression est fort. Donc globalement, ce modèle est
#significatif vu que R-squared est proche de 1.

#3. Les coefficients du modèle
#p-value nous aide a déterminer quel sont les termes qui
#seront gardés dans le modèle regressionnel. En théorie, plus p-value 
#est proche de 0, plus le modèle regressionnel est important,
#plus c'est proche de 1, plus le modèle regressionnel est faible,
#notre modèle regressionnel est fort puis p-value vaut 3.686e-07. 
#On remarque dans la 4ème colonne (les p-values), que les 
#coefficients les plus significatifs sont ceux qui ont des 
#étoiles, du coup, les coefficients: Age, Ed, U2, X sont 
#significatifs et seront gardés dans ce modèle.
#Les coefficient n'ont pas tous le même interêt pour la variable
#cible, vu qu'il ya ceux qui nesont pas important(ceux qui n'ont pas
#d'étoiles).

conf=confint(res,level = 0.05) 
#avec la fonction confint, on a des intervals pour chaque coefficient
#si la plage et petite, alors on es sûre que le coefficient
#appartient a cet interval, si la plage est large alors on es pas
#sûre que le coefficient appartient a cette plage.

#4.Etude des valeurs prédites
pred=predict(res,tab)

#1ère colonne de tab
#On trace une droite pour voir là où il y a des erreurs ou pas.
Y=tab[,1]
plot(Y,pred,xlim=c(0,200),ylim=c(0,200))
abline(a=0,b=1,col="green")
#J'ai précisé la limite de l'échelle pour afficher la droite,
#je met b=1 et a=0 car l'équation de la droite est bx+a
#les points qui sont au dessus de la droite signifient qu'il n y a pas
#d'erreurs, les points au dessous signifient qu'il n y a des erreurs
#les points qui sont sur la droite,signifient que l'erreur est nulle

#5.Etude des résidus
taux_err=sqrt(mean((pred-Y))^2)
#Affichage des résidus en fonction de Y
plot(Y,res$residuals)
#Distribution empirique des résidus
qqnrm=qqnorm(Y)
#Dans le graphe de qqnorm, on voit que certains points ont des images
#très proche surtout dans l'interval [-1,1]
qline=qqline(Y,FALSE)
#la fonction qqline rajoute une ligne a la courbe produite par qqnorm. 
#la plupart des points se trouvent sur la ligne droite et la suivent depuis le départ, il
#y a quelques déviations, mais vu que la plupart des points se trouvent sur la ligne, on
#dire que les données sont distribuées normalement, et que le modèle est conforme aux
#attentes.
#Test de normalité des résidus
shapiro.test(Y)
shapiro.test(pred)
#le test de normalité de Y montre qu'il est plus significatif par 
#rapport aux predictions (p-value de Y est 0.001882,
#p-value de pred est 0.02388)

#6.Performances du modèle sur de nouvelles données:

#on crée la liste indTest pour selectionner après dans tab
indTest=seq(1,47,3)
tabTest=tab[indTest,]
#View(tabTest)
test=tabTest[,1]
tabTrain=tab[-indTest,]
#View(tabTrain)

resTrain=lm('R~.',data = tabTrain)
new_prediction = predict(resTrain,tabTest)
erreur_quad_Train=sqrt(mean((test - new_prediction))^2)

print(erreur_quad_Train)
#L'erreur quadratique est trop élevée
#7. Analyse graphique
x11();par(mfrow=c(2,2));plot(res)
#  Influence(Leverage) : une mesure qui montre a combien, une variable indépendante dévie de sa
#  moyenne. 
#  distance de Cook: Une mesure combinant l'inforamtion du Résidue et de l'influence 
#  d'une observation donnée.

#  Les 4 graphes montrent que les observations 8, 46, 29, 19 & 11, plus particulièrement
# 11 et 19 qui sont présents sur les 4 graphes, représentent un problème pour 
# notre modèle, ces observations ont soit un leverage très élevés ou bien 
# un résidue très large.
