#Nom- BELMOKADDEM
#Prénom- Adil
#Exercices 1 et 2 du TP1UVSQ

######Exercice 1######
#Dimensions de la matrice (tableau 2D).
n=50   # Observations
p=2    # Variables

#Génération des éléments aléatoires de la matrice (i.e n*p cellules).
cells=rnorm(n*p)

#Allocation de la matrice X[n][p] en mémoire, en remplissant ses cellules avec les éléments de data générés precedemment.
X=matrix(cells,nrow=n,ncol=p) 

#Calcul de Y estimé 
Y=5 + 2*X[,1] -3*X[,2]+rnorm(n) 

#Ajout d'une colonne de 1 au début de la matrice X
colAdd=matrix(1,nrow=n,ncol=p+1) #cette colonne ne contient que des 1, on incrémente p de 1
colAdd[,-1] = X #ajout de la colonne

#Calcul des coeficients de B
B=(solve(t(colAdd)%*%colAdd))%*%(t(colAdd)%*%Y) 

#Produit matriciel
prodMat=colAdd%*%B

#Calcul de l'erreur
Erreur <- norm(Y - prodMat,"2")/nrow(colAdd)
#Affichage de l'erruer avec la fonction cat
cat("Erreur=",Erreur,"\n")

#Une fonction MCO qui retourne fait le même travail qu'avant
mco=function(X,Y)
{
  n=nrow(X) 
  p=ncol(X) 
  colAdd=matrix(1,nrow=n,ncol=p+1) 
  colAdd[,-1] = X 
  B=(solve(t(colAdd)%*%colAdd))%*%(t(colAdd)%*%Y) 
  prodMat=colAdd%*%B
  Erreur <- norm(Y - prodMat,"2")/nrow(colAdd)
  cat("Inside function MCO---Erreur=",Erreur,"\n")
  
  return(list(B,Erreur))
}
mco(X,Y)

######Exercice 2######
require(stats)
require(graphics)
#Analyse des données de Orange
help(Orange) 
n=nrow(Orange) # nombre d'observations
p=ncol(Orange) #nombre de variables
demo(graphics)
coplot(circumference ~ age | Tree, data = Orange, show.given = FALSE)

fm1 <- nls(circumference ~ SSlogis(age, Asym, xmid, scal),data = Orange, subset = Tree == 5)

plot(circumference ~ age, data = Orange, subset = Tree == 5,xlab = "Tree age",
     ylab = "Tree circumference (mm)", las = 1,
     main = "Orange tree data and fitted model (Tree 5 only)")
age <- seq(0, 1600, length.out = 101)
lines(age, predict(fm1, list(age = age)))
pairs(Orange[1:3], main="Edgar Anderson's Orange Data", pch=21,bg=c("red", "green3", "blue"))

boxplot(circumference~age,data=Orange,main="Boxplot: Error of circumference by age",
                col="red",xlab="age",ylab="circumference")

plot(Orange$age,Orange$circumference,main="Predict Orange", 
    pch=21,bg=c("red","green3","yellow","blue","gray"))


