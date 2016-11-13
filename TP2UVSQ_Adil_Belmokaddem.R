######## BELMOKADDEM Adil ########
#. TP2 : Modèle de mélange et algorithme EM.
#############################################################################################

# Exercice1 - Modèle statistique de mélange

#1. Réalisation

realisation= function(p,n)
{
  #n : Nombre de réalisations
  #p:  Vecteur de probas, contient k probas
  cums=cumsum(p)
  class=c(1:n)
  for (i in 1:n) 
  {
    X=(runif(1, min =0 , max =1 ));#generate n uniform random numbers lie in the interval (min, max)
    class[i]=sum(((X-cums) > 0))+1
  }
  return(class)
  ##pur tester: realisation(  c(0.1, 0.5, 0.2, 0.2 ), 50)
}

#2. Loi de mélange de gaussiennes
library(MASS)
melange_gaussienne=function(mu,Sigma, p, n)
{
  d = nrow(mu)
  print(d)
  mv=matrix(0,n,d)
  class=realisation(p,n)
  for (i in 1:n) 
  {
    mv[i,]=mvrnorm(1, mu[ ,class[i]], Sigma[, ,class[i]], tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
  }
  MAT=matrix(0,n,d+1)
  MAT[ ,1:d]=mv
  MAT[ ,d+1]=class
  
  return(MAT)
}

# Appel des deux fonctions implémentées dans les questions 1 & 2
p=c(0.1, 0.5, 0.9)
k=length(p)
d=2
mu=matrix(0,nrow=d,ncol=k)
for(i in 1:k)
{
  mu[,i]=runif(d, min =i , max =i+1 )
}
Sigma=array(0,c(d,d,k))
for(i in 1:k)
{
  #Sigma[,,i]=runif(d, min =0 , max =1 )
  for(j in 1:d)
  {
    Sigma[j,j,i]=0.1
  }
}
MAT=melange_gaussienne(mu,Sigma, p, 2000)
mv=MAT[,1:d]

class=MAT[ ,d+1]

#############################################################################################

# Exercice2 - Modèles de mélange dans un cadre supervisé

#. Analyse discriminante linéaire prédictive

#1. Chargement de la librairie MASS
library("MASS")

#2.
help(lda)

#. Analyse discrimiante linéaire
model=lda(Species~.,data=iris)

#3. Affichage
print(model)
#. ** Prior probabilities of groups - proportion de l'espèce dans chaque groupe, on a les
# les mêmse proportions vu qu'on a pas spécifier une donnée pour la fonction lda.
#. ** Group means - proportion des données dans chaque groupe avant le début de l'analyse
# discriminante linéaire "lda".
#. ** Coefficients of linear discriminants - On s'en servira pour désigner la ligne entre
# chaque groupe et les séparer.

#4. Plot
plot(model)
#. le plot donne un graphe des trois espèces, setosa, versicolor et virginica, LD1 en
# fonction de LD2. On a deux groupes, setosa dans le groupe de la droite, et versicolor 
# et virginica dans celui de la gauche.

#5. partitionnement aléatoire

#. On fait le partitionnement des 80% des observations pour la base d'apprentissage.
part1 = sample(1:150, .8*150) 
print(part1)

#6. Prédictions
pred_apprentissage1 = lda(Species~.,data=iris[part1,])
predApp1 = predict(pred_apprentissage1,iris[part1,])

pred_reste1 = lda(Species~., data=iris[-part1,])
predRes1 = predict(pred_reste1,iris[-part1,])

#7. Erreur sur la base d'apprentissage
confApp1 = table(iris[-part1, 5], predRes1$class)
erreurApp1 = (5+14)/30
cat("\n\n Erreur a base d'apprentissage :",erreurApp1)

#8. Erreur sur la base de test
confTest1 = table(iris[part1,5], predApp1$class)
erreurTest1 = (43+1)/120
cat("\n\n Erreur a base de test :",erreurTest1)

#. L'erreur sur la base de test est plus petite que l'erreur à base d'apprentissage.
#. l'erreur à base d'apprentissage vaut généralement le double de l'erreur à base de test.

#9. Répartition aléatoire des deux bases et reétude 

#. Si on réexécute les instructions précédentes, depuis la question 5 jusqu'à la question
# 8 les valeurs changent pour chaque nouvelle éxécution.

#. Ici on fait une base d'apprentissage qui contient 43% des données, et une base de test
# qui contient les 57% restantes.

part2 = sample(1:150, .43*150) 
print(part2)

pred_apprentissage2 = lda(Species~., data=iris[part2,])
predApp2 = predict(pred_apprentissage2, iris[part2,])

pred_reste2 = lda(Species~., data=iris[-part2,])
predRes2 = predict(pred_reste2,iris[-part2,])

confApp2 = table(iris[-part2, 5], predRes2$class)
erreurApp2 = (28+28)/30
cat("\n\n Erreur a base d'apprentissage :",erreurApp2)

confTest2 = table(iris[part2,5], predApp2$class)
erreurTest2 = (22+23)/64
cat("\n\n Erreur a base de test :",erreurTest2)

#. En faisant un partionnement aléatoire nouveau, et en reéxécutant les instructions  
# précédentes, on aura pas exactement les même valeurs pour les erreurs, mais l'erreur à 
# base d'apprentissage vaudra toujours (presque) le double de la valeur de l'erreur 
# à base de test.

cat("\n\n -- FIN -- ")