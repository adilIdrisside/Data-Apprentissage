######## BELMOKADDEM Adil ########
#. TP5 
###########################################################################################

#Analyse préliminaire.
tab=read.table('spam.txt', header=T, sep=";")
#View(tab)
dim(tab)   # Pour afficher les dimensions de tab i.e nombre de variables et d'obervations

# Nombre d'observations : 4601
# Nombre de variables   : 58

# le fichier "spaminfo.txt" indique que les variables (colonnes) représentent des e-mails.

# A l'aide du fichier "spaminfo.txt" on comprend que la dernière colonne determine si le 
# mail reçu est un spam ou non (1 si oui 0 sinon). 
# 48 variables (colonnes) qui représentent des valeurs réelles (flottantes) qui montrent  
# le pourcentage d'apparition d'une chaîne de caractères donnée dans l'email.
# 6 variables (colonnes) qui représentent des valeurs réelles (flottantes) qui montrent le 
# pourcentage d'apparition d'une lettre (char) donnée dans l'email.
# 1 variable (colonne) qui représente des valeurs réelles (flottantes) qui montrent la 
# longueur moyenne d'une séquence de lettres majuscules dans chaque e-mail.
# 1 variable (colonne) qui représente des valeurs entières qui sont les tailles des plus 
# longues séquences de lettres majuscules dans chaque e-mail.
# 1 variable (colonne) qui représente des valeurs entières du nombre total des lettres   
# majuscules dans chaque e-mail.

names(tab) # Pour afficher les noms des variables 
# 
idx_train=as.matrix(read.table('indtrain.txt', header = T))
tabindapp = read.table('indtrain.txt', header = T)
idx_test=as.matrix(tab[-idx_train,]);
nrow(tabindapp)  # Nombre de lignes de indtrain.
ncol(tabindapp)  # Une seule colonne.
dim(tabindapp)   
names(tabindapp) 
###########################################################################################

#Arbres de Classification

#1. Construction et visualisation de l'arbre

library(rpart)      # on utilise rpart pour construire l'arbre.
library(rpart.plot) # on s'en servira pour une visualisation plus graphique de l'arbre.
ad.spam.cnt <- rpart.control (minsplit = 1);
# Construction de l'arbre de décision
tree <- rpart (spam ~ .,tab[idx_train,], control = ad.spam.cnt, method = "class")

# Visualisation de l'arbre

# La variable la plus influente est la colonne A.53. 
# D'après l'arbre, on voit 5 variables qui y interviennent
plot(tree,branch=.2, uniform=T, compress=T, margin=.1)
text(tree)
#. On utilise rpart.plot pour affichage d'arbre plus clair.
rpart.plot(tree)

#2. Calcul des erreurs
#Prédictions
predTrain=predict(tree,tab[idx_train,], type="class")
plot(predTrain)
confTrain <- table(predTrain,tab[idx_train,]$spam)


predTest=predict(tree,tab[-idx_train,], type="class")
plot(val_predites_test)
confTest <- table(val_predites_test,tab[-idx_train,]$spam)

# Erruer sur la base de test.
errEmailTest=97/659
errSpamTest=30/365
cat("\nErreur sur la base de test:", errTest=(97+30)/1151)

# Erreur sur la base d'apprentissage:
errEmailTrain=244/2001
errSpamTrain=98/1107
cat("\nErreur sur la base d'apprentissage:", errTrain=(244+98)/3450)
###########################################################################################

# Agrégation de modèles -Bagging-

#3. Génération de plusieurs arbres 
library(randomForest)
library(ipred)
help(bagging)
names(tab)
class(tab$spam)
tab$spam <- factor(tab$spam)
#tabTrain$spam<- as.factor(tab[idx_train,]$spam)
#Bagging
tab.bagging <- bagging(spam ~., data=tab, mfinal=15, control=rpart.control(maxdepth=5, minsplit=15))
names(tab.bagging)
print(tab.bagging)
length(tab.bagging)
# Avec la fonction print on voit qu'il y a 25 replications, donc 25 arbres sont générés.

#4. Prédiction
library(e1071)
library(caret)
# On commence par les matrices de confusion.
tab$pred.class <- predict(tab.bagging,tab, type="class")
plot(tab$pred.class)
#train_conf_matrix <- table(predmod_train,tab$spam[idx_train])
bagging_train_conf_matrix=confusionMatrix(data=tab[idx_train,]$pred.class,reference=tab[idx_train,]$spam)
#test_conf_matrix <- table(predmod_test,tab$spam[-idx_train])
bagging_test_conf_matrix=confusionMatrix(data=tab[-idx_train,]$pred.class,reference=tab[-idx_train,]$spam)

# Calcule des erreurs.
# Erreur sur la base d'apprentissage.
errMailTrainBagging=84/663
errSpamTrainBagging=26/378
cat("\nErreur sur la base de test:", errTrainBagging=(26+84)/1151)
# Erreur sur la base de test.
errMailTestBagging=203/2007
errSpamTestBagging=92/1148
cat("\nErreur sur la base de test:", errTestBagging=(203+92)/3450)

# On en conclus que l'erruer a base de test et d'apprentissage sont plus petites par 
# rapport au au tests d'avant.

###########################################################################################

# Agrégation de modèle -Random Forest-

#5. Génération de plusieurs arbres de classification.
randFrst <- randomForest(spam~ ., data=tab)

length(randFrst)
print(randFrst)

#. 500 arbres 

#6. Performance des modèles agrégés
predRandFrstTrain <- predict(randFrst,tab[idx_train,], type="class")
plot(predRandFrstTrain)
trainConf <- table(predRandFrstTrain,tab$spam[idx_train])
View(trainConf)

predRandFrstTest <- predict(randFrst,tab[-idx_train,], type="class")
plot(predRandFrstTest)
testConf <- table(predRandFrstTest,tab$spam[-idx_train])
View(testConf)

#7. 
require(randomForest)
varimplot <- varImpPlot(randFrst,type=2)
plot(varimplot)
#########################################################################################

#. Comparaison des méthodes de classification.

#8.  
samp = sample(2:4601,3450)
appr = tab[samp,1:58]      
# Base d'apprentissage contenant 75 % des observations
test = tab[-samp,1:58]     
# Base test contenant les 25 % des observations restantes
cat("--FIN--")