######## BELMOKADDEM Adil ########
#. TP6 (Final) : Classification non supervisée et réduction de dimension.
#############################################################################################

#. Exercice 1 : Application a la segmentation d'images.

#1. 
irm=as.matrix(read.table("irm_thorax.txt",header=F,sep=";"))

#2.

# Affichage de l'image.
img = image(irm)
# Stockage des valeurs dans un vecteur.
X = as.vector(irm)
# Histogramme des couleurs.
colors = c("red", "yellow", "green", "violet", "orange", "blue", "pink", "cyan")
hist(X,col=colors)
# On observe que 

#3.
resk = kmeans(X,center=2)

#4.
print(resk$centers)
#barycentre des clusters
print(resk$cluster)
# pour choisir les clusters pour chaque pixel
print(resk$withinss)
# la dispersion intra-cluster des differnts cluster
print(resk$betweenss)
# la dispersion entre les differernts clusters
help(coordinate)
#5.
# Pour k=2
r_cluster=resk$cluster;
m=matrix(r_cluster, nrow = nrow(irm), ncol = ncol(irm))
image(m,col=gray(1/255*(1:255)))
# Pour k=3
resk3=kmeans(X,3)
print(resk3$cluster);
r_cluster2=resk$cluster;
z=matrix(r_cluster2, nrow = nrow(irm), ncol = ncol(irm))
image(z,col=gray(1/255*(1:255)))

# Pour k=5
resk5=kmeans(X,5)
print(resk5$cluster);
r_cluster5=resk5$cluster;
z=matrix(r_cluster5, nrow = nrow(irm), ncol = ncol(irm))
image(z,col=gray(1/255*(1:255)))
# Plus le coefficient k augumente plus la segmentation devient pertinente et bien précise.

#############################################################################################

#. Exercice 2 : Réduction de dimension.

#. Analyes Préliminaire

#1. On visualisant le fichier "cardata.txt" avec un éditeur de texte, on remarque que ce 
# ce fichier décrit les caractéristiques de chaque voiture.

#2.
X = read.table("cardata.txt", sep=";", header = 1)
#. 24 Obserations, 7 Variables

#3.
plot(X)

mat = X[,-1]
corr=cor(mat)
View(corr)
# On en conculs que les varibles sont corrélées.
#############################################################################################

#. Réduction de dimension par ACP.

#4.
dimension = dim(mat)
rows = nrow(mat)
ncol = ncol(mat)
print(rows)
res = prcomp(mat, scale=TRUE)

attributes(res)
print(res$sdev)
#
print(res$rotation)
#
print(res$center)
#
print(res$scale)
#
print(res$x)
#

#5. 
library(ade4)
matXY=dudi.pca(mat,row.w = rep(1, nrow(mat))/nrow(mat), col.w = rep(1, ncol(mat)),center = TRUE, scale = TRUE,scannf = TRUE, nf = 2)
s.corcircle(mat)
colmean=colMeans(mat)
p=cumsum(res$sdev)/sum(res$sdev)

for (i in 1:6){
  if(p[i]>=0.95){ 
    print(i);
    break;
  }
}
# Pour les 95% de la variance on garde les 5 premières composantes 
for (j in 1:6){
  if(p[j]>=0.98){ 
    print(j);
    break;
  }
}
#pour les 98% de la variance on garde les 5 premières composantes

#6.
biplot(res$x[,1:2], res$x[,2:3]) 
x11()
#############################################################################################

#. Classification non supervisée

#7. 
Z = as.matrix(read.table("cardata.txt",sep=";", header=1))
Y = Z[,-1]

cardata_resk1 = kmeans(Y,center=1)
cardata_resk2 = kmeans(Y,center=2)
cardata_resk3 = kmeans(Y,center=3)
cardata_resk4 = kmeans(Y,center=4)

#8.
plot(res)
#############################################################################################

#1. 
library(nFactors)
ev <- res$sdev^2
valpr = as.vector(ev/sum(ev))
# L'éboulis des valeurs propres

plot(valpr,type="lines")

cat("--FIN--")