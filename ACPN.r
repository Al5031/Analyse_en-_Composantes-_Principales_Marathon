#TP sur l'ACPN
#1). Importation des tables
#la table contient les identifiants des individus (nom des athlètes) dans la première colonne : row.names=1
#Les noms des variables sont sur la première ligne : header=T
#séparateur ; : sep=";"
decathlon<-read.table("//datastore.epucfe.eu/stleger/Bureau_Windows/Mes_documents/Cours/Analyse_de_données/ACP/decathlon/decathlon.csv",header=T,sep=";",row.names=1)
head(decathlon)

#2). population : décathloniens
#variables : r100m, ---, r1500m : résultats des 10 épreuves du decathlon : quantitatives
#Points : quantitative (calculé à partir des 10 résultats=>résumé des 10 variables =>à ne pa utiliser ou en supplémentaire
#Compétition : qualitative à 2 modalités

#3). pour décrire les données, on va réaliser une ACP ou une ACPN, puisque la majorité des variables sont quantitatives
#variables à utiliser : les 10 résultats du décathlon
#on ne peut pas utiliser Compétition qui est qualitative et Points qui contient une information redondante aux autres

#On va maintenant mettre en place les différentes étapes de l'ACPN
#4). Etape 1 : ACP ou ACPN?
#2 raisons pour choisir entre les deux analyses :
#Les données dépendent d'unités de mesure. C'est le cas ici puisque les courses sont mesurées en sec ou en minutes et les lancés en m
#Les variables présentent des écarts-types différents.
#calcul des écarts-types :
apply(decathlon[,1:10],2,FUN=sd)
#écart-type de la hauteur : 0.09 et du 1500m : 11.67.
#Les données présentent des unités de mesure. De plus les écarts-types sont différents => on va réaliser une ACPN

#5). Etape 2 : Etude de la matrice des corrélations
cor(decathlon[,1:10])
#Pas de corrélation au dessus de 0.7. Très peu de corrélation au dessus de 0.5. 
#Corrélation la plus forte : 0.6157 entre disque et poids
#Nous allons avoir du mal à baisser de façon significative le nombre d'axes à analyser.

#6). Etape 3 : ACPN
?princomp
#choses importantes à noter : 
#- Par défaut, princomp réalise une ACP. 
#Si l'on souhaite faire une ACPN, il faut ajouter l'option COR=T

#- Permet de mettre des individus en supplémentaires en utilisant l'option
# subsets=dataframe

#- princomp renvoie une liste qui contient :
# des écarts-type des composantes principales : sqrt(valeur propre):sdev
# les vecteurs propres : loadings
# la moyenne des variables : center
# les écarts-types des variables : scale
# le nbre d'observations : n.obs
# Les projections des individus sur les nouveaux axes=composantes principales : scores

decathlon_acpn=princomp(decathlon[,1:10],cor=T)
summary(decathlon_acpn)
#sqrt(valeur propre =standard déviation, % d'information de l'axe=Proportion of variance, % d'information cumulée : Cumulative Proportion)
plot(decathlon_acpn)
#diagramme des valeurs propres
biplot(decathlon_acpn)

#diagramme qui supperpose le cercle des corrélations et la projection des individus =>illisible et inutile
#Etape 3 : choix du nombre d'axes
plot(decathlon_acpn)
#ou graph refait à la main afin d'ajouter la droite y=1
valeurpropre=decathlon_acpn$sdev**2
plot(1:10,valeurpropre,xlab="numéro des axes",ylab="valeur propre",type="b")
abline(h=1)
#affichage du pourcentage d'information cumulé
print(cumsum(valeurpropre/10*100))
#règle de Kaiser : valeur prore>1 : 4 axes
#règle du coude : 1, 2, 3 ou 4 axes possibles
#pourcentage d'information cumulé : 2 axes 50% pas assez, 3 axes 64%, résultat encore faible, 4 axes 74.7% : OK
#On conserve 4 axes

#Etape 4 : signification des axes : nécessite le coefficient de corrélation linéaie. Pas fourni par R, il faut le recalculer
#cor(axei,varj)=sqrt(valeurpropre_i)*vecteurpropre_ij
coefcor=matrix(rep(0,40),ncol=4,dimnames=list(colnames(decathlon)[1:10],1:4))
for (i in 1:4)#i représente le ième axe
{
  coefcor[,i]=decathlon_acpn$sdev[i]*decathlon_acpn$loadings[,i]
}
#affichage des valeurs sur les différents axes
nbreaxes=readline("Combien d'axes voulez-vous afficher ? ")
for (i in 1:nbreaxes)
{
  cat("coef sur l'axe ",i,"\n")
  for (j in 1:10)
    cat(colnames(decathlon[j]),coefcor[j,i],"\n")
  rep=readline("taper sur entrer pour continuer")
}
#ou
print(coefcor)
#axe 1 : slongueur-------------->r100m, r110haie
#axe 2 : -------------------->poids,disque,(400m)
#axe 3: r1500m--------------->
#axe 4 : javelot ------------>

#réalisation du cercle des corrélations sur l'axe 1-2
val=seq(-pi,pi,by=0.01)
x=cos(val)
y=sin(val)

plot(x,y,type="l")
abline(v=0, h=0)
points(coefcor[,1],coefcor[,2],pch="+")
text(coefcor[,1],coefcor[,2],labels=colnames(decathlon))
title("Cercle des corrélations des axes 1 et 2")

#réalisation des cercles des corrélations sur l'axe 1-2
plot(x,y,type="l")
abline(v=0, h=0)
points(coefcor[,1],coefcor[,3],pch="+")
text(coefcor[,1],coefcor[,3],labels=colnames(decathlon))
title("Cercle des corrélations des axes 1 et 3")

#réalisation des cercles des corrélations sur l'axe 1-4
plot(x,y,type="l")
abline(v=0, h=0)
points(coefcor[,1],coefcor[,4],pch="+")
text(coefcor[,1],coefcor[,4],labels=colnames(decathlon))
title("Cercle des corrélations des axes 1 et 4")

#réalisation des cercles des corrélations sur l'axe 2-3
plot(x,y,type="l")
abline(v=0, h=0)
points(coefcor[,2],coefcor[,3],pch="+")
text(coefcor[,2],coefcor[,3],labels=colnames(decathlon))
title("Cercle des corrélations des axes 2 et 3")

#réalisation des cercles des corrélations sur l'axe 2-4
plot(x,y,type="l")
abline(v=0, h=0)
points(coefcor[,2],coefcor[,4],pch="+")
text(coefcor[,2],coefcor[,4],labels=colnames(decathlon))
title("Cercle des corrélations des axes 2 et 4")

#réalisation des cercles des corrélations sur l'axe 3-4
plot(x,y,type="l")
abline(v=0, h=0)
points(coefcor[,3],coefcor[,4],pch="+")
text(coefcor[,3],coefcor[,4],labels=colnames(decathlon))
title("Cercle des corrélations des axes 3 et 4")



#Etape 5 : etude des projections des individus
#calcul des contributions :
contrib=matrix(nrow=dim(decathlon)[1],ncol=4)
for (i in 1:4)#i représente le ième axe
  contrib[,i]=1/dim(decathlon)[1]*decathlon_acpn$scores[,i]**2*100/decathlon_acpn$sdev[i]**2
#points ayant une forte contribution :
#sur l'axe 1 :
print(rownames(decathlon)[abs(decathlon_acpn$scores[,1])>decathlon_acpn$sdev[1]])
print(contrib[abs(decathlon_acpn$scores[,1])>decathlon_acpn$sdev[1],1])
#pas de points avec une contribution trop élevée. Les valeurs le splus élevées sont pour Bourguignon, Sebrle, Clay et Karpov, 
#sur l'axe 2 :
print(rownames(decathlon)[abs(decathlon_acpn$scores[,2])>decathlon_acpn$sdev[2]])
print(contrib[abs(decathlon_acpn$scores[,2])>decathlon_acpn$sdev[2],2])
#une valeur plus importante sans qu'elle soit problématique pour Casarsa
#sur l'axe 3 :
print(rownames(decathlon)[abs(decathlon_acpn$scores[,3])>decathlon_acpn$sdev[3]])
print(contrib[abs(decathlon_acpn$scores[,3])>decathlon_acpn$sdev[3],3])
#pas de valeurs trop élevées ici, valeur la plus forte pour Korkizoglou

#sur l'axe 4 :
print(rownames(decathlon)[abs(decathlon_acpn$scores[,4])>decathlon_acpn$sdev[4]])
print(contrib[abs(decathlon_acpn$scores[,4])>decathlon_acpn$sdev[4],4])
#pas de valeurs trop élevées, les plus fortes pour BERNARD et NOOL

#calcul des cos²
cosinus=matrix(nrow=dim(decathlon)[1],ncol=4)
norme=apply(decathlon_acpn$scores[,1:10]**2,1,FUN=sum)
for (i in 1:dim(decathlon)[1])#i reprèsente les athlètes
{
  cosinus[i,]=decathlon_acpn$scores[i,1:4]**2/norme[i]
}
#points mal projetés sur l'axe 1 :
rownames(decathlon)[cosinus[,1]<0.2]
#points mal projetés sur l'axe 2 :
rownames(decathlon)[cosinus[,2]<0.2]
#points mal projetés sur l'axe 3 :
rownames(decathlon)[cosinus[,3]<0.2]
#points mal projetés sur l'axe 4 :
rownames(decathlon)[cosinus[,4]>0.2]

#Projection des individus sur les axes 1 et 2
plot(decathlon_acpn$scores[,1],decathlon_acpn$scores[,2])
abline(h=0,v=0)
text(decathlon_acpn$scores[,1],decathlon_acpn$scores[,2],labels=rownames(decathlon))
#on peut voir un groupe de 3 athlètes (Sebrle, Clay, Karpob) qui se projettent à l'extrême gauche de l'axe 1. ce sont donc des athlètes qui courent vite et sotent loin
# A l'inverse, bourguigon est à l'extrême gauche, il est donc lent sur les courtes distances et non performant sur le saut en longueur.
#carsarsa est sur le haut de l'axe 2, alors que Drewz est sur le bas. Le premier est caractérisé par des performances au lancé alors que le second semble défaillant dans ces épreuves.
#Pour les autres athlètes, il est difficile de réaliser des groupes. Avant de faire les graphiques sur les autres axes, nous allons réaliser une CHA qui va nous aider à réaliser des groupes homogènes et permettra une interprétation plus complète et plus simple de l'ACPN

#réaliastion d'une CHA suite à l'ACPN
#Calcul des distances entre les individus
decathlon_dist=dist(decathlon_acpn$scores[,1:4],method="euclidean")
#réalisation de la CHA par la méthode de ward
decathlon_cha=hclust(decathlon_dist,method="ward.D")
#affichage du dendrogramme
plot(decathlon_cha)
#4 groupes semblent un bon compromis
#séparation des individus en différents groupes
decathlon_groupe=cutree(decathlon_cha,k=4)

#graphique avec les différents groupes mis en couleur
plot(decathlon_acpn$scores[decathlon_groupe==1,1],decathlon_acpn$scores[decathlon_groupe==1,2],col="red",pch="+",xlim=c(min(decathlon_acpn$scores[,1]),max(decathlon_acpn$scores[,1])),ylim=c(min(decathlon_acpn$scores[,2]),max(decathlon_acpn$scores[,2])))
text(decathlon_acpn$scores[decathlon_groupe==1,1],decathlon_acpn$scores[decathlon_groupe==1,2],col="red",labels=rownames(decathlon)[decathlon_groupe==1])
points(decathlon_acpn$scores[decathlon_groupe==2,1],decathlon_acpn$scores[decathlon_groupe==2,2],col="blue",pch="*")
text(decathlon_acpn$scores[decathlon_groupe==2,1],decathlon_acpn$scores[decathlon_groupe==2,2],col="blue",labels=rownames(decathlon)[decathlon_groupe==2])
points(decathlon_acpn$scores[decathlon_groupe==3,1],decathlon_acpn$scores[decathlon_groupe==3,2],col="green",pch="o")
text(decathlon_acpn$scores[decathlon_groupe==3,1],decathlon_acpn$scores[decathlon_groupe==3,2],col="green",labels=rownames(decathlon)[decathlon_groupe==3])
points(decathlon_acpn$scores[decathlon_groupe==4,1],decathlon_acpn$scores[decathlon_groupe==4,2],col="black",pch="1")
text(decathlon_acpn$scores[decathlon_groupe==4,1],decathlon_acpn$scores[decathlon_groupe==4,2],col="black",labels=rownames(decathlon)[decathlon_groupe==4])

#l'axe 1 permet d'opposé le groupe noir (athlètes courant vite et sautant loin), au groupe vert)
#ces deux axes ne permettent pas de caractériser les deux autres groupes qui sont projetés au cente du plan 1/2
#On regarde sur les autres axes si l'on peut avoir une caractérisation

#graphique avec les différents groupes mis en couleur
plot(decathlon_acpn$scores[decathlon_groupe==1,3],decathlon_acpn$scores[decathlon_groupe==1,4],col="red",pch="+",xlim=c(min(decathlon_acpn$scores[,3]),max(decathlon_acpn$scores[,3])),ylim=c(min(decathlon_acpn$scores[,4]),max(decathlon_acpn$scores[,4])))
text(decathlon_acpn$scores[decathlon_groupe==1,3],decathlon_acpn$scores[decathlon_groupe==1,4],col="red",labels=rownames(decathlon)[decathlon_groupe==1])
points(decathlon_acpn$scores[decathlon_groupe==2,3],decathlon_acpn$scores[decathlon_groupe==2,4],col="blue",pch="*")
text(decathlon_acpn$scores[decathlon_groupe==2,3],decathlon_acpn$scores[decathlon_groupe==2,4],col="blue",labels=rownames(decathlon)[decathlon_groupe==2])
points(decathlon_acpn$scores[decathlon_groupe==3,3],decathlon_acpn$scores[decathlon_groupe==3,4],col="green",pch="o")
text(decathlon_acpn$scores[decathlon_groupe==3,3],decathlon_acpn$scores[decathlon_groupe==3,4],col="green",labels=rownames(decathlon)[decathlon_groupe==3])
points(decathlon_acpn$scores[decathlon_groupe==4,3],decathlon_acpn$scores[decathlon_groupe==4,4],col="black",pch="1")
text(decathlon_acpn$scores[decathlon_groupe==4,3],decathlon_acpn$scores[decathlon_groupe==4,4],col="black",labels=rownames(decathlon)[decathlon_groupe==4])
#opposition des groupes rouge et bleu sur l'axe 3. 
#Le groupe rouge est constitué d'athlètes réalisant le 1500m lentement alors que c'est le contraire pour le groupe bleu