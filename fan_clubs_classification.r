
install.packages('tinytex')
library(tinytex)



library(readr)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(RColorBrewer)
library(questionr)

dataset<-read_delim("base.csv", ";", escape_double = FALSE, 
                    trim_ws = TRUE)

team<-dataset[dataset$LFC==5 & dataset$MUFC!=5 & dataset$AFC!=5 &
                dataset$CFC!=5 & dataset$MCFC!=5 & dataset$FCB!=5 &
                dataset$RM!=5 & dataset$JFC!=5 & dataset$ACM!=5 & 
                dataset$IM!=5 &dataset$BM!=5 & dataset$BVB!=5 &
                dataset$PSG!=5,
              c("sexe","age",	"situation professionnelle","Anglais","Espagnol",
                "Allemand","Italien","fan_since","LFC","MUFC","AFC","CFC","MCFC","FCB","RM"	,"JFC","ACM","IM","BM","BVB","PSG",	
                "equipe-tun","nation","Joueurs","attachement",
                "enfance","joueur_prefere","gagne","histoire","plus_talentueuse","principe","spectacle",
                "satisfaction resultat",
                "MoneyBall","Gegenpressing","Tiki_Taka","Catinaccio","Grinta","Pragmatisme",	
                "transmis_passion")]


reasons<-team[c("enfance","joueur_prefere","gagne","histoire",
                "plus_talentueuse","principe","spectacle")]



###3 ACP sur les raisons des supporter une Ã©quipe:


##3-a Observaton des corrÃ©lations entre les variables
reasons_matrix<-as.matrix(team[c("enfance","joueur_prefere","gagne",
                                 "histoire","plus_talentueuse","principe","spectacle")])

M2<-round(cor(reasons_matrix),3)
M2
corrplot(M2, type="upper", order="hclust", 
         col=brewer.pal(n=8, name="RdBu"))

##3-b Choix de nombre d'axes

reasons_pca<-PCA(reasons,graph = F)

head(round(reasons_pca$eig,3))
fviz_screeplot(reasons_pca , ncp=10)

##3-c Carte des variables
round(reasons_pca$var$coord,3)
round(reasons_pca$var$cos2,2)

fviz_pca_var(reasons_pca)
fviz_pca_var(reasons_pca, col.var="cos2") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.6) + 
  theme_minimal()

##3-d Carte des individus
fviz_pca_ind(reasons_pca, pointsize = 4 , col.ind.sup = 'blue')
fviz_pca_ind(reasons_pca,geom = "point",col.ind.sup = 'blue')
fviz_pca_ind(reasons_pca,geom = "point",pointsize = 4 , col.ind="cos2")+
  scale_color_gradient2(low="blue", mid="white", 
                        high="red", midpoint=0.5)

### Quelque chose de spÃ©cial -carte des individus 3d-
library(pca3d)

pca <- prcomp(reasons, scale.=TRUE)
pca3d(pca)

###4 AFC sur le lien entre le joueur prÃ©fÃ¨rÃ© et l'equipe nationale prÃ©fÃ©rÃ©e:

##4-a PrÃ©paration des donnÃ©es

tab3<-table(dataset$nation,dataset$Joueurs)

lprop(tab3)
cprop(tab3)
##4-b Test du chi-2

test<-chisq.test(tab3)
test

##4-c ExÃ©cussion de l'AFC 
afc_gen<-CA(tab3,graph=F)
summary(afc_gen)

##4-d Choix du nombre d'axes Ã  retenir
eig_val_gen <- afc_gen$eig
round(afc_gen$eig,3)

barplot(eig_val_gen[, 2], 
        names.arg = 1:nrow(eig_val_gen), 
        main = "Variances Explained by Dimensions (%)",
        xlab = "Principal Dimensions",
        ylab = "Percentage of variances",
        col ="steelblue")
lines(x = 1:nrow(eig_val_gen), eig_val_gen[, 2], 
      type = "b", pch = 19, col = "red")
fviz_screeplot (afc_gen, addlabels = TRUE, ylim = c(0, 50))

##4-e ReprÃ©sentation et interprÃ©tation de la carte des profils-colonnes 

round(cbind(afc_gen$col$coord[,1:2],afc_gen$col$contrib[,1:2], afc_gen$col$cos2[,1:2]),2)

plot(afc_gen, invisible="row")

fviz_ca_col(afc_gen, col.col = "contrib")+
  scale_color_gradient2(low = "white", mid = "blue",
                        high = "red", midpoint = 25) +
  theme_minimal()
##4-f ReprÃ©sentation et interprÃ©tation de la carte des profils-lignes
round(cbind(afc_gen$row$coord[,1:2],afc_gen$row$contrib[,1:2], afc_gen$row$cos2[,1:2]),2)
plot(afc_gen, invisible="col")

fviz_ca_row(afc_gen, col.row = "contrib")+
  scale_color_gradient2(low = "white", mid = "blue",
                        high = "red", midpoint = 25) +
  theme_minimal()

##4-g  Carte de reprÃ©sentation simultannÃ©e
fviz_ca_biplot(afc_gen,col.col = "contrib" ,col.row = "contrib") + scale_color_gradient2(low = "white", mid = "blue",
                                              high = "blue", midpoint = 15) +
  theme_minimal()


fviz_ca_biplot(
  afc_gen,
  axes = c(1, 2),
  geom = c("point", "text"),
  geom.row = geom,
  geom.col = geom,
  label = "all",
  invisible = "none",
  arrows = c(FALSE, FALSE),
  repel = FALSE,
  title = "CA - Biplot")
  

install.packages("FactoInvestigate")
library(FactoInvestigate)

investiga


