
library(fmsb)
library(plotrix)
library(waffle)
library(ggthemes)
library(tidyverse)
library(RColorBrewer)



myPalette <- brewer.pal(9, "Set1") 


Team_names_vec<-c("LFC","MUFC","AFC","CFC","MCFC","FCB","RM"	,"JFC","ACM","IM","BM","BVB","PSG")
players_names<-c("Lionel Messi","Cristiano Ronaldo","Eden Hazard","Neymar","Mohamed Salah","Sadio ManÃ©","Virgil Van Dijk",
                 "Sergio Ramos","Kylian MbappÃ©","Luis Suarez","Paul Pogba","Mourad el Hedhli","Luka Modric",
                 "Robert Lewandowski","autre")

nations_names<-c("France","Allemagne","Espagne","Italie","Angleterre","Pays-bas",
                 "BrÃ©sil","Argentine","autre")

Reasons_vec<-c("L Ã©quipe de l'enfance","L equipe du joueur prÃ©fÃ©rÃ©",
               "L equipe qui gagne","L equipe d'une grande histoire",
               "L equipe la plus talentueuse","L equipe qui prÃ©sente mes principes",
               "L equipe qui offre le meilleur spectacle")              

Notions_vec<-c("MoneyBall"	,"Gegenpressing"	,"Tiki_Taka",	"Catinaccio",
               "Grinta","Pragmatisme")

###########################stat descriptive pure##################################

#Importation des donneÃ©s par equipe
total<-dataset[c("sexe","age",	"situation professionnelle","Anglais","Espagnol",
                              "Allemand","Italien","fan_since","LFC","MUFC","AFC","CFC","MCFC","FCB","RM"	,"JFC","ACM","IM","BM","BVB","PSG",	
                              "equipe-tun","nation","Joueurs","attachement",
                              "enfance","joueur_prefere","gagne","histoire","plus_talentueuse","principe","spectacle",
                              "satisfaction resultat",
                              "MoneyBall","Gegenpressing","Tiki_Taka","Catinaccio","Grinta","Pragmatisme",	
                              "transmis_passion")]

sexe_names<-c("Homme","Femme")
c<-length(total$sexe)
c
sexe_values<-c(100*(sum(total$sexe=="Homme")/c),100*(sum(total$sexe=="Femme")/c))
print(sexe_values)
names(sexe_values)<-sexe_names


waffle(sexe_values,rows = 8,title = "Repartition par sexe",
       colors=c("#004aad", "#d1292b"))

################################################################################
fan_since<-c(total$fan_since)
mean(fan_since)
max(fan_since)
min(fan_since)
median(fan_since)
#orientation vers autres equipes 
LFC_like_vec<-c(mean(total$LFC),mean(total$MUFC),mean(total$AFC),mean(total$CFC),
                mean(total$MCFC),mean(total$FCB),mean(total$RM),mean(total$JFC),
                mean(total$ACM),mean(total$IM),mean(total$BM),mean(total$BVB),
                mean(total$PSG))
typeof(LFC_like_vec)
names(LFC_like_vec)<-Team_names_vec
print(sort(LFC_like_vec,decreasing = T))


########################################################################
players<-(total["Joueurs"])
typeof(Players_vec)
Players_vec<-c(sum(players=="Lionel Messi"),
               sum(players=="Cristiano Ronaldo"),
               sum(players=="Eden Hazard"),
               sum(players=="Neymar"),
               sum(players=="Mohamed Salah"),
               sum(players=="Sadio ManÃ©"),
               sum(players=="Virgil Van Dijk"),
               sum(players=="Sergio Ramos"),
               sum(players=="Kylian MbappÃ©"),
               sum(players=="Luis Suarez"),
               sum(players=="Paul Pogba"),
               sum(players=="Mourad el Hedhli"),
               sum(players=="Luka Modric"),
               sum(players=="Robert Lewandowski")
               ,sum(players=="autre"))
names(Players_vec)<-players_names
print(sort(Players_vec,decreasing = T))

barpl <- barplot(Players_vec , border=F , names.arg=players_names , 
                  las=2 , 
                  col= myPalette, 
                  ylim=c(0,170) , 
                  main="Joueurs prÃ©fÃ©rÃ©s" )

abline(v=c(4.9 , 9.7) , col="grey")

text(barpl, Players_vec , paste("n: ", Players_vec, sep="") ,cex=1) 





#Nation prÃ©fÃ©rÃ©e

Nations<-(total["nation"])
nations_vec<-c(sum(Nations=="France"),
               sum(Nations=="Allemagne"),
               sum(Nations=="Espagne"),
               sum(Nations=="Italie"),
               sum(Nations=="Angleterre"),
               sum(Nations=="Pays-bas"),
               sum(Nations=="BrÃ©sil"),
               sum(Nations=="Argentine"),
               sum(Nations=="Autre"))
names(nations_vec)<-nations_names
print(sort(nations_vec,decreasing = T))
lbls <- paste(nations_names, "\n", nations_vec, sep="")

pie(nations_vec , labels =lbls,
    border="white", col=myPalette , main = "les nations prÃ©fÃ©rÃ©s" )



my_bar <- barplot(nations_vec , border=F , names.arg=nations_names , 
                  las=2 , 
                  col= myPalette, 
                  ylim=c(0,70) , 
                  main="Nations prÃ©fÃ©rÃ©es" )

abline(v=c(4.9 , 9.7) , col="grey")

text(my_bar, nations_vec , paste("n: ", nations_vec, sep="") ,cex=1) 






