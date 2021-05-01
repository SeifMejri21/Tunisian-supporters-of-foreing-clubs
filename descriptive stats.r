install.packages('fmsb')
install.packages("waffle")
install.packages("ggthemes")


library(fmsb)
library(plotrix)
library(waffle)
library(ggthemes)
library(tidyverse)



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
LFC<-dataset[dataset$LFC==5 &  dataset$FCB!=5 & 	dataset$RM!=5 &  dataset$BVB!=5 ,
             c("sexe","age",	"situation professionnelle","Anglais","Espagnol",
             "Allemand","Italien","fan_since","LFC","MUFC","AFC","CFC","MCFC","FCB","RM"	,"JFC","ACM","IM","BM","BVB","PSG",	
             "equipe-tun","nation","Joueurs","attachement",
             "enfance","joueur_prefere","gagne","histoire","plus_talentueuse","principe","spectacle",
             "satisfaction resultat",
             "MoneyBall","Gegenpressing","Tiki_Taka","Catinaccio","Grinta","Pragmatisme",	
             "transmis_passion")]
#repartion par sexe
sexe_names<-c("Homme","Femme")
c<-length(LFC$sexe)
print(length(LFC$sexe))
sexe_values<-c(100*(sum(LFC$sexe=="Homme")/c),100*(sum(LFC$sexe=="Femme")/c))
print(round(sexe_values,2))
names(sexe_values)<-sexe_names

waffle(sexe_values,rows = 5,title = paste("Homme: ",round(sexe_values["Homme"],2),"%",
                  "       Femme: ",round(sexe_values["Femme"],2),"%"),
       colors=c("#004aad", "#d1292b"))


#annÃ©e de bebut comme fan
fan_since<-c(LFC$fan_since)
mean(fan_since)
print(round(2020-mean(fan_since)+13,2))
max(fan_since)
min(fan_since)
median(fan_since)
#orientation vers autres equipes 
LFC_like_vec<-c(mean(LFC$LFC),mean(LFC$MUFC),mean(LFC$AFC),mean(LFC$CFC),
                mean(LFC$MCFC),mean(LFC$FCB),mean(LFC$RM),mean(LFC$JFC),
                mean(LFC$ACM),mean(LFC$IM),mean(LFC$BM),mean(LFC$BVB),
                mean(LFC$PSG))
typeof(LFC_like_vec)
names(LFC_like_vec)<-Team_names_vec
print(round(sort(LFC_like_vec,decreasing = T),2))


#Joueurs prÃ©fÃ©rÃ©s

players<-(LFC["Joueurs"])
typeof(Players_vec)
Players_vec<-c(sum(players=="Lionel Messi"),
sum(players=="Cristiano Ronaldo"),
sum(players=="Eden Hazard"),
sum(players=="Neymar"),
sum(players=="Mohamed Salah"),
sum(players=="Sadio Mane"),
sum(players=="Virgil Van Dijk"),
sum(players=="Sergio Ramos"),
sum(players=="Kylian Mbappe"),
sum(players=="Luis Suarez"),
sum(players=="Paul Pogba"),
sum(players=="Mourad el Hedhli"),
sum(players=="Luka Modric"),
sum(players=="Robert Lewandowski")
,sum(players=="autre"))
names(Players_vec)<-players_names
print(round(sort(Players_vec,decreasing = T),2))

sort(round((Players_vec/c)*100,2),decreasing = T)


#Nation prÃ©fÃ©rÃ©e

Nations<-(LFC["nation"])
nations_vec<-c(sum(Nations=="France"),
               sum(Nations=="Allemagne"),
               sum(Nations=="Espagne"),
               sum(Nations=="Italie"),
               sum(Nations=="Angleterre"),
               sum(Nations=="Pays-bas"),
               sum(Nations=="Bresil"),
               sum(Nations=="Argentine"),
               sum(Nations=="Autre"))
names(nations_vec)<-nations_names
print(sort(nations_vec,decreasing = T))

sort(round((nations_vec/c)*100,2),decreasing = T)


#DegrÃ© d'attachement

attachement<-c(LFC["attachement"])
f<-unlist(lapply(attachement,mean))
2*round(f,2)
#DegrÃ© de Satisfaction des rÃ©sultats

satisfaction<-c(LFC["satisfaction resultat"])
e<-unlist(lapply(satisfaction,mean))
2*round(e,2)

#Les raisons

reasons_means<-c(
lapply(LFC["enfance"],mean),
lapply(LFC["joueur_prefere"],mean),
lapply(LFC["gagne"], mean),
lapply(LFC["histoire"], mean),
lapply(LFC["plus_talentueuse"], mean),
lapply(LFC["principe"], mean),
lapply(LFC["spectacle"], mean))

a<-unlist(reasons_means)
reasons_matrix <- rbind(c(5,5,5,5,5,5,5), c(0,0,0,0,0,0,0), a)
reasons_df <- as.data.frame(reasons_matrix, stringsAsFactors=FALSE)
colnames(reasons_df)<-Reasons_vec

radarchart( reasons_df  , axistype=6 ,  pcol=rgb(0.2,0.5,0.5,0.9) ,
            pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=5 , cglcol="grey",
            cglty=1, axislabcol="black", caxislabels=seq(0,6,1), cglwd=0.8,
            vlcex=0.9 )

 


#Les notions importantes

notions_means<-c(
  lapply(LFC["MoneyBall"],mean),
  lapply(LFC["Gegenpressing"],mean),
  lapply(LFC["Tiki_Taka"], mean),
  lapply(LFC["Catinaccio"], mean),
  lapply(LFC["Grinta"], mean),
  lapply(LFC["Pragmatisme"], mean))
names(notions_means)<-Notions_vec
b<-unlist(notions_means)
print(round(sort(b,decreasing = T),2))

notions_matrix <- rbind(c(5,5,5,5,5,5), c(0,0,0,0,0,0), b)
notions_df <- as.data.frame(notions_matrix, stringsAsFactors=FALSE)
colnames(notions_df)<-Notions_vec

radarchart( notions_df  , axistype='b' ,  pcol=rgb(0.2,0.5,0.5,0.9) ,
            pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=3 , cglcol="grey",
            cglty=1, axislabcol="black", caxislabels=seq(1,5,1), cglwd=0.8,
            vlcex=0.9 )








