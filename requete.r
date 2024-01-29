# Appel de la librairie readxl pour lire des Excel
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(RPostgreSQL)

# Lecture du fichiers excel
resultats_pres2022 <- read_excel("C:/SIG/HUBERT/R/resultats_pres2022.xlsx")
View(resultats_pres2022) # Visualisation du fichier importé
summary(resultats_pres2022$Part_Macron) # Edition d'un résumé des données Part_Macron (part)

# Affichage de moyennes en string
moyenneMacron <- mean(resultats_pres2022$Part_Macron) # Assignation de la variable moyenneMacron
paste("La moyenne de % de vote Macron est de", moyenneMacron) #Affichage de la moyenne (séparateur string ',')
paste("La moyenne des parts de vote Macron est de ", round(moyenneMacron, digits=2), "%")
moyenneAbs <- mean(resultats_pres2022$Part_abs)
paste("La moyenne de % des absententions est de", moyenneAbs)

# Test sur les communes ayant plus de X %
Requete1 <- resultats_pres2022 [resultats_pres2022$Part_LePen > 10 , ] #Creation de la requete - choix sur l'attribut > 10%
View (Requete1)
count(Requete1) # calcul du nombre de lignes de la requete 1 = nbre de communes

# Test sur les communes ayant plus de X %
Requete2 <- resultats_pres2022 [resultats_pres2022$Part_LePen > 30 & resultats_pres2022$Part_LePen < 60 , ] # Requete avec des bornes
voteJadot2040 <- resultats_pres2022 [resultats_pres2022$Part_Jadot > 20 & resultats_pres2022$Part_Jadot < 40 , ] # Jadot plus de 20% et moins de 40%

# Requete textuelle

voteRochefort <- resultats_pres2022 [resultats_pres2022$Libelle =='Rochefort' & resultats_pres2022$Code_dep == '17' ,] #Requete textuel sur Rochefort (17)
View (voteRochefort) # permet de visualiser la nouvelle extraction

voteSaintes <- resultats_pres2022 [resultats_pres2022$Libelle =='Saintes' & resultats_pres2022$Code_dep == '17' ,] #Requete textuel sur Rochefort (17)
View (voteSaintes) # permet de visualiser la nouvelle extraction

# Extraction par colonnes 
voteCandidats1_4 <- resultats_pres2022 [ , c("Part_Macron", "Part_LePen", "Part_Melenchon", "Part_Pecresse")] #Candidats les mieux classés
voteCandidats1_3 <- resultats_pres2022 [resultats_pres2022$Libelle =='Saintes' & resultats_pres2022$Code_dep == '17' , c("Part_Macron", "Part_LePen", "Part_Melenchon")] #Candidats les mieux classés
sd(voteCandidats1_3$Part_Macron) # Calcul de l'écart type avec une part
sd(voteCandidats1_3$Part_Melenchon) # Calcul de l'écart type avec une part
sd(voteCandidats1_3$Part_LePen) # Calcul de l'écart type avec une part

# Creation/Modification sur les colonnes 
voteCandidats1_4$colonneTest <- NA # Creation d'une nouvelle colonne vide
voteCandidats1_4$colonneTest <- voteCandidats1_4$Part_Macron + voteCandidats1_4$Part_Pecresse # Ajout de valeurs de Pecresse et Macron
voteCandidats1_4$colonneTest <- rowSums(voteCandidats1_4[,c('Part_Macron', 'Part_Pecresse')], na.rm=TRUE) # Comptage par ligne en sautant les valeurs NA (null)

voteCandidats1_4$colonneTest <- ifelse(voteCandidats1_4$Part_Macron > 25,voteCandidats1_4$Part_Macron + voteCandidats1_4$Part_Pecresse,NA) # Boucle calculant la somme uniqyuement si Macron est à plus de 25% (3 argument : cdt,calcul cdt vérifiée, calcul cdt non-verif )
Transpo <- pivot_longer(voteCandidats1_4,cols = c(Part_Macron, Part_LePen, Part_Melenchon, Part_Pecresse),names_to="Candidat", values_to="Parts_vote") # passage des colonnes en lignes
View(Transpo)


# Ajout des valeurs dans une colonne si macron sup > 30
voteCandidats1_4$MacronSup30 <- NA # Creation d'une nouvelle colonne vide
voteCandidats1_3$MacronSup30 <- ifelse(voteCandidats1_3$Part_Macron > 30,'MacronSup30',NA) # Boucle calculant la somme uniqyuement si Macron est à plus de 30% (3 argument : cdt,calcul cdt vérifiée, calcul cdt non-verif )
length(which(voteCandidats1_3=='MacronSup30')) # Calcul de la longueur des elements non vides du tableau

# Boucles 
sapply(voteCandidats1_4, mean) #moyenne sur chacunes des colonnes
tapply(resultats_pres2022$Part_abs,resultats_pres2022$Libelle_dep, mean ) # fonction permet de caluler les moyennes de la premiere colonne, groupées par une deuxieme, via un opérateur (mean ici)

# Calcul de la moyenne des votes Arthaud par communes dans le vaucluse
voteArthaudVaucluse <- resultats_pres2022 [resultats_pres2022$Libelle_dep =='Vaucluse', c("Libelle","Part_Arthaud")]  #Creation d'une nouvelle variable contenant uniqument les résultats d'arthaud pour chaque commune du vaucluse
moyArthaudCom84 <- tapply(voteArthaudVaucluse$Part_Arthaud,voteArthaudVaucluse$Libelle, mean ) # calcul des moyennes regroupées par communes

# Graphique en camembert 
col1 <- c("Macron", "Melenchon", "Le Pen", "Pecresse") # variable colonne
col2 <- c(mean(voteCandidats1_4$Part_Macron), mean(voteCandidats1_4$Part_Melenchon),mean(voteCandidats1_4$Part_LePen), mean(voteCandidats1_4$Part_Pecresse)) # nouvelle variable moyenne pouyr chaque colonne
data <- data.frame(group=col1, value=col2) # On crée le tableau de données du graphique en spécifiant que les valeurs du graph correspondent à « col2 »

ggplot(data, aes(x="", y=value , fill=group)) + ## On crée le graphique :
 geom_bar(stat="identity", width=1) +
 geom_col() +
 coord_polar("y", start=0) +
 geom_text(aes(label = round(value, digits=2)), position = position_stack(vjust = 0.5)) + # On ajoute les valeurs de % de vote sur le graph et on personnalise les couleurs
 scale_fill_manual(values = c("#0a3895", "#b831f3", "#f33157", "#6091f6")

# Graphique en barres (on réutilise les données d'au-dessus)
ggplot(data= data, aes(x=reorder(group, -value), y=value, fill=group)) +
 geom_bar(stat="identity")+
 geom_text(aes(label=round(value, digits=2)), vjust=1.6, color="white", size=3.5)+# On ajoute les valeurs de % de vote sur le graph et on personnalise les couleurs
 scale_fill_manual(values = c("#0a3895", "#b831f3","#f33157", "#6091f6")

# M7 Graphique en camenbert
colX1 <- c("Melenchon", "Jadot", "Hidalgo") # variable colonne
voteGaucheLR <- resultats_pres2022 [resultats_pres2022$Code_dep ==17 & resultats_pres2022$Libelle=='La Rochelle', c("Part_Melenchon","Part_Jadot","Part_Hidalgo")] #nouvelle variable de l'ensemble des votes à gauche de LR
colY1 <- c(mean(resultats_pres2022$Part_Melenchon),mean(resultats_pres2022$Part_Jadot), mean(resultats_pres2022$Part_Hidalgo)) # variable colonne
data <- data.frame(group=colX1, value=colY1) # On crée le tableau de données du graphique en spécifiant que les valeurs du graph correspondent à « col2 »
ggplot(data, aes(x="", y=value , fill=group)) + ## On crée le graphique :
 geom_bar(stat="identity", width=1) +
 geom_col() +
 coord_polar("y", start=0) +
 geom_text(aes(label = round(value, digits=2)), position = position_stack(vjust = 0.5)) + # On ajoute les valeurs de % de vote sur le graph et on personnalise les couleurs
 scale_fill_manual(values = c("#CB1010", "#4C9556",  "#F1C2E7")
 )

 # M8 Graphique à moustaches

colX2 <- c("Dupont-Aignan", "Le Pen", "Pecresse", "Zemmour") # variable colonne 
voteDroiteLR <- resultats_pres2022 [resultats_pres2022$Code_dep ==17 & resultats_pres2022$Libelle=='La Rochelle', c("Part_DupontAignan","Part_LePen","Part_Pecresse","Part_Zemmour")] #nouvelle variable de l'ensemble des votes à gauche de LR
colY2 <- pivot_longer(voteDroiteLR,cols = c(Part_DupontAignan, Part_LePen, Part_Pecresse, Part_Zemmour),names_to="Candidat", values_to="Parts_vote") # passage des colonnes en lignes

ggplot(colY2, aes(x=as.factor(Candidat), y=Parts_vote, fill=Candidat )) + ## On crée le graphique :
 geom_boxplot()

 # Multigraphiques 
 voteRochefort_Candidats1_4 <- voteRochefort [ , c("Code_BV", "Part_Macron" , "Part_LePen" , "Part_Melenchon", "Part_Pecresse")] #tableau multigrpahique
 reformat <- pivot_longer(voteRochefort_Candidats1_4,cols=c("Part_Macron", "Part_LePen", "Part_Melenchon", "Part_Pecresse"),names_to='candidats', values_to='parts') # regroupement en classe via du pivot_longer

ggplot(data=reformat, aes(x=candidats,y=parts,fill=candidats)) + # On réalise autant de graphs qu’il existe de Bureaux de vote (« facet_wrap »)
 geom_bar(stat="identity") +
 facet_wrap(~Code_BV) +
 ggtitle("Parts de vote dans les BV de Rochefort") +
 theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color="#555555")) +
 theme(axis.text.x = element_text(angle=90)) +
 scale_fill_manual(values = c("#0a3895", "#b831f3","#f33157", "#6091f6"))

# M9 
voteRoyan_Candidats1_4 <- resultats_pres2022 [resultats_pres2022$Libelle =='Royan' & resultats_pres2022$Code_dep == '17' , c("Code_BV", "Part_Macron" , "Part_LePen" , "Part_Melenchon", "Part_Pecresse")] #tableau multigrpahique
reformat <- pivot_longer(voteRoyan_Candidats1_4,cols=c("Part_Macron", "Part_LePen", "Part_Melenchon", "Part_Pecresse"),names_to='candidats', values_to='parts') # regroupement en classe via du pivot_longer

ggplot(data=reformat, aes(x=candidats,y=parts,fill=candidats)) + # On réalise autant de graphs qu’il existe de Bureaux de vote (« facet_wrap »)
 geom_bar(stat="identity") +
 facet_wrap(~Code_BV) +
 coord_polar(start=0) +
 ggtitle("Parts de vote dans les BV de Rochefort") +
 coord_polar(theta = "y")+
 theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color="#555555")) +
 theme(axis.text.x = element_text(angle=90)) +
 scale_fill_manual(values = c("#0a3895", "#b831f3","#f33157", "#6091f6"))

# Interaction avec la base PostgreSQL
# Paramétrage de la connexion PostGreSQL
db <- "FME"
db_host <- "localhost"
db_port <- "5432"
db_user <- "postgres"
db_pass <- "postgres"
conn <- dbConnect(RPostgres::Postgres(),dbname = db,host = db_host,port = db_port,user = db_user,password = db_pass)
conn # doit renvoyer <PqConnection> postgres@localhost:5432

requete <- dbGetQuery(conn, 'SELECT * from ex24.plu_secteur;')