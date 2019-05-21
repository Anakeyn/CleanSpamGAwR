############################################################################
# CleanSpamGAwR
# Nettoyage du Spam dans Google Analytics avec R
# Auteur : Pierre Rouarch - 2019.
# Licence : GPL 3 - http://www.gnu.org/licenses/gpl-3.0.html
#########################################################################
# Installer la version stable ou en cours de dev de googleAnalyticsR 
# décommenter la ligne désirée :
# version stable :
# install.packages("googleAnalyticsR", dependencies = TRUE) 
# Version en cours de dev  :
# devtools::install_github("MarkEdmondson1234/googleAnalyticsR")  


# googleAnalyticsR fournit par défaut un ID Client et un Code secret 
# sinon vous pouvez utiliser les votres pas les notres  :
# ceux-ci sont faux 
myclient_id <-  "XXXXXXXXXXXtvh1eh0msnekhkeaeb01vtuvpq8j.apps.googleusercontent.com" 
myclient_secret <-   "XXXXXXXXXXX9NS-R3pKXDa" 


##########################################################################
#Initialiser les codes OAuth  avant de charger la bibliothèque 
#googleAnalyticsR
options(googleAuthR.client_id = myclient_id )
options(googleAuthR.client_secret = myclient_secret ) 
#Optionnel : indiquez l'étendue de l'utilisation des API Google 
#options(googleAuthR.scopes.selected = "https://www.googleapis.com/auth/analytics")
library(googleAnalyticsR)  #! mettre mes info OAuth avant de recharger.

##########################################################################
#connexion via son compte google (un navigateur va s'ouvrir lors 
# de la première connexion)
ga_auth() 

#liste des comptes dans Google Analytics pour récupérer le code viewID
account_list <- ga_account_list()
account_list  #Afficher la liste
# Attention c'est l'ID de vue qu'il nous faut viewID
ga_id <- account_list[1, 'viewId']  #Pour moi c'est la 1ere ligne de mes 
#vues dans Google Analytics

##########################################################################
#Variables disponibles dans l'API GA Reporting 
meta <- google_analytics_meta()  #Recupération de la liste des variables
View(meta) #Affichage de la liste des Variables disponible dans l'API de 
#Google Analytics.
#Sauvegarde en csv ; ça peut aider pour consulter les variables ailleurs.
#attention cette liste n'est pas tout à fait à jour préférez le guide 
#Google :
#https://developers.google.com/analytics/devguides/reporting/core/dimsmets#mode=api
write.csv2(meta, file = "metaGA.csv",  row.names=FALSE) 
##########################################################################

##########################################################################
#Autres pâckages et bibliothèques utiles
##########################################################################

#install.packages("lubridate")  #si vous ne l'avez pas

#install.packages("tseries")
#install.packages("devtools")
#devtools::install_github("twitter/AnomalyDetection")
#install.packages("XML")
#install.packages("stringi")
#install.packages("BSDA")
#install.packages("BBmisc")
#install.packages("stringi")
#install.packages("FactoMineR")
#install.packages("factoextra")
#install.packages("rcorr")

#install.packages("lubridate")  #si vous ne l'avez pas
#library (lubridate) #pour yday

#library(tseries) #pour ts
#library(AnomalyDetection) #anomalydetectionVec
#library(XML) # pour xmlParse
#library(stringi) #pour stri_replace_all_fixed(x, " ", "")
#library(BSDA)  #pour SIGN.test 
#library(BBmisc) #pour which.first
#install.packages("stringi")
library(stringi) #pour stri_detect
#library(ggfortify)  #pour ploter autoplot type ggplot


############### On demarre ici #############################

##########################################################################
# Nettoyage de données de spam Google Analytics avec R
##########################################################################
# Utilisation de la bibliothèque googleAnalyticsR
##########################################################################
# googleAnalyticsR  est une bibliothèque R compatible avec la version 4 
# de l'API de Google Analytics Développée par Mark Edmonson spécialiste 
# du développement Google Analytics
# Page de la bibliothèque googleAnalyticsR sur le site de Mark Edmonson
# http://code.markedmondson.me/googleAnalyticsR/index.html
##########################################################################


##########################################################################
#Packages utiles 
#install.packages("tidyverse")  #si vous ne l'avez pas #pour gggplot2, dplyr, tidyr, readr, purr, tibble, stringr, forcats 
#install.packages("forecast") #pour ma
#Chargement des bibliothèques utiles
library(tidyverse) #pour gggplot2, dplyr, tidyr, readr, purr, tibble, stringr, forcats 
library(forecast)  #pour  arima, ma, tsclean


#QQ dates utiles 
FirstJuly2011 <- as.Date("2011-07-01")  #début du site étudié
the31thDec2018 <- as.Date("2018-12-31")   #fin de l'étude.



##########################################################################
# RECUPERATION DES DONNEES BRUTES POUR FILTRAGE 
##########################################################################
#Pour mémoire Dimensions & Metrics Explorer 
#https://developers.google.com/analytics/devguides/reporting/core/dimsmets

#Attention le nombre de dimensions est limité à 9 et de Metrics à 10.

###############
gaPVAllYears <- google_analytics(ga_id, date_range = c(FirstJuly2011 , the31thDec2018), 
                            metrics = c( "Pageviews"), 
                            dimensions = c("date", 
                                           "hostname", #pour filtrer le spam sur des nom d'hôtes non autorisés 
                                           "browser",  #pour filtrer les robots et browser curieux.
                                           "fullReferrer",  #pour filtrer les fausses pages référentes
                                           "sourceMedium", #pour filtrer les crawlers spammers ou faux sites référents
                                           "language",  #pour filtrer les langues suspectes
                                           "landingPagePath",  #pour voir si la page est en entrée
                                           "pagePath",  #page recherchée
                                           "keyword"  #Peut être récupéré si Bing (et peut être autres moteurs)
                                            ),
                            max = 100000
                            )

#on, récupère 51790 lignes.
#split sourceMedium en source et medium
gaPVAllYears <- separate(gaPVAllYears, sourceMedium, c("source", "medium"), " / ")

# une ligne par observation : décomptage des Pageviews.
gaPVAllYears <- gaPVAllYears %>%
  uncount(Pageviews)

#Verifs
head(gaPVAllYears, n=20) #visualison l'entête
tail(gaPVAllYears, n=20) #visualison fin
nrow(gaPVAllYears) #nombre de lignes 82559  on a le même décompte qu'avec Google Analytics en mode console.

##########################################################################
# Affichage des données brutes
##########################################################################

#creation de la dataframe daily_data par jour
dfDatePV <- as.data.frame(gaPVAllYears$date)
colnames(dfDatePV)[1] <- "date"                   #change le nom de la colonne.
daily_data <- dfDatePV  %>%                       #daily_data à partir de dfDatePV
  group_by(date) %>%                              #groupement par date
  mutate(Pageviews = n()) %>%                     #total des pageviews = nombre d'observations / date
  as.data.frame() %>%                             #sur d'avoir une data.frame
  unique() %>%                                    #ligne unique par jour.
  mutate(cnt_ma30 = ma(Pageviews, order=30)) %>%  #variable moyenne mobile (moving average 30 jours)
  mutate(year = format(date,"%Y"))             #creation de la variable year


#Graphique pages vues par jour
ggplot(daily_data , aes(x=date, y=Pageviews, color=year)) + 
  geom_line() +
  xlab("Année") +
  ylab("Nbre pages vues / jour") +
  labs(title = "Il semble y avoir une anomalie en fin 2016",
       subtitle = "Le trafic est bien au dessus du reste des observations.",
       caption = "Nombre de pages vues par jour depuis 2011 - Données Brutes",
       color = "Année") 
#sauvegarde du dernier ggplot
ggsave(filename = "PV-s2011.jpeg",  dpi="print") #sauvegarde du dernier ggplot.


#Graphique Moyenne Mobile 30 jours.
ggplot(daily_data, aes(x=date, y=cnt_ma30, color=year)) + 
  geom_line() +
  xlab("Année") +
  ylab("Nbre pages vues / jour en moyenne mobile") +
  labs(title = "L'anomalie fin 2016 est bien visible",
       subtitle = "Le trafic est bien au dessus du reste des observations.",
       caption = "Nbre pages vues par jour depuis 2011 en moy. mob. 30 j - Données brutes",
       color = "Année")
ggsave(filename = "PV-s2011-mm30.jpeg",  dpi="print") #sauvegarde du dernier ggplot.

#lissage avec des années de différentes couleurs. méthode loess
ggplot(daily_data, aes(x=date, y=Pageviews, color=year)) + 
  geom_smooth(method = "loess") + 
  xlab("Année") +
  ylab("Nbre pages vues / jour lissage loess") +
  labs(title = "l'anomalie fin 2016 est bien visible",
       subtitle = "On constate en général une baisse de trafic l'été",
       caption = "Nombre de pages vues par jour depuis 2011 \n lissage méthode Loess par an - Données brutes",
       color = "Année") 
#sauvegarde du dernier ggplot.
ggsave(filename = "PV-s2011-lissage-loess-an.jpeg",  dpi="print") 

#lissage linéaire par an 
ggplot(daily_data, aes(x=date, y=Pageviews, color=year)) + 
  geom_smooth(method = "lm") +
  xlab("Année") +
  ylab("Nbre pages vues / jour lissage linéaire") +
  labs(title = "l'anomalie fin 2016 est bien visible",
       subtitle = "",
       caption = "Nombre de pages vues par jour depuis 2011 \n lissage linéaire par an - Données brutes",
       color = "Année") 
#sauvegarde du dernier ggplot.
ggsave(filename = "PV-s2011-lissage-lm-an.jpeg",  dpi="print") 

#lissage loess sur toutes les données depuis 2011
ggplot(daily_data, aes(x=date, y=Pageviews)) + 
  geom_smooth(method = "loess") +
  xlab("Année") +
  ylab("Nbre pages vues / jour lissé") +
  labs(title = "Le trafic monte jusqu'en 2014 puis reste stable jusqu'en 2016.",
       subtitle = "Après 2016 la décroissance est forte.",
       caption = "Nombre de pages vues par jour depuis 2011 lissage méthode Loess - Données brutes") 
#sauvegarde du dernier ggplot.
ggsave(filename = "PV-s2011-lissage-loess.jpeg",  dpi="print") 


#Sauvegarde si besoin.
write.csv2(gaPVAllYears, file = "gaPVAllYears.csv", row.names=FALSE) #csv avec ;



##########################################################################
#Nettoyage des langues suspectes.
##########################################################################
indexGoodlang <- grep(pattern = "^[a-zA-Z]{2,3}([-/][a-zA-Z]{2,3})?$", gaPVAllYears$language)
gaPVAllYearsCleanLanguage <- gaPVAllYears[indexGoodlang,]
nrow(gaPVAllYearsCleanLanguage) #on supprime environ 6000 lignes 
#nombre de ligne 76733


#verifs
head(gaPVAllYearsCleanLanguage, n=20) #verif
summary(gaPVAllYearsCleanLanguage)
dim(gaPVAllYearsCleanLanguage)
class(gaPVAllYearsCleanLanguage$date)


#creation de la dataframe daily_data par jour
dfDatePV <- as.data.frame(gaPVAllYearsCleanLanguage$date)
colnames(dfDatePV)[1] <- "date"                   #change le nom de la colonne.
daily_data <- dfDatePV  %>%                       #daily_data à partir de dfDatePV
  group_by(date) %>%                              #groupement par date
  mutate(Pageviews = n()) %>%                     #total des pageviews = nombre d'observations / date
  as.data.frame() %>%                             #sur d'avoir une data.frame
  unique() %>%                                    #ligne unique par jour.
  mutate(cnt_ma30 = ma(Pageviews, order=30)) %>%  #variable moyenne mobile (moving average 30 jours)
  mutate(year = format(date,"%Y"))             #creation de la variable year

                                   
#Graphique pages vues par jour
ggplot(daily_data , aes(x=date, y=Pageviews, color=year)) + 
  geom_line() +
  xlab("Année") +
  ylab("Nbre pages vues / jour") +
  labs(title = "L'anomalie de fin 2016 a disparu ",
       subtitle = "suite au nettoyage des langues.",
       caption = "Nombre de pages vues par jour depuis 2011 \n Données nettoyées variable langue",
       color = "Année") 
#sauvegarde du dernier ggplot
ggsave(filename = "PV-s2011-Clean-Lang.jpeg",  dpi="print") #sauvegarde du dernier ggplot.


#Graphique Moyenne Mobile 30 jours.
ggplot(daily_data, aes(x=date, y=cnt_ma30, color=year)) + 
  geom_line() +
  xlab("Année") +
  ylab("Nbre pages vues / jour en moyenne mobile") +
  labs(title = "L'anomalie de fin 2016 a disparu ",
       subtitle = "suite au nettoyage des langues.",
       caption = "Nbre pages vues par jour depuis 2011 en moy. mob. 30 j \n Données net. variable langue",
       color = "Année")
ggsave(filename = "PV-s2011-Clean-Lang-mm30.jpeg",  dpi="print") #sauvegarde du dernier ggplot.


#Sauvegarde si besoin.
write.csv2(gaPVAllYearsCleanLanguage, file = "gaPVAllYearsCL.csv",  row.names=FALSE) #sauvegarde en csv avec ;




patternGoodHostname <- c("networking-morbihan\\.com", "translate\\.googleusercontent\\.com", 
                         "webcache\\.googleusercontent\\.com", 
                         "networking-morbihan\\.com\\.googleweblight\\.com", 
                         "web\\.archive\\.org")

patternBadHostname = "loc\\.networking-morbihan\\.com"

##########################################################################
#nettoyage des hostnames non légitimes (Ghostnames)
##########################################################################
#Pour faciliter la lecture on va créer un vecteur de pattern des sites 
#légitimes : nos sites et les sites de cache comme par exemple webcache.googleusercontent.com
#ou web.archive.org
#on garde ceux qui nous intéressent. 
# remarque adpatez le nom de votre site en remplacment de networking-morbihan.
patternGoodHostname <- c("networking-morbihan\\.com", "translate\\.googleusercontent\\.com", 
                         "webcache\\.googleusercontent\\.com", 
                         "networking-morbihan\\.com\\.googleweblight\\.com", 
                         "web\\.archive\\.org")

indexGoodHostname <- grep(pattern = paste(patternGoodHostname, collapse="|"), gaPVAllYearsCleanLanguage$hostname)
gaPVAllYearsCleanHost <- gaPVAllYearsCleanLanguage[indexGoodHostname,]

#Verifs.
nrow(gaPVAllYearsCleanHost) #76170 #~560 lignes supprimmées 
summary(gaPVAllYearsCleanHost$hostname)  #verif


#on vire un mauvais sous domaine qui restait
patternBadHostname = "loc\\.networking-morbihan\\.com"
indexBadHostname <- -grep(pattern = patternBadHostname, gaPVAllYearsCleanHost$hostname)
gaPVAllYearsCleanHost <- gaPVAllYearsCleanHost[indexBadHostname,]
nrow(gaPVAllYearsCleanHost) #76159 #nombre de lignes  nettoyées 11 



#creation de la dataframe daily_data par jour
dfDatePV <- as.data.frame(gaPVAllYearsCleanHost$date)
colnames(dfDatePV)[1] <- "date"                   #change le nom de la colonne.
daily_data <- dfDatePV  %>%                       #daily_data à partir de dfDatePV
  group_by(date) %>%                              #groupement par date
  mutate(Pageviews = n()) %>%                     #total des pageviews = nombre d'observations / date
  as.data.frame() %>%                             #sur d'avoir une data.frame
  unique() %>%                                    #ligne unique par jour.
  mutate(cnt_ma30 = ma(Pageviews, order=30)) %>%  #variable moyenne mobile (moving average 30 jours)
  mutate(year = format(date,"%Y"))                #creation de la variable year
  
  
#Graphique pages vues par jour
ggplot(daily_data , aes(x=date, y=Pageviews, color=year)) + 
  geom_line() +
  xlab("Année") +
  ylab("Nbre pages vues / jour") +
  labs(title = "L'évolution du nombre de pages vues ne se voit pas à l'oeil nu ",
       subtitle = "suite au nettoyage des Hostnames par rapport au nettoyage des langues.",
       caption = "Nbre pages vues par jour depuis 2011 - Données net. variable hostname",
       color = "Année")
ggsave(filename = "PV-s2011-Clean-Host.jpeg",  dpi="print") #sauvegarde du dernier ggplot.


#Graphique Moyenne Mobile 30 jours.
ggplot(daily_data, aes(x=date, y=cnt_ma30, color=year)) + 
  geom_line() +
  xlab("Année") +
  ylab("Nbre pages vues / jour en moyenne mobile") +
  labs(title = "L'évolution du nombre de pages vues ne se voit pas à l'oeil nu ",
       subtitle = "suite au nettoyage des Hostnames par rapport au nettoyage des langues.",
       caption = "Nbre pages vues par jour depuis 2011 en moy. mob. 30 jours - Données net. variable hostname",
       color = "Année")
ggsave(filename = "PV-s2011-Clean-Host-mm30.jpeg",  dpi="print") #sauvegarde du dernier ggplot.

#Sauvegarde pour si besoin.
write.csv2(gaPVAllYearsCleanHost, file = "gaPVAllYearsCH.csv",  row.names=FALSE) #sauvegarde en csv avec ;


##########################################################################
#nettoyage des browsers suspects - peut contenir des robots crawlers
##########################################################################
#voyons ce qu'il y a dedans 
unique(gaPVAllYearsCleanHost$browser)
plyr::count(as.factor(gaPVAllYearsCleanHost$browser))
#on vire les "curiosités et les bots (cela peut varier selon vos le contenu que vous trouver dans browser)
patternBadBrowser <- c("not set","Google\\.com", "en-us", 
                         "GOOG", "PagePeeker\\.com", 
                         "bot")
indexBadBrowser <- -grep(pattern = paste(patternBadBrowser, collapse="|"), gaPVAllYearsCleanHost$browser)
gaPVAllYearsCleanBrowser <- gaPVAllYearsCleanHost[indexBadBrowser,]

#Verifs
head(gaPVAllYearsCleanBrowser, n=20) #verif
nrow(gaPVAllYearsCleanBrowser) #76126 lignes nettoyées environ 30
unique(gaPVAllYearsCleanBrowser$browser)
plyr::count(as.factor(gaPVAllYearsCleanBrowser$browser))

#creation de la dataframe daily_data par jour
dfDatePV <- as.data.frame(gaPVAllYearsCleanBrowser$date)
colnames(dfDatePV)[1] <- "date"                   #change le nom de la colonne.
daily_data <- dfDatePV  %>%                       #daily_data à partir de dfDatePV
  group_by(date) %>%                              #groupement par date
  mutate(Pageviews = n()) %>%                     #total des pageviews = nombre d'observations / date
  as.data.frame() %>%                             #sur d'avoir une data.frame
  unique() %>%                                    #ligne unique par jour.
  mutate(cnt_ma30 = ma(Pageviews, order=30)) %>%  #variable moyenne mobile (moving average 30 jours)
  mutate(year = format(date,"%Y"))                #creation de la variable year


#Graphique pages vues par jour
ggplot(daily_data , aes(x=date, y=Pageviews, color=year)) + 
  geom_line() +
  xlab("Année") +
  ylab("Nbre pages vues / jour") +
  labs(title = "L'évolution du nombre de pages vues ne se voit pas à l'oeil nu ",
       subtitle = "suite au nettoyage des browsers suspects par rapport aux nettoyages précédents.",
       caption = "Nbre pages vues par jour depuis 2011 - Données net. variable browser",
       color = "Année")
#sauvegarde du dernier ggplot.
ggsave(filename = "PV-s2011-Clean-Browser.jpeg",  dpi="print") #sauvegarde du dernier ggplot.


#Graphique Moyenne Mobile 30 jours.
ggplot(daily_data, aes(x=date, y=cnt_ma30, color=year)) + 
  geom_line() +
  xlab("Année") +
  ylab("Nbre pages vues / jour en moyenne mobile") +
  labs(title = "L'évolution du nombre de pages vues ne se voit pas à l'oeil nu ",
       subtitle = "suite au nettoyage des browsers suspects par rapport aux nettoyages précédents.",
       caption = "Nbre pages vues par jour depuis 2011 moy. mob. 30 jours - Données net. variable browser",
       color = "Année")
ggsave(filename = "PV-s2011-Clean-Browser-mm30.jpeg",  dpi="print") #sauvegarde du dernier ggplot.

#Sauvegarde si besoin.
write.csv2(gaPVAllYearsCleanBrowser, file = "gaPVAllYearsCB.csv",  row.names=FALSE) #sauvegarde en csv avec ;


##########################################################################
#nettoyage des Crawlers Spammers et autres sources de trafic non désirées 
#dans source
##########################################################################
#install.packages("stringi")  #si vous ne l'avez pas 
library(stringi) #pour stri_detect
gaPVAllYearsCleanSource <- gaPVAllYearsCleanBrowser
#la liste des sites et mots clés non désirés est dans un fichier que 
#nous avons créé.
patternsBadSource <- read.csv("blacklist-source-sites.csv", header=TRUE)
head(patternsBadSource)
#pour des raisons de mémoire on est obligé de faire une boucle avec des paquets de pattern.

blacklistSourceSitesPacks <- vector() 
step <- 500
steps <- seq(1, length(patternsBadSource$blacksites) , by=step)
#steps <- 1 #pour faire des tests
j <- 1
for (i in steps)  {
  if ( i+step < length(patternsBadSource$blacksites))   {
    iMax <-i+step
    
  } 
  else {
    iMax <- length(patternsBadSource$blacksites)
  }
  
  patternBadSourcePack <- paste(paste(patternsBadSource$blacksites[i:iMax], collapse="|"))
  write.table(patternBadSourcePack, file = stri_replace_all_fixed(paste("blacklist-sites-",j,".txt"), " ", ""), row.names = FALSE, col.names=FALSE)
  str(patternBadSourcePack)
  cat("Pattern", patternBadSourcePack, "\n")
  blacklistSourceSitesPacks[j] <-  patternBadSourcePack #pour sauvegarde
  cat("j:", j, "\n")
  #grep ne déecte pas tout, notamment les nombres en notation scientifique planqués 
  #dans les chaines de caractères  :  préférer stri_detect_regex
  #indexBadSource <- -grep(pattern = as.character(patternBadSourcePack), format(gaPVAllYearsCleanSource$source, scientific=FALSE))
  #cat("trouvés:", indexBadSource, "\n")
  indexBadSource <- -which(stri_detect_regex(format(gaPVAllYearsCleanSource$source, scientific=FALSE), as.character(patternBadSourcePack)))
   cat("trouvés avec stri_detect :", indexBadSource, "\n")
    if (length(indexBadSource) > 0) gaPVAllYearsCleanSource <- gaPVAllYearsCleanSource[indexBadSource,]
  j <- j+1
}
#verif de ce que l'on a :
head(plyr::count(as.factor((gaPVAllYearsCleanSource$source)))) 
##########################################################################
#On sauvegarde les paquets de patterns pour filtres et segments à utiliser 
#pour Google Analytics "manuel"
#Rem : cette sauvegarde ne nous sert à rien ici mais peut servir pour  créer 
#des filtres à la main dans Google Analytics.
write.csv2(blacklistSourceSitesPacks, file = stri_replace_all_fixed(paste("blacklist-source-sites-packs-",step,".csv"), " ", ""), row.names=FALSE)
#
write.table(blacklistSourceSitesPacks, file = stri_replace_all_fixed(paste("blacklist-source-sites-packs-",step,".txt"), " ", ""), row.names = FALSE, col.names=FALSE)

#Verifs
nrow(gaPVAllYearsCleanSource) #74275 lignes nettoyées environ 1850


#creation de la dataframe daily_data par jour
dfDatePV <- as.data.frame(gaPVAllYearsCleanSource$date)
colnames(dfDatePV)[1] <- "date"                   #change le nom de la colonne.
daily_data <- dfDatePV  %>%                       #daily_data à partir de dfDatePV
  group_by(date) %>%                              #groupement par date
  mutate(Pageviews = n()) %>%                     #total des pageviews = nombre d'observations / date
  as.data.frame() %>%                             #sur d'avoir une data.frame
  unique() %>%                                    #ligne unique par jour.
  mutate(cnt_ma30 = ma(Pageviews, order=30)) %>%  #variable moyenne mobile (moving average 30 jours)
  mutate(year = format(date,"%Y"))                #creation de la variable year


#Graphique pages vues par jour
ggplot(daily_data , aes(x=date, y=Pageviews, color=year)) + 
  geom_line() +
  xlab("Année") +
  ylab("Nbre pages vues / jour") +
  labs(title = "L'évolution du nombre de pages vues ne se voit pas à l'oeil nu ",
       subtitle = "suite au nettoyage des referrers suspects par rapport aux nettoyages précédents.",
       caption = "Nbre pages vues par jour depuis 2011 - Données net. variable source",
       color = "Année")
#sauvegarde du dernier ggplot.
ggsave(filename = "PV-s2011-Clean-Source.jpeg",  dpi="print")

#Graphique Moyenne Mobile 30 jours.
ggplot(daily_data, aes(x=date, y=cnt_ma30, color=year)) + 
  geom_line() +
  xlab("Année") +
  ylab("Nbre pages vues / jour en moyenne mobile") +
  labs(title = "L'évolution du nombre de pages vues ne se voit pas à l'oeil nu ",
       subtitle = "suite au nettoyage des referrers suspects par rapport aux nettoyages précédents.",
       caption = "Nbre pages vues par jour depuis 2011 moy. mob. 30 jours - Données net. variable source",
       color = "Année")
#sauvegarde du dernier ggplot.
ggsave(filename = "PV-s2011-Clean-Source-mm30.jpeg",  dpi="print")  

#Sauvegarde si besoin #sauvegarde en csv avec ;
write.csv2(gaPVAllYearsCleanSource, file = "gaPVAllYearsCS.csv",  row.names=FALSE) 

##########################################################################
#nettoyage des fausses pages référentes dans fullReferrer
##########################################################################
gaPVAllYearsCleanFullReferrer <- gaPVAllYearsCleanSource
patternsBadFullReferrer <- read.csv("blacklist-fullRefferer-Page.csv", header=TRUE)
indexBadFullReferrer <- -grep(pattern = paste(patternsBadFullReferrer$Blackpages, collapse="|"), gaPVAllYearsCleanFullReferrer$fullReferrer)
gaPVAllYearsCleanFullReferrer <- gaPVAllYearsCleanFullReferrer[indexBadFullReferrer,]
#Verifs
nrow(gaPVAllYearsCleanFullReferrer) #73829 lignes nettoyées environ 450  

#creation de la dataframe daily_data par jour
dfDatePV <- as.data.frame(gaPVAllYearsCleanFullReferrer$date)
colnames(dfDatePV)[1] <- "date"                   #change le nom de la colonne.
daily_data <- dfDatePV  %>%                       #daily_data à partir de dfDatePV
  group_by(date) %>%                              #groupement par date
  mutate(Pageviews = n()) %>%                     #total des pageviews = nombre d'observations / date
  as.data.frame() %>%                             #sur d'avoir une data.frame
  unique() %>%                                    #ligne unique par jour.
  mutate(cnt_ma30 = ma(Pageviews, order=30)) %>%  #variable moyenne mobile (moving average 30 jours)
  mutate(year = format(date,"%Y"))                #creation de la variable year


#Graphique pages vues par jour
ggplot(daily_data , aes(x=date, y=Pageviews, color=year)) + 
  geom_line() +
  xlab("Année") +
  ylab("Nbre pages vues / jour") +
  labs(title = "L'évolution du nombre de pages vues ne se voit pas à l'oeil nu ",
       subtitle = "suite au nettoyage des pages référentes suspectes par rapport aux nettoyages précédents.",
       caption = "Nbre pages vues par jour depuis 2011 - Données net. variable fullReferrer",
       color = "Année")
#sauvegarde du dernier ggplot.
ggsave(filename = "PV-s2011-Clean-FullReferrer.jpeg",  dpi="print")  


#Graphique Moyenne Mobile 30 jours.
ggplot(daily_data, aes(x=date, y=cnt_ma30, color=year)) + 
  geom_line() +
  xlab("Année") +
  ylab("Nbre pages vues / jour en moyenne mobile") +
  labs(title = "L'évolution du nombre de pages vues ne se voit pas à l'oeil nu ",
       subtitle = "suite au nettoyage des pages référentes suspectes par rapport aux nettoyages précédents.",
       caption = "Nbre pages vues par jour depuis 2011 moy. mob. 30 jours - Données net. variable fullReferrer",
       color = "Année")
#sauvegarde du dernier ggplot.
ggsave(filename = "PV-s2011-Clean-FullReferrer-mm30.jpeg",  dpi="print")  

#Sauvegarde si besoin. #sauvegarde en csv avec ;
write.csv2(gaPVAllYearsCleanFullReferrer, file = "gaPVAllYearsCFR.csv",  row.names=FALSE) 


##########################################################################
#nettoyage des pages d'administration dans pagePath
##########################################################################
gaPVAllYearsCleanPagePath <-  gaPVAllYearsCleanFullReferrer

patternBadPagePath <- c("/wp-login\\.php", "/wp-admin/", "/cron/", "/?p=\\d", "/wp-admini", 
                        "wadmini", "admini")
indexBadPagePath  <- -grep(pattern = paste(patternBadPagePath, collapse="|"), gaPVAllYearsCleanPagePath$pagePath)
gaPVAllYearsCleanPagePath  <- gaPVAllYearsCleanPagePath [indexBadPagePath,]

#verifs
nrow(gaPVAllYearsCleanPagePath) #73301 environ 530 lignes de nettoyées.
summary(gaPVAllYearsCleanPagePath$pagePath)  #verif


#creation de la dataframe daily_data par jour
dfDatePV <- as.data.frame(gaPVAllYearsCleanPagePath$date)
colnames(dfDatePV)[1] <- "date"                   #change le nom de la colonne.
daily_data <- dfDatePV  %>%                       #daily_data à partir de dfDatePV
  group_by(date) %>%                              #groupement par date
  mutate(Pageviews = n()) %>%                     #total des pageviews = nombre d'observations / date
  as.data.frame() %>%                             #sur d'avoir une data.frame
  unique() %>%                                    #ligne unique par jour.
  mutate(cnt_ma30 = ma(Pageviews, order=30)) %>%  #variable moyenne mobile (moving average 30 jours)
  mutate(year = format(date,"%Y"))                #creation de la variable year


#Graphique pages vues par jour
ggplot(daily_data , aes(x=date, y=Pageviews, color=year)) + 
  geom_line() +
  xlab("Année") +
  ylab("Nbre pages vues / jour") +
  labs(title = "L'évolution du nombre de pages vues ne se voit pas à l'oeil nu ",
       subtitle = "suite au nettoyage des pages d'administration par rapport aux nettoyages précédents.",
       caption = "Nbre pages vues par jour depuis 2011 - Données net. variable pagePath",
       color = "Année")
ggsave(filename = "PV-s2011-Clean-PagePath.jpeg",  dpi="print") #sauvegarde du dernier ggplot. 

#Graphique Moyenne Mobile 30 jours.
ggplot(daily_data, aes(x=date, y=cnt_ma30, color=year)) + 
  geom_line() +
  xlab("Année") +
  ylab("Nbre pages vues / jour en moyenne mobile") +
  labs(title = "L'évolution du nombre de pages vues ne se voit pas à l'oeil nu ",
       subtitle = "suite au nettoyage des pages d'administration par rapport aux nettoyages précédents.",
       caption = "Nbre pages vues par jour depuis 2011 moy. mob. 30 jours - Données net. variable pagePath",
       color = "Année")
#sauvegarde du dernier ggplot.
ggsave(filename = "PV-s2011-Clean-PagePath-mm30.jpeg",  dpi="print")  

#Sauvegarde si besoin #sauvegarde en csv avec ;
write.csv2(gaPVAllYearsCleanPagePath, file = "gaPVAllYearsCPP.csv",  row.names=FALSE) 

##########################################################################
#nettoyage des pages dont l'entrée sur le site s'est faite 
#via l'administration, variable landingPagePath
##########################################################################

gaPVAllYearsCleanLandingPagePath <-  gaPVAllYearsCleanPagePath
patternBadLandingPagePath <- c("/wp-login\\.php", "/wp-admin/", "/cron/", "/?p=\\d")
indexBadLandingPagePath  <- -grep(pattern = paste(patternBadLandingPagePath, collapse="|"), gaPVAllYearsCleanLandingPagePath$landingPagePath)
gaPVAllYearsCleanLandingPagePath  <- gaPVAllYearsCleanLandingPagePath [indexBadLandingPagePath,]

#verifs
nrow(gaPVAllYearsCleanLandingPagePath) #72822 environ 500 lignes de nettoyées.
summary(gaPVAllYearsCleanLandingPagePath$landingPagePath)  #verif


#creation de la dataframe daily_data par jour
dfDatePV <- as.data.frame(gaPVAllYearsCleanLandingPagePath$date)
colnames(dfDatePV)[1] <- "date"                   #change le nom de la colonne.
daily_data <- dfDatePV  %>%                       #daily_data à partir de dfDatePV
  group_by(date) %>%                              #groupement par date
  mutate(Pageviews = n()) %>%                     #total des pageviews = nombre d'observations / date
  as.data.frame() %>%                             #sur d'avoir une data.frame
  unique() %>%                                    #ligne unique par jour.
  mutate(cnt_ma30 = ma(Pageviews, order=30)) %>%  #variable moyenne mobile (moving average 30 jours)
  mutate(year = format(date,"%Y"))                #creation de la variable year


#Graphique pages vues par jour
ggplot(daily_data , aes(x=date, y=Pageviews, color=year)) + 
  geom_line() +
  xlab("Année") +
  ylab("Nbre pages vues / jour") +
  labs(title = "Le maximum de pages vues par jour est maintenant autour de 200 (vs 300) ",
       subtitle = "suite au nettoyage des pages d'administration référentes \n par rapport aux nettoyages précédents.",
       caption = "Nbre pages vues par jour depuis 2011 - Données net. variable landingPagePath",
       color = "Année")
ggsave(filename = "PV-s2011-Clean-LandingPagePath.jpeg",  dpi="print") #sauvegarde du dernier ggplot. 


#Graphique Moyenne Mobile 30 jours.
ggplot(daily_data, aes(x=date, y=cnt_ma30, color=year)) + 
  geom_line() +
  xlab("Année") +
  ylab("Nbre pages vues / jour en moyenne mobile ") +
  labs(title = "En moyenne Mobile, l'évolution du nombre de pages vues \n ne se voit pas à l'oeil nu ",
       subtitle = "suite au nettoyage des pages d'administration référentes \n par rapport aux nettoyages précédents.",
       caption = "Nbre pages vues par jour depuis 2011 moy. mob. 30 jours - Données net. variable landingPagePath",
       color = "Année")
#sauvegarde du dernier ggplot. 
ggsave(filename = "PV-s2011-Clean-LandingPagePath-mm30.jpeg",  dpi="print") 

#Sauvegarde  si besoin. sauvegarde en csv avec ;
write.csv2(gaPVAllYearsCleanLandingPagePath, file = "gaPVAllYearsCLPP.csv",  row.names=FALSE) 




#Graphique pages vues par  jour pour la présenatation du 7 février 
ggplot(daily_data , aes(x=date, y=Pageviews, color=year)) + 
  geom_line() +
  xlab("Année") +
  ylab("Nbre pages vues / jour") +
  labs(title = "Le maximum de pages vues par jour est \n maintenant autour de 200 (vs 800) ",
       subtitle = "suite au nettoyage complet des données.",
       caption = "Nbre pages vues par jour depuis 2011 - Données nettoyées",
       color = "Année")
ggsave(filename = "PV-s2011-Clean-All.jpeg",  dpi="print") #sauvegarde du dernier ggplot. 





#Sauvegarde si besoin. sauvegarde en csv avec ;
write.csv2(gaPVAllYearsCleanLandingPagePath, file = "gaPVAllYearsCLPP.csv",  row.names=FALSE) 






##########################################################################
# Jeu de données nettoyé
##########################################################################
#install.packages("lubridate")  #si vous ne l'avez pas
library (lubridate) #pour yday
#nom de sauvegarde plus facile à retenir :
dfPageViews <- gaPVAllYearsCleanLandingPagePath 
#Sauvegarde  si besoin. Bon c'est le même que le précédent ...
#sauvegarde en csv avec ;
write.csv2(dfPageViews, file = "dfPageViews.csv",  row.names=FALSE) 

#visualisation comparatif des années 
dfDatePV <- as.data.frame(dfPageViews$date)
colnames(dfDatePV)[1] <- "date"
daily_data <- dfDatePV  %>%                       #daily_data à partir de dfDatePV
  group_by(date) %>%                              #groupement par date
  mutate(Pageviews = n()) %>%                     #total des pageviews = nombre d'observations / date
  as.data.frame() %>%                             #sur d'avoir une data.frame
  unique() %>%                                    #ligne unique par jour.
  mutate(cnt_ma30 = ma(Pageviews, order=30)) %>%  #variable moyenne mobile (moving average 30 jours)
  mutate(year = format(date,"%Y")) %>%               #creation de la variable year
  mutate(dayOfYear = yday(date))                  #creation de la variable dayOfYear

#comparatifs 2011 - 2018
ggplot() +
  geom_line(data = daily_data, aes(x = dayOfYear, y = cnt_ma30, col=year))  +
  xlab("Numéro de Jour dans l'année") +
  ylab("Nbre pages vues / jour en moyenne mobile ") +
  labs(title = "Les données présentent une saisonnalité : ",
       subtitle = "Le trafic baisse en général en été.",
       caption = "Comparatif Nbre pages vues par jour  par an moy. mob. 30 jours \n Données nettoyées",
       color = "Année")
#sauvegarde du dernier ggplot.
ggsave(filename = "PV-Comparatif-mm30.jpeg",  dpi="print") 


#Sauvegarde de daily_data pour SAS 
write.csv2(daily_data, file = "DailyDataClean.csv",  row.names=FALSE) #sauvegarde en csv avec ;



##########################################################################
# MERCI pour votre attention !
##########################################################################


 