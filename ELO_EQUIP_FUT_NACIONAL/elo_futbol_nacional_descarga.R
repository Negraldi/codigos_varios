library(lubridate)
library(rvest)
library(readr)
library(pbapply)

### 1. Descargar los Resultados de los partidos con su elo de los equipos nacionales ####

#Urls de las descargas
urls="https://www.eloratings.net/<<x>>_results.tsv"
anio=1901:year(Sys.time())
urls=sapply(anio,function(x) gsub("<<x>>",x,urls,fixed = T))
names(urls)=anio

#Descargar
results=
pblapply(urls,function(xx) {
read_delim(xx, 
           delim = "\t", escape_double = FALSE, 
           col_names = FALSE, trim_ws = TRUE,show_col_types =F,
           na = "")
  })

#Unir los datos
results=plyr::rbind.fill(results)

#Asignar nombre a las columnas de los resultados
colnames(results)=c("Anio","Mes","Dia","Equip1",
                    "Equip2","Score1","Score2","Torneo",
                    "Lugar","ELO_D","ELO1","ELO2",
                    "rank_d1","rank_d2","rank1","rank2")

#Poner formato numerico a los ranking de cada equipo
results$rank_d1=as.numeric(results$rank_d1)
results$rank_d2=as.numeric(results$rank_d2)

### 2. Descargar Diccionario de los equipos Cod y nombres####

#Leer y descargar datos de los equipos
en_teams <- read_delim("https://www.eloratings.net/en.teams.tsv", 
                       delim = "\t", escape_double = FALSE, 
                       col_names = FALSE, trim_ws = TRUE,
                       show_col_types = FALSE,na = "")

#Dividir cuando se tiene mas de un nombre alternativo
en_teams$X2=strsplit(en_teams$X2,"\t")
#Escoger el primer nombre como nombre principal
en_teams$X3=sapply(en_teams$X2,function(x) x[1])
#Darle nombre a las columnas
colnames(en_teams)=c("COD","NOMBRES","NOMB_PRINCIPAL")

### 3. Descargar datos de herencia de los equipos nacionales ####
#Ejemplo: El equipo nacional Ruso actual es heredera del equipo nacional de la Union Sovietica

#Descargo los datos
teams_h<- read_delim("https://www.eloratings.net/teams.tsv", 
                       delim = "\t", escape_double = FALSE, 
                       col_names = FALSE, trim_ws = TRUE,
                       show_col_types = FALSE,na = "")
#Doy nombre a las columnas
colnames(teams_h)=c("COD_F","COD_S")


### 4. Descargar el diccionario de campeonatos de los equipos nacionales ####
#Descargar y leer los datos
tourn<- read_delim("https://www.eloratings.net/en.tournaments.tsv", 
                     delim = "\t", escape_double = FALSE, 
                     col_names = FALSE, trim_ws = TRUE,
                     show_col_types = FALSE,na = "")
#Convertir en lista cuando un campeonantos tiene mas de un nombre alternativo
tourn$X2=strsplit(tourn$X2,"\t")
#Elegir el primer nombre como nombre principal
tourn$X3=sapply(tourn$X2,function(x) x[1])
#Nombres a las columnas
colnames(tourn)=c("COD","NOMBRES","NOMB_PRINCIPAL")

### 5. Descargar datos de la información de campeonatos equivalentes ####
tourn_h<- read_delim("https://www.eloratings.net/tournaments.tsv", 
                   delim = "|", escape_double = FALSE, 
                   col_names = FALSE, trim_ws = TRUE,
                   show_col_types = FALSE,na="")
tourn_h=strsplit(tourn_h$X1,"\t")
results$Mes[results$Mes=="00"]="12"
results$Dia[results$Dia=="00"]=as.character(days_in_month(as.integer(results$Mes[results$Dia=="00"])))
results$Equip1[is.na(results$Equip1)]="NA"
results$Equip2[is.na(results$Equip2)]="NA"
en_teams$COD[is.na(en_teams$COD)]="NA"

### 6. Limpiar y dar formato a datos de los partidos junto con el ELO despues del partido. ####
#Dar formato a fechas
results$fecha=
  as.Date(
    paste0(
      results$Anio,"-",
      results$Mes,"-",
      results$Dia
    ))

#El lugar cuando no se indica coincide con el primer equipo en el partido
results$Lugar[is.na(results$Lugar)]=results$Equip1[is.na(results$Lugar)]

#Convertir la informacion de los partidos a informacion del elo de los equipos
#despues de cada partido en base a la fecha que jugo tal partido
elo_hist=
  rbind(
    data.frame(
      fch=results$fecha,
      equip=results$Equip1,
      elo=results$ELO1,
      torneo=results$Torneo,
      lugar=results$Lugar,
      score=results$Score1,
      dif_score=results$Score1-results$Score2
    ),
    data.frame(
      fch=results$fecha,
      equip=results$Equip2,
      elo=results$ELO2,
      torneo=results$Torneo,
      lugar=results$Lugar,
      score=results$Score2,
      dif_score=results$Score2-results$Score1)
  ) %>%
  group_by(fch,equip) %>%
  summarise(elo=tail(elo,1),torneo=tail(torneo,1),lugar=tail(lugar,1),
            score=tail(score,1),dif_score=tail(dif_score,1))
