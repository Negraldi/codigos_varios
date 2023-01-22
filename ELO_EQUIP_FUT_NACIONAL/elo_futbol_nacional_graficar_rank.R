source("https://raw.githubusercontent.com/Negraldi/codigos_varios/master/ELO_EQUIP_FUT_NACIONAL/elo_futbol_nacional_descarga.R", echo=TRUE)

library(lubridate)
library(rvest)
library(readr)
library(pbapply)
library(dplyr)
library(reshape2)
library(forcats)
library(ggplot2)


#Definir el equipo ganador de la Copa del Mundo
equip_campeon="AR"

### 1. Camperonato de la Copa del Mundo de Catar 2022 ####
#Filtrar la informacion elo segun el torneo de la copa del mundo y el lugar Catar
z=
elo_hist %>%
  filter(torneo=="WC" & lugar=="QA")

#obtener el elo de los equipos participante en la copa del mundo antes del mundial
z_ant=
elo_hist %>%
  filter((equip %in% z$equip) & !(torneo=="WC" & lugar=="QA") & fch<=max(z$fch)) %>%
  group_by(equip) %>%
  slice_max(order_by = fch)

#Unir la informacion del elo anterior con el score elo durante el mundia
z=rbind(z,z_ant);rm(z_ant)

#Limpiar en caso con un mismo equipo tenga dos partidos en el mismo dia
z=
  z %>% 
  group_by(fch,equip) %>%
  slice_max(order_by = fch)


#Calcular el numero de partido por fecha
z=
z %>%
  group_by() %>%
  mutate(mx_fch=max(fch)) %>%
  group_by(equip) %>%
  arrange(equip,fch) %>%
  mutate(n_part=order(fch)-1) %>%
  #Asignar la final como el partido n. 9 para diferenciar con el partido del tercer lugar
  mutate(n_part=ifelse(fch==mx_fch & n_part==7,8,n_part)) %>%
  ungroup()

#Definir el numero partido maximo o ultimo
ult_part=max(z$n_part)

#Extender el seguimiento elo a los partido que no participa
z_ext=
z %>%
  group_by(equip) %>%
  summarise(
    extrap=list(data.frame(approx(n_part,elo,xout = 0:ult_part,method="constant",rule = 1:2))),
    n_mx=max(n_part)
  ) %>%
  tidyr::unnest(cols = "extrap") %>%
  rename(n_part=x,elo=y) %>%
  mutate(
    Estado=
      case_when(
          (equip==equip_campeon & n_part==ult_part) ~ "Campeón",
          n_part<=n_mx ~ "Participa",
          n_part>n_mx ~ "No Participa"
        )
  ) %>%
  group_by(n_part) %>%
  mutate(Rank=-order(order(-elo))) %>%
  select(-n_mx) %>%
  ungroup()

#Establecer un orden en los equipos en base hasta donde llego en el mundial
ord_equip=
z_ext %>%
  group_by(equip) %>%
  summarise(ult_part=max(n_part[Estado %in% c("Participa","Campeón")]),
            Estado=Estado[ult_part+1])  %>%
  arrange(-ult_part,Estado)

#Dar formato a los equipos segun el orden de los equipos determinado anteriormente
z_ext$equip=factor(z_ext$equip,ord_equip$equip)

## Grafico ####
graf=
  z_ext %>%
  #Solo los datos de los 8 mejores equipos del mundial mas Ecuador, Ghana y Catar
  filter((as.integer(equip)<=8) | (equip=="EC") | (equip=="GH") | (equip=="QA")) %>%
  droplevels() %>%
  #Reasignar los nombres
  mutate(equip=forcats::fct_recode(equip, 
                           Argentina="AR",
                           Francia="FR",
                           Croacia="HR",
                           Marruecos="MA",
                           Brasil="BR",
                           Inglaterra="EN",
                           "Paises Bajos"="NL",
                           Portugal="PT",
                           Ecuador="EC",
                           Ghana="GH",
                           Catar="QA")) %>%
  mutate(n_part=n_part+1) %>%
  #Renombrar las variables
  rename(Equipo=equip,Partido=n_part) %>%
  #Garfico
  ggplot(.)+
  #Lineas horizontales de color oro (la ultima linea tiene mas grosor)
  geom_hline(yintercept=-c(32:1),col=c(rep("transparent",32-1),"gold"),lwd=c(rep(0.5,32-1),0.65))+
  #Lineas Verticales de color oro (la ultima linea tiene mas grosor)
  geom_vline(xintercept=1:9,col=c(rep("transparent",8),"gold"),lwd=c(rep(0.5,8),0.65))+
  #Graficar las linea de evolución por equipo y coloreado por equipo
  geom_path(aes(x=Partido,y=Rank,group=Equipo,col=Equipo),lwd=0.75)+
  #Graficar los puntos de evolución por equipo, tipo de punto por Estado
  geom_point(aes(x=Partido,y=Rank,group=Equipo,col=Equipo,pch=Estado,fill=Equipo),size=4)+
  #Definir los colores de cada equipo
  scale_color_manual(values = (c("lightblue","#28036a",
                                 "#DFB770","darkred",
                                 "#19AE47","grey",
                                 "#F36C21","red","#F6BE00","black",
                                 "#C58080")))+
  scale_fill_manual(values=c("lightblue","#28036a",
                             "#DFB770","darkred",
                             "#19AE47","grey",
                             "#F36C21","red","#F6BE00","black","#C58080"))+
  #Forma de los puntos
  scale_shape_manual(values=c(8,22,23))+
  #Definir las etiquetas del eje horizontal
  scale_x_continuous(breaks = 1:9,
                     labels = c("Antes del Mundial","Despues del \n1er Partido de Grupo",
                                "Despues del \n2do Partido de Grupo",
                                "Despues del \n3er Partido de Grupo",
                                "Despues del \n8vos de Finales",
                                "Despues del \n4tos de Finales",
                                "Despues del \nSemifinales",
                                "Despues del \nTercer Lugar",
                                "Despues de \nLa Final"),minor_breaks = NULL)+
  #Definir las etiquetas del eje vertical
  scale_y_continuous(breaks=-c(32:1),minor_breaks = NULL,labels = 32:1)+
  #Definir los titulos de cada eje
  xlab("Partidos del Mundial")+ylab("Ranking Elo de los Equipos del Mundial")+
  #Temas
  theme_bw()+  theme_minimal() +
  #Etiquetas de los eje horizontal en vertical, leyenda en la parte de arriba
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="top",legend.box = "vertical")


png("grafico23.png",width = round(768/1.5*300/72),height = round(768*16/9/1.5*300/72),res = 400)
graf
dev.off()
file.show("grafico23.png")