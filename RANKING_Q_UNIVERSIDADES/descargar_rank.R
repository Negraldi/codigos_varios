library(readxl)
library(reshape2)
library(dplyr)

url1="https://insights.qs.com/hubfs/Rankings%20Excel%20Reports/2022%20Subject%20Rankings%20Results%20(for%20qs.com).xlsx?utm_medium=email&_hsmi=65742003&_hsenc=p2ANqtz-9Hl0n0qmUQRGLsExlRuLqVCA9wZ3dr0Jl4bXxD-fp2aE7Hxe-QztIAtgBGAifJuJO9NWFrQ7suZe91dFkXW1XGUm7c7Q&utm_content=65742003&utm_source=hs_automation"
url2="https://insights.qs.com/hubfs/Rankings%20Excel%20Reports/2022_QS_World_University_Rankings_Results_public_version.xlsx?utm_medium=email&_hsmi=65742003&_hsenc=p2ANqtz-8CT-Brfg1w_r5zS5ZnKOeDjc_0vYw3OGUK1Ddl1wN2DTRR5kR1zeIJZAIaF9bVEWzID-7GsA2t2MKrjnhZUpRLvNE-3g&utm_content=65742003&utm_source=hs_automation"
fl1=tempfile(fileext = ".xlsx")
fl2=tempfile(fileext = ".xlsx")
download.file(url1,fl1,mode = "wb")
download.file(url2,fl2,mode = "wb")

dat_g <- read_excel(fl2,sheet = "PUBLISHED", skip = 3)
colnames(dat_g)[grep("rank display",colnames(dat_g))]=c("rank display 2022","rank display 2021")
dat_g$rnk=c(1:(nrow(dat_g)))/(nrow(dat_g)+1)
dat_g$`score scaled`=as.numeric(dat_g$`score scaled`)
dat_g$`score scaled`=as.numeric(dat_g$`isr rank`)
colnames(dat_g)=c("rank_pais",
                  "rank_subregion",
                  "rank_2022",
                  "rank_2021",
                  "institucion",
                  "codigo_pais",
                  "pais",
                  "tamanio",
                  "enfoque",
                  "intesidad_investigativa",
                  "antiguedad",
                  "estatus",
                  "score_reputacion_academica",
                  "rank_reputacion_academica",
                  "score_reputacion_empleadora",
                  "rank_reputacion_empleadora",
                  "score_reputacion_estudiantil",
                  "rank_reputacion_estudiantil",
                  "score_reputacion_referencias",
                  "rank_reputacion_referencias",
                  "score_reputacion_internacional_univer",
                  "rank_reputacion_internacional_univer",
                  "score_reputacion_internacional_estudiantil",
                  "rank_reputacion_internacional_estudiantil",
                  "score_escalado",
                  "ranking_01")

dat_g$rank_u=1:nrow(dat_g)
pgs=readxl::excel_sheets(fl1)[-1]
names(pgs)=pgs
dat=
  lapply(pgs,function(pg) {
    read_excel(fl1,sheet = pg, skip = 12)
  })
pgs=rep(pgs,sapply(dat,nrow))
dat=plyr::rbind.fill(dat)
dat$pgs=pgs
dat$...10=NULL
dat=dat[!is.na(dat$Institution),]
dat=
  dat %>%
  dplyr::group_by(pgs) %>%
  mutate(rank_u=1:length(pgs)) %>%
  mutate(ranking_01=(rank_u)/max(rank_u+1)) %>%
  ungroup()

dat$H=as.numeric(dat$H)
dat$International=as.numeric(dat$International)
colnames(dat)=c("rank_2022","rank_2021","Institucion","Locacion","scr_acedemico",
                "scr_empleador","scr_mencion_inv","H_indice","scr_internacional",
                "score_final","materia","rank_u","ranking_01")

ps=sort(unique(tolower(c(dat$Locacion,dat_g$pais))))
ps_cod=rvest::html_table(rvest::read_html("https://www.iban.com/country-codes"))[[1]]
ps_cod$Country=tolower(ps_cod$Country)
ps_ix=match(ps,ps_cod$Country)
ps_ix[is.na(ps_ix)]=
stringdist::amatch(ps[is.na(ps_ix)],
                   trimws(gsub("\\(.{1,}\\)","",ps_cod$Country)),
                   maxDist = 0.24,method = "jw")
ps_ix[is.na(ps_ix)]=
stringdist::amatch(
  gsub("south|north","",ps[is.na(ps_ix)]),
  trimws(gsub("\\(.{1,}\\)","",ps_cod$Country)),
  maxDist = 0.34,method="jw")


ps_cod=ps_cod[ps_ix,]
ps_cod$Country=ps
rm(ps)
dat_g$codigo_pais=factor(ps_cod$`Alpha-2 code`[match(tolower(dat_g$pais),ps_cod$Country)],sort(ps_cod$`Alpha-2 code`))
dat$codigo_pais=factor(ps_cod$`Alpha-2 code`[match(tolower(dat$Locacion),ps_cod$Country)],sort(ps_cod$`Alpha-2 code`))
library(readr)
w_rg <- read_csv("https://michaelminn.net/tutorials/regions-world/world-regions.csv")
w_rg$UNName=NULL
ps_cod=
cbind(ps_cod,
w_rg[match(ps_cod$`Alpha-3 code`,
      w_rg$ISO3),-c(1,2)])
colnames(ps_cod)=make.unique(tolower(make.names(colnames(ps_cod))))


ps_cod[ps_cod$country=="taiwan",c("fourcontinent","fivecontinent",
                                   "uncontinent","unsubregion","unsubregionlabel")]=
  data.frame(fourcontinent="Afro-Eurasia",
             fivecontinent="Asia",
             uncontinent="Asia",
             unsubregion="South-eastern Asia",
             unsubregionlabel="Southeast Asia")

ps_cod[,-c(1:3)]=
  lapply(ps_cod[,-c(1:3)],function(x) {
    if("character" %in% class(x)) {
      factor(x)
    } else  {
      x
    }
  })
