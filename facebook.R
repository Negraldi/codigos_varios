library(RSelenium)
library(wdman)
library(rvest)
library(plyr)
library(pbapply)
library(parallel)
library(igraph)
library(Rtsne)

getLoginDetails <- function(){
  ## Based on code by Barry Rowlingson
  ## http://r.789695.n4.nabble.com/tkentry-that-exits-after-RETURN-tt854721.html#none
  require(tcltk)
  tt <- tktoplevel()
  tkwm.title(tt, "Get login details")
  Name <- tclVar("Login ID")
  Password <- tclVar("Password")
  entry.Name <- tkentry(tt,width="20", textvariable=Name)
  entry.Password <- tkentry(tt, width="20", show="*", 
                            textvariable=Password)
  tkgrid(tklabel(tt, text="Please enter your login details."))
  tkgrid(entry.Name)
  tkgrid(entry.Password)
  
  OnOK <- function()
  { 
    tkdestroy(tt) 
  }
  OK.but <-tkbutton(tt,text=" OK ", command=OnOK)
  tkbind(entry.Password, "<Return>", OnOK)
  tkgrid(OK.but)
  tkfocus(tt)
  tkwait.window(tt)
  
  invisible(c(loginID=tclvalue(Name), password=tclvalue(Password)))
}


INICIAR=function(email,pass,n) {
  #Abrir un programa phatomjs
  n=as.integer(n)
  prc<-wdman::phantomjs(port = n)
  drv<-RSelenium:::remoteDriver(port = n)  
  drv$open()
  
  #abrir facebook y login
  drv$navigate("https://m.facebook.com/")
  el=drv$findElement(using = "css",value = "._56bg._4u9z._5ruq")
  Sys.sleep(runif(1)+1)
  el$sendKeysToElement(sendKeys = list(email))
  Sys.sleep(runif(1)+1)
  el=drv$findElement(using = "css",value = "._56bg._4u9z._27z2")
  Sys.sleep(runif(1)+1)
  el$sendKeysToElement(sendKeys = list(paste0(pass,"\n")))
  Sys.sleep(runif(1)+1)
  list(drv=drv,prc=prc,n=n)
}

aux=function(drv,x,lv) {
  #Abrir la pagina de amigos
  drv$navigate(paste0("https://m.facebook.com/profile.php?v=friends&id=",x))
  
  #bajar en la pagina hasta que el objeto de carga desapareza 
  n_intentos=0
  src=length(drv$findElements(using = "css",value = "._55wo._55x2"))
  vr=T
  k=Sys.time()
  while(vr) {
    vr=grepl('class="seeMoreFriends',drv$getPageSource(),fixed = T)
    drv$executeScript(script ='scrollBy(0,2500000)' )
    
    if(as.numeric(difftime(Sys.time(),k,units = "min"))>1){
      k=Sys.time()
      old=src  
      src=length(drv$findElements(using = "css",value = "._55wo._55x2"))
      
      #Cuando no cambia el numero de bloques y mantiene el objeto de carga por mas de un minuto 
      #Se renicia y vuelve intentar hasta dos veces
      if(old==src){
        if(n_intentos<2){
          drv$refresh()
          drv$navigate("https://m.facebook.com/")
          drv$navigate(paste0("https://m.facebook.com/profile.php?v=friends&id=",x))
        } else {
          vr=F
        }
        n_intentos<-n_intentos+1
      }
    }
  }
  
  #leer las paginas
  src=drv$getPageSource()
  src=read_html(src[[1]])
  src=html_nodes(src,css = '._55wp._4g33._5pxa')
  
  #obtener los nombres, codigos, etc.
  te=html_node(src,'._52jh._5pxc a')
  nam=gsub("\\&.{1,}$|^[a-z]{1,}\\=","",gsub("\\?.{1,}|\\#","",gsub("/","",gsub("id=","",html_attr(te,'href'),fixed = T),fixed = T)))
  
  id=
    gsub("^.{1,}let\\,{1,}id\\:|\\,sc\\:.{1,}$",'',
         gsub('"','',html_attr(html_node(src,css='.touchable.right._41g3'),'data-store'),fixed = T)
    )
  
  nm=html_text(te)
  
  data.frame(id=id,
             nam=nam,
             nm=nm,
             lv=rep(lv,min(length(nm),1)),stringsAsFactors = F)
}

library(tcltk)


credentials <- getLoginDetails()
# Correo y contrasenia
email=credentials[1]
pass=credentials[2]
rm(credentials)

#Numero de puerta
n=1246L
navg=try({INICIAR(email,pass,n)})

#Ir a la pagina de inicio de facebook
Sys.sleep(5)
navg$drv$navigate("https://m.facebook.com/")

#ir a la pagina personal
el=navg$drv$findElement(using = "css",value = "._5xu4 a")
el$clickElement()

#esperar que se carga los elementos
while(!grepl("_39pi _1mh-",navg$drv$getPageSource()[[1]],fixed = T)){}
el=navg$drv$findElement(using = "css",value = "._39pi._1mh-")
id=gsub("^.{1,}\\&id\\=|\\&set\\=.{1,}$","",el$getElementAttribute(attrName = "href")[[1]])
while(!grepl("_42b6 _403j",navg$drv$getPageSource()[[1]],fixed = T)){}
el=navg$drv$findElement(using = "css",value = "._42b6._403j")
while(!grepl("img profpic",navg$drv$getPageSource()[[1]],fixed = T)){}

#Extraer la informacion personal
nm=navg$drv$findElement(using = "css",value = "._391s")$getElementText()[[1]]
nam=gsub("https://m.facebook.com/","",navg$drv$getCurrentUrl()[[1]],fixed = T)



#Nivel personal
lv=0
rel=
  data.frame(
    id=id,
    nam=nam,
    nm=nm,
    lv=lv,stringsAsFactors = F
  )

#Iniciar la lista de amigos en el nivel 0 y su red
w=rel$id[!is.na(rel$nam) & rel$lv==lv & rel$nam!=""]
w=na.omit(w)
red=NULL

#cerrar el navegador inicial
navg$prc$stop()
rm(navg)

cl=makeCluster(detectCores()*2)

for(j in 1:length(cl)) {
  clusterExport(cl,"j")
  clusterEvalQ(cl = cl[j],expr = {
    library(RSelenium)
    library(wdman)
    library(rvest)
    library(plyr)
    library(pbapply)
    library(parallel)
    i=j
    rm(j)
  })
}
n=sample(1000,size = length(cl))+50000
clusterExport(cl,c("n","email","pass","aux","INICIAR"))

clusterEvalQ(cl = cl,expr = {
  n=n[i]
  navg=try({INICIAR(email,pass,n)})
  Sys.sleep(5)
  navg$drv$navigate("https://m.facebook.com/")
  if(grepl('class="_52je _52jb _2t_z"',navg$drv$getPageSource()[[1]],fixed = T)){
    el=navg$drv$findElement(using="css",value="#u_0_3")
    el$clickElement()
    while(!grepl('class="_56bg _55wq _21m8"',navg$drv$getPageSource()[[1]],fixed = T)){}
      el=navg$drv$findElement(using="css",value="._56bg._55wq._21m8")
      el$sendKeysToElement(sendKeys = list(paste0(pass,"\n")))
      Sys.sleep(5)
      navg$drv$navigate("https://m.facebook.com/")
  }
})



#Mientras existan amigos
vr=T
while(vr & lv<2){
  clusterExport(cl,c("lv"))
  
  z=pblapply(w,function(x) 
    try({
      aux(navg$drv,x,lv+1)
    }),cl = cl)
  
  red=rbind(red,
            data.frame(frm=rep(w,sapply(z,nrow)),
                       to=unlist(lapply(z,function(x) x$id)),
                       stringsAsFactors = F))
  
  rel=rbind(rel,rbind.fill(z))
  rel=rel[!duplicated(rel$id),]
  lv<-lv+1
  w=rel$id[!is.na(rel$nam) & rel$lv==lv & rel$nam!=""]
  w=na.omit(w)
  lw=length(w)
  vr=lw!=0
}
rm(z,email,pass)
clusterEvalQ(cl = cl,expr = {navg$prc$stop()})
stopCluster(cl)
rm(cl,w,vr,lw,lv,lw,n,nam,nm,pass,vr,w,el,email,id,j)

red=
matrix(match(as.character(as.matrix(red)),rel$id),ncol = 2)

red=rbind(red,red[,2:1])
red=red[red[,1]<red[,2],]

save.image("red_social.RData")



sl=red[,1]!=1 & (red[,1] %in% red[red[,1]==1,2]) & (red[,2] %in% red[red[,1]==1,2]) 

library(igraph)
temp=as.data.frame(red[sl,])
colnames(temp)=c("from","to")
rel=cbind(id2=1:nrow(rel),rel)
gr=igraph::graph_from_data_frame(temp,
                                 vertices = rel,
                                 directed = F)

cl=components(gr,mode = "strong")
gr<-induced.subgraph(gr,which(cl$membership==which.max(cl$csize)))
gr<-simplify(gr)
V(gr)$bt=log(betweenness(gr)+1)
E(gr)$bt=log(edge.betweenness(gr,directed = F))
E(gr)$bt=E(gr)$bt-min(E(gr)$bt)

#Graficar
set.seed(1994)
D=distances(gr)
D=Rtsne::Rtsne(D,max_iter=10000,pca_scale=T)$Y
D=scale(D)
D=svd(D)$u
D=scale(D)

library(plotly)
library(ggplot2)

D=as.data.frame(D)
D=cbind(get.data.frame(gr,what = "vertices")[,-5],D)
e=as.data.frame(e)
colnames(e)=c("e1","e2")


png("a.png",width = 10000,height = 5625,res = 10000/2500*72)

par(mai=c(0,0,0,0),bg='black')
plot(range(D$V1)*1.1,range(D$V2)*1.1, xaxs="i", yaxs="i",xaxt="none",yaxt="none",
     bty="n",col='red',bg='black',asp=1,type='n')

segments(x0 = D$V1[e[,1]],y0 = D$V2[e[,1]],
         x1 = D$V1[e[,2]],y1=D$V2[e[,2]],col=rgb(1,0,0,1-E(gr)$bt/max(E(gr)$bt)^5))

points(D$V1,D$V2,col='blue',pch=19,cex=D$bt/max(D$bt)^5*10+2 )
textplot(D$V1,D$V2,paste0('|',D$nm,'|'),new = F,col='white',cex = (D$bt/max(D$bt))^5*2.25+0.75,
         font=c(2))
dev.off()
file.show("a.png")

rm(D,e,gr,sl,cl)


