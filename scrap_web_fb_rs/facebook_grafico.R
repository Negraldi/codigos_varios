library(RSelenium)
library(wdman)
library(rvest)
library(plyr)
library(pbapply)
library(parallel)
library(igraph)
library(Rtsne)
library(wordcloud)

gr=igraph::graph_from_data_frame(red,vertices = cbind(id2=1:nrow(rel),rel), directed = F)
gr=induced_subgraph(gr,as.integer(ego(gr,order=1,nodes=1)[[1]])[-1])

while(sum(betweenness(gr,directed = F)==0)!=0) {
  gr=induced_subgraph(gr,which(betweenness(gr,directed = F)!=0))
}

V(gr)$bt=log1p(betweenness(gr,directed = F))
E(gr)$bt=log1p(edge.betweenness(gr,directed = F))

set.seed(1994)
D=distances(gr)
D=Rtsne(as.dist(D),is_distance = T,perplexity=2,theta=0,max_iter=10000)$Y
D=scale(svd(scale(D))$u)

D=as.data.frame(D)
D=cbind(get.data.frame(gr,what = "vertices")[,-5],D)
e=get.edgelist(gr,names = F)
e=as.data.frame(e)
colnames(e)=c("e1","e2")

#Graficar
pdf("a.pdf",width = 2048/72,height = 1493/72)
par(mai=c(0,0,0,0),bg='black')
plot(range(D$V1),range(D$V2), xaxs="i", yaxs="i",xaxt="none",yaxt="none",
     bty="n",col='red',bg='black',type='n')

segments(x0 = D$V1[e[,1]],y0 = D$V2[e[,1]],
         x1 = D$V1[e[,2]],y1=D$V2[e[,2]],col=rgb(0,0,1,1-(1-E(gr)$bt/max(E(gr)$bt))^2))

points(D$V1,D$V2,col='darkgreen',pch=19,cex=D$bt/max(D$bt)^5*5+1 )
textplot(D$V1,D$V2,paste0('|',D$nm,'|'),new = F,col='white',
         cex = (D$bt/max(D$bt))^5*2+1,
         font=c(2),xlim=range(D$V1),ylim=range(D$V2))
dev.off()
file.show("a.pdf")

rm(D,e,gr,sl,cl)
