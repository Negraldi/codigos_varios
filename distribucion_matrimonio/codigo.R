#Este código en R descarga dos archivos en formato SPSS (.sav) de una dirección URL específica, los descomprime y los lee en R. Después de leer los archivos, 
#se ajustan algunas variables, como la eliminación de los valores 999 en las columnas "edad_1" y "edad_2", y se crea una variable categórica "tip" que combina 
#las columnas "sexo_1" y "sexo_2".
#Luego, se crea un gráfico de histograma de la edad de matrimonio para hombres y mujeres heterosexuales y homosexuales. Se definen las etiquetas del eje x e y, 
#los colores de relleno y borde de las barras de histograma y los valores máximos del eje y para cada gráfico. Finalmente, se crea un histograma de la edad de 
#matrimonio y se dibujan líneas para la mediana y la moda en cada gráfico. El gráfico final se guarda como un archivo PNG.

library(haven)


url='https://www.ecuadorencifras.gob.ec/documentos/web-inec/Poblacion_y_Demografia/Matrimonios_Divorcios/2021/Base_de_datos_EDV_EMA_2021.zip'
file=tempfile(fileext = ".zip")
download.file(url,file)
unzip(file,exdir = dirname(file))
fls=list.files(dirname(file),full.names = T)
fls=fls[grep("Base_de",fls)]
lapply(fls,function(x) unzip(x,exdir =dirname(file) ))
fls=list.files(dirname(file),full.names = T,recursive = T)
fls=fls[grep("\\.sav",basename(fls))]
edv=haven::read_sav(fls[1])
ema=haven::read_sav(fls[2])

s1=as_factor(ema$sexo_1)
s2=as_factor(ema$sexo_2)
s3=interaction(s1,s2)
levels(s3)=c("H.H","H.M","H.M","M.M")
ema$sexo_1=s1
ema$sexo_2=s2
ema$tip=s3;rm(s1,s2,s3)


ema$edad_1[ema$edad_1==999]=NA
ema$edad_2[ema$edad_2==999]=NA

##GRAFICA
png("tmp2.png",width = 900,height = 1600,res=175)
par(mfrow=c(4,1))

v_main=c(
  "Edad de Matrimonio Heterosexual\nen Hombres (2021)\n\n",
  "Edad de Matrimonio Heterosexual\nen Mujeres (2021)\n\n",
  "Edad de Matrimonio Homosexual\nen Hombres (2021)\n\n",
  "Edad de Matrimonio Homosexual\nen Mujeres (2021)\n\n"
)
v_xlab="Edad en que contrajo Matrimonio"
v_ylab="Porcentaje\n"
v_col=c("lightblue","pink","lightblue","pink")
v_border=c("black","black","#FF00FF","blue")

vf_sexo=c("Hombre","Mujer","Hombre","Mujer")
vf_tip=c("H.M","H.M","H.H","M.M")
v_ymx=c(0.06,0.06,0.095,0.095)


for(i in 1:4) {
  par(mar=c(5.1,5.1,5.1,0))
  plot(c(18,100),c(0,v_ymx[i]),type="n",
       main=v_main[i],
       xlab=v_xlab,
       ylab=v_ylab,
       las=2,axes = F)
  
  axis(1,seq(0,100,by=5),labels = seq(0,100,by=5),las=2)
  axis(2,seq(0,v_ymx[i],by=0.005),
       labels = format(seq(0,v_ymx[i],by=0.005),
                       nsmall=3),las=2)
  
  abline(v=seq(0,100,by=5),col="gray")
  abline(h=seq(0,v_ymx[i],by=0.005),col="gray")
  
  x=c(ema$edad_1[ema$tip==vf_tip[i] & ema$sexo_1==vf_sexo[i]],
      ema$edad_2[ema$tip==vf_tip[i] & ema$sexo_2==vf_sexo[i]])
  
  hst=hist(x,breaks = 60,xlim=c(0,100),probability = T,add=T,
           col=v_col[i],border=v_border[i])
  med=median(x,na.rm = T)
  mod=hst$mids[which.max(hst$counts)]
  abline(v=med,col="red",lwd=2)
  abline(v=mod,col="darkgreen",lwd=2)
  axis(3,round(c(med,mod)),round(c(med,mod)),las=2)
}
dev.off()
