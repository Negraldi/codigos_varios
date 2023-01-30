#Este código en R descarga dos archivos en formato SPSS (.sav) de una dirección URL específica, los descomprime y los lee en R. Después de leer los archivos, 
#se ajustan algunas variables, como la eliminación de los valores 999 en las columnas "edad_1" y "edad_2", y se crea una variable categórica "tip" que combina 
#las columnas "sexo_1" y "sexo_2".
#Luego, se crea un gráfico de histograma de la edad de matrimonio para hombres y mujeres heterosexuales y homosexuales. Se definen las etiquetas del eje x e y, 
#los colores de relleno y borde de las barras de histograma y los valores máximos del eje y para cada gráfico. Finalmente, se crea un histograma de la edad de 
#matrimonio y se dibujan líneas para la mediana y la moda en cada gráfico. El gráfico final se guarda como un archivo PNG.

# Primero, cargamos la libreria "haven"
library(haven)

# Definimos la url donde se encuentra el archivo zip que queremos descargar
url='https://www.ecuadorencifras.gob.ec/documentos/web-inec/Poblacion_y_Demografia/Matrimonios_Divorcios/2021/Base_de_datos_EDV_EMA_2021.zip'

# Creamos un archivo temporal con la extensión .zip
file=tempfile(fileext = ".zip")

# Descargamos el archivo zip a partir de la url y lo guardamos en el archivo temporal creado
download.file(url,file)

# Descomprimimos el archivo zip en el directorio del archivo temporal
unzip(file,exdir = dirname(file))

# Obtenemos una lista de los archivos en el directorio del archivo temporal
fls=list.files(dirname(file),full.names = T)

# Filtramos la lista de archivos para solo obtener aquellos que comienzan con "Base_de"
fls=fls[grep("Base_de",fls)]

# Descomprimimos los archivos filtrados en el directorio del archivo temporal
lapply(fls,function(x) unzip(x,exdir =dirname(file) ))

# Obtenemos una lista de todos los archivos en el directorio y subdirectorios del archivo temporal
fls=list.files(dirname(file),full.names = T,recursive = T)

# Filtramos la lista de archivos para solo obtener aquellos que tienen la extensión .sav
fls=fls[grep("\\.sav",basename(fls))]

# Leemos el primer archivo de la lista de archivos filtrados y lo asignamos a la variable "edv"
edv=haven::read_sav(fls[1])

# Leemos el segundo archivo de la lista de archivos filtrados y lo asignamos a la variable "ema"
ema=haven::read_sav(fls[2])

# Convertimos las variables "sexo_1" y "sexo_2" a factores y las asignamos a las variables "s1" y "s2"
s1=as_factor(ema$sexo_1)
s2=as_factor(ema$sexo_2)

# Creamos una variable "s3" que es la interacción de las variables "s1" y "s2"
s3=interaction(s1,s2)

# Cambiamos los niveles de la variable "s3" a "H.H", "H.M", "M.M"
levels(s3)=c("H.H","H.M","H.M","M.M")

# Asignamos las variables "s1", "s2", y "s3" a las variables "sexo_1", "sexo_2" y "tip" respectiveamente
ema$sexo_1=s1
ema$sexo_2=s2
ema$tip=s3;rm(s1,s2,s3)

# Asignamos los valores 999 a NA en las variables "edad_1" y "edad_2"
ema$edad_1[ema$edad_1==999]=NA
ema$edad_2[ema$edad_2==999]=NA

# Creación de las variables para las etiquetas principales y secundarias
v_main <- c("Edad de Matrimonio Heterosexual en Hombres (2021)\n\n",
            "Edad de Matrimonio Heterosexual en Mujeres (2021)\n\n",
            "Edad de Matrimonio Homosexual en Hombres (2021)\n\n",
            "Edad de Matrimonio Homosexual en Mujeres (2021)\n\n")

v_xlab <- "Edad en que contrajo Matrimonio"
v_ylab <- "Porcentaje\n"
v_col <- c("lightblue", "pink", "lightblue", "pink")
v_border <- c("black", "black", "#FF00FF", "blue")

# Creación de las variables para identificar el tipo de matrimonio y el género
vf_sexo <- c("Hombre", "Mujer", "Hombre", "Mujer")
vf_tip <- c("H.M", "H.M", "H.H", "M.M")
v_ymx <- c(0.06, 0.06, 0.095, 0.095)

# Creación del gráfico
png("tmp2.png", width = 900, height = 1600, res = 175)
par(mfrow = c(4,1))

# Creación de un ciclo para graficar cada una de las cuatro combinaciones de género y tipo de matrimonio
for (i in 1:4) {
  
  # Configuración de las características del gráfico
  par(mar = c(5.1, 5.1, 5.1, 0))
  plot(c(18,100), c(0, v_ymx[i]), type = "n",
       main = v_main[i],
       xlab = v_xlab,
       ylab = v_ylab,
       las = 2, axes = F)
  
  # Agregar ejes y líneas de marcación
  axis(1, seq(0, 100, by = 5), labels = seq(0, 100, by = 5), las = 2)
  axis(2, seq(0, v_ymx[i], by = 0.005),
       labels = format(seq(0, v_ymx[i], by = 0.005), nsmall = 3), las = 2)
  abline(v = seq(0, 100, by = 5), col = "gray")
  abline(h = seq(0, v_ymx[i], by = 0.005), col = "gray")
  
  # Agregar los datos al gráfico
  x=c(ema$edad_1[ema$tip==vf_tip[i] & ema$sexo_1==vf_sexo[i]],
      ema$edad_2[ema$tip==vf_tip[i] & ema$sexo_2==vf_sexo[i]])
  
  #Crear el histograma
  hst=hist(x,breaks = 60,xlim=c(0,100),probability = T,add=T,
           col=v_col[i],border=v_border[i])
  #Calcular la mediana
  med=median(x,na.rm = T)
  #Calcular la moda
  mod=hst$mids[which.max(hst$counts)]
  #Crear la linea roja sobre la mediana
  abline(v=med,col="red",lwd=2)
  #Crear la linea verde oscura sobre la moda
  abline(v=mod,col="darkgreen",lwd=2)
  #Poner encima de la grafica la mediana y la moda aproximada
  axis(3,round(c(med,mod)),round(c(med,mod)),las=2)
}
dev.off()
