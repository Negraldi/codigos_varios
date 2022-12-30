### 1. Datos de tipo cambio Dolar a Sucre ####
# Se cargan las librerías necesarias
library(curl)
library(readxl)
library(stringi) 
library(dplyr)

# Se descarga el archivo con la información desde la url
url <- "https://contenido.bce.fin.ec/documentos/MercadosInternacionales/Cotizaciones/tipoCambio.xls"
destfile <- "tipoCambio.xls"
curl::curl_download(url, destfile)

# Se leen los datos del archivo descargado
tipo_cambio <- suppressMessages(suppressWarnings(
  read_excel(destfile, col_names = FALSE, 
             col_types = c("text", "numeric", "numeric", 
                           "numeric", "numeric", "numeric", 
                           "numeric", "numeric", "numeric", 
                           "numeric", "numeric", "numeric", 
                           "numeric"), skip = 7)
))

# Se obtiene el número máximo de filas no nulas en el campo 4
nmx=max(which(!is.na(tipo_cambio[,4])))

# Se seleccionan solo las filas correspondientes
tipo_cambio=tipo_cambio[1:nmx,]

# Se asignan nombres a las columnas
colnames(tipo_cambio)=c("mes","Mercado_Oficial.Promedio.Compra",
                        "Mercado_Oficial.Promedio.Venta",
                        "Mercado_Oficial.Fin_de_Periodo.Compra",
                        "Mercado_Oficial.Fin_de_Periodo.Venta",
                        "Mercado_de_intervencion.Promedio.Compra",
                        "Mercado_de_intervencion.Promedio.Venta",
                        "Mercado_de_intervencion.Fin_de_Periodo.Compra",
                        "Mercado_de_intervencion.Fin_de_Periodo.Venta",
                        "Mercado_Libre_de_Cambios.Promedio.Compra",
                        "Mercado_Libre_de_Cambios.Promedio.Venta",
                        "Mercado_Libre_de_Cambios.Fin_de_Periodo.Compra",
                        "Mercado_Libre_de_Cambios.Fin_de_Periodo.Venta")

# Se obtiene un vector para identificar los años
indic_anio=suppressWarnings(!is.na(as.numeric(tipo_cambio$mes)))

# Se asigna el campo anio
tipo_cambio$anio=tipo_cambio$mes[indic_anio][cumsum(indic_anio)]

# Se eliminan las filas correspondientes a los años
tipo_cambio=tipo_cambio[!indic_anio,]

# Se extrae el nombre del mes de la columna mes
tipo_cambio$mes=stringi::stri_extract(tipo_cambio$mes,regex="[A-Za-z]{1,}")

# Se crea un factor correspondiente al nombre del mes
tipo_cambio$mes=
  factor(tipo_cambio$mes,
         c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto",
           "Septiembre","Octubre","Noviembre","Diciembre"))

# Se transforman los campos anio y crear mes_n en base de la columna mes
tipo_cambio$anio=as.integer(tipo_cambio$anio)
tipo_cambio$mes_n=as.integer(tipo_cambio$mes)

# Se crea el campo anio12mes (anio*12+men-1)
tipo_cambio$anio12mes=tipo_cambio$mes_n+tipo_cambio$anio*12-1

# Se crea el campo fecha
tipo_cambio$fch=
  as.Date(paste0(
    floor((tipo_cambio$anio12mes+1)/12),"-",
    ((tipo_cambio$anio12mes+1) %% 12)+1,"-",
    1))-1

# Se reordenan las columnas
tipo_cambio=tipo_cambio[,c("anio","mes","mes_n","anio12mes","fch",colnames(tipo_cambio)[!(colnames(tipo_cambio) %in% c("anio","mes","mes_n","anio12mes","fch"))])]

# Se ordena el dataframe por anio12mes
tipo_cambio=tipo_cambio[order(tipo_cambio$anio12mes),]

# Se cambia el valor del último campo anio12mes
tipo_cambio[tipo_cambio$anio12mes==24002,-c(1:5)]=25000

# Se reemplazan los valores no nulos que son iguales al valor anterior a este con NA 
#Ejem: c(5,5,5,2,5,5,3)->c(5,NA,NA,2,5,NA,3)
tipo_cambio[,-c(1:5)]=
  lapply(tipo_cambio[,-c(1:5)],function(x) {
    x[is.na(x)]=-Inf
    ix=x!=dplyr::lag(x,1,default = -Inf)
    x[!ix]=NA
    x[is.infinite(x)]=NA
    x
  })

# Se elimina el archivo descargado
unlink(destfile)
rm(destfile,indic_anio,nmx,url)

### 2. Datos del IPC del Ecuador ####
# Cargar librerías
library(curl)
library(rvest)
library(readxl)
library(dplyr)

# Leer el HTML de la pagina web
h <- rvest::read_html("https://www.ecuadorencifras.gob.ec/indice-de-precios-al-consumidor/")

# Extraer el enlace a la descarga
h <- rvest::html_node(h,"#content-main > div:nth-child(7) > table:nth-child(2) > tbody > tr:nth-child(2) > td:nth-child(1) > a")
url <- URLencode(rvest::html_attr(h,"href"))
rm(h)

# Establecer el nombre del archivo descargado
destfile <- gsub(" {1,}","_",URLdecode(basename(url)))

# Descargar el archivo ZIP
curl::curl_download(url, destfile)

# Obtener el nombre del archivo dentro del ZIP
lst <- unzip(zipfile = destfile, list = TRUE)
lst <- lst$Name[grep("SERIE HISTORICA IPC",lst$Name)]

# Extraer el archivo del ZIP
files <- lst
unzip(destfile,files =files)

# Leer el archivo
IPC_ec <- read_excel(files,sheet = 2,skip = 5,col_names = F)

# Renombrar las columnas del archivo
colnames(IPC_ec) <- c("anio","Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio",
                      "Agosto","Septiembre","Octubre","Noviembre","Diciembre")

# Obtener el último registro
nmx <- max(which(is.na(IPC_ec$anio)))-1

# Obtener los datos relevantes
IPC_ec <- IPC_ec[1:nmx,]

# Convertir el formato
IPC_ec <- reshape2::melt(IPC_ec,id.vars=c("anio"),value.name = "ipc_ec")
colnames(IPC_ec)[colnames(IPC_ec) %in% "variable"] <- "mes"

# Convertir a variables numéricas
IPC_ec$anio <- as.integer(IPC_ec$anio)
IPC_ec$mes_n <- as.integer(IPC_ec$mes)
IPC_ec$anio12mes <- IPC_ec$mes_n+IPC_ec$anio*12-1

# Crear la fecha
IPC_ec$fch <- as.Date(paste0(
  floor((IPC_ec$anio12mes+1)/12),"-",
  ((IPC_ec$anio12mes+1) %% 12)+1,"-",
  1))-1

# Reordenar las columnas
IPC_ec <- IPC_ec[,c("anio","mes","mes_n","anio12mes","fch",colnames(IPC_ec)[!(colnames(IPC_ec) %in% c("anio","mes","mes_n","anio12mes","fch"))])]

# Convertir el IPC a numérico
IPC_ec$ipc_ec <- as.numeric(IPC_ec$ipc_ec)

# Eliminar los NA's
IPC_ec <- IPC_ec[!is.na(IPC_ec$ipc_ec),]

#Ordenar por anio12mes y resetear el nombre de las filas
IPC_ec <- IPC_ec[order(IPC_ec$anio12mes),]
rownames(IPC_ec)=NULL

# Eliminar los archivos descargados
unlink(URLdecode(basename(url)))
unlink(dirname(files))
rm(destfile,files,lst,nmx,url)

### 3. Datos del IPC de Estados Unidos ####

# Primero cargamos las librerias necesarias
library(rvest) 
library(reshape2)

# Despues obtenemos la URL de la pagina
url <- "https://www.usinflationcalculator.com/inflation/consumer-price-index-and-annual-percent-changes-from-1913-to-2008/"

# Leemos la pagina con read_html
h <- rvest::read_html(url)

# Usamos la funcion html_table para obtener la informacion de la tabla y la guardamos en IPC_us
IPC_us <- html_table(h)[[1]]; rm(h)

# Quitamos cuatro columnas
IPC_us <- IPC_us[-c(1,2), 1:13]

# Renombramos la columnas
colnames(IPC_us) <- c("anio","Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")

# Usamos la funcion melt para aplanar la tabla anterior
IPC_us <- reshape2::melt(IPC_us, id.vars = c("anio"), value.name = "ipc_us")

# Renombramos la columna variable
colnames(IPC_us)[colnames(IPC_us) %in% "variable"] <- "mes"

# Convertimos la columna anio a tipo integer
IPC_us$anio <- as.integer(IPC_us$anio)

# Convertimos la columna mes a tipo integer
IPC_us$mes_n <- as.integer(IPC_us$mes)

# Creamos una nueva columna para los años y meses
IPC_us$anio12mes <- IPC_us$mes_n + IPC_us$anio * 12 - 1

# Creamos una nueva columna con la fecha
IPC_us$fch <- as.Date(paste0(floor((IPC_us$anio12mes+1)/12),"-",((IPC_us$anio12mes+1) %% 12)+1,"-",1))-1

# Reordenamos las columnas
IPC_us <- IPC_us[, c("anio","mes","mes_n","anio12mes","fch",colnames(IPC_us)[!(colnames(IPC_us) %in% c("anio","mes","mes_n","anio12mes","fch"))])]

# Convertimos la columna ipc_us a tipo numeric
IPC_us$ipc_us <- as.numeric(IPC_us$ipc_us)

# Quitamos los valores NA
IPC_us <- IPC_us[!is.na(IPC_us$ipc_us),]

# Reordenamos los valores de la columna anio12mes
IPC_us <- IPC_us[order(IPC_us$anio12mes),]

# Quitamos los nombres de las filas
rownames(IPC_us) <- NULL

### 4. Fuentes Historicas ####
# Se importan las librerias necesarias
library(magrittr)
library(lubridate)
library(readr)

# Se lee la url donde se encuentran los datos
url <- 'https://raw.githubusercontent.com/Negraldi/codigos_varios/master/INFLACION_ESTIMADA_ECUADOR/INDICE_PRECIO_TASA_CAMBIO.txt'

# Se leen 100 lineas del archivo
txt <- readLines(url, n = 100)

# Se busca la palabra normalizacion en el archivo y se extraen los valores correspondientes
n_norm_ind <-  grep("NORMALIZACION", txt)[1] + 1:2
norm_ind <- txt %>%
  .[n_norm_ind] %>%
  strsplit(., "\t") %>%
  simplify2array(.) %>%
  t

# Se colocan los nombres de las columnas
colnames(norm_ind) <- norm_ind[1,]

# Se crea una variable para guardar los nombres de columnas
nm <- colnames(norm_ind)

# Se convierten los valores a numericos
norm_ind <- as.numeric(norm_ind[2,])

# Se colocan los nombres de las columnas
names(norm_ind) <- nm

# Se elimina la variable creada
rm(nm)

# Se busca la palabra unidad en el archivo
n <- grep("UNIDAD", txt)[1] - 1

# Se lee el archivo y se guarda en una variable
FUENT_HIST <- read_delim(url, 
                         delim = "\t", escape_double = FALSE, 
                         col_types = cols(FECHA = col_date(format = "%Y-%m-%d")), 
                         trim_ws = TRUE, skip = n)

# Se elimina las variables creadas
rm(n, txt)

# Se crea una variable para guardar los valores correspondientes a la normalizacion
k <- coalesce(norm_ind[match(FUENT_HIST$CAT, names(norm_ind))], 1)

# Se convierten a numericos los valores y se dividen entre la variable creada
FUENT_HIST$VALOR <- as.numeric(FUENT_HIST$VALOR/k)

# Se elimina la variable creada
rm(k)

# Se reorganizan las columnas del archivo
FUENT_HIST <- dcast(FUENT_HIST, FECHA~CAT, value.var = "VALOR")

# Se crea una nueva variable
FUENT_HIST$fch <- FUENT_HIST$FECHA

# Se elimina la columna FECHA
FUENT_HIST$FECHA <- NULL

# Se crea la columna anio
FUENT_HIST$anio <- lubridate::year(FUENT_HIST$fch)

# Se crea la columna mes_n
FUENT_HIST$mes_n <- lubridate::month(FUENT_HIST$fch)

# Se crea la columna mes
FUENT_HIST$mes <- factor(c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre"))[FUENT_HIST$mes_n]

# Se crea la columna anio12mes
FUENT_HIST$anio12mes <- FUENT_HIST$anio*12 + FUENT_HIST$mes_n - 1

# Se renombran las columnas
colnames(FUENT_HIST)[1:11] <- paste0("VAL_FUENT_HIST.", gsub("\\.{1,}", "_", make.names(colnames(FUENT_HIST)[1:11])))

# Se organizan las columnas
FUENT_HIST <- FUENT_HIST[, c(match(c("anio","mes","mes_n","anio12mes","fch"), colnames(FUENT_HIST)), grep("INDICE", colnames(FUENT_HIST)), grep("TIPO_", colnames(FUENT_HIST)))]

# Se elimina la variable creada
rm(n_norm_ind, url)
