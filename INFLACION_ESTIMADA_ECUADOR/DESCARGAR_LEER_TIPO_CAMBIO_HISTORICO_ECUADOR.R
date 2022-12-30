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
