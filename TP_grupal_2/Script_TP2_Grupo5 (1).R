##########################################################
#                                                        #
#                       TP2 - CCA                        #
#                                                        #
##########################################################

## Asignatura: Computación Científica Actuarial
## 2er Cuatrimestre de 2020
## Docente: Rodrigo Del Rosso
## Alumnos: Agustina Pacini (889656), Ezequiel Paladini (891492), Facundo López (883195), Vassili Jigalov(890016)

##### SETEO DE CARPETA #####

getwd()

TP2_CCA<-choose.dir()

getwd()

dir()

##### LEEMOS EL ARCHIVO #####

## Datos obtenidos de: https://datos.gob.ar/dataset/sspm-indices-precios-internacionales-productos-basicos

Indices<-read.csv2("indices-precios-internacionales-productos-basicos-mensual.csv",
                   header=T,
                   sep=",",
                   stringsAsFactors=F)
View(Indices)

data.class(Indices)

## CARGA DE PAQUETES ##

install.packages("zoo")
install.packages("tseries")
install.packages("forecast")
install.packages("astsa")
install.packages("Quandl")
install.packages("quantmod")
install.packages("xts")
install.packages("ggplot2")
library(zoo)
library(tseries)
library(forecast)
library(astsa)
library(Quandl)
library(quantmod)
library(xts)
library(ggplot2)

##### CONFIGURAMOS COMO SERIE DE TIEMPO #####

data.class(Indices)

data.class(Indices$petroleo_general)

time<-as.Date(Indices$indice_tiempo)

data.class(time)

Precio<-round(as.numeric(Indices$petroleo_general),2)

data.class(Precio)

is.vector(Precio)

Precio_petroleo<-as.xts(Precio,order.by = time)

data.class(Precio_petroleo)

Precio_petroleo


##### GRÁFICO DE LA VARIABLE EN RELACIÓN AL TIEMPO #####

autoplot(Precio_petroleo, main="Figura 1: Índices de precio del petróleo")+
  labs(x="Tiempo", y="Precio del petróleo")+
  geom_line(color="red", size=1)

## A simple vista ya podemos ver que la serie no es estacionaria dado que la 
## media y la varianza no se mantiene constante en el tiempo, se puede observar
## que presenta una tendencia

## ESTADISTICA DESCRIPTIVA DE LA SERIE DE TIEMPO

data.class(Precio_petroleo)

hist(Precio_petroleo,
     main="Distribucion de la variable",
     ylab="Frecuencia", xlab="Petróleo",
     col="orange",
     border="white")

Est_descriptiva<-function(x){
  Media_x<-mean(x,na.rm=T)
  Varianza_x<-var(x,na.rm=T)
  Desv_Std_x<-sqrt(Varianza_x)
  Datos_ED_x<-c(Media_x,Varianza_x,Desv_Std_x)
  Est_desc_x<-rbind(Datos_ED_x)
  colnames(Est_desc_x)<-c("Media","Varianza","Desvío estándar")
  rownames(Est_desc_x)<-c("Valor")
  Est_desc_x
}

Est_descriptiva(Precio_petroleo)

quantile(Precio_petroleo, probs=c(0.10, 0.25, 0.5, 0.75, 1))


##### FUNCIÓN DE AUTOCORRELACIÓN Y CORRELOGRAMA #####

ggAcf(Precio_petroleo, type="correlation", col="red", size=2)+
  ggtitle("Figura 2: Funcion de Autocorrelacion: Correlacion")

## Disminuye lentamente, por lo tanto, la serie es NO estacionaria
## Dado a esto, vamos a transformarla en una serie estacionaria
## para ello, se utilizará diferencias hasta que se convierta

ndiffs(Precio_petroleo) ## Nos indica cuantas diferenciaciones necesita para ser estacionaria

Petroleo_diferenciado<-diff(Precio_petroleo,1)

ndiffs(Petroleo_diferenciado)

autoplot(Petroleo_diferenciado, main="Figura 3: Serie diferenciada")+
  labs(x="Tiempo")+geom_line(color="red", size=1)

## Podemos ver que se volvió estacionaria porque la media se mantiene constante
## si bien presenta picos en la varianza, como en el 2008 aprox, ya se trata de
## una serie estacionaria porque la media se mantiene constante

ggAcf(Petroleo_diferenciado, col="red", size=2)+
  ggtitle("Figura 4: Funcion de Autocorrelacion (Serie diferenciada)")

## También podemos ver que se transformó en una serie estacionaria dado que tiene
## un decrecimiento exponencial al inicio


##### DESCOMPOSICIÓN DE LA SERIE DE TIEMPO #####

Petroleo_ts<-ts(Precio,star=c(1980,1),end=c(2017,6),frequency=12)

cycle(Petroleo_ts)

Modelo_multiplicativo<-decompose(Petroleo_ts,type="mult")

plot(Modelo_multiplicativo)

## Usamos el modelo multiplicativo en lugar del aditivo porque la varianza no es constante
## El primer gráfico hace referencia a los datos observados
## El segundo es la tendencia de los datos
## El tercero hace referencia a la estacionalidad
## El último nos muestra el error, donde podemos ver que la varianza no es constante a lo largo del tiempo


##### MODELO AUTORREGRESIVO INTEGRADO DE MEDIA MOVIL (ARIMA) #####

## Prueba de estacionariedad: Prueba de Dickey-Fuller

adf.test(Petroleo_ts)

# Como el p-value es mayor a 0.05, la serie NO es estacionaria

Petroleo_diferido_ts<-diff(Petroleo_ts,1)

adf.test(Petroleo_diferido_ts)

# Como el p-value es menor a 0.05, la serie SI es estacionaria

## Correlaciones

ggAcf(Petroleo_diferido_ts, col="red", size=2)+
  ggtitle("Funcion de Autocorrelacion")

# Nos muestra el número de medias moviles (2)

ggAcf(Petroleo_diferido_ts, type="partial", col="red", size=2)+
  ggtitle("Funcion de Autocorrelacion: Parcial")

# El parcial nos muestra el número de autorregresivos (1)

## Armamos el modelo

Modelo<-arima(Petroleo_ts,order=c(1,1,2))

# (1) autorregresivo, (1) diferencia. (2) medias móviles

tsdiag(Modelo) #Diagnóstico del modelo

Box.test(residuals(Modelo),type="Ljung-Box")

## Según prueba realizada de Ljung-Box, existe ruido blanco y nuestro modelo
## se ajusta bien ya que el p-value del test es mayor a 0.05


##### PRONÓSTICOS #####

Pronostico<-forecast::forecast(Modelo,h=12)
Pronostico

## Se predice los siguientes 12 meses (un año). El último mes donde teníamos datos
## es junio del 2017, por lo que se predice hasta junio del 2018)

## La primer columna muestra la fecha y la segunda columna la predicción
## La tercer y cuarta columnas nos muestra el límite inferior y superior con un 80% de confianza
## La quinta y sexta columnas nos muestra el límite inferior y superior con un 95% de confianza

plot(Pronostico, 
     main="Pronóstico de precio del petróleo hasta Junio 2018",
     xlab="Tiempo",
     ylab="Preciod el petróleo")
