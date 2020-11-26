##########################################################
#                                                        #
#                       TP1 - CCA                        #
#                                                        #
##########################################################

# Alumnos: Ezequiel Paladini - Facundo López - Agustina Pacini 
# Docente: Del Rosso Rodrigo
# Materia: Computación científica actuarial 
# Año/Cuatrimestre: 2020/2
#Cod. Mat./Curso: 746/98

##### SETEO DE CARPETA #####

getwd()

TP1_CCA<-choose.dir()

getwd()

dir()


#######################################################
##################### Ejercicio 1 #####################
#######################################################

##LEEMOS EL ARCHIVO##

Sabores_cacao<-read.csv2("flavors_of_cacao.csv",
                         header=T,
                         sep=",",
                         stringsAsFactors=F)

View(Sabores_cacao)

data.class(Sabores_cacao)

#######################################################
##                      1 - A                        ##
#######################################################

################# PORCENTAJE DE CACAO #################

## Convertimos el vector "character" a "numeric"

data.class(Sabores_cacao$Cocoa.Percent)

?sub()

Porcentaje_cacao<-as.numeric(sub("%","",Sabores_cacao$Cocoa.Percent))
Porcentaje_cacao

length(Porcentaje_cacao)

table(Porcentaje_cacao)

Tabla_frecuencia<-function(x){
  Valor_x<-as.numeric(levels(factor(x))) #Valores de la variable
  Frec_abs_x<-as.vector(table(x)) #Columna de frecuencia absoluta
  Frec_acum_abs_x<-cumsum(Frec_abs_x) #Columna de frecuencia absoluta acumulada
  Frec_rel_x<-round(Frec_abs_x/length(x),4) #Columna de frecuencia relativa
  Tabla_x<-data.frame(Valor_x,
                      Frec_abs_x,
                      Frec_acum_abs_x,
                      Frec_rel_x) #Data frame que refleja la tabla de frecuencia
  Tabla_x
}
Tabla_momentos<-function(x){
  Valor_x<-as.numeric(levels(factor(x))) #Valores de la variable
  Frec_abs_x<-as.vector(table(x)) #Columna de frecuencia absoluta
  Frec_acum_abs_x<-cumsum(Frec_abs_x) #Columna de frecuencia absoluta acumulada
  Frec_rel_x<-round(Frec_abs_x/length(x),4) #Columna de frecuencia relativa
  Tabla_x<-data.frame(Valor_x,
                      Frec_abs_x,
                      Frec_acum_abs_x,
                      Frec_rel_x) #Data frame que refleja la tabla de frecuencia
  Total_obs_x<-Tabla_x$Frec_acum_abs_x[length(Valor_x)] #Cantidad de observaciones
  MA1_x<-round(sum((Tabla_x$Valor_x^1)*Tabla_x$Frec_abs_x)/Total_obs_x,4) #Momento absoluto de orden 1
  MA2_x<-round(sum((Tabla_x$Valor_x^2)*Tabla_x$Frec_abs_x)/Total_obs_x,4) #Momento absoluto de orden 2
  MA3_x<-round(sum((Tabla_x$Valor_x^3)*Tabla_x$Frec_abs_x)/Total_obs_x,4) #Momento absoluto de orden 3
  MA4_x<-round(sum((Tabla_x$Valor_x^4)*Tabla_x$Frec_abs_x)/Total_obs_x,4) #Momento absoluto de orden 4
  MC1_x<-round(sum(((Tabla_x$Valor_x-MA1_x)^1)*Tabla_x$Frec_abs_x)/Total_obs_x,4) #Momento centrado de orden 1
  MC2_x<-round(sum(((Tabla_x$Valor_x-MA1_x)^2)*Tabla_x$Frec_abs_x)/Total_obs_x,4) #Momento centrado de orden 2
  MC3_x<-round(sum(((Tabla_x$Valor_x-MA1_x)^3)*Tabla_x$Frec_abs_x)/Total_obs_x,4) #Momento centrado de orden 3
  MC4_x<-round(sum(((Tabla_x$Valor_x-MA1_x)^4)*Tabla_x$Frec_abs_x)/Total_obs_x,4) #Momento centrado de orden 4
  Datos_MA_x<-c(MA1_x,MA2_x,MA3_x,MA4_x) #Vector con los momentos absolutos
  Datos_MC_x<-c(MC1_x,MC2_x,MC3_x,MC4_x) #Vector con los momentos centrados
  Momentos_x<-rbind(Datos_MA_x,Datos_MC_x) #Armamos una matriz con los dos vectores anteriores
  colnames(Momentos_x)<-c("Momento 1","Momento 2","Momento 3","Momento 4")
  rownames(Momentos_x)<-c("Absoluto","Centrado")
  Momentos_x
}
Est_descriptiva<-function(x){
  Media_x<-mean(x,na.rm=T)
  Varianza_x<-var(x,na.rm=T)
  Desv_Std_x<-sqrt(Varianza_x)
  Datos_ED_x<-c(Media_x,Varianza_x,Desv_Std_x)
  Est_desc_x<-rbind(Datos_ED_x)
  colnames(Est_desc_x)<-c("Media","Varianza","Desvío estándar")
  rownames(Est_desc_x)<-c("Dato")
  Est_desc_x
}

Tabla_frecuencia(Porcentaje_cacao)
Tabla_momentos(Porcentaje_cacao)
Est_descriptiva(Porcentaje_cacao)
hist(Porcentaje_cacao,
     right=F,
     ylab="Frecuencia",
     xlab="Cacao(%)",
     lwd=2,
     col="red",
     main="Distribución del porcentaje de cacao")

# Al ser positivo su MC3, se trata de una asimetría hacia la derecha
# Es decir, su cola se alarga para valores mayores a la media



#################### CALIFICACION #####################

Calificacion_cacao<-as.numeric(Sabores_cacao$Rating)
Calificacion_cacao

length(Calificacion_cacao)

table(Calificacion_cacao)

Tabla_frecuencia(Calificacion_cacao)
Tabla_momentos(Calificacion_cacao)
Est_descriptiva(Calificacion_cacao)
hist(Calificacion_cacao,
     breaks=L_x(Calificacion_cacao),
     right=F,
     ylab="Frecuencia",
     xlab="Calificación",
     lwd=2,
     col="red",
     main="Calificación del cacao")

# Al ser negativo su MC3, se trata de una asimetría hacia la izquierda
# Es decir, su cola se alarga para valores menores a la media



################ ORIGEN DE LA FABRICA #################

table(Sabores_cacao$Company.Location)

## Se corregirá los que estén mal escritos
## Se modificaran las ciudades cambiandolas por su país de origen

Fabrica_origen<-Sabores_cacao$Company.Location

Fabrica_origen[Fabrica_origen=="Eucador"]<-"Ecuador"
Fabrica_origen[Fabrica_origen=="Grenada"]<-"Spain"
Fabrica_origen[Fabrica_origen=="Amsterdam"]<-"Netherlands"
Fabrica_origen[Fabrica_origen=="Niacragua"]<-"Nicaragua"
Fabrica_origen[Fabrica_origen=="Sao Tome"]<-"Santo Tome y Principe"

table(Fabrica_origen)

barplot(Frec_FO,
        ylab="Frecuencia",
        lwd=2,
        col="red",
        las=2,
        main="País de origen de la Fábrica")

# El país que fabricó más observaciones de la muestra es, por diferencia, EEUU



################## ORIGEN DEL GRANO ###################

table(Sabores_cacao$Broad.Bean.Origin)

## Tiene muchos datos faltantes, generalizados o mal escritos
## Se buscará trabajar con aquellos datos donde se especifica un solo país
## Se corregirá los que estén mal escritos
## Se agruparán en "Desconocido" a aquellos que no especifican país o incluyan a varios a la vez

Grano_origen<-Sabores_cacao$Broad.Bean.Origin

Grano_origen[Grano_origen==""]<-"Desconocido"
Grano_origen[grepl("Â",Grano_origen)]<-"Desconocido"
Grano_origen[Grano_origen=="Africa, Carribean, C. Am."]<-"Desconocido"
Grano_origen[Grano_origen=="Carribean"]<-"Desconocido"
Grano_origen[Grano_origen=="Carribean(DR/Jam/Tri)"]<-"Desconocido"
Grano_origen[Grano_origen=="Central and S. America"]<-"Desconocido"
Grano_origen[Grano_origen=="Colombia, Ecuador"]<-"Desconocido"
Grano_origen[Grano_origen=="Cost Rica, Ven"]<-"Desconocido"
Grano_origen[Grano_origen=="Criollo"]<-"Desconocido"
Grano_origen[Grano_origen=="Dom. Rep., Madagascar"]<-"Desconocido"
Grano_origen[Grano_origen=="Dominican Rep., Bali"]<-"Desconocido"
Grano_origen[Grano_origen=="DR, Ecuador, Peru"]<-"Desconocido"
Grano_origen[Grano_origen=="Ecuador, Costa Rica"]<-"Desconocido"
Grano_origen[Grano_origen=="Ecuador, Mad., PNG"]<-"Desconocido"
Grano_origen[Grano_origen=="Forastero (Arriba)"]<-"Desconocido"
Grano_origen[Grano_origen=="Forastero (Nacional)"]<-"Desconocido"
Grano_origen[Grano_origen=="Ghana & Madagascar"]<-"Desconocido"
Grano_origen[Grano_origen=="Ghana, Domin. Rep"]<-"Desconocido"
Grano_origen[Grano_origen=="Ghana, Panama, Ecuador"]<-"Desconocido"
Grano_origen[Grano_origen=="Gre., PNG, Haw., Haiti, Mad"]<-"Desconocido"
Grano_origen[Grano_origen=="Guat., D.R., Peru, Mad., PNG"]<-"Desconocido"
Grano_origen[Grano_origen=="Indonesia, Ghana"]<-"Desconocido"
Grano_origen[Grano_origen=="Mad., Java, PNG"]<-"Desconocido"
Grano_origen[Grano_origen=="Madagascar & Ecuador"]<-"Desconocido"
Grano_origen[Grano_origen=="Peru, Belize"]<-"Desconocido"
Grano_origen[Grano_origen=="Peru, Dom. Rep"]<-"Desconocido"
Grano_origen[Grano_origen=="Peru, Ecuador"]<-"Desconocido"
Grano_origen[Grano_origen=="Peru, Ecuador, Venezuela"]<-"Desconocido"
Grano_origen[Grano_origen=="Peru, Mad., Dom. Rep."]<-"Desconocido"
Grano_origen[Grano_origen=="Peru, Madagascar"]<-"Desconocido"
Grano_origen[Grano_origen=="PNG, Vanuatu, Mad"]<-"Desconocido"
Grano_origen[Grano_origen=="South America"]<-"Desconocido"
Grano_origen[Grano_origen=="South America, Africa"]<-"Desconocido"
Grano_origen[Grano_origen=="Trinidad, Ecuador"]<-"Desconocido"
Grano_origen[Grano_origen=="Trinitario"]<-"Desconocido"
Grano_origen[Grano_origen=="Ven, Bolivia, D.R."]<-"Desconocido"
Grano_origen[Grano_origen=="Ven, Trinidad, Ecuador"]<-"Desconocido"
Grano_origen[Grano_origen=="Ven., Indonesia, Ecuad."]<-"Desconocido"
Grano_origen[Grano_origen=="Ven., Trinidad, Mad."]<-"Desconocido"
Grano_origen[Grano_origen=="Ven.,Ecu.,Peru,Nic."]<-"Desconocido"
Grano_origen[Grano_origen=="Venez,Africa,Brasil,Peru,Mex"]<-"Desconocido"
Grano_origen[Grano_origen=="Venezuela, Carribean"]<-"Desconocido"
Grano_origen[Grano_origen=="Venezuela, Dom. Rep."]<-"Desconocido"
Grano_origen[Grano_origen=="Venezuela, Ghana"]<-"Desconocido"
Grano_origen[Grano_origen=="Venezuela, Java"]<-"Desconocido"
Grano_origen[Grano_origen=="Venezuela, Trinidad"]<-"Desconocido"
Grano_origen[Grano_origen=="Venezuela/ Ghana"]<-"Desconocido"
Grano_origen[Grano_origen=="West Africa"]<-"Desconocido"

Grano_origen[Grano_origen=="Domincan Republic"]<-"Dominican Republic"
Grano_origen[Grano_origen=="Peru(SMartin,Pangoa,nacional)"]<-"Peru"
Grano_origen[Grano_origen=="Principe"]<-"Santo Tome y Principe"
Grano_origen[Grano_origen=="Sao Tome"]<-"Santo Tome y Principe"
Grano_origen[Grano_origen=="Sao Tome & Principe"]<-"Santo Tome y Principe"
Grano_origen[Grano_origen=="Tobago"]<-"Trinidad y Tobago"
Grano_origen[Grano_origen=="Trinidad"]<-"Trinidad y Tobago"
Grano_origen[Grano_origen=="Trinidad-Tobago"]<-"Trinidad y Tobago"
Grano_origen[Grano_origen=="Trinidad, Tobago"]<-"Trinidad y Tobago"

table(Grano_origen)

barplot(table(Grano_origen),
        ylab="Frecuencia",
        las=2,
        lwd=2,
        col="red",
        main="País de origen del grano")

# Hay 5 paises que destacan sobre el resto, producen más granos
# República Dominicana, Ecuador, Madagascar, Perú y Venezuela
# No tomamos en cuenta a "Desconocido" dentro de este grupo dado que no consiste en un país sino de un conjunto de paises



#######################################################
##                      1 - B                        ##
#######################################################

##### Nuevo data frame #####

## Primero armamos un nuevo data frame con los datos limpios

data.class(Sabores_cacao)

Cacao<-data.frame(Porcentaje_cacao,Fabrica_origen,Grano_origen,Calificacion_cacao)
Cacao



########## FABRICA DE ORIGEN VS CALIFICACION ##########

FO_vs_C<-table(Cacao$Fabrica_origen,Cacao$Calificacion_cacao)
FO_vs_C

as.factor(labels(table(Cacao$Fabrica_origen)))

Pises_FO<-c("Argentina", "Australia", "Austria", "Belgium", "Bolivia", "Brazil", "Canada", "Chile", "Colombia", "Costa Rica", "Czech Republic", "Denmark", "Domincan Republic", "Ecuador", "Fiji", "Finland", "France", "Germany", "Ghana", "Guatemala", "Honduras", "Hungary", "Iceland", "India", "Ireland", "Israel", "Italy", "Japan", "Lithuania", "Madagascar", "Martinique", "Mexico", "Netherlands", "New Zealand", "Nicaragua", "Peru", "Philippines", "Poland", "Portugal", "Puerto Rico", "Russia", "Santo Tome y Principe", "Scotland", "Singapore", "South Africa", "South Korea", "Spain", "St. Lucia", "Suriname", "Sweden", "Switzerland", "U.K.", "U.S.A.", "Venezuela", "Vietnam", "Wales")
length(Pises_FO)

promedio_FOvsC<-function(x){
  tabla_C_FO<-Cacao[Cacao$Fabrica_origen==x,]
  promedio_pais<-mean(tabla_C_FO$Calificacion_cacao,na.rm=T)
  promedio_pais
}
Tabla_FOvsC<-function(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,
                        aa,ab,ac,ad,ae,af,ag,ah,ai,aj,ak,al,am,an,ao,ap,aq,ar,as,at,au,av,aw,ax,ay,az,
                        ba,bb,bc,bd){
  FxC<-cbind(c(promedio_FOvsC(a),promedio_FOvsC(b),promedio_FOvsC(c),promedio_FOvsC(d),promedio_FOvsC(e),promedio_FOvsC(f),promedio_FOvsC(g),
               promedio_FOvsC(h),promedio_FOvsC(i),promedio_FOvsC(j),promedio_FOvsC(k),promedio_FOvsC(l),promedio_FOvsC(m),promedio_FOvsC(n),
               promedio_FOvsC(o),promedio_FOvsC(p),promedio_FOvsC(q),promedio_FOvsC(r),promedio_FOvsC(s),promedio_FOvsC(t),promedio_FOvsC(u),
               promedio_FOvsC(v),promedio_FOvsC(w),promedio_FOvsC(x),promedio_FOvsC(y),promedio_FOvsC(z),
               promedio_FOvsC(aa),promedio_FOvsC(ab),promedio_FOvsC(ac),promedio_FOvsC(ad),promedio_FOvsC(ae),promedio_FOvsC(af),promedio_FOvsC(ag),
               promedio_FOvsC(ah),promedio_FOvsC(ai),promedio_FOvsC(aj),promedio_FOvsC(ak),promedio_FOvsC(al),promedio_FOvsC(am),promedio_FOvsC(an),
               promedio_FOvsC(ao),promedio_FOvsC(ap),promedio_FOvsC(aq),promedio_FOvsC(ar),promedio_FOvsC(as),promedio_FOvsC(at),promedio_FOvsC(au),
               promedio_FOvsC(av),promedio_FOvsC(aw),promedio_FOvsC(ax),promedio_FOvsC(ay),promedio_FOvsC(az),
               promedio_FOvsC(ba),promedio_FOvsC(bb),promedio_FOvsC(bc),promedio_FOvsC(bd)
               ))
  rownames(FxC)<-Paises_FO
  colnames(FxC)<-c("Promedio")
  FxC
}

Resultado_FOvsC<-Tabla_FOvsC("Argentina", "Australia", "Austria", "Belgium", "Bolivia", "Brazil", "Canada", "Chile", "Colombia", "Costa Rica", "Czech Republic", "Denmark", "Domincan Republic", "Ecuador", "Fiji", "Finland", "France", "Germany", "Ghana", "Guatemala", "Honduras", "Hungary", "Iceland", "India", "Ireland", "Israel", "Italy", "Japan", "Lithuania", "Madagascar", "Martinique", "Mexico", "Netherlands", "New Zealand", "Nicaragua", "Peru", "Philippines", "Poland", "Portugal", "Puerto Rico", "Russia", "Santo Tome y Principe", "Scotland", "Singapore", "South Africa", "South Korea", "Spain", "St. Lucia", "Suriname", "Sweden", "Switzerland", "U.K.", "U.S.A.", "Venezuela", "Vietnam", "Wales")
Resultado_FOvsC

data.class(Resultado_FOvsC)

Resultado_FOvsC[order(Resultado_FOvsC[,"Promedio"],decreasing=T),]

## Los chocolates fabricados en Chile tienen el mayor promedio de calificación



########### GRANO DE ORIGEN VS CALIFICACION ###########

GO_vs_C<-table(Cacao$Grano_origen,Cacao$Calificacion_cacao)
GO_vs_C

as.factor(labels(table(Cacao$Grano_origen)))

Paises_GO<-c("Australia", "Belize", "Bolivia", "Brazil", "Burma", "Cameroon", "Colombia", "Congo", "Costa Rica", "Cuba", "Desconocido", "Dominican Republic", "Ecuador", "El Salvador", "Fiji", "Gabon", "Ghana", "Grenada", "Guatemala", "Haiti", "Hawaii", "Honduras", "India", "Indonesia", "Ivory Coast", "Jamaica", "Liberia", "Madagascar", "Malaysia", "Martinique", "Mexico", "Nicaragua", "Nigeria", "Panama", "Papua New Guinea", "Peru", "Philippines", "Puerto Rico", "Samoa", "Santo Tome y Principe", "Solomon Islands", "Sri Lanka", "St. Lucia", "Suriname", "Tanzania", "Togo", "Trinidad y Tobago", "Uganda", "Vanuatu", "Venezuela", "Vietnam")
length(Paises_GO)

promedio_GOvsC<-function(x){
  tabla_C_GO<-Cacao[Cacao$Grano_origen==x,]
  promedio_pais_G<-mean(tabla_C_GO$Calificacion_cacao,na.rm=T)
  promedio_pais_G
}
Tabla_GOvsC<-function(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,
                      aa,ab,ac,ad,ae,af,ag,ah,ai,aj,ak,al,am,an,ao,ap,aq,ar,as,at,au,av,aw,ax,ay){
  GxC<-cbind(c(promedio_GOvsC(a),promedio_GOvsC(b),promedio_GOvsC(c),promedio_GOvsC(d),promedio_GOvsC(e),promedio_GOvsC(f),promedio_GOvsC(g),
               promedio_GOvsC(h),promedio_GOvsC(i),promedio_GOvsC(j),promedio_GOvsC(k),promedio_GOvsC(l),promedio_GOvsC(m),promedio_GOvsC(n),
               promedio_GOvsC(o),promedio_GOvsC(p),promedio_GOvsC(q),promedio_GOvsC(r),promedio_GOvsC(s),promedio_GOvsC(t),promedio_GOvsC(u),
               promedio_GOvsC(v),promedio_GOvsC(w),promedio_GOvsC(x),promedio_GOvsC(y),promedio_GOvsC(z),
               promedio_GOvsC(aa),promedio_GOvsC(ab),promedio_GOvsC(ac),promedio_GOvsC(ad),promedio_GOvsC(ae),promedio_GOvsC(af),promedio_GOvsC(ag),
               promedio_GOvsC(ah),promedio_GOvsC(ai),promedio_GOvsC(aj),promedio_GOvsC(ak),promedio_GOvsC(al),promedio_GOvsC(am),promedio_GOvsC(an),
               promedio_GOvsC(ao),promedio_GOvsC(ap),promedio_GOvsC(aq),promedio_GOvsC(ar),promedio_GOvsC(as),promedio_GOvsC(at),promedio_GOvsC(au),
               promedio_GOvsC(av),promedio_GOvsC(aw),promedio_GOvsC(ax),promedio_GOvsC(ay)
               ))
  rownames(GxC)<-Paises_GO
  colnames(GxC)<-c("Promedio")
  GxC
}

Resultado_GOvsC<-Tabla_GOvsC("Australia", "Belize", "Bolivia", "Brazil", "Burma", "Cameroon", "Colombia", "Congo", "Costa Rica", "Cuba", "Desconocido", "Dominican Republic", "Ecuador", "El Salvador", "Fiji", "Gabon", "Ghana", "Grenada", "Guatemala", "Haiti", "Hawaii", "Honduras", "India", "Indonesia", "Ivory Coast", "Jamaica", "Liberia", "Madagascar", "Malaysia", "Martinique", "Mexico", "Nicaragua", "Nigeria", "Panama", "Papua New Guinea", "Peru", "Philippines", "Puerto Rico", "Samoa", "Santo Tome y Principe", "Solomon Islands", "Sri Lanka", "St. Lucia", "Suriname", "Tanzania", "Togo", "Trinidad y Tobago", "Uganda", "Vanuatu", "Venezuela", "Vietnam")
Resultado_GOvsC

data.class(Resultado_GOvsC)

Resultado_GOvsC[order(Resultado_GOvsC[,"Promedio"],decreasing=T),]

## Los granos cosechados en las Islas Salomón tienen el mayor promedio de calificación



######### PORCENTAJE DE CACAO VS CALIFICACION #########

## Analisis de correlación 

Cacao2<-data.frame(Cacao$Porcentaje_cacao,Cacao$Calificacion_cacao)
names(Cacao2)
Cacao2<-Cacao2[!is.na(Cacao2$Cacao.Porcentaje_cacao),]
Cacao2<-Cacao2[!is.na(Cacao2$Cacao.Calificacion_cacao),]

cor(Cacao2)

pairs(Cacao2)

# Baja correlacion entre las variables, además se trata de una relación inversa

## Grafico de dispersión (nube de puntos)

plot(Cacao$Calificacion_cacao,Cacao$Porcentaje_cacao,
     ylab="Porcentaje de cacao",
     xlab="Calificación",
     main="Gráfico de dispersión: Cacao(%) vs Calificación")

regresion<-lm(Cacao.Porcentaje_cacao~Cacao.Calificacion_cacao,Cacao2) #Planteamos que la calificación depende del (%) de cacao
summary(regresion)

# Sólo un 2% de los valores están siendo explicados por este modelo (Multiple R-squared:  0.02717), es muy bajo

## Bondad de ajuste

anova(regresion)

plot(Cacao$Calificacion_cacao,Cacao$Porcentaje_cacao,
     ylab="Porcentaje de cacao",
     xlab="Calificación",
     main="Gráfico de dispersión: Cacao(%) vs Calificación")
abline(regresion)

## Como se calculó con el summary, tiene pendiente negativa (-2.1800)

par(mfrow=c(2,2))
plot(lm(Cacao.Porcentaje_cacao~Cacao.Calificacion_cacao,Cacao2))

par(mfrow=c(1,1))
plot(lm(Cacao.Porcentaje_cacao~Cacao.Calificacion_cacao,Cacao2))

# Aleatoriedad (Residuals vs fitted)
# Podemos ver que son datos aleatorios, no presentan un patrón

# Distribucion normal (Normal Q-Q)
# Todos los datos próximos a la línea se distribuyen normalmente. Los que se alejan son datos anómalos

# Homocedasticidad (Scale-Location)
# No se observa una tendencia

# Distancia de Cook (Residuals vs Leverage) 
# Mide el efecto que tiene una observación sobre el conjunto de coeficientes en un modelo lineal.



###################### OUTLIERS #######################

boxplot(Cacao2$Cacao.Porcentaje_cacao,horizontal=T)
boxplot.stats(Cacao2$Cacao.Porcentaje_cacao)

boxplot(Cacao2$Cacao.Calificacion_cacao,horizontal=T)
boxplot.stats(Cacao2$Cacao.Calificacion_cacao)

# Aquellos valores que están por afuera de las lineas se tratan de outliers
# $stats: Son los valores que definen el gráfico ("bigote inferior" - Límite inferior de la caja - La mediana del conjunto - Límite superior - "bigote" superior) )
# $n: Cantidad de observaciones
# $conf: Intervalos de confianza al 95%
# $out: Lista de outliers

install.packages("car")
library(car)

# REGRESION: Existen outliers? + Ver en un grafico aquellos valores influyentes

outlierTest(regresion)

influenceIndexPlot(regresion)



#######################################################
##                      1 - C                        ##
#######################################################

##### Respuestas #####

## (A) Los granos cosechados en las Islas Salomón tienen el mayor promedio de calificación

## (B) Los chocolates fabricados en Chile tienen el mayor promedio de calificación

## (C) La correlación entre las variables es baja, además presentan una relación inversa, es decir, con pendiente negativa



#######################################################
##################### Ejercicio 2 #####################
#######################################################

##### Variable: Tipo de grano #####

## "Limpiamos" los valores de la variable Tipo de grano para poder trabajar con dicha variable

table(Sabores_cacao$Bean.Type)

## Tiene muchos datos faltantes o generalizados
## Se agrupará según tipo de cacao
## Aquellos que incluyan dos o más tipo se eligira el primero que se nombre

Tipo_grano<-Sabores_cacao$Bean.Type

Tipo_grano[Tipo_grano==""]<-"Desconocido"
Tipo_grano[grepl("Â",Tipo_grano)]<-"Desconocido"
Tipo_grano[Tipo_grano=="Amazon mix"]<-"Amazon"
Tipo_grano[Tipo_grano=="Amazon, ICS"]<-"Amazon"
Tipo_grano[Tipo_grano=="Blend-Forastero,Criollo"]<-"Blend"
Tipo_grano[Tipo_grano=="Criollo (Amarru)"]<-"Criollo"
Tipo_grano[Tipo_grano=="Criollo (Ocumare 61)"]<-"Criollo"
Tipo_grano[Tipo_grano=="Criollo (Ocumare 67)"]<-"Criollo"
Tipo_grano[Tipo_grano=="Criollo (Ocumare 77)"]<-"Criollo"
Tipo_grano[Tipo_grano=="Criollo (Ocumare)"]<-"Criollo"
Tipo_grano[Tipo_grano=="Criollo (Porcelana)"]<-"Criollo"
Tipo_grano[Tipo_grano=="Criollo (Wild)"]<-"Criollo"
Tipo_grano[Tipo_grano=="Criollo, +"]<-"Criollo"
Tipo_grano[Tipo_grano=="Criollo, Forastero"]<-"Criollo"
Tipo_grano[Tipo_grano=="Criollo, Trinitario"]<-"Criollo"
Tipo_grano[Tipo_grano=="Forastero (Amelonado)"]<-"Forastero"
Tipo_grano[Tipo_grano=="Forastero (Arriba)"]<-"Forastero"
Tipo_grano[Tipo_grano=="Forastero (Arriba) ASS"]<-"Forastero"
Tipo_grano[Tipo_grano=="Forastero (Arriba) ASSS"]<-"Forastero"
Tipo_grano[Tipo_grano=="Forastero (Catongo)"]<-"Forastero"
Tipo_grano[Tipo_grano=="Forastero (Nacional)"]<-"Forastero"
Tipo_grano[Tipo_grano=="Forastero (Parazinho)"]<-"Forastero"
Tipo_grano[Tipo_grano=="Forastero(Arriba, CCN)"]<-"Forastero"
Tipo_grano[Tipo_grano=="Forastero, Trinitario"]<-"Forastero"
Tipo_grano[Tipo_grano=="Nacional (Arriba)"]<-"Nacional"
Tipo_grano[Tipo_grano=="Trinitario (85% Criollo)"]<-"Trinitario"
Tipo_grano[Tipo_grano=="Trinitario (Amelonado)"]<-"Trinitario"
Tipo_grano[Tipo_grano=="Trinitario (Scavina)"]<-"Trinitario"
Tipo_grano[Tipo_grano=="Trinitario, Criollo"]<-"Trinitario"
Tipo_grano[Tipo_grano=="Trinitario, Forastero"]<-"Trinitario"
Tipo_grano[Tipo_grano=="Trinitario, Nacional"]<-"Trinitario"
Tipo_grano[Tipo_grano=="Trinitario, TCGA"]<-"Trinitario"

table(Tipo_grano)

Cacao<-cbind(Cacao,Tipo_grano)

TG_vs_C<-table(Cacao$Tipo_grano,Cacao$Calificacion_cacao)
TG_vs_C

as.factor(labels(table(Cacao$Tipo_grano)))

Paises_TG<-c("Amazon", "Beniano", "Blend", "CCN51", "Criollo", "Desconocido", "EET", "Forastero", "Matina", "Nacional", "Trinitario")
length(Paises_TG)

promedio_TGvsC<-function(x){
  tabla_C_TG<-Cacao[Cacao$Tipo_grano==x,]
  promedio_pais_G<-mean(tabla_C_TG$Calificacion_cacao,na.rm=T)
  promedio_pais_G
}
Tabla_TGvsC<-function(a,b,c,d,e,f,g,h,i,j,k){
  TxC<-cbind(c(promedio_TGvsC(a),promedio_TGvsC(b),promedio_TGvsC(c),promedio_TGvsC(d),promedio_TGvsC(e),promedio_TGvsC(f),promedio_TGvsC(g),
               promedio_TGvsC(h),promedio_TGvsC(i),promedio_TGvsC(j),promedio_TGvsC(k)
  ))
  rownames(TxC)<-Paises_TG
  colnames(TxC)<-c("Promedio")
  TxC
}

Resultado_TGvsC<-Tabla_TGvsC("Amazon", "Beniano", "Blend", "CCN51", "Criollo", "Desconocido", "EET", "Forastero", "Matina", "Nacional", "Trinitario")
Resultado_TGvsC

data.class(Resultado_TGvsC)

Resultado_TGvsC[order(Resultado_TGvsC[,"Promedio"],decreasing=T),]



##################### EJ 2 - (1) ######################

#### Armamos un data frame del dataset original pero con los datos "limpios"

## Se omitirán las columnas del fabricante y nombre de la barra (columnas 1 y 2)
## Dado que presentan muchos valores posibles dentro de la variable y no se puede establecer una relación entre ellas
## También se omitirá la columba del número de referencia (3) dado que no ofrece ningun tipo de información

Cacao3<-data.frame(Calificacion_cacao,Porcentaje_cacao,Fabrica_origen,Grano_origen,Tipo_grano)
Cacao3

#### VARIABLE OBJETIVO: "Rating" ---> Variable dependiente

## Armamos dos data frame, uno para trabajar a la VO como v.continua y el otro para trabajarla como v.discreta

Cacao_C<-Cacao3
Cacao_D<-Cacao3

## Al data frame de la v. discreta, agarramos la variable "Rating" y cambiamos sus valores
## Para cuando sea 5, lo cambiamos a 1. Para el resto modificamos a 0

Cacao_D$Calificacion_cacao[Cacao_D$Calificacion_cacao<5]<-0
Cacao_D$Calificacion_cacao[Cacao_D$Calificacion_cacao==5]<-1
Cacao_D

## Armamos una lista con los dos dataset

List_cacao<-list(Cacao_C,Cacao_D)
List_cacao



##################### EJ 2 - (2) ######################

#### Para el data frame donde vamos a trabajar a la V.Obj como una v. continua
#### Se agrupará a las demás variables en intervalos cuyas calificaciones sean similares
#### Para esto se analizará el promedio de calificación que adopte cada valor de cada
#### variable y se las irá agrupando en intervalos

## Por incisos anteriores, tenemos una tabla de promedio para todas las variables menor
## para la variable del porcentaje, así que armaremos una

P_vs_C<-table(Cacao$Porcentaje_cacao,Cacao$Calificacion_cacao)
P_vs_C

as.factor(labels(table(Cacao$Porcentaje_cacao)))

Paises_P<-c(42, 46, 50, 53, 55, 56, 57, 58, 60, 60.5, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 72.5, 73, 73.5, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 99, 100)
length(Paises_P)

promedio_PvsC<-function(x){
  tabla_C_P<-Cacao[Cacao$Porcentaje_cacao==x,]
  promedio_pais_P<-mean(tabla_C_P$Calificacion_cacao,na.rm=T)
  promedio_pais_P
}
Tabla_PvsC<-function(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,
                     aa,ab,ac,ad,ae,af,ag,ah,ai,aj,ak,al,am,an,ao,ap,aq,ar,as){
  PxC<-cbind(c(promedio_PvsC(a),promedio_PvsC(b),promedio_PvsC(c),promedio_PvsC(d),promedio_PvsC(e),promedio_PvsC(f),promedio_PvsC(g),
               promedio_PvsC(h),promedio_PvsC(i),promedio_PvsC(j),promedio_PvsC(k),promedio_PvsC(l),promedio_PvsC(m),promedio_PvsC(n),
               promedio_PvsC(o),promedio_PvsC(p),promedio_PvsC(q),promedio_PvsC(r),promedio_PvsC(s),promedio_PvsC(t),promedio_PvsC(u),
               promedio_PvsC(v),promedio_PvsC(w),promedio_PvsC(x),promedio_PvsC(y),promedio_PvsC(z),
               promedio_PvsC(aa),promedio_PvsC(ab),promedio_PvsC(ac),promedio_PvsC(ad),promedio_PvsC(ae),promedio_PvsC(af),promedio_PvsC(ag),
               promedio_PvsC(ah),promedio_PvsC(ai),promedio_PvsC(aj),promedio_PvsC(ak),promedio_PvsC(al),promedio_PvsC(am),promedio_PvsC(an),
               promedio_PvsC(ao),promedio_PvsC(ap),promedio_PvsC(aq),promedio_PvsC(ar),promedio_PvsC(as)
  ))
  rownames(PxC)<-Paises_P
  colnames(PxC)<-c("Promedio")
  PxC
}

Resultado_PvsC<-Tabla_PvsC(42, 46, 50, 53, 55, 56, 57, 58, 60, 60.5, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 72.5, 73, 73.5, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 99, 100)
Resultado_PvsC

data.class(Resultado_PvsC)

Resultado_PvsC[order(Resultado_PvsC[,"Promedio"],decreasing=T),]


## Analizaremos los promedio para armar intervalos de valor en las variables según su calificación

Resultado_PvsC[order(Resultado_PvsC[,"Promedio"],decreasing=T),]

# [Intervalo 1] = 42, 46, 57, 60.5, 72.5, 89, 99, 100, 91, 53
# [Intervalo 2] = 60, 79, 85, 90, 62, 76, 83, 55, 81, 84
# [Intervalo 3] = 75, 73, 65, 58, 73.5, 71, 77, 82, 61, 80
# [Intervalo 4] = 78, 68, 70, 56, 86, 87, 74, 88, 64, 72
# [Intervalo 5] = 50, 63, 69, 66, 67

Resultado_FOvsC[order(Resultado_FOvsC[,"Promedio"],decreasing=T),]

# [Intervalo 1] = Santo Tome y Principe, Peru, Ireland, Czech Republic, Ghana, Martinique, Portugal, Wales, Mexico, South Africa, Puerto Rico, India
# [Intervalo 2] = Colombia, Lithuania, U.S.A., Madagascar, Costa Rica, Belgium, Japan, U.K., Sweden, Ecuador, St. Lucia, Nicaragua
# [Intervalo 3] = Israel, Russia, Suriname, Austria, Spain, Honduras, Hungary, Domincan Republic, South Korea, New Zealand, Germany, Venezuela
# [Intervalo 4] = Guatemala, Switzerland, Singapore, Italy, Scotland, Canada, Argentina, Denmark, France, Bolivia, Fiji, Finland
# [Intervalo 5] = Chile, Netherlands, Philippines, Iceland, Vietnam, Brazil, Poland, Australia

Resultado_GOvsC[order(Resultado_GOvsC[,"Promedio"],decreasing=T),]

# [Intervalo 1] = Grenada, Mexico, Burma, Philippines, St. Lucia, El Salvador, Sri Lanka, Uganda, Ivory Coast, Martinique, Puerto Rico
# [Intervalo 2] = Jamaica, Santo Tome y Principe, Costa Rica, Peru, Ecuador, Fiji, India, Samoa, Ghana, Liberia, Togo, Desconocido
# [Intervalo 3] = Vanuatu, Venezuela, Belize, Colombia, Trinidad y Tobago, Hawaii, Dominican Republic, Tanzania, Nicaragua, Bolivia, Indonesia, Panama
# [Intervalo 4] = Madagascar, Australia, Cameroon, Cuba, Gabon, Malaysia, Nigeria, Suriname
# [Intervalo 5] = Solomon Islands, Haiti, Honduras, Congo, Guatemala, Vietnam, Papua New Guinea, Brazil

Resultado_TGvsC[order(Resultado_TGvsC[,"Promedio"],decreasing=T),]

# [Intervalo 1] = Forastero, Desconocido, Nacional
# [Intervalo 2] = Trinitario, Criollo
# [Intervalo 3] = Blend, Matina
# [Intervalo 4] = CCN51, EET, Beniano
# [Intervalo 5] = Amazon


## Una vez definidos los intervalos, reemplazaremos los valores de cada variable por
## el número de su respectivo intervalo en el data frame de la v. continua

Prom_grupo1<-round(mean(c(2.750000, 2.750000, 2.750000, 2.750000, 2.750000, 2.625000, 2.625000, 2.250000, 2.166667, 2.000000)),2)
Prom_grupo1
Cacao_C$Porcentaje_cacao[Cacao_C$Porcentaje_cacao==42]<-Prom_grupo1
Cacao_C$Porcentaje_cacao[Cacao_C$Porcentaje_cacao==46]<-Prom_grupo1
Cacao_C$Porcentaje_cacao[Cacao_C$Porcentaje_cacao==57]<-Prom_grupo1
Cacao_C$Porcentaje_cacao[Cacao_C$Porcentaje_cacao==60.5]<-Prom_grupo1
Cacao_C$Porcentaje_cacao[Cacao_C$Porcentaje_cacao==72.5]<-Prom_grupo1
Cacao_C$Porcentaje_cacao[Cacao_C$Porcentaje_cacao==89]<-Prom_grupo1
Cacao_C$Porcentaje_cacao[Cacao_C$Porcentaje_cacao==99]<-Prom_grupo1
Cacao_C$Porcentaje_cacao[Cacao_C$Porcentaje_cacao==100]<-Prom_grupo1
Cacao_C$Porcentaje_cacao[Cacao_C$Porcentaje_cacao==91]<-Prom_grupo1
Cacao_C$Porcentaje_cacao[Cacao_C$Porcentaje_cacao==53]<-Prom_grupo1
Prom_grupo2<-round(mean(c(3.005814, 3.000000, 2.986111, 2.968750, 2.964286, 2.945652, 2.937500, 2.859375, 2.850000, 2.812500)),2)
Prom_grupo2
Cacao_C$Porcentaje_cacao[Cacao_C$Porcentaje_cacao==60]<-Prom_grupo2
Cacao_C$Porcentaje_cacao[Cacao_C$Porcentaje_cacao==79]<-Prom_grupo2
Cacao_C$Porcentaje_cacao[Cacao_C$Porcentaje_cacao==85]<-Prom_grupo2
Cacao_C$Porcentaje_cacao[Cacao_C$Porcentaje_cacao==90]<-Prom_grupo2
Cacao_C$Porcentaje_cacao[Cacao_C$Porcentaje_cacao==62]<-Prom_grupo2
Cacao_C$Porcentaje_cacao[Cacao_C$Porcentaje_cacao==76]<-Prom_grupo2
Cacao_C$Porcentaje_cacao[Cacao_C$Porcentaje_cacao==83]<-Prom_grupo2
Cacao_C$Porcentaje_cacao[Cacao_C$Porcentaje_cacao==55]<-Prom_grupo2
Cacao_C$Porcentaje_cacao[Cacao_C$Porcentaje_cacao==81]<-Prom_grupo2
Cacao_C$Porcentaje_cacao[Cacao_C$Porcentaje_cacao==84]<-Prom_grupo2
Prom_grupo3<-round(mean(c(3.177928, 3.175000, 3.169872, 3.125000, 3.125000, 3.088710, 3.060606, 3.058824, 3.031250, 3.027778)),2)
Prom_grupo3
Cacao_C$Porcentaje_cacao[Cacao_C$Porcentaje_cacao==75]<-Prom_grupo3
Cacao_C$Porcentaje_cacao[Cacao_C$Porcentaje_cacao==73]<-Prom_grupo3
Cacao_C$Porcentaje_cacao[Cacao_C$Porcentaje_cacao==65]<-Prom_grupo3
Cacao_C$Porcentaje_cacao[Cacao_C$Porcentaje_cacao==58]<-Prom_grupo3
Cacao_C$Porcentaje_cacao[Cacao_C$Porcentaje_cacao==73.5]<-Prom_grupo3
Cacao_C$Porcentaje_cacao[Cacao_C$Porcentaje_cacao==71]<-Prom_grupo3
Cacao_C$Porcentaje_cacao[Cacao_C$Porcentaje_cacao==77]<-Prom_grupo3
Cacao_C$Porcentaje_cacao[Cacao_C$Porcentaje_cacao==82]<-Prom_grupo3
Cacao_C$Porcentaje_cacao[Cacao_C$Porcentaje_cacao==61]<-Prom_grupo3
Cacao_C$Porcentaje_cacao[Cacao_C$Porcentaje_cacao==80]<-Prom_grupo3
Prom_grupo4<-round(mean(c(3.338235, 3.287234, 3.276042, 3.250000, 3.250000, 3.250000, 3.235000, 3.218750, 3.191176, 3.190476)),2)
Prom_grupo4
Cacao_C$Porcentaje_cacao[Cacao_C$Porcentaje_cacao==78]<-Prom_grupo4
Cacao_C$Porcentaje_cacao[Cacao_C$Porcentaje_cacao==68]<-Prom_grupo4
Cacao_C$Porcentaje_cacao[Cacao_C$Porcentaje_cacao==70]<-Prom_grupo4
Cacao_C$Porcentaje_cacao[Cacao_C$Porcentaje_cacao==56]<-Prom_grupo4
Cacao_C$Porcentaje_cacao[Cacao_C$Porcentaje_cacao==86]<-Prom_grupo4
Cacao_C$Porcentaje_cacao[Cacao_C$Porcentaje_cacao==87]<-Prom_grupo4
Cacao_C$Porcentaje_cacao[Cacao_C$Porcentaje_cacao==74]<-Prom_grupo4
Cacao_C$Porcentaje_cacao[Cacao_C$Porcentaje_cacao==88]<-Prom_grupo4
Cacao_C$Porcentaje_cacao[Cacao_C$Porcentaje_cacao==64]<-Prom_grupo4
Cacao_C$Porcentaje_cacao[Cacao_C$Porcentaje_cacao==72]<-Prom_grupo4
Prom_grupo5<-round(mean(c(3.750000, 3.604167, 3.500000, 3.380435, 3.351852)),2)
Prom_grupo5
Cacao_C$Porcentaje_cacao[Cacao_C$Porcentaje_cacao==50]<-Prom_grupo5
Cacao_C$Porcentaje_cacao[Cacao_C$Porcentaje_cacao==63]<-Prom_grupo5
Cacao_C$Porcentaje_cacao[Cacao_C$Porcentaje_cacao==69]<-Prom_grupo5
Cacao_C$Porcentaje_cacao[Cacao_C$Porcentaje_cacao==66]<-Prom_grupo5
Cacao_C$Porcentaje_cacao[Cacao_C$Porcentaje_cacao==67]<-Prom_grupo5
Cacao_C$Porcentaje_cacao<-as.numeric(Cacao_C$Porcentaje_cacao)
Cacao_C$Porcentaje_cacao

Prom_grupo1<-round(mean(c(2.937500, 2.897059, 2.812500, 2.750000, 2.750000, 2.750000, 2.750000, 2.750000, 2.687500, 2.666667, 2.625000, 2.500000)),2)
Prom_grupo1
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="Santo Tome y Principe"]<-Prom_grupo1
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="Peru"]<-Prom_grupo1
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="Ireland"]<-Prom_grupo1
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="Czech Republic"]<-Prom_grupo1
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="Ghana"]<-Prom_grupo1
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="Martinique"]<-Prom_grupo1
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="Portugal"]<-Prom_grupo1
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="Wales"]<-Prom_grupo1
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="Mexico"]<-Prom_grupo1
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="South Africa"]<-Prom_grupo1
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="Puerto Rico"]<-Prom_grupo1
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="India"]<-Prom_grupo1
Prom_grupo2<-round(mean(c(3.173913, 3.166667, 3.154123, 3.147059, 3.138889, 3.093750, 3.088235, 3.054688, 3.050000, 3.009091, 3.000000, 2.958333)),2)
Prom_grupo2
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="Colombia"]<-Prom_grupo2
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="Lithuania"]<-Prom_grupo2
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="U.S.A."]<-Prom_grupo2
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="Madagascar"]<-Prom_grupo2
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="Costa Rica"]<-Prom_grupo2
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="Belgium"]<-Prom_grupo2
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="Japan"]<-Prom_grupo2
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="U.K."]<-Prom_grupo2
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="Sweden"]<-Prom_grupo2
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="Ecuador"]<-Prom_grupo2
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="St. Lucia"]<-Prom_grupo2
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="Nicaragua"]<-Prom_grupo2
Prom_grupo3<-round(mean(c(3.250000, 3.250000, 3.250000, 3.240385, 3.223214, 3.208333, 3.204545, 3.200000, 3.200000, 3.191176, 3.178571, 3.175000)),2)
Prom_grupo3
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="Israel"]<-Prom_grupo3
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="Russia"]<-Prom_grupo3
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="Suriname"]<-Prom_grupo3
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="Austria"]<-Prom_grupo3
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="Spain"]<-Prom_grupo3
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="Honduras"]<-Prom_grupo3
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="Hungary"]<-Prom_grupo3
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="Domincan Republic"]<-Prom_grupo3
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="South Korea"]<-Prom_grupo3
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="New Zealand"]<-Prom_grupo3
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="Germany"]<-Prom_grupo3
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="Venezuela"]<-Prom_grupo3
Prom_grupo4<-round(mean(c(3.350000, 3.342105, 3.333333, 3.325397, 3.325000, 3.324000, 3.305556, 3.283333, 3.251603, 3.250000, 3.250000, 3.250000)),2)
Prom_grupo4
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="Guatemala"]<-Prom_grupo4
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="Switzerland"]<-Prom_grupo4
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="Singapore"]<-Prom_grupo4
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="Italy"]<-Prom_grupo4
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="Scotland"]<-Prom_grupo4
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="Canada"]<-Prom_grupo4
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="Argentina"]<-Prom_grupo4
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="Denmark"]<-Prom_grupo4
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="France"]<-Prom_grupo4
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="Bolivia"]<-Prom_grupo4
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="Fiji"]<-Prom_grupo4
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="Finland"]<-Prom_grupo4
Prom_grupo5<-round(mean(c(3.750000, 3.500000, 3.500000, 3.416667, 3.409091, 3.397059, 3.375000, 3.357143)),2)
Prom_grupo5
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="Chile"]<-Prom_grupo5
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="Netherlands"]<-Prom_grupo5
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="Philippines"]<-Prom_grupo5
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="Iceland"]<-Prom_grupo5
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="Vietnam"]<-Prom_grupo5
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="Brazil"]<-Prom_grupo5
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="Poland"]<-Prom_grupo5
Cacao_C$Fabrica_origen[Cacao_C$Fabrica_origen=="Australia"]<-Prom_grupo5
Cacao_C$Fabrica_origen<-as.numeric(Cacao_C$Fabrica_origen)
Cacao_C$Fabrica_origen

Prom_grupo1<-round(mean(c(3.026316, 3.008333, 3.000000, 3.000000, 2.968750, 2.875000, 2.875000, 2.812500, 2.800000, 2.750000, 2.500000)),2)
Prom_grupo1
Cacao_C$Grano_origen[Cacao_C$Grano_origen=="Grenada"]<-Prom_grupo1
Cacao_C$Grano_origen[Cacao_C$Grano_origen=="Mexico"]<-Prom_grupo1
Cacao_C$Grano_origen[Cacao_C$Grano_origen=="Burma"]<-Prom_grupo1
Cacao_C$Grano_origen[Cacao_C$Grano_origen=="Philippines"]<-Prom_grupo1
Cacao_C$Grano_origen[Cacao_C$Grano_origen=="St. Lucia"]<-Prom_grupo1
Cacao_C$Grano_origen[Cacao_C$Grano_origen=="El Salvador"]<-Prom_grupo1
Cacao_C$Grano_origen[Cacao_C$Grano_origen=="Sri Lanka"]<-Prom_grupo1
Cacao_C$Grano_origen[Cacao_C$Grano_origen=="Uganda"]<-Prom_grupo1
Cacao_C$Grano_origen[Cacao_C$Grano_origen=="Ivory Coast"]<-Prom_grupo1
Cacao_C$Grano_origen[Cacao_C$Grano_origen=="Martinique"]<-Prom_grupo1
Cacao_C$Grano_origen[Cacao_C$Grano_origen=="Puerto Rico"]<-Prom_grupo1
Prom_grupo2<-round(mean(c(3.162500, 3.152778, 3.144737, 3.137048, 3.134715, 3.125000, 3.125000, 3.125000, 3.090909, 3.083333, 3.083333, 3.056985)),2)
Prom_grupo2
Cacao_C$Grano_origen[Cacao_C$Grano_origen=="Jamaica"]<-Prom_grupo2
Cacao_C$Grano_origen[Cacao_C$Grano_origen=="Santo Tome y Principe"]<-Prom_grupo2
Cacao_C$Grano_origen[Cacao_C$Grano_origen=="Costa Rica"]<-Prom_grupo2
Cacao_C$Grano_origen[Cacao_C$Grano_origen=="Peru"]<-Prom_grupo2
Cacao_C$Grano_origen[Cacao_C$Grano_origen=="Ecuador"]<-Prom_grupo2
Cacao_C$Grano_origen[Cacao_C$Grano_origen=="Fiji"]<-Prom_grupo2
Cacao_C$Grano_origen[Cacao_C$Grano_origen=="India"]<-Prom_grupo2
Cacao_C$Grano_origen[Cacao_C$Grano_origen=="Samoa"]<-Prom_grupo2
Cacao_C$Grano_origen[Cacao_C$Grano_origen=="Ghana"]<-Prom_grupo2
Cacao_C$Grano_origen[Cacao_C$Grano_origen=="Liberia"]<-Prom_grupo2
Cacao_C$Grano_origen[Cacao_C$Grano_origen=="Togo"]<-Prom_grupo2
Cacao_C$Grano_origen[Cacao_C$Grano_origen=="Desconocido"]<-Prom_grupo2
Prom_grupo3<-round(mean(c(3.250000, 3.245327, 3.234694, 3.225000, 3.223684, 3.214286, 3.206325, 3.205882, 3.200000, 3.197368, 3.187500, 3.178571)),2)
Prom_grupo3
Cacao_C$Grano_origen[Cacao_C$Grano_origen=="Vanuatu"]<-Prom_grupo3
Cacao_C$Grano_origen[Cacao_C$Grano_origen=="Venezuela"]<-Prom_grupo3
Cacao_C$Grano_origen[Cacao_C$Grano_origen=="Belize"]<-Prom_grupo3
Cacao_C$Grano_origen[Cacao_C$Grano_origen=="Colombia"]<-Prom_grupo3
Cacao_C$Grano_origen[Cacao_C$Grano_origen=="Trinidad y Tobago"]<-Prom_grupo3
Cacao_C$Grano_origen[Cacao_C$Grano_origen=="Hawaii"]<-Prom_grupo3
Cacao_C$Grano_origen[Cacao_C$Grano_origen=="Dominican Republic"]<-Prom_grupo3
Cacao_C$Grano_origen[Cacao_C$Grano_origen=="Tanzania"]<-Prom_grupo3
Cacao_C$Grano_origen[Cacao_C$Grano_origen=="Nicaragua"]<-Prom_grupo3
Cacao_C$Grano_origen[Cacao_C$Grano_origen=="Bolivia"]<-Prom_grupo3
Cacao_C$Grano_origen[Cacao_C$Grano_origen=="Indonesia"]<-Prom_grupo3
Cacao_C$Grano_origen[Cacao_C$Grano_origen=="Panama"]<-Prom_grupo3
Prom_grupo4<-round(mean(c(3.265517, 3.250000, 3.250000, 3.250000, 3.250000, 3.250000, 3.250000, 3.250000)),2)
Prom_grupo4
Cacao_C$Grano_origen[Cacao_C$Grano_origen=="Madagascar"]<-Prom_grupo4
Cacao_C$Grano_origen[Cacao_C$Grano_origen=="Australia"]<-Prom_grupo4
Cacao_C$Grano_origen[Cacao_C$Grano_origen=="Cameroon"]<-Prom_grupo4
Cacao_C$Grano_origen[Cacao_C$Grano_origen=="Cuba"]<-Prom_grupo4
Cacao_C$Grano_origen[Cacao_C$Grano_origen=="Gabon"]<-Prom_grupo4
Cacao_C$Grano_origen[Cacao_C$Grano_origen=="Malaysia"]<-Prom_grupo4
Cacao_C$Grano_origen[Cacao_C$Grano_origen=="Nigeria"]<-Prom_grupo4
Cacao_C$Grano_origen[Cacao_C$Grano_origen=="Suriname"]<-Prom_grupo4
Prom_grupo5<-round(mean(c(3.437500, 3.388889, 3.350000, 3.325000, 3.321429, 3.315789, 3.291667, 3.284483)),2)
Prom_grupo5
Cacao_C$Grano_origen[Cacao_C$Grano_origen=="Solomon Islands"]<-Prom_grupo5
Cacao_C$Grano_origen[Cacao_C$Grano_origen=="Haiti"]<-Prom_grupo5
Cacao_C$Grano_origen[Cacao_C$Grano_origen=="Honduras"]<-Prom_grupo5
Cacao_C$Grano_origen[Cacao_C$Grano_origen=="Congo"]<-Prom_grupo5
Cacao_C$Grano_origen[Cacao_C$Grano_origen=="Guatemala"]<-Prom_grupo5
Cacao_C$Grano_origen[Cacao_C$Grano_origen=="Vietnam"]<-Prom_grupo5
Cacao_C$Grano_origen[Cacao_C$Grano_origen=="Papua New Guinea"]<-Prom_grupo5
Cacao_C$Grano_origen[Cacao_C$Grano_origen=="Brazil"]<-Prom_grupo5
Cacao_C$Grano_origen<-as.numeric(Cacao_C$Grano_origen)
Cacao_C$Grano_origen

Prom_grupo1<-round(mean(c(3.150000, 3.138514, 3.112245)),2)
Prom_grupo1
Cacao_C$Tipo_grano[Cacao_C$Tipo_grano=="Forastero"]<-Prom_grupo1
Cacao_C$Tipo_grano[Cacao_C$Tipo_grano=="Desconocido"]<-Prom_grupo1
Cacao_C$Tipo_grano[Cacao_C$Tipo_grano=="Nacional"]<-Prom_grupo1
Prom_grupo2<-round(mean(c(3.269953, 3.245413)),2)
Prom_grupo2
Cacao_C$Tipo_grano[Cacao_C$Tipo_grano=="Trinitario"]<-Prom_grupo2
Cacao_C$Tipo_grano[Cacao_C$Tipo_grano=="Criollo"]<-Prom_grupo2
Prom_grupo3<-round(mean(c(3.416667, 3.363095)),2)
Prom_grupo3
Cacao_C$Tipo_grano[Cacao_C$Tipo_grano=="Blend"]<-Prom_grupo3
Cacao_C$Tipo_grano[Cacao_C$Tipo_grano=="Matina"]<-Prom_grupo3
Prom_grupo4<-round(mean(c(3.583333, 3.583333, 3.500000)),2)
Prom_grupo4
Cacao_C$Tipo_grano[Cacao_C$Tipo_grano=="CCN51"]<-Prom_grupo4
Cacao_C$Tipo_grano[Cacao_C$Tipo_grano=="EET"]<-Prom_grupo4
Cacao_C$Tipo_grano[Cacao_C$Tipo_grano=="Beniano"]<-Prom_grupo4
Prom_grupo5<-round(mean(c(3.600000)),2)
Prom_grupo5
Cacao_C$Tipo_grano[Cacao_C$Tipo_grano=="Amazon"]<-Prom_grupo5
Cacao_C$Tipo_grano<-as.numeric(Cacao_C$Tipo_grano)
Cacao_C$Tipo_grano

Cacao_C$Calificacion_cacao
data.class(Cacao_C$Calificacion_cacao)


#### Para el data frame donde vamos a trabajar a la VO como una v. discreta
#### Las demás variables debemos desagregarlas en variables "dummy"

#### Cantidad de columnas (m-1)

## Variable % de cacao
Valores_PC<-as.numeric(levels(factor(Cacao_D$Porcentaje_cacao)))
Valores_PC
length(Valores_PC)
# Como tiene 45 valores distintos, creamos 44 variables dummy

## Variable país de la fábrica
Valores_FO<-levels(factor(Cacao_D$Fabrica_origen))
Valores_FO
length(Valores_FO)
# Como tiene 56 valores distintos, creamos 55 variables dummy

## Variable país del grano
Valores_GO<-levels(factor(Cacao_D$Grano_origen))
Valores_GO
length(Valores_GO)
# Como tiene 51 valores distintos, creamos 50 variables dummy

## Variable tipo de grano
Valores_TG<-levels(factor(Cacao_D$Tipo_grano))
Valores_TG
length(Valores_TG)
# Como tiene 11 valores distintos, creamos 10 variables dummy


##### Armamos las columnas dummys

install.packages("dummies")
library(dummies)

Cacao_D$Calificacion_cacao
data.class(Cacao_D$Calificacion_cacao)

Dummy1<-dummy.data.frame(Cacao_D,"Porcentaje_cacao")
Dummy2<-dummy.data.frame(Dummy1,"Fabrica_origen")
Dummy3<-dummy.data.frame(Dummy2,"Grano_origen")
Dummy4<-dummy.data.frame(Dummy3,"Tipo_grano")
Dummy4

data.class(Dummy4)
data.class(Dummy4$Calificacion_cacao)
data.class(Dummy4$Porcentaje_cacao42)

Cacao_D<-Dummy4
Cacao_D

## Armamos una lista con ambos data frame

List_cacao<-list(Cacao_C,Cacao_D)
List_cacao



##################### EJ 2 - (3) ######################

install.packages("caret")
install.packages("e1071")
library(caret)
library(e1071)

## Separamos los datos: un 70% para el entrenamiento y un 30% para la prueba

trainindex_C<-createDataPartition(List_cacao[[1]]$Calificacion_cacao,p=0.7,list=F,times=1)
trainindex_C<-createDataPartition(List_cacao[[2]]$Calificacion_cacao,p=0.7,list=F,times=1)

Cacao_train_C<-List_cacao[[1]][trainindex,] #De las 1795 observaciones, 1258 (70%) van a ser usados para prueba
Cacao_test_C<-List_cacao[[1]][-trainindex,] #De las 1795 observaciones, 537 (30%) van a ser usados para prueba

Cacao_train_D<-List_cacao[[2]][trainindex,]
Cacao_test_D<-List_cacao[[2]][-trainindex,]

## Armamos la lista

List_cacao_part<-list(Cacao_train_C,Cacao_test_C,Cacao_train_D,Cacao_test_D)
List_cacao_part



##################### EJ 2 - (4) ######################

#### Para la variable objetivo continua

Modelo_C<-lm(Calificacion_cacao~.,data=Cacao_train_C) #armamos el modelo
Modelo_C
summary(Modelo_C)

# p-value: < 2.2e-16 // Al ser menor a 0.05, podemos rechazar la hipotesis nula, por lo tanto, el modelo es válido
# Adjusted R-squared:  0.1465 // Sólo el 14,65% de la variabilidad de la calificación es predicha por el modelo teniendo en cuenta las 4 variables independientes
# Las 4 variables aportan significativamente al modelo, sus p-value con menores a 0.05
## Porcentaje_cacao = < 2e-16 ***
## Fabrica_origen = < 2e-16 ***
## Grano_origen = 6.11e-09 ***
## Tipo_grano = 0.00302 **
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Cacao_prediccion_C<-round(predict(Modelo_C,Cacao_test_C),2)
Cacao_prediccion_C

table(Cacao_prediccion_C,Cacao_test_C$Calificacion_cacao)

## Es la tabla podemos ver que calificación predijo el modelo (primera columna)
## contra los verdaderos valores de nuestra muestra de validación
## por ejemplo, la muestra de validación sólo poseía una observación con la
## calificación de 1. Dicha observación nuestro modelo predijo 3.01
## Otro ejemplo que que sólo una observación real de 3.25 fue predicha perfectamente
## por nuestro modelo


#### Para la variable objetivo discreta

Cacao_train_D$Calificacion_cacao

Modelo_D<-glm(Calificacion_cacao~.,data=Cacao_train_D,family=binomial(link="logit"))
summary(Modelo_D)

## Eliminamos los NA del modelo

Modelo_D2<-glm(Calificacion_cacao~.-(Porcentaje_cacao46 +Porcentaje_cacao87+Porcentaje_cacao89+Porcentaje_cacao100+Fabrica_origenIndia+Fabrica_origenMartinique+Fabrica_origenSuriname+Fabrica_origenWales+Grano_origenMartinique+Grano_origenSuriname+Grano_origenVietnam+Tipo_granoTrinitario),data=Cacao_train_D,family=binomial(link="logit"))
summary(Modelo_D2)

Cacao_prediccion_D<-predict(Modelo_D2,Cacao_test_D,type="response")
Cacao_prediccion_D

clas<-ifelse(Cacao_prediccion_D<0.5,0,1)
table(clas)

table(clas,Cacao_test_D$Calificacion_cacao)

Cacao_test_D$Calificacion_cacao ## observamos que no había ninguna observación que fuera 
## calificada con 5 (por ende, que tenga el valor 1 en la variable transformada a la 
## binomial) A su vez, al haber muy pocas calificaciones de 5 en todo el data set original,
## la probabilidad de encontrar uno es muy baja. Esto ayuda al modelo que predijo que todas
## las obervaciones tenian una calificación de 0 y, si bien en el grupo de validación todas
## las observaciones también eran de 0, se debió más a la escasez 

table(Cacao_D$Calificacion_cacao) ## Al haber muy pocas calificaciones de 5 en todo el 
## data set original, la probabilidad de encontrar uno es muy baja. Esto ayuda al modelo 
## que predijo que todas las obervaciones tenían una calificación de 0 y, si bien en el 
## grupo de validación todas las observaciones también eran de 0, su buena predicción se 
## debió a la escasez de las calificación 5 más que a una eficiencia del modelo.



##################### EJ 2 - (5) ######################

#### Calificaciones con la mejor calificación

table(Sabores_cacao$Rating)

## Sólo hay dos observaciones en el data set original que llega a la máxima calificación

Sabores_cacao[Sabores_cacao$Rating==5,]

## En ambas observaciones, se coincide que la empresa que lo creó fue Amedei
## Ambos poseen un 70% de cacao
## En ambos, la fabrica donde se produjo se encuentran en Italia
## El pais de origen del grano para uno es Venezuela y del otro es un dato desconocido
## Para la observación cuyo grano de origen es de Venezuela, el tipo utilizado es el 
## Trinitario. Mientras que para el desconocido fue Blend


#### Calificaciones con la peor calificación

table(Sabores_cacao$Rating)

## Sólo hay cuatro observaciones en el data set original que llega a la mimina calificación

Sabores_cacao[Sabores_cacao$Rating==1,]

## A diferencia del anterior, se dan valores diferentes entre las observaciones
## No se presenta una coincidencia en la empresa productora, son 4 distintas
## En cunato al porcentaje de cacao, se repite el 70%, pero no creemos que haya una reclación
## dado que el 70% lo enconytramos también en los chocolates mejores calificados.
## Un dato curioso que si se repite en 3 de estas 4 observaciones, es que el pais de
## residencia de la fábrica productora se trata de Bélgica
## en cuanto al tipo de grano, 3 de 4 son desconocidos, en restante se trata del Forastero
## y por último, en relación al país de origen del grano, dos son desconocidos y los 
## restantes se trata de Ecuador y Santo Tome y Principe 
