#======================================================================
# ESTADÍSTICA - GRADO DE MATEMÁTICAS - ULL
# PRÁCTICA DE LABORATORIO 01 - Operaciones con un data.frame (datasets)
# Prof.: Hipólito Hernández Pérez
#======================================================================

getwd()                      # muestra el directorio de trabajo
setwd("L:/RTRABAJO/EST1")     # establece el directorio de trabajo

# Nuestra primera tarea será crear o importar un conjunto de datos para analizar

# Importar HIPER150 desde el menu desplegable File, opcion de Import Dataset o desde Environment con la pestaña 
# Import Dataset
#Importación del fichero HIPER150.sav mediante el RStudio
rm(HIPER150)

# Importar el fichero  HIPER150, en formato texto columna fija, con el comando read.table
HIPER150<-read.table("HIPER1502014.txt",header=TRUE, sep="\t", na.strings= "-9") 

save(HIPER150,file="HIPER150.RData")
load("HIPER150.RData")
str(HIPER150)
attach(HIPER150)
HH150<-HIPER150    # realizamos una copia, para trabajar con el dataframe HIPER150

# declaración de variables cualitativas como factores, segun cada columna
attach(HH150)
HH150$sexo<-factor(sexo,levels=c(1,2),labels=c("masculino","femenino"),ordered=FALSE)
HH150$profesio<-factor(profesio,labels=c("campo","pescador","construccion","oficina","liberal","hogar","estudiante","otras"),ordered=FALSE)
HH150$sit_labo<-factor(sit_labo,labels=c("cuenta ajena","autnomo","parado","jubilado","otras"),ordered=FALSE)
HH150$estudios<-factor(estudios,labels=c("sin estudios","primarios","secundarios","superiores"),ordered=TRUE) 
HH150$t_tabaco<-factor(t_tabaco, labels=c("no fumador","< 5 años","> 5 años"),ordered=TRUE)
HH150$cl_tabac<-factor(cl_tabac,labels=c("no fumador","poco","moderado","muy fumador"),ordered=TRUE)       
HH150$t_sin_fu<-factor(t_sin_fu, labels=c("no fumador","fuma actualmente","no fuma desde <5 años","no fuma desde >5 años"),ordered=TRUE)
HH150$t_alcoho<-factor(t_alcoho, labels=c("no bebedor","menos de 1 año","mas de 1 año"),ordered=TRUE)
HH150$cl_alcoh<-factor(cl_alcoh,labels=c("poco","moderado","alcoholico","gran bebedor","no bebedor"),ordered=FALSE)                          
HH150$t_sin_be<-factor(t_sin_be, labels=c("no bebedor","bebe actualmente","menos de 1 año","mas de 1 año"), ordered=FALSE)
HH150$cafe<-factor(cafe,labels=c("no toma","poco","moderado","mucho"),ordered=TRUE)
HH150$act_fisi<-factor(act_fisi,labels=c("escasa","moderada","intensa"),ordered=TRUE)
HH150$sal<-factor(sal,labels=c("poca","normal","mucha"),ordered=TRUE)
HH150$es_hip<-factor(es_hip,labels=c("si","no","no lo sabe"))
HH150$trat_hip<-factor(trat_hip, labels=c("no por innecesario","no por desidia",
  "alguna vez regularmente no en la actualidad","alguna vez irregularmente no en la actualidad",
  "actualmente con regularidad","actualmente sin regularidad"), ordered=FALSE)
HH150$cl_peso<-factor(cl_peso,labels=c("normal","obesidad discreta", "obesidad moderada", "obesidad grave","obesidad morbida"), ordered=TRUE)
HH150$conc_hta<-factor(conc_hta,labels=c("normotenso","bordeline","hipertenso"),ordered=TRUE)

# declaracion de variables cuantitativas como variables numéricas, segun cada columna
HH150$edad<-as.numeric(HH150$edad)
HH150$peso<-as.numeric(HH150$peso)
HH150$talla<-as.numeric(HH150$talla)
HH150$sist_ini<-as.numeric(HH150$sist_ini)
HH150$dias_ini<-as.numeric(HH150$dias_ini)
HH150$sist_fin<-as.numeric(HH150$sist_fin)
HH150$dias_fin<-as.numeric(HH150$dias_fin)
attach(HH150)
str(HH150)
HIPER150<-HH150

save(HIPER150,file="HIPER150.RData")

remove(list=ls())                # vacia el contenido del workspace
load("HIPER150.RData")

#==============================================================
# PEQUEÑAS COSAS QUE PODEMOS CAMBIAR EN UN DATA FRAME (dataset)
#==============================================================
# Cambiar el nombre de una variable

names(HIPER150)[names(HIPER150)=="sist_ini"]<-c("TAsist0")     #por su nombre
names(HIPER150)[names(HIPER150)=="dias_ini"]<-c("TAdias0")     #por su nombre
names(HIPER150)[names(HIPER150)=="sist_fin"]<-c("TAsist1")     #por su nombre
names(HIPER150)[names(HIPER150)=="dias_fin"]<-c("TAdias1")     #por su nombre
names(HIPER150)[2]<-"genero"                                #por su posición en el fichero

# Cambiar las etiquetas de los niveles de un factor
library(plyr)
attach(HIPER150)
HIPER150$genero1<-revalue(genero,c("masculino"="M","femenino"="F"))

# Recodificar las etiquetas de los niveles de un factor
library(DescTools)
HIPER150$cl_tabac1<-Recode(HIPER150$cl_tabac, 
                           "no fumador" = c("no fumador"),
                           "fumador" = c("poco","moderado","muy fumador"))

# Cambiar una variable númerica a factor
HIPER150$edad1<-cut(HIPER150$edad, breaks=c(15,30,65,90), labels=c("joven", "maduro", "jubilado"))

# Crea una variable con el número secuencial de cada caso
HIPER150$id<-seq(dim(HIPER150)[1])   

# Seleccionar subconjuntos de datos y de variables
HIPER15002<-subset(HIPER150,select=c(-genero1,-cl_tabac1,-edad1))  #eliminamos las variable derivadas que hemos creado
HIPER15003<-subset(HIPER150, genero=="femenino")           #todas las variable pero solo para femenino
HIPER15004<-subset(HIPER150, edad>=30 & genero=="masculino" & t_tabaco=="no fumador")
mean(edad)
attach(HIPER150)
mean(HIPER150$edad)

mean(HIPER15002$edad)
mean(HIPER15003$edad)
mean(HIPER15004$edad)
remove(HIPER15002,HIPER15003,HIPER15004)

# Reordenacion FINAL de columnas en el date frame
HIPER150<-HIPER150[c(36,1,17:18,20:23,2:16,19,26)]   #estructura definitiva del fichero HIPER150

save(HIPER150,file="HIPER150.RData")

#=======================
# FINAL DE LA PRÁCTICA #
#=======================


# Comandos utiles para buscar los conjuntos de datos (datasets) disponibles en R
# tanto del paquete básico como de diferentes librerias que se van añadiendo

# Se necesita instalar las librerias: datasets, HSAUR3, load, plyr y DescTools

data()   # muestra los datasets de la libreria datasets (conjunto de datos de las librerias base)
library(help="datasets")  #mismo efecto que la sentencia anterior

data(iris)
iris
library(HSAUR3)
ls("package:HSAUR3")             #indica en la consola los datasets asociados a esa libreria 
ls("package:datasets")

library(datasets.load)   #interfase para cargar datos
datasets.load()   #permite cargar datos de datasets asociados a las librerias instaladas desde una ventana emergente
getDatasetInfo()  #nos indica el directorio de los actuales datasets disponibles
datasets(package="HSAUR3")   #nos indica el directorio de los datasets asociados a la libreria especifica

dat <- as.data.frame(data(package = .packages(all.available = TRUE))$results)  #nos indica el directorio de los actuales datasets

ls()

help.start()   # nos proporciona ayuda a diferentes niveles de información









