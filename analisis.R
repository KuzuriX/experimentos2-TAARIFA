#Aqui van las rutas de la compu de cada uno donde tienen el repositorio

setwd('/Users/ramonosx/Documents/GitHub/experimentos2-TAARIFA')


# Librerias



# Carga de los datos
## LA VARIABLE A PREDECIR ES status_group

predictoras<-read.csv('predictoras.csv', header = T)
respuesta<-read.csv('respuesta.csv', header = T)

base<-merge(predictoras,respuesta,by= 'id')

attach(base)


# Analisis preliminar

table(status_group)

## anidamiento de lugares

levels(region)
levels(subvillage)

levels(as.factor(region_code))

levels(as.factor(district_code))

length(base$region[region_code=99])

apply(table(region_code,region), 1, sum)

dim(table(region_code,region))

###  mejor usar region code porque hay regiones con 2 codigos de region


