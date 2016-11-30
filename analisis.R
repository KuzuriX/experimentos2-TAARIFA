#Aqui van las rutas de la compu de cada uno donde tienen el repositorio

setwd('/Users/ramonosx/Documents/GitHub/experimentos2-TAARIFA')


# Librerias

library('dplyr')

# Carga de los datos
## LA VARIABLE A PREDECIR ES status_group

predictoras<-read.csv('predictoras.csv', header = T)
respuesta<-read.csv('respuesta.csv', header = T)

base<-merge(predictoras,respuesta,by= 'id')

attach(base)


# Analisis preliminar

table(status_group)

## anidamiento de lugares

length(levels(region))
length(levels(subvillage))
levels(subvillage)

levels(as.factor(region_code))

length(levels(as.factor(region_code)))

length(levels(as.factor(district_code)))

length(base$region[region_code=99])

apply(table(region_code,region), 1, sum)

dim(table(region_code,region))

table(region_code, district_code)

table(region, district_code)

###  mejor usar region code porque hay regiones con 2 codigos de region

#hola amiguis

table(status_group)

base<- base[!(status_group=="functional needs repair"),] 
base$status = 1*(base$status_group=='functional') 
attach(base)
summary(status)

###### DESCRICION DE LAS VARIABLES 
# amount_tsh - Total static head (amount water available to waterpoint)
# funder - Who funded the well
# gps_height - Altitude of the well
# installer - Organization that installed the well
# region_code - Geographic location (coded)
# subvillage - Geographic location
# basin - Geographic water basin
# population - Population around the well
# scheme_management - Who operates the waterpoint
# scheme_name - Who operates the waterpoint
# construction_year - Year the waterpoint was constructed
# management - How the waterpoint is managed
# payment_type - What the water costs
# quality_group - The quality of the water
# quantity - The quantity of water
# source - The source of the water
# waterpoint_type_group - The kind of waterpoint



##### VARIABLES IMPORTANTES
#region_code+subvillage+amount_tsh+gps_height+installer+funder
#+basin+population+scheme_management+scheme_name+construction_year
#+extraction_type_class+extraction_type_group+management+payment_type
#+quality_group+quantity+source+waterpoint_type_group

##Variables anidadas 
#  region_code con subvillage
# scheme_management con scheme_name
# extraction_type_class con extraction_type_group


#### PRIMER MODELO!!!! WOOOOOOOO
set.seed(123)
base1<- base  %>% sample_n(1000)

# 
# ## 50% of the sample size
# smp_size <- floor(0.8 * nrow(base))
# 
# ## set the seed to make your partition reproductible
# 
# train_ind <- sample(seq_len(nrow(train)), size = smp_size)
# 
# train1 <- train[train_ind, ]
# test1 <- train[-train_ind, ]

mod1<-glm(status~region_code+subvillage+amount_tsh+gps_height+installer+funder
          +basin+population+scheme_management+scheme_name+construction_year
          +extraction_type_class+extraction_type_group+management+payment_type
          +quality_group+quantity+source+waterpoint_type_group,
          family = 'binomial', data = base1)

summary(mod1)
drop1mod1<-drop1(mod1, test='LRT')

step(lm(mpg~wt+drat+disp+qsec,data=mtcars),direction="backward")
