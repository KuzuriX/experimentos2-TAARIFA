#Aqui van las rutas de la compu de cada uno donde tienen el repositorio

setwd('/Users/ramonosx/Documents/GitHub/experimentos2-TAARIFA')


# Librerias

library('dplyr')

# Carga de los datos
## LA VARIABLE A PREDECIR ES status_group

predictoras<-read.csv('predictoras.csv', header = T)
respuesta<-read.csv('respuesta.csv', header = T)

base<-merge(predictoras,respuesta,by= 'id')

predictoras<-NULL
respuesta<-NULL

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



table(status_group)

##VAR respuesta con ceros y unos

base<- base[!(status_group=="functional needs repair"),] 
base$status = 1*(base$status_group=='functional') 
attach(base)
summary(status)


##### limpieza de variables 

basepayment_type



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

step1mod1<-step(mod1,direction="backward")


##################   MIGUEL  ########################




























































##################   NATY  ########################

































































##################   ANDRES  ########################