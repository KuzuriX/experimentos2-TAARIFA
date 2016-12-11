#Aqui van las rutas de la compu de cada uno donde tienen el repositorio

setwd('/Users/ramonosx/Documents/GitHub/experimentos2-TAARIFA')


# Librerias

library('dplyr')
library('fmsb')
library('lme4')
library('lmerTest')
options(contrasts=c("contr.sum","contr.poly"))

# Carga de los datos
## LA VARIABLE A PREDECIR ES status_group

predictoras<-read.csv('predictoras.csv', header = T)
respuesta<-read.csv('respuesta.csv', header = T)

base<-merge(predictoras,respuesta,by= 'id')

predictoras<-NULL
respuesta<-NULL


# Analisis preliminar

##VAR respuesta con ceros y unos

base<- base[!(base$status_group=="functional needs repair"),] 
base$status = 1*(base$status_group=='functional') 
summary(base$status)


##### limpieza de variables 


##################   MIGUEL  ########################

#Funder - reduce factor levels
NUM_LEVELS_FUNDER = 20 #Funder will have this many + 1 levels
#############################################################################################
length(levels(base$funder))
funderNames <- names(summary(base$funder)[1:NUM_LEVELS_FUNDER])
funder <- factor(base$funder, levels=c(funderNames, "Other"))
funder[is.na(funder)] <- "Other"
base$funder <- funder
length(levels(funder))


#Installer - reduce factor levels
NUM_LEVELS_INSTALLER = 20 #Installer will have this many + 1 levels
#############################################################################################
length(levels(base$installer))

installerNames <- names(summary(base$installer)[1:NUM_LEVELS_INSTALLER])
installer <- factor(base$installer, levels=c(installerNames, "Other"))
installer[is.na(installer)] <- "Other"
base$installer <- installer
length(levels(installer))

base$waterpoint_type_group[base$waterpoint_type_group=="dam"]<-"other"
base$waterpoint_type_group[base$waterpoint_type_group=="cattle trough"]<-"other"

base<-base[!(base$management=='unknown'),]
base<-base[!(base$scheme_management==''),]
base<-base[!(base$source=='unknown'),]
base<-base[!(base$quantity=='unknown'),]
base<-base[!(base$payment_type=='unknown'),]
base<-base[!(base$quality_group=='unknown'),]
base<-base[!(base$construction_year==0),]
base$age<-base$construction_year-1960





#### PRIMER MODELO!!!! WOOOOOOOO
set.seed(123)
train<- base  %>% sample_n(1000)
test<- base  %>% sample_n(200)

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


#modelo con todo

mod1<-glmer(status~region+amount_tsh+gps_height+(1|installer)+(1|funder)
          +basin+population+scheme_management+age
          +extraction_type_class+extraction_type_group+management+payment_type
          +quality_group+quantity+source+(1|waterpoint_type_group),
          family = 'binomial', data = train)

#todas las interacciones

mod2<-lmer(status~region*amount_tsh*gps_height*(1|installer)*(1|funder)
            *basin*population*scheme_management*age
            *extraction_type_class*extraction_type_group*management*payment_type
            *quality_group*quantity*source*(1|waterpoint_type_group)
            , data = train)

summary(mod1)
NagelkerkeR2(mod1)

drop1mod1<-drop1(mod1, test='LRT');drop1mod1

step1mod1<-lmerTest::step(mod2); step1mod1


mod3<-glmer(status ~ region + amount_tsh + gps_height + 
            (1 | funder) + population + age + extraction_type_class + 
            management + payment_type + quantity + source + 
              (1 | waterpoint_type_group), 
          family = "binomial", data = train)

summary(mod3)
NagelkerkeR2(mod2)


mod4<-glmer(status ~ region + amount_tsh + population + 
              +gps_height
              +extraction_type_class 
              +age 
              +management 
              +source 
            +payment_type
              +quantity
            + (1 | funder) +
              (1 | waterpoint_type_group:quantity), 
            family = "binomial", data = train)

anova(mod3,mod4)


###### modelo 

modFinal<-glmer(status ~ region + amount_tsh + population + 
                  +gps_height
                +extraction_type_class 
                +age 
                +management 
                +source 
                +payment_type
                +quantity
                + (1 | funder) 
                + (1 | funder:amount_tsh) 
                +(1 | funder:population) 
                + (1 | waterpoint_type_group)
                 + (1 | waterpoint_type_group:population)
+ (1 | waterpoint_type_group:gps_height)
   +  (1 | waterpoint_type_group:extraction_type_class)
       +  (1 | waterpoint_type_group:age)
       +  (1 | waterpoint_type_group:payment_type), 
                family = "binomial", data = train)

summary(modFinal)
drop1(modFinal, test="Chisq")

##interacciones
drop1(mod4, test='LRT')
NagelkerkeR2(modFinal)

#########vamos a asumir que no hay interaccion

modFinal2<-glmer(status ~ region + amount_tsh + population + 
                +gps_height+extraction_type_class +age 
                +management+source+payment_type+quantity+ (1 | funder) 
                +(1 | waterpoint_type_group), family = "binomial", data = train)

summary(modFinal2)

#nagelkere 
NagelkerkeR2(modFinal2)

#referencias
contrasts(factor(train$status)) # referencia es el 1: funcionan

#########

#efectos

#1.region 

mod1<-glmer(status ~amount_tsh + population + 
                   +gps_height+extraction_type_class +age 
                 +management+source+payment_type+quantity+ (1 | funder) 
                 +(1 | waterpoint_type_group), family = "binomial", data = train)

anova(mod1,modFinal2)
#Se obtuvo: estadistico=20.664 y p-value=0.1918 --no se rechaza 

#2.amount_tsh 

mod2<-glmer(status ~ region + population + 
                   +gps_height+extraction_type_class +age 
                 +management+source+payment_type+quantity+ (1 | funder) 
                 +(1 | waterpoint_type_group), family = "binomial", data = train)

anova(mod2,modFinal2)
#Se obtuvo: estadistico=4.91 y p-value=0.0267 --se rechaza , var continua

#3population
mod3<-glmer(status ~ region + amount_tsh +gps_height+extraction_type_class +age 
                 +management+source+payment_type+quantity+ (1 | funder) 
                 +(1 | waterpoint_type_group), family = "binomial", data = train)

anova(mod3,modFinal2)
#Se obtuvo: estadistico=0 y p-value=1 --no se rechaza , var continua

#4gps_height
mod4<-glmer(status ~ region + amount_tsh + population + extraction_type_class +age 
                 +management+source+payment_type+quantity+ (1 | funder) 
                 +(1 | waterpoint_type_group), family = "binomial", data = train)
anova(mod4,modFinal2)
#Se obtuvo: estadistico=0.997 y p-value=0.318 --no se rechaza , var continua

#5 extraction_type_class 
mod5<-glmer(status ~ region + amount_tsh + population +gps_height+age 
                 +management+source+payment_type+quantity+ (1 | funder) 
                 +(1 | waterpoint_type_group), family = "binomial", data = train)
anova(mod5,modFinal2)
#Se obtuvo: estadistico=11.249 y p-value=0.04665 -- se rechaza 

#6 age
mod6<-glmer(status ~ region + amount_tsh + population +gps_height+extraction_type_class
                 +management+source+payment_type+quantity+ (1 | funder) 
                 +(1 | waterpoint_type_group), family = "binomial", data = train)

anova(mod6,modFinal2)
#Se obtuvo: estadistico=19.318 y p-value=1.106e-05 -- se rechaza , var continua

#7management
mod7<-glmer(status ~ region + amount_tsh + population+gps_height+extraction_type_class +age 
                 +source+payment_type+quantity+ (1 | funder)+(1 | waterpoint_type_group), family = "binomial", data = train)
anova(mod7,modFinal2)
#Se obtuvo: estadistico=33.303 y p-value=0.00024 -- se rechaza 

#8source
mod8<-glmer(status ~ region + amount_tsh + population +gps_height+extraction_type_class +age 
                 +management+payment_type+quantity+ (1 | funder) 
                 +(1 | waterpoint_type_group), family = "binomial", data = train)
anova(mod8,modFinal2)
#Se obtuvo: estadistico=17.08 y p-value=0.02929 -- se rechaza 


#9payment_type
mod9<-glmer(status ~ region + amount_tsh + population+gps_height+extraction_type_class +age 
                 +management+source+quantity+ (1 | funder) 
                 +(1 | waterpoint_type_group), family = "binomial", data = train)
anova(mod9,modFinal2)
#Se obtuvo: estadistico=27.137 y p-value=5.363e-05 -- se rechaza 


#10quantity
mod10<-glmer(status ~ region + amount_tsh + population+gps_height+extraction_type_class +age 
                 +management+source+payment_type+(1 | funder) 
                 +(1 | waterpoint_type_group), family = "binomial", data = train)
anova(mod10,modFinal2)
#Se obtuvo: estadistico=147.02 y p-value=2.2e-16 -- se rechaza 


#11(1 | funder) 

mod11<-glmer(status ~ region + amount_tsh + population + 
                   +gps_height+extraction_type_class +age 
                 +management+source+payment_type+quantity 
                 +(1 | waterpoint_type_group), family = "binomial", data = train)

anova(mod11,modFinal2)
#Se obtuvo: estadistico=3.1021 y p-value=0.07819 -- no se rechaza 


#12(1 | waterpoint_type_group)

mod12<-glmer(status ~ region + amount_tsh + population + 
                   +gps_height+extraction_type_class +age 
                 +management+source+payment_type+quantity+ (1 | funder), family = "binomial", data = train)

anova(mod12,modFinal2)
#Se obtuvo: estadistico=10.602 y p-value=0.00113 -- se rechaza 

###aleatorias hacer graficos de prediccion 
