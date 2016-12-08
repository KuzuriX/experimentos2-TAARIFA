#Aqui van las rutas de la compu de cada uno donde tienen el repositorio

setwd('/Users/ramonosx/Documents/GitHub/experimentos2-TAARIFA')


# Librerias

library('dplyr')
library('fmsb')

# Carga de los datos
## LA VARIABLE A PREDECIR ES status_group

predictoras<-read.csv('predictoras.csv', header = T)
respuesta<-read.csv('respuesta.csv', header = T)

base<-merge(predictoras,respuesta,by= 'id')

predictoras<-NULL
respuesta<-NULL


# Analisis preliminar

##VAR respuesta con ceros y unos

base<- base[!(status_group=="functional needs repair"),] 
base$status = 1*(base$status_group=='functional') 
summary(status)


##### limpieza de variables 


##################   MIGUEL  ########################

#Funder - reduce factor levels
NUM_LEVELS_FUNDER = 10 #Funder will have this many + 1 levels
#############################################################################################
length(levels(funder))
funderNames <- names(summary(base$funder)[1:NUM_LEVELS_FUNDER])
funder <- factor(base$funder, levels=c(funderNames, "Other"))
funder[is.na(funder)] <- "Other"
base$funder <- funder
length(levels(funder))


#Installer - reduce factor levels
NUM_LEVELS_INSTALLER = 10 #Installer will have this many + 1 levels
#############################################################################################
length(levels(installer))

installerNames <- names(summary(base$installer)[1:NUM_LEVELS_INSTALLER])
installer <- factor(base$installer, levels=c(installerNames, "Other"))
installer[is.na(installer)] <- "Other"
base$installer <- installer
length(levels(installer))

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

mod1<-glm(status~region+amount_tsh+gps_height+installer+funder
          +basin+population+scheme_management+age
          +extraction_type_class+extraction_type_group+management+payment_type
          +quality_group+quantity+source+waterpoint_type_group,
          family = 'binomial', data = train)

summary(mod1)
NagelkerkeR2(mod1)

drop1mod1<-drop1(mod1, test='LRT');drop1mod1

step1mod1<-step(mod1,direction="backward"); step1mod1

mod2<-glm(status ~ region + funder + population + age + extraction_type_group + 
            management + payment_type + quantity + source + waterpoint_type_group, 
          family = "binomial", data = train)

summary(mod2)
NagelkerkeR2(mod2)

mod3<-glm(status ~ region + funder + population + age + extraction_type_group + 
            management + payment_type + quantity + source + waterpoint_type_group, 
          family = "binomial", data = train)
drop1(mod3, test='LRT')
NagelkerkeR2(mod3)

mod4<-glm(status ~ region*population*quantity*waterpoint_type_group 
          *funder*management * age * extraction_type_group * 
          payment_type * source , 
          family = "binomial", data = train)
drop1(mod4, test='LRT')
NagelkerkeR2(mod3)
