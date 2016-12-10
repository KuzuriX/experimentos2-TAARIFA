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

base<- base[!(status_group=="functional needs repair"),] 
base$status = 1*(base$status_group=='functional') 
summary(status)


##### limpieza de variables 


##################   MIGUEL  ########################

#Funder - reduce factor levels
NUM_LEVELS_FUNDER = 20 #Funder will have this many + 1 levels
#############################################################################################
length(levels(funder))
funderNames <- names(summary(base$funder)[1:NUM_LEVELS_FUNDER])
funder <- factor(base$funder, levels=c(funderNames, "Other"))
funder[is.na(funder)] <- "Other"
base$funder <- funder
length(levels(funder))


#Installer - reduce factor levels
NUM_LEVELS_INSTALLER = 20 #Installer will have this many + 1 levels
#############################################################################################
length(levels(installer))

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

mod1<-glmer(status~region+amount_tsh+gps_height+(1|installer)+(1|funder)
          +basin+population+scheme_management+age
          +extraction_type_class+extraction_type_group+management+payment_type
          +quality_group+quantity+source+(1|waterpoint_type_group),
          family = 'binomial', data = train)

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

modFinal<-glmer(status ~ region + amount_tsh + population + 
                  +gps_height
                +extraction_type_class 
                +age 
                +management 
                +source 
                +payment_type
                +quantity
                + (1 | funder:amount_tsh) 
                +(1 | funder:population) 
                 + (1 | waterpoint_type_group:population)
+ (1 | waterpoint_type_group:gps_height)
   +  (1 | waterpoint_type_group:extraction_type_class)
       +  (1 | waterpoint_type_group:age)
       +  (1 | waterpoint_type_group:payment_type), 
                family = "binomial", data = train)

summary(modFinal)
drop1(modFinal, test="Chisq")

drop1(mod4, test='LRT')
NagelkerkeR2(mod3)
