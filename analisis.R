#Aqui van las rutas de la compu de cada uno donde tienen el repositorio

setwd('/Users/ramonosx/Documents/GitHub/experimentos2-TAARIFA')


# Librerias

library('dplyr')
library('fmsb')
library('lme4')
library('lmerTest')
library(lattice)
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


####aleatorias

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
#modelo sin restringir

modFinal2_sin<-glmer(status ~ region + amount_tsh + population + 
                   +gps_height+extraction_type_class +age 
                 +management+source+payment_type+quantity+ (1 | funder) 
                 +(1 | waterpoint_type_group), family = "binomial", data = train,REML=F)
summary(modFinal2)

#no sirve
prof=profile(modFinal2_sin)
xyplot(prof,aspect=1.3)
confint(modFinal2,level=0.95)

###########intervalos a mano  FALTA

var_aleatorios=c(0.1518,0.8801)  #orden:funder,waterpoint
confint(modFinal2,level=0.95,trace=FALSE)


#OR


#continuas
#amount

beta_a=0.000234
sd_beta=0.00012

OR_amount=exp(100*beta_a)  
OR_amount  

z_bonf=pnorm(1-(0.05/(2*51)))
ic_beta=c(beta_a-z_bonf*sd_beta,beta_a+z_bonf*sd_beta)
ic_OR_amount=exp(100*ic_beta)
ic_OR_amount


#age

beta_age=0.0373
sd_beta_age=0.0085

OR_age=exp(5*beta_age)  #5 años
OR_age 

z_bonf=pnorm(1-(0.05/(2*51)))
ic_beta=c(beta_age-z_bonf*sd_beta_age,beta_age+z_bonf*sd_beta_age)
ic_OR_age=exp(5*ic_beta)
ic_OR_age

#categoricas

#########extraction 
betas_extrac=c(0.116,1.3,-0.375,-0.205,-0.103)
sd_beta_extrac=c(0.481,0.502,0.577,0.453,0.825)


#para hacer ic
dif_betas=diff(combn(betas_extrac,2))

comb=combn(sd_beta_extrac,2)
sum_sd=0
for(i in 1:10){
  sum_sd[i]=sum(comb[,i])
}
sum_sd

#Referencia es submersible---  en la categoria wind-powered no hay datos
#categorias eb orden:  gravity, handpump,motorpump,other,rope pump,submersible   


OR_extrac=numeric(0)
ic_OR_ex=matrix(0,10,ncol=2)
ic_b=numeric(0)
z_bonf=pnorm(1-(0.05/(2*51)))

for(i in 1:10){
  OR_extrac[i] =exp(dif_betas[i])
  ic_b=c(dif_betas[i]-z_bonf*sum_sd[i],dif_betas[i]+z_bonf*sum_sd[i])
  ic_OR_ex[i,] =exp(ic_b)
}
mat_or_extract=cbind(OR_extrac,ic_OR_ex)
rownames(mat_or_extract)=c("1-2","1-3","1-4","1-5","2-3","2-4","2-5","3-4","3-5","4-5")
colnames(mat_or_extract)=c("OR","Lim inf","Lim sup")
mat_or_extract


####managment
#############REVISAR LOS IC DE OR SE INDEFINEN

betas=c(-2.384,1.074,-17.9,0.839,1.194,16.94,-0.128,-0.465,0.309,0.789)
sd_beta=c(802.1,802.1,4174,802.1,802.1,6896,802.1,802.1,802.1,802.1)


#para hacer ic
dif_betas=diff(combn(betas,2))

comb=combn(sd_beta,2)
sum_sd=0
for(i in 1:length(comb[1,])){
  sum_sd[i]=sum(comb[,i])
}
sum_sd

#Referencia es wug
#categorias eb orden:  "company" "other" "other - school"   "parastatal" "private operator" "trust"           
# "vwc"     "water authority"  "water board"      "wua"  "wug"     


OR=numeric(0)
ic_OR=matrix(0,length(comb[1,]),ncol=2)
ic_b=numeric(0)
z_bonf=pnorm(1-(0.05/(2*51)))

for(i in 1:length(comb[1,])){
  OR[i] =round(exp(dif_betas[i]),3)
  ic_b=c(dif_betas[i]-z_bonf*sum_sd[i],dif_betas[i]+z_bonf*sum_sd[i])
  ic_OR[i,] =exp(ic_b)
}
mat_or=cbind(OR,ic_OR)
rownames(mat_or)=c("1-2","1-3","1-4","1-5","1-6","1-7","1-8","1-9","1-10","2-3"
                           ,"2-4","2-5","2-6","2-7","2-8","2-9","2-10","3-4","3-5","3-6"
                           ,"3-7","3-8","3-9","3-10","4-5","4-6","4-7","4-8","4-9","4-10"
                           ,"5-6","5-7","5-8","5-9","5-10","6-7","6-8","6-9","6-10","7-8"
                           ,"7-9","7-10","8-9","8-10","9-10")
colnames(mat_or)=c("OR","Lim inf","Lim sup")
mat_or





####SOURCE

betas=c(0.0088,-0.531,-2.316,0.13,-0.258,1.64,1.006,-0.38)
sd_beta=c(0.559,0.736,1.168,0.354,1.162,0.81,0.3677,0.424)

#para hacer ic
dif_betas=diff(combn(betas,2))

comb=combn(sd_beta,2)
sum_sd=0
for(i in 1:length(comb[1,])){
  sum_sd[i]=sum(comb[,i])
}
sum_sd

OR=numeric(0)
ic_OR=matrix(0,length(comb[1,]),ncol=2)
ic_b=numeric(0)
z_bonf=pnorm(1-(0.05/(2*51)))

for(i in 1:length(comb[1,])){
  OR[i] =round(exp(dif_betas[i]),3)
  ic_b=c(dif_betas[i]-z_bonf*sum_sd[i],dif_betas[i]+z_bonf*sum_sd[i])
  ic_OR[i,] =exp(ic_b)
}
mat_or=cbind(OR,ic_OR)
rownames(mat_or)=c("1-2","1-3","1-4","1-5","1-6","1-7","1-8","2-3"
                   ,"2-4","2-5","2-6","2-7","2-8","3-4","3-5","3-6"
                   ,"3-7","3-8","4-5","4-6","4-7","4-8"
                   ,"5-6","5-7","5-8","6-7","6-8","7-8")
colnames(mat_or)=c("OR","Lim inf","Lim sup")
mat_or

####PAYMENT_TYPE

betas=c(0.248,0.779,-0.447,0.2397,-1.461)
sd_beta=c(0.3919,0.2841,0.2464,0.3735,0.7677)

#para hacer ic
dif_betas=diff(combn(betas,2))

comb=combn(sd_beta,2)
sum_sd=0
for(i in 1:length(comb[1,])){
  sum_sd[i]=sum(comb[,i])
}
sum_sd

OR=numeric(0)
ic_OR=matrix(0,length(comb[1,]),ncol=2)
ic_b=numeric(0)
z_bonf=pnorm(1-(0.05/(2*51)))

for(i in 1:length(comb[1,])){
  OR[i] =round(exp(dif_betas[i]),3)
  ic_b=c(dif_betas[i]-z_bonf*sum_sd[i],dif_betas[i]+z_bonf*sum_sd[i])
  ic_OR[i,] =exp(ic_b)
}
mat_or=cbind(OR,ic_OR)
rownames(mat_or)=c("1-2","1-3","1-4","1-5","2-3"
                   ,"2-4","2-5","3-4","3-5","4-5")
colnames(mat_or)=c("OR","Lim inf","Lim sup")
mat_or




####quantity  ######da raro

betas=c(-14.96,5.15,4.66)
sd_beta=c(757.5,252.5,252.5)

#para hacer ic
dif_betas=diff(combn(betas,2))

comb=combn(sd_beta,2)
sum_sd=0
for(i in 1:length(comb[1,])){
  sum_sd[i]=sum(comb[,i])
}
sum_sd

OR=numeric(0)
ic_OR=matrix(0,length(comb[1,]),ncol=2)
ic_b=numeric(0)
z_bonf=pnorm(1-(0.05/(2*51)))

for(i in 1:length(comb[1,])){
  OR[i] =round(exp(dif_betas[i]),3)
  ic_b=c(dif_betas[i]-z_bonf*sum_sd[i],dif_betas[i]+z_bonf*sum_sd[i])
  ic_OR[i,] =exp(ic_b)
}
mat_or=cbind(OR,ic_OR)
rownames(mat_or)=c("1-2","1-3","2-3")
colnames(mat_or)=c("OR","Lim inf","Lim sup")
mat_or

