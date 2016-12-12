#Aqui van las rutas de la compu de cada uno donde tienen el repositorio

setwd('/Users/ramonosx/Documents/GitHub/experimentos2-TAARIFA')


# Librerias

library('dplyr')
library('fmsb')
library('lme4')
library('lmerTest')
library(lattice)
library(piecewiseSEM)
library(data.table)
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
base$management[base$management=="other - school"]<-"other"
base$management[base$management=="trust"]<-"other"
base$management[base$management=="company"]<-"private operator"
base$management[base$management=="water board"]<- "water authority"



base<-base[!(base$management=='unknown'),]
base<-base[!(base$scheme_management==''),]
base<-base[!(base$source=='unknown'),]
base<-base[!(base$quantity=='unknown'),]
base<-base[!(base$payment_type=='unknown'),]
base<-base[!(base$quality_group=='unknown'),]
base<-base[!(base$construction_year==0),]
base$age<-base$construction_year-1960



train$status

#### PRIMER MODELO!!!! WOOOOOOOO
set.seed(123)
train<- base  %>% sample_n(1000)
test<- base  %>% sample_n(200)
#############################################################################
##Descriptivo
conteo<-table(train$extraction_type)
conteo=conteo[conteo != 0]
barplot(conteo)

pago<-table(train$payment_type)
pago=pago[pago != 0]
barplot(pago,ylim=c(0,500))


#############################################################################
# 
# #modelo con todo
# 
# mod1<-glmer(status~region+amount_tsh+gps_height+(1|installer)+(1|funder)
#           +basin+population+scheme_management+age
#           +extraction_type_class+extraction_type_group+management+payment_type
#           +quality_group+quantity+source+(1|waterpoint_type_group),
#           family = 'binomial', data = train)
# 
# #todas las interacciones
# 
# mod2<-lmer(status~region*amount_tsh*gps_height*(1|installer)*(1|funder)
#             *basin*population*scheme_management*age
#             *extraction_type_class*extraction_type_group*management*payment_type
#             *quality_group*quantity*source*(1|waterpoint_type_group)
#             , data = train)
# 
# summary(mod1)
# NagelkerkeR2(mod1)
# 
# drop1mod1<-drop1(mod1, test='LRT');drop1mod1
# 
# step1mod1<-lmerTest::step(mod2); step1mod1
# 
# 
# mod3<-glmer(status ~ region + amount_tsh + gps_height + 
#             (1 | funder) + population + age + extraction_type_class + 
#             management + payment_type + quantity + source + 
#               (1 | waterpoint_type_group), 
#           family = "binomial", data = train)
# 
# summary(mod3)
# NagelkerkeR2(mod2)
# 
# 
# mod4<-glmer(status ~ region + amount_tsh + population + 
#               +gps_height
#               +extraction_type_class 
#               +age 
#               +management 
#               +source 
#             +payment_type
#               +quantity
#             + (1 | funder) +
#               (1 | waterpoint_type_group:quantity), 
#             family = "binomial", data = train)
# 
# anova(mod3,mod4)
# 
# 
# ###### modelo 
# 
# modFinal<-glmer(status ~ region + amount_tsh + population + 
#                   +gps_height
#                 +extraction_type_class 
#                 +age 
#                 +management 
#                 +source 
#                 +payment_type
#                 +quantity
#                 + (1 | funder) 
#                 + (1 | funder:amount_tsh) 
#                 +(1 | funder:population) 
#                 + (1 | waterpoint_type_group)
#                  + (1 | waterpoint_type_group:population)
# + (1 | waterpoint_type_group:gps_height)
#    +  (1 | waterpoint_type_group:extraction_type_class)
#        +  (1 | waterpoint_type_group:age)
#        +  (1 | waterpoint_type_group:payment_type), 
#                 family = "binomial", data = train)
# 
# summary(modFinal)
# drop1(modFinal, test="Chisq")

##interacciones
drop1(mod4, test='LRT')
NagelkerkeR2(modFinal)




#########vamos a asumir que no hay interaccion

modFinal2<-glmer(status ~ region + amount_tsh + population + 
                +gps_height+extraction_type_class +age 
                +management+source+payment_type+quantity+ (1 | funder) 
                +(1 | waterpoint_type_group), family = "binomial", data = train)

modFinal2
####################################################################################3
ilogit=function(x) exp(x)/(1+exp(x))

h=c(1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0)
length(h)==length(coef)
modFinal2
coef=fixef(modFinal2)
b=coef[18]#tsh, la probabilidad conforme aumenta la cantidad de agua disponible de que la bomba sea funcional
m1=sum(coef*h)#me imagino que está en litros
curve(ilogit(m1+b*x),xlim=c(0,50000), ylim=c(0,1), ylab="Probabilidad",xlab="Agua")
###Este para todas 1 de la variables categoricas, es decir, fijando para la región 1, tipo de extracción 1,
#administración 1, fuente 1, tipo de pago 1 y cantidad 1

b1=coef[19]#cantidad de población alrededor
curve(ilogit(m1+b1*x),xlim=c(0,500000), ylim=c(0,1), col=2,xlab="Población cercana al pozo",ylab="Probabilidad")

b2=coef[26]
curve(ilogit(m1+b2*x),xlim=c(0,250), ylim=c(0,1), col=3,xlab="Edad del pozo",ylab="Probabilidad")

##################################################################################################
#R2 

rsquared(modFinal2)  

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
#Se obtuvo: estadistico=26.312 y p-value=0.04979 -- se rechaza 

#2.amount_tsh 

mod2<-glmer(status ~ region + population + 
                   +gps_height+extraction_type_class +age 
                 +management+source+payment_type+quantity+ (1 | funder) 
                 +(1 | waterpoint_type_group), family = "binomial", data = train)

anova(mod2,modFinal2)
#Se obtuvo: estadistico=3.9941 y p-value=0.04566 --se rechaza , var continua



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
#Se obtuvo: estadistico=0.3859 y p-value=0.5345 --no se rechaza , var continua

#5 extraction_type_class 
mod5<-glmer(status ~ region + amount_tsh + population +gps_height+age 
                 +management+source+payment_type+quantity+ (1 | funder) 
                 +(1 | waterpoint_type_group), family = "binomial", data = train)
anova(mod5,modFinal2)
#Se obtuvo: estadistico=13.865 y p-value=0.01649 -- se rechaza 

#6 age
mod6<-glmer(status ~ region + amount_tsh + population +gps_height+extraction_type_class
                 +management+source+payment_type+quantity+ (1 | funder) 
                 +(1 | waterpoint_type_group), family = "binomial", data = train)

anova(mod6,modFinal2)
#Se obtuvo: estadistico=20.027 y p-value=17.635e-06 -- se rechaza , var continua

#7management
mod7<-glmer(status ~ region + amount_tsh + population+gps_height+extraction_type_class +age 
                 +source+payment_type+quantity+ (1 | funder)+(1 | waterpoint_type_group), family = "binomial", data = train)
anova(mod7,modFinal2)
#Se obtuvo: estadistico=8.4876 y p-value=0.2045 -- NO se rechaza 

#8source
mod8<-glmer(status ~ region + amount_tsh + population +gps_height+extraction_type_class +age 
                 +management+payment_type+quantity+ (1 | funder) 
                 +(1 | waterpoint_type_group), family = "binomial", data = train)
anova(mod8,modFinal2)
#Se obtuvo: estadistico=21.317 y p-value=0.006352 -- se rechaza 


#9payment_type
mod9<-glmer(status ~ region + amount_tsh + population+gps_height+extraction_type_class +age 
                 +management+source+quantity+ (1 | funder) 
                 +(1 | waterpoint_type_group), family = "binomial", data = train)
anova(mod9,modFinal2)
#Se obtuvo: estadistico=22.477 y p-value=0.0004248 -- se rechaza 


#10quantity
mod10<-glmer(status ~ region + amount_tsh + population+gps_height+extraction_type_class +age 
                 +management+source+payment_type+(1 | funder) 
                 +(1 | waterpoint_type_group), family = "binomial", data = train)
anova(mod10,modFinal2)
#Se obtuvo: estadistico=151.02 y p-value=2.2e-16 -- se rechaza 


####aleatorias

#11(1 | funder) 

mod11<-glmer(status ~ region + amount_tsh + population + 
                   +gps_height+extraction_type_class +age 
                 +management+source+payment_type+quantity 
                 +(1 | waterpoint_type_group), family = "binomial", data = train)

anova(mod11,modFinal2)
#Se obtuvo: estadistico=5.0903 y p-value=0.02406 -- se rechaza 


#12(1 | waterpoint_type_group)

mod12<-glmer(status ~ region + amount_tsh + population + 
                   +gps_height+extraction_type_class +age 
                 +management+source+payment_type+quantity+ (1 | funder), family = "binomial", data = train)

anova(mod12,modFinal2)
#Se obtuvo: estadistico=7.776 y p-value=0.005294 -- se rechaza 

###aleatorias hacer graficos de prediccion 
#modelo sin restringir

modFinal2_sin<-glmer(status ~ region + amount_tsh + population + 
                   +gps_height+extraction_type_class +age 
                 +management+source+payment_type+quantity+ (1 | funder) 
                 +(1 | waterpoint_type_group), family = "binomial", data = train,REML=F)
summary(modFinal2_sin)
rsquared(modFinal2_sin)

#no sirve
prof=profile(modFinal2_sin)
xyplot(prof,aspect=1.3)
confint(modFinal2,level=0.95)

###########intervalos a mano  FALTA

var_aleatorios=c(0.1518,0.8801)  #orden:funder,waterpoint
confint(modFinal2,level=0.95,trace=FALSE)


#OR
coefs<-data.table(summary(modFinal2_sin)$coef[,1:2])

#continuas
#amount

beta_a=coefs['amount_tsh',1]
beta_a=coefs[18,1]
sd_beta=coefs['amount_tsh',2]
sd_beta=coefs[18,2]



OR_amount=exp(100*beta_a)  
OR_amount  

z_bonf=pnorm(1-(0.05/(2*51)))
ic_beta=c(beta_a-z_bonf*sd_beta,beta_a+z_bonf*sd_beta)
ic_beta=c(0.000120,0.000358)
ic_OR_amount=exp(100*ic_beta)
ic_OR_amount


#age

beta_age=coefs['age',1]
beta_age=coefs[26,1]
sd_beta_age=coefs['age',2]
sd_beta_age=coefs[26,2]

OR_age=exp(5*beta_age)  #5 a?os
OR_age 

z_bonf=pnorm(1-(0.05/(2*51)))
ic_beta=c(beta_age-z_bonf*sd_beta_age,beta_age+z_bonf*sd_beta_age)
ic_beta=c(0.0304,0.0446)
ic_OR_age=exp(5*ic_beta)
ic_OR_age

#categoricas

#########extraction 
betas_extrac=coefs[paste0("extraction_type_class",c(1,2,3,4,5)),1]
sd_beta_extrac=coefs[paste0("extraction_type_class",c(1,2,3,4,5)),2]

betas_extrac=c(0.1178,1.389,-0.5362,-0.3721,0.0984)
sd_beta_extrac=c(0.4696,0.4786,0.5599,0.4480,0.7977)

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


####region
#############REVISAR LOS IC DE OR SE INDEFINEN

betas=coefs[paste0("region",c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)),1]
sd_beta=coefs[paste0("region",c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)),2]
betas=round(coefs[2:17,1],3)
sd_beta=round(coefs[2:17,2],3)

betas=c(0.252,0.259,0.529,-1.173,-1.030,-0.433,-0.292,-0.203,-0.059,0.901,-15.287,-0.435,-0.846,0.009,17.697,-0.152)
sd_beta=c(771.818,771.818,771.818,771.818,771.818,771.818,771.818,771.818,771.818,771.818,7117.465,771.818,771.818,771.818,10121.135,771.818)


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
##hay que poberle los nombres a las comparaciones
rownames(mat_or)=c("1-2","1-3","1-4","1-5","1-6","1-7","1-8","1-9","1-10","2-3"
                           ,"2-4","2-5","2-6","2-7","2-8","2-9","2-10","3-4","3-5","3-6"
                           ,"3-7","3-8","3-9","3-10","4-5","4-6","4-7","4-8","4-9","4-10"
                           ,"5-6","5-7","5-8","5-9","5-10","6-7","6-8","6-9","6-10","7-8"
                           ,"7-9","7-10","8-9","8-10","9-10")
colnames(mat_or)=c("OR","Lim inf","Lim sup")
mat_or






####SOURCE

betas=coefs[paste0("source",c(1,2,3,4,5,6,7,8)),1]
sd_beta=coefs[paste0("source",c(1,2,3,4,5,6,7,8)),2]

betas=round(coefs[33:40,1],3)
sd_beta=round(coefs[33:40,2],3)

betas=c(-0.102,-0.577,-2.458,0.059,-0.005,1.858,1.051,-0.462)
sd_beta=c(0.556,0.714,1.163,0.357,1.248,0.803,0.365,0.429)

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
  ic_b=round(c(dif_betas[i]-z_bonf*sum_sd[i],dif_betas[i]+z_bonf*sum_sd[i]),3)
  ic_OR[i,] =exp(ic_b)
}
mat_or=round(cbind(OR,ic_OR),3)
rownames(mat_or)=c("1-2","1-3","1-4","1-5","1-6","1-7","1-8","2-3" ,"2-4","2-5","2-6","2-7","2-8","3-4","3-5","3-6"
                   ,"3-7","3-8","4-5","4-6","4-7","4-8","5-6","5-7","5-8","6-7","6-8","7-8")
colnames(mat_or)=c("OR","Lim inf","Lim sup")
mat_or

####PAYMENT_TYPE

betas=coefs[paste0("source",c(1,2,3,4,5)),1]
sd_beta=coefs[paste0("source",c(1,2,3,4,5)),2]

betas=round(coefs[41:45,1],2)
sd_beta=round(coefs[41:45,2],2)

betas=c(0.22,0.51,-0.40,0.23,-1.26)
sd_beta=c(0.40,0.27,0.25,0.37,0.74)



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

betas=coefs[paste0("quantity",c(1,2,3)),1]
sd_beta=coefs[paste0("quantity",c(1,2,3)),2]


betas=round(coefs[46:48,1],3)
sd_beta=round(coefs[46:48,2],3)
betas=c(-14.99,5.22,4.69)
sd_beta=c(761.86,253.95,253.95)

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

