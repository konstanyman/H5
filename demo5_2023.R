  
library(foreign)
dat<-read.spss("demoaineisto2015r.sav", to.data.frame=TRUE)
attach(dat)

# logistinen binäärinen regressio
logr_tyotilan <- glm(tyotilan ~ oma_tulo+ika+omistusa+koulutus, data=dat, family=binomial)
logr_tyotilan

# p-arvot

summary(logr_tyotilan)

# Odds Ratiot

exp(cbind(OR=coef(logr_tyotilan), confint(logr_tyotilan)))

# Selitysaste

install.packages ("fmsb")
library(fmsb)
data.nagel<-NagelkerkeR2(logr_tyotilan)
data.nagel

# multinomiaalinen logistinen regressio

install.packages("mnlogit")
install.packages("nnet")
library("mnlogit")
library("nnet")
dat$taltyyt2 <- relevel(dat$taltyyt, ref = "erittain tyytymaton")
test <- multinom(taltyyt2 ~ oma_tulo+ika+omistusa, data = dat)
summary (test)
confint(test)

# OR

exp(coef(test))

# Z-arvot

z <- summary(test)$coefficients/summary(test)$standard.errors
z

# P-arvot

p <- (1 - pnorm(abs(z), 0, 1))*2
p


#Loglineaarisen mallin muuttujien tarkastelu

#yhden muuttujan marginaalitaulut

attach(dat)
table(omistusa)
table(tyotilan)
table(yltyyt)


# Kolmen muuttujan taulu 

mytable <- table(omistusa,tyotilan,yltyyt) 
ftable(mytable)

# loglineaarinen mallitus

library(MASS)

mytable <- xtabs(~omistusa+tyotilan+yltyyt, data=dat)

# täydellinen riippumattomuus
loglm(~omistusa+tyotilan+yltyyt, mytable)
#
# yltyyt riippumaton muuttujaparista
loglm(~omistusa+tyotilan+yltyyt+tyotilan*omistusa, mytable)
#
# omistusa riippumaton muuttujaparista
loglm(~omistusa+tyotilan+yltyyt+tyotilan*yltyyt, mytable)
#
# tyotilan riippumaton muuttujaparista
loglm(~omistusa+tyotilan+yltyyt+omistusa*yltyyt, mytable)
#
#ehdollinen riippumattomuusmalli: omistusa ja yltyyt riippumattomia
loglm(~omistusa+tyotilan+yltyyt+omistusa*tyotilan+tyotilan*yltyyt, mytable)
#
#ehdollinen riippumattomuusmalli: tyotilan ja yltyyt asunto riippumattomia
loglm(~omistusa+tyotilan+yltyyt+omistusa*tyotilan+omistusa*yltyyt, mytable)
#
#ehdollinen riippumattomuusmalli: tyotilan ja omistusa riippumattomia
loglm(~omistusa+tyotilan+yltyyt+omistusa*yltyyt+tyotilan*yltyyt, mytable)
#
#parittaisten riippuvuuksien malli
loglm(~omistusa+tyotilan+yltyyt+omistusa*tyotilan+omistusa*yltyyt+ tyotilan*yltyyt, mytable)


# mallin jatkotarkastelu

taulu1 <- table(omistusa,yltyyt)  
prop.table(taulu1, 1) 
 
taulu2 <- table(tyotilan,yltyyt)  
prop.table(taulu2, 1) 
 
 