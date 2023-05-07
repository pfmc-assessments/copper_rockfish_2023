###################################################################################
#
#              Copper rockfish 2023
#   Estimate biological parameters using survey and 
# 	 age and length read from various data sources.	
#   			   February 2023
#
#############################################################################################

library(nwfscSurvey)
library(ggplot2)
library(here)
library(FSA)
library(FSAdata)
library(nlstools)
library(here)
library(dplyr)
library(tidyr)

dir <- file.path(here(), "data")
setwd(here())

pngfun <- function(dir, name, w = 7,h = 7, pt = 12){
  file <- file.path(dir, name)
  #cat('writing PNG to',file,'\n')
  png(filename=file,
      width=w,height=h,
      units='in',res=300,pointsize=pt)
}

load(file.path(getwd(),"data","ages", 
               "formatted_age_files", 
               "cleaned_all_copper_ages.Rdata"))

Alldat <- all_ages %>%
  filter(!Sex =="U") %>%
  rename(Length = length_cm,
         Program = program) %>%
    mutate_at(vars(Sex, area), as.factor)


###############################################################################
#Vonbert Models
###############################################################################
Startval = vbStarts(Length~Age, data=Alldat)
Startval=list(Linf=49,K=.2,t0=-1)
Startvalt0=list(Linf=49,K=.2)
####fit model to all data and also force through t0
vbTypical <- Length~Linf*(1-exp(-K*(Age-t0)))
vbTypicalt0 <- Length~Linf*(1-exp(-K*(Age-0)))
fitTyp = nls(vbTypical, data=Alldat, start=Startval)
fitTypt0 = nls(vbTypicalt0, data=Alldat, start=Startvalt0)
# Linf = 29.03  K=0.20158    t0= -0.9939


# Look at difference among Growth_var
#2 Growth_var model
svGen = lapply(Startval,rep,2)

vbGen <- Length~Linf[Growth_var]*(1-exp(-K[Growth_var]*(Age-t0[Growth_var])))
##########Suite of models
# 1 Linf, 2 K, 2 t0 
vb1L2K2T <- Length~Linf*(1-exp(-K[Growth_var]*(Age-t0[Growth_var])))
sv1L2K2T <- mapply(rep,Startval,c(1,2,2))

# 2 Linf, 1 K, 2 t0
vb2L1K2T <- Length~Linf[Growth_var]*(1-exp(-K*(Age-t0[Growth_var])))
sv2L1K2T <- mapply(rep,Startval,c(2,1,2))

# 2 Linf, 2 k, 1 t0
vb2L2K1T <- Length~Linf[Growth_var]*(1-exp(-K[Growth_var]*(Age-t0)))
sv2L2K1T <- mapply(rep,Startval,c(2,2,1))

# 1 Linf, 1 K, 2 t0
vb1L1K2T <- Length~Linf*(1-exp(-K*(Age-t0[Growth_var])))
sv1L1K2T <- mapply(rep,Startval,c(1,1,2))

# 1 Linf, 2 k, 1 t0
vb1L2K1T <- Length~Linf*(1-exp(-K[Growth_var]*(Age-t0)))
sv1L2K1T <- mapply(rep,Startval,c(1,2,1))

# 2 Linf, 1 K, 1 t0
vb2L1K1T <- Length~Linf[Growth_var]*(1-exp(-K*(Age-t0)))
sv2L1K1T <- mapply(rep,Startval,c(2,1,1))

# All parameters in common
vbCom <- Length~Linf*(1-exp(-K*(Age-t0)))


##########################################################################################
####All data looking at difference in Growth_var
#remove some outliers

d = ggplot(Alldat, aes(x=Age, y=Length, color=Sex)) 
d + geom_point() + facet_wrap(~Program, ncol=2)


Alldat$Growth_var <- Alldat$Sex
####Fit the suite of models looking at Growth_var
fitGen <- nls(vbGen, data = Alldat, start = svGen)
fit1L2K2T <- nls(vb1L2K2T,data=Alldat,start=sv1L2K2T)
fit2L1K2T <- nls(vb2L1K2T,data=Alldat,start=sv2L1K2T)
fit2L2K1T <- nls(vb2L2K1T,data=Alldat,start=sv2L2K1T)
fit1L1K2T <- nls(vb1L1K2T,data=Alldat,start=sv1L1K2T)
fit1L2K1T <- nls(vb1L2K1T,data=Alldat,start=sv1L2K1T)
fit2L1K1T <- nls(vb2L1K1T,data=Alldat,start=sv2L1K1T)
fitCom <- nls(vbCom,data=Alldat,start=Startval)



# compare AIC
AIC(fitGen, fit1L2K2T, fit2L1K2T, fit2L2K1T, fit1L1K2T, fit1L2K1T, fit2L1K1T, fitCom)  #fit1L2K2T
BIC(fitGen, fit1L2K2T, fit2L1K2T, fit2L2K1T, fit1L1K2T, fit1L2K1T, fit2L1K1T, fitCom)

anova(fitGen,fit2L2K1T, fitCom)  #no difference

coef(fitGen)
#Linf1      Linf2         K1         K2        t01        t02 
#46.8333503 45.7232419  0.2061381  0.2337891 -0.8046669 -0.5878047 

coef(fitCom)
#Linf          K         t0 
#46.2392153  0.2198119 -0.6867527








### Plot the fit1L1K2T model
x11()
#png("vonB_fitGen.png", res=500,units='in',width=7,height=7)
plot(Length~jitter(Age,0.3),data=Alldat,subset=Growth_var=="F",pch=16,xlab="Age (yrs)",
     col=gray(level=0, alpha=0.2),ylab="Total Length (cm)",xlim=c(0,30),ylim=c(0,40))
points(Length~jitter(Age,0.3),data=Alldat,subset=Growth_var=="M",pch=15,col=gray(level=0, alpha=0.2))
points(Length~jitter(Age,0.3),data=Alldat,subset=Growth_var=="U",pch=17,col=gray(level=0, alpha=0.2))
vbTypical <- vbFuns("typical")
curve(vbTypical(x,Linf = coef(fit1L1K2T)[1], K=coef(fit1L1K2T)[2], t0=coef(fit1L1K2T)[3]),from=1,to=30,lwd=2,add=TRUE,col="maroon3")
curve(vbTypical(x,Linf = coef(fit1L1K2T)[1], K=coef(fit1L1K2T)[2], t0=coef(fit1L1K2T)[4]),from=1,to=30,lwd=2,add=TRUE,col="dodgerblue3")
curve(vbTypical(x,Linf = coef(fitCom)[1], K=coef(fitCom)[2], t0=coef(fitCom)[3]),from=1,to=30,lwd=2,add=TRUE,col="green")
legend("topleft",legend=c("BYEL","BYEL","GPHR","GPHR","Unkown","Common"),
       col=c("maroon3","gray","dodgerblue2","gray",'gray','green'),
       lwd=3,lty=c(1,NA,1,NA,NA,1),pch=c(NA,16,NA,15,17,NA),cex=.9)


text(12,8,"BYEL \nLinf = 28.5  \nK = 0.223  \nt0 = -0.443", cex=.9)
text(20,8,"GPHR  \nLinf = 28.5  \nK = 0.223  \nt0 = -1.215", cex=.9)
text(28,8,"GPHR  \nLinf = 28.39  \nK = 0.245  \nt0 = -0.217", cex=.9)

################################################################################
#Now look at north and south differnces
####Fit the suite of models looking at area
Alldat$Growth_var <- Alldat$area
fitGen <- nls(vbGen, data = Alldat, start = svGen)
fit1L2K2T <- nls(vb1L2K2T,data=Alldat,start=sv1L2K2T)
fit2L1K2T <- nls(vb2L1K2T,data=Alldat,start=sv2L1K2T)
fit2L2K1T <- nls(vb2L2K1T,data=Alldat,start=sv2L2K1T)
fit1L1K2T <- nls(vb1L1K2T,data=Alldat,start=sv1L1K2T)
fit1L2K1T <- nls(vb1L2K1T,data=Alldat,start=sv1L2K1T)
fit2L1K1T <- nls(vb2L1K1T,data=Alldat,start=sv2L1K1T)
fitCom <- nls(vbCom,data=Alldat,start=Startval)



# compare AIC
AIC(fitGen, fit1L2K2T, fit2L1K2T, fit2L2K1T, fit1L1K2T, fit1L2K1T, fit2L1K1T, fitCom)  #fit1L2K2T
BIC(fitGen, fit1L2K2T, fit2L1K2T, fit2L2K1T, fit1L1K2T, fit1L2K1T, fit2L1K1T, fitCom)

anova(fitGen,fit2L2K1T)  #no difference

coef(fitGen)
#Linf1      Linf2         K1         K2        t01        t02 
#46.8333503 45.7232419  0.2061381  0.2337891 -0.8046669 -0.5878047  

#------------------------------------------------------------------------------
schnute_params <- data.frame(matrix(ncol = 4, nrow = 3, 
                                    dimnames = list(c("L1","L3","k"),
                                                    c("North_F","North_M","South_F","South_M"))))

#Schnute parameterization
SchStarts = vbStarts(Length~Age, data=Alldat,type='Schnute')
SchStarts

vb3 <- vbFuns("Schnute",simple=FALSE)

fit_north_F <- nls(Length~vb3(Age,L1, L3,K, t1=2,t3=20),
                   data = Alldat %>% filter(area=="north", Sex == "F"),
            start=SchStarts)
schnute_params$North_F <- coef(fit_north_F)

fit_north_M <- nls(Length~vb3(Age,L1, L3,K, t1=2,t3=20),
                   data = Alldat %>% filter(area=="north", Sex == "M"),
                   start=SchStarts)
schnute_params$North_M <- coef(fit_north_M)

#south
fit_south_F <- nls(Length~vb3(Age,L1, L3,K, t1=2,t3=20),
                   data = Alldat %>% filter(area=="south", Sex == "F"),
                   start=SchStarts)
schnute_params$South_F <- coef(fit_south_F)

fit_south_M <- nls(Length~vb3(Age,L1, L3,K, t1=2,t3=20),
                   data = Alldat %>% filter(area=="south", Sex == "M"),
                   start=SchStarts)
schnute_params$South_M <- coef(fit_south_M)

write.csv(schnute_params, file.path(getwd(),"data","biology","External_growth_Schnute.csv"))

