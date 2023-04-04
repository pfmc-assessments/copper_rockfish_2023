#Copper rockfish maturity analysis#
#using data from Melissa Head
# Melissa Monk 2023 - stealing some of E.J's code

#Remove all variables and turn off open graphics
rm(list=ls(all=TRUE))
graphics.off()

library(tidyr)
library(dplyr)
library(here)
library(car)
library(ggplot2)

dir<- file.path(here(),"data","biology","maturity")
setwd(dir)

Cop.mat<-read.csv("2014_2019WCGBT_HL_CopperMaturity1.csv")
data <- Cop.mat




# using 'certainty=1' data, functional maturity only
data_cert <- data %>%
  filter(Certainty == 1, 
         !is.na(Certainty),
         !is.na(Functional_maturity),
         Latitude > 32.40) %>%
           dplyr::select(Functional_maturity,
                           Length, Latitude, Year, 
                                 month, Survey.Type) %>%
  mutate(area = ifelse(Latitude <= 34.45,"south", "north")) %>%
  mutate(FL_2cm = floor(Length/2)*2) %>%
  mutate_at(vars(Survey.Type, month, area), as.factor)

#changing the value for the WCGBT fish 45 cm to functionally mature;
#comment for this fish: This fish was post spawning, a residual embryo present
data_cert$Functional_maturity[data_cert$Length==45.0 & data_cert$Year == 2016] <- 1


with(data_cert, table(area, Functional_maturity))
#Functional_maturity
#area   0  1
#north 15 14
#south 33 78

ggplot(data_cert, aes(x = Length, y = Functional_maturity)) +
  geom_point()


###########Biological Maturity###########
data.glm <- glm(Functional_maturity ~ Length, data=data_cert, 
                family = binomial(link ="logit"))
summary(data.glm)

##see if area is significant - yes, but low sample sizes in the north
data.glm1 <- glm(Functional_maturity ~ Length + area, data=data_cert, 
                family = binomial(link ="logit"))
summary(data.glm1)

#see if month is significant - doesn't seem to be
data.glm2 <- glm(Functional_maturity ~ Length + as.factor(month), data=data_cert, 
                 family = binomial(link ="logit"))
summary(data.glm2)



# results above match the 'deltaMethod' function in the 'car' package,
# which also appears to be using first-order Taylor Series approximation
L50 <- deltaMethod(data.glm, "-b0/b1", parameterNames= paste("b", 0:1, sep=""))
L50

obs.prop.df <- as.data.frame(data_cert %>% group_by(FL_2cm) %>% 
                               summarize(n=length(Functional_maturity),
                                         s=sum(Functional_maturity),
                                         obs.prop=mean(Functional_maturity)))
obs.prop.df

f <- function(x, a=coef(data.glm)[1], b=coef(data.glm)[2])
{
  eta <- a+b*x
  exp(eta) / (1 + exp(eta))
}

# plot the predicted curve against binned proportions
# added 1 to the lower edge to 'place' the proportion in the middle of each 2cm bin
#png(filename = "ilion_functional_maturity.png", width = 7, height = 5, units = "in", res = 600)
with(obs.prop.df, plot(FL_2cm+1, obs.prop, 
                       xlim=c(0,65), bty='l', 
                       xlab="Fork Length [cm]", 
                       ylab="Proportion Mature"))
curve(f,0,65,add=TRUE)
text(10,0.8,paste0("L50 = ",round(L50[[1]],2)))
#dev.off()
#----------------------------------------------------------------------------
######Now what if we limit it to just Sept/Oct
ggplot(data_cert, aes(x = Length, 
                      color = month,
                      fill = month)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~Survey.Type, ncol = 1)
  
with(data_cert, table(month,Survey.Type))
#     Survey.Type
#month HL WCGBT
# 6    0     5
# 7    0    23
# 9   71     4
# 10  23    14

data_cert_fall <- data_cert %>%
  filter(month %in% c(9, 10))

data.glm.fall <- glm(Functional_maturity ~ Length, data=data_cert_fall, 
                family = binomial(link ="logit"))
summary(data.glm.fall)

L50_fall <- deltaMethod(data.glm.fall, "-b0/b1", parameterNames= paste("b", 0:1, sep=""))
L50_fall
obs.prop.df.fall <- as.data.frame(data_cert_fall %>% group_by(FL_2cm) %>% 
                               summarize(n=length(Functional_maturity),
                                         s=sum(Functional_maturity),
                                         obs.prop=mean(Functional_maturity)))
obs.prop.df.fall
with(obs.prop.df.fall, plot(FL_2cm+1, obs.prop, 
                       xlim=c(0,65), bty='l', 
                       xlab="Fork Length [cm]", 
                       ylab="Proportion Mature"))
curve(f,0,65,add=TRUE)
text(10,0.8,paste0("L50 = ",round(L50_fall[[1]],2)))

#-------------------------------------------------------------------------------
#####Look at north and south
with(data_cert_fall, table(area))
#area
#north south 
#18    94 

##north
data.glm.north <- glm(Functional_maturity ~ Length, 
                      data = data_cert %>% filter(area=="north"), 
                       family = binomial(link ="logit"))
summary(data.glm.north)
L50_north <- deltaMethod(data.glm.north, "-b0/b1", 
                         parameterNames= paste("b", 0:1, sep=""))
L50_north
  
##south
data.glm.south <- glm(Functional_maturity ~ Length, 
                      data = data_cert %>% filter(area=="south"), 
                      family = binomial(link ="logit"))
summary(data.glm.south)
L50_south <- deltaMethod(data.glm.south, "-b0/b1", 
                         parameterNames= paste("b", 0:1, sep=""))
L50_south



#-------------------------------------------------------------------------------
#####Look at north and south fall only
##north
data.glm.north.fall <- glm(Functional_maturity ~ Length, 
                      data = data_cert_fall %>% filter(area=="north"), 
                      family = binomial(link ="logit"))
summary(data.glm.north)
L50_north_fall <- deltaMethod(data.glm.north.fall, "-b0/b1", 
                         parameterNames= paste("b", 0:1, sep=""))
L50_north_fall

##south
data.glm.south.fall <- glm(Functional_maturity ~ Length, 
                      data = data_cert_fall %>% filter(area=="south"), 
                      family = binomial(link ="logit"))
summary(data.glm.south)
L50_south_fall <- deltaMethod(data.glm.south.fall, "-b0/b1", 
                         parameterNames=paste("b", 0:1, sep=""))
L50_south_fall



#combine all estimates in a dataframe
all.estimates <- 
  data.frame(rbind(L50, L50_fall, L50_north, L50_south, L50_fall, 
        L50_north_fall, L50_south_fall))
all.estimates$model <- 
  c("L50", "L50_fall", "L50_north", "L50_south", "L50_fall", 
        "L50_north_fall", "L50_south_fall")

all.estimates

sample_size <- data_cert %>%
  mutate(season = ifelse(month %in% c(9,10),"fall","summer")) %>%
  group_by(season, area, Functional_maturity) %>%
  summarise(SampleSize = n())
sample_size

#Leaning towards exluding the summer samples since the notes in Melissa Head's
#file indicates that the certainty is lower in the summer months; 
#18 fish from 34.5 to 48 marked as uncertain, but she couldn't tell if they 
#had spawned or were resting; 

####E.J. did this all by hand to make sure the car package gave the correct 
####answer
# # get bits and pieces for delta method "by hand"
# vcov(data.glm)
# a <- coef(data.glm)[[1]]
# b <- coef(data.glm)[[2]]
# var.a <- vcov(data.glm)[1,1]
# var.b <- vcov(data.glm)[2,2]
# cov.ab <- vcov(data.glm)[2,1]
# a
# b
# var.a
# var.b
# cov.ab
# 
# # first-order delta method for ratio of correlated random variables
# e.first <- -a/b
# v.first <- (a/b)^2 * (var.a/(a^2) + var.b/(b^2) - 2*cov.ab/(a*b))
# s.first <- sqrt(v.first)
# int.first.95 <- c(e.first - qnorm(0.975)*s.first, e.first + qnorm(0.975)*s.first)
# e.first
# v.first
# s.first
# int.first.95
