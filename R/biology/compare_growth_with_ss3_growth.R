library(here)

ages <- 1:50
a.linear <- 1
L1f <- 21.4
L2f <- 47.3
kf <- 0.17
L1m <- 20.3
L2m <- 46.4
km <- 0.20
a3 <- 2
a4 <- 20
len.step <- 4:54

len.slope <- (L1f-len.step[1])/a3
len_lin_f <- len.step[1]+len.slope*(seq(1,(a.linear+1),1)-1)  #For smallest fish length is a linear function  

len.slope <- (L1m-len.step[1])/a3
len_lin_m <- len.step[1]+len.slope*(seq(1,(a.linear+1),1)-1)  #For smallest fish length is a linear function  


Linf_f <-  L1f + ((L2f - L1f) / (1 - exp( -kf * (a4 - a3))))
Linf_m <-  L1m + ((L2m - L1m) / (1 - exp( -km * (a4 - a3))))

#Length at the start of the year (cm)
#Growth based on the VB
len_m <- c(len_lin_m, Linf_m+(L1m-Linf_m)*exp(-km*ages[3:50]))
len_f <- c(len_lin_f, Linf_f+(L1f-Linf_f)*exp(-kf*ages[3:50]))


# Model estimated
L1f <- 14.58
L2f <- 48.3
kf <- 0.154
L1m <- 12.64
L2m <- 46.49
km <- 0.195
ages <- 1:50
sexes <- 2
a3 <- 2; a4 <- 20
len <- matrix(NA, max(ages), sexes)
mid.len <- matrix(0, max(ages), sexes)

#L infinity (cm)
Linf_f <- L1f + ((L2f - L1f) / (1 - exp( -kf * (a4 - a3))))
Linf_m <- L1m + ((L2m - L1m) / (1 - exp( -km * (a4 - a3))))
len.slope.f <- (L1f-len.step[1])/a3
len.slope.m <- (L1m-len.step[1])/a3

#Length at the start of the year (cm)
len[1:(a.linear+1),1]<-len.step[1]+len.slope.f*(seq(1,(a.linear+1),1)-1)  #For smallest fish length is a linear function  
len[1:(a.linear+1),2]<-len.step[1]+len.slope.m*(seq(1,(a.linear+1),1)-1) 

#Growth based on the VB
len[(a.linear+2):max(ages), 2] <- Linf_m+(L1m-Linf_m)*exp(-km*((seq(a.linear+2,max(ages),1)-1)-a3))
len[(a.linear+2):max(ages), 1] <- Linf_f+(L1f-Linf_f)*exp(-kf*((seq(a.linear+2,max(ages),1)-1)-a3))


#=======================================================================
colors <- viridis::viridis(2)

HandyCode::pngfun(wd = file.path(here(), "data", "biology", "plots"), 'north_external_schnute_model_estiamted.png.png', w = 10, h = 7)

plot(ages, len_m, type = 'l', col = colors[1], lwd = 2, ylim = c(0, 55),
     ylab = "Length (cm)", xlab = "Age")
lines(ages, len_f, lty = 1, col = colors[2], lwd = 2)
lines(1:50, len[,2], lty = 2, col = colors[1], lwd = 2)
lines(1:50, len[,1], lty = 2, col = colors[2], lwd = 2)
legend('bottomright', bty = 'n', lty = c(1,1,2,2), col = c(colors[1:2], colors[1:2]), lwd = 2, 
       legend = c("Male - External", "Female - External", "Male - Model Est.",
                  "Female - Model Est."))
dev.off()


#===============================================================================================
# South

ages <- 1:50
a.linear <- 1
L1f <- 12.78
L2f <- 44.928
kf <- 0.243
L1m <- 12.72
L2m <- 44.23
km <- 0.279
a3 <- 2
a4 <- 20
len.step <- 4:54

len.slope <- (L1f-len.step[1])/a3
len_lin_f <- len.step[1]+len.slope*(seq(1,(a.linear+1),1)-1)  #For smallest fish length is a linear function  

len.slope <- (L1m-len.step[1])/a3
len_lin_m <- len.step[1]+len.slope*(seq(1,(a.linear+1),1)-1)  #For smallest fish length is a linear function  


Linf_f <-  L1f + ((L2f - L1f) / (1 - exp( -kf * (a4 - a3))))
Linf_m <-  L1m + ((L2m - L1m) / (1 - exp( -km * (a4 - a3))))

#Length at the start of the year (cm)
#Growth based on the VB
len_m <- Linf_m+(L1m-Linf_m)*exp(-km*ages)
len_f <- Linf_f+(L1f-Linf_f)*exp(-kf*ages)


# Model estimated
L1f <- 15.4
L2f <- 46.8
kf <- 0.191
L1m <- 15.77
L2m <- 45.401
km <- 0.215
ages <- 1:50
sexes <- 2
a3 <- 2; a4 <- 20
len <- matrix(NA, max(ages), sexes)
mid.len <- matrix(0, max(ages), sexes)

#L infinity (cm)
Linf_f <- L1f + ((L2f - L1f) / (1 - exp( -kf * (a4 - a3))))
Linf_m <- L1m + ((L2m - L1m) / (1 - exp( -km * (a4 - a3))))
len.slope.f <- (L1f-len.step[1])/a3
len.slope.m <- (L1m-len.step[1])/a3

#Length at the start of the year (cm)
len[1:(a.linear+1),1]<-len.step[1]+len.slope.f*(seq(1,(a.linear+1),1)-1)  #For smallest fish length is a linear function  
len[1:(a.linear+1),2]<-len.step[1]+len.slope.m*(seq(1,(a.linear+1),1)-1) 

#Growth based on the VB
len[(a.linear+2):max(ages), 2] <- Linf_m+(L1m-Linf_m)*exp(-km*((seq(a.linear+2,max(ages),1)-1)-a3))
len[(a.linear+2):max(ages), 1] <- Linf_f+(L1f-Linf_f)*exp(-kf*((seq(a.linear+2,max(ages),1)-1)-a3))


#=======================================================================
colors <- viridis::viridis(2)

HandyCode::pngfun(wd = file.path(here(), "data", "biology", "plots"), 'south_external_schnute_model_estiamted.png.png', w = 10, h = 7)

plot(ages, len_m, type = 'l', col = colors[1], lwd = 2, ylim = c(0, 55),
     ylab = "Length (cm)", xlab = "Age")
lines(ages, len_f, lty = 1, col = colors[2], lwd = 2)
lines(1:50, len[,2], lty = 2, col = colors[1], lwd = 2)
lines(1:50, len[,1], lty = 2, col = colors[2], lwd = 2)
legend('bottomright', bty = 'n', lty = c(1,1,2,2), col = c(colors[1:2], colors[1:2]), lwd = 2, 
       legend = c("Male - External", "Female - External", "Male - Model Est.",
                  "Female - Model Est."))
dev.off()
