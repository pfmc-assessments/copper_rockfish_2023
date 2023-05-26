library(r4ss)
library(ggplot2)
library(cowplot)

getLtable=function(modelsum, legend.labels=NULL) {
  parameters <- modelsum$pars[modelsum$pars$recdev==FALSE, c(modelsum$n+1,1:modelsum$n)]
  parms.that.change <- parameters[apply(parameters[2:(modelsum$n+1)], 1, function(x) length(unique(x))>1),]
  # catchabilities are not estimated
  parms.of.interest <- subset(parms.that.change, substr(Label, 1, 3) != "LnQ")
  likelihood <- modelsum$likelihoods[modelsum$likelihoods$Label %in%
                                       c("TOTAL","Survey","Length_comp","Age_comp","Recruitment","Parm_priors",""), c(modelsum$n+1,1:modelsum$n)]
  # create row with number of estimated parameters per model (including rec devs)
  npar.vec <- likelihood[1,] # use first row of likelihood to initialize object
  npar.vec["Label"] <- "N.Parms"
  npar.vec[1,2:(modelsum$n+1)] <- modelsum$npar
  names(npar.vec) <- names(likelihood)
  
  # get derived quantities
  der.quants <- subset(modelsum$quants, Label %in% c("SSB_unfished","Totbio_unfished","Bratio_2023","Recr_unfished","Dead_Catch_SPR","OFLCatch_2023"))
  der.quants <- der.quants[, c(modelsum$n+1,1:modelsum$n)] # reorder columns
  
  model.comparison.table <- rbind(npar.vec, likelihood, parms.of.interest, der.quants)
  row.names(model.comparison.table) <- 1:nrow(model.comparison.table)
  if(!is.null(legend.labels)) names(model.comparison.table)[2:(modelsum$n+1)] <- legend.labels
  return(model.comparison.table)
}

#setwd("~/Rockfish/Rockfish assessment/vermillion 2021/SSmodel")
setwd("C:/Assessments/2023/copper_rockfish_2023/models/nca/_sensitivities/9.8_selex_fix_bivariate_m_equal")
setwd("C:/Assessments/2023/copper_rockfish_2023/models/sca/_sensitivities/14.0_base_bivariate_m_both")


# set 1 #####
mydir = getwd()
starter <- SS_readstarter(file.path(mydir, "starter.ss"))
# change control file name in the starter file
starter[["ctlfile"]] <- "control_modified.ss"
# make sure the prior likelihood is calculated
# for non-estimated quantities
starter[["prior_like"]] <- 1
# write modified starter file
SS_writestarter(starter, dir = mydir, overwrite = TRUE)

par_table = expand.grid(M = seq(0.05, 0.14, by = 0.01), h = seq(0.25, 0.95, by = 0.05))
#tester
# par_table=expand.grid(M=seq(0.11,0.115,by = 0.005),h=seq(0.75,0.80,by = 0.05))

profile <- r4ss::profile(
  dir = mydir, # directory
  oldctlfile = "control.ss_new",
  newctlfile = "control_modified.ss",
  #linenum = c(73, 122), #mortality, steepness
  linenum = c(76, 125), # sca
  profilevec = par_table,
  extras = "-nohess -nox"
)

# set 2 ####
#par_table2=rbind(expand.grid(M=seq(0.145,0.15,by = 0.005),h=seq(0.60,0.90,by = 0.05)),
#                 expand.grid(M=seq(0.11,0.15,by = 0.005),h=0.95))
#par_table3=rbind(par_table,par_table2)
#
#profile <- SS_profile(
#  dir = mydir, # directory
#  masterctlfile = "control.ss_new",
#  newctlfile = "control_modified.ss",
#  linenum = c(70, 118), #mortality, steepness
#  profilevec = par_table3,
#  whichruns = 29:51,
#  extras = "-nohess -nox"
#)

# get model output
profilemodels <- r4ss::SSgetoutput(
  dirvec = mydir,
  keyvec = 1:nrow(par_table))

#add base model (optional)
# basemodel <- SS_output("Southern CA Vermilion post-STAR base/")
# profilemodels[["base"]] <- basemodel

profilesummary <- SSsummarize(profilemodels)

ptable = getLtable(profilesummary)
sumtable = subset(ptable, Label %in% c("TOTAL", "NatM_uniform_Fem_GP_1", "SR_BH_steep",
                                     "Bratio_2023"))
sumtablelong=t(sumtable[c(2,3,1,4),2:ncol(sumtable)])
colnames(sumtablelong) = c("M", "h", "NLL", "Depletion")
rownames(sumtablelong) = NULL
Sdgrid = as.data.frame(sumtablelong)

#john's results
#Sdgrid1=readxl::read_xlsx("bivariate_post-STAR/southbase.bivariate.table.xlsx",sheet = "longformat")
#Sdgrid=rbind(Sdgrid1,Sdgrid)
#Sdgrid=dplyr::arrange(Sdgrid,h,M)
#write.csv(Sdgrid,"bivariate_post-STAR/south_bivariate_table.csv",row.names = F)

#base model
basemodel <- SS_output("C:/Assessments/2023/copper_rockfish_2023/models/nca/9.8_selex_fix")
basemodel <- SS_output("C:/Assessments/2023/copper_rockfish_2023/models/sca/14.0_base_forecast")

Mbase=basemodel$parameters["NatM_uniform_Fem_GP_1","Value"]
hbase=basemodel$parameters["SR_BH_steep","Value"]
nllbase=basemodel$likelihoods_used["TOTAL","values"]

#figures 

#model with min NLL
Sdgridmin=Sdgrid[which.min(Sdgrid$NLL),]
#models within 2 of min NLL
Sdgridminset=Sdgrid[which(abs(Sdgrid$NLL-nllbase) < 2.99),]
Sdgridnll=Sdgrid[Sdgrid$NLL == min(Sdgrid$NLL),]


ggplot(Sdgrid,aes(x=M,y=h,fill=NLL)) +
  geom_tile() +
  geom_tile(data=Sdgridminset, color="tomato", lwd=1, fill=NA) +
  #geom_tile(data=Sdgridnll, color="white", lwd=1, fill=NA) +
  geom_point(data=data.frame(M=Mbase,h=hbase), fill="white", color="black",pch=21, size=4) +
  geom_text(aes(label=round(NLL,1)), color="white", size = 4, fontface = "bold") +
  #geom_text(data=Sdgridmin, aes(label=round(NLL,1)), color="tomato") +
  theme_bw() + scale_fill_viridis_c(direction = 1) +
  scale_x_continuous(expand = c(0,0), breaks = unique(Sdgrid$M)) + 
  scale_y_continuous(expand = c(0,0), breaks = unique(Sdgrid$h)) +
  theme_bw(base_size = 18)
ggsave("_plots/S_Mhgrid_nll_both_sex_m_2.99.png",width = 10, height = 10)

ggplot(Sdgrid,aes(x=M,y=h,fill=Depletion)) +
  geom_tile() +
  geom_tile(data=Sdgridminset, color="tomato", lwd=1, fill=NA) +
  geom_point(data=data.frame(M=Mbase,h=hbase), fill="white", color="black",pch=21, size=4) +
  geom_text(aes(label=round(Depletion,2)), color="white", size = 5, parse = TRUE, fontface = "bold") +
  #geom_text(data=Sdgridmin, aes(label=round(Depletion,2)), color="tomato") +
  theme_bw() + scale_fill_viridis_c(direction = 1) +
  scale_x_continuous(expand = c(0,0), breaks = unique(Sdgrid$M)) + 
  scale_y_continuous(expand = c(0,0), breaks = unique(Sdgrid$h)) +
  theme_bw(base_size = 18)
ggsave("_plots/S_Mhgrid_depletion_both_sex_m_2.99.png",width = 10, height = 10)

#p3=ggplot(Sdgrid,aes(x=M,y=h,fill=EquilMSY)) +
#  geom_tile() +
#  geom_tile(data=Sdgridminset, color="tomato", lwd=1, fill=NA) +
#  geom_point(data=data.frame(M=Mbase,h=hbase), fill="white", color="black",pch=21, size=2) +
#  geom_text(aes(label=round(EquilMSY,1)), color="white") +
#  #geom_text(data=Sdgridmin, aes(label=round(EquilMSY,1)), color="tomato") +
#  theme_bw() + scale_fill_viridis_c(direction = 1) +
#  scale_x_continuous(expand = c(0,0), breaks = unique(Sdgrid$M)) + 
#  scale_y_continuous(expand = c(0,0), breaks = unique(Sdgrid$h))
#ggsave("bivariate_post-STAR/S_Mhgrid_equilmsy.png",width = 5.5,height = 4)

#p4=ggplot(Sdgrid,aes(x=M,y=h,fill=OFL_2023)) +
#  geom_tile() +
#  geom_tile(data=Sdgridminset, color="tomato", lwd=1, fill=NA) +
#  geom_point(data=data.frame(M=Mbase,h=hbase), fill="white", color="black",pch=21, size=2) +
#  geom_text(aes(label=round(OFL_2023,1)), color="white") +
#  #geom_text(data=Sdgridmin, aes(label=round(OFL_2023,1)), color="tomato") +
#  theme_bw() + scale_fill_viridis_c(direction = 1) +
#  scale_x_continuous(expand = c(0,0), breaks = unique(Sdgrid$M)) + 
#  scale_y_continuous(expand = c(0,0), breaks = unique(Sdgrid$h))
#ggsave("bivariate_post-STAR/S_Mhgrid_OFL.png",width = 5.5,height = 4)

plot_grid(p1,p2, nrow = 2)
ggsave("bivariate_post-STAR/S_Mhgrid_ALL.png",width = 12,height = 18)


#================================
out.mle = nllbase
output <- Sdgrid
base_vec <- c(0.108, 0.72, out.mle, basemodel$current_depletion)
output <- rbind(output, base_vec)
colnames(output) <- c("M", "h", "negLogLike", "Depletion")

out.mle <- output[nrow(output),]
out <- as.data.frame(output[ -nrow(output), ])
out$diffNegLogLike <- out$negLogLike - min(out$negLogLike)#out.mle["negLogLike"]
#out$diff_M <- out$M_f - out$M_m
out <- out[order(out$M, out$h),]

find <- out[out$negLogLike == min(out$negLogLike), ]

x <- unique(round(out$M, 4))
y <- unique(round(out$h, 4))
z <- matrix(out$diffNegLogLike, 
            ncol = length(y),
            byrow = TRUE, 
            dimnames = list(as.character(x), as.character(y)))

library(reshape2); library(metR)
mtrx_melt <- melt(z, id.vars = c("M", "h"), measure.vars = 'Delta_NLL')
names(mtrx_melt) = c("M", "h", "Delta_NLL")
min_nll <- mtrx_melt[mtrx_melt$Delta_NLL, ]

# Plot_ly figure
#plot_ly(mtrx_melt, x = ~M_f, y = ~M_m, z = ~Delta_NLL, type = 'contour', 
#        width = 600, height = 600)

#HandyCody::pngfun(wd = getwd(), file = "joint_m_profile_ggplot_large_range.png", w = 14, h = 12, pt = 12)
ggplot(mtrx_melt, aes(x = M, y = h)) +
    geom_contour_filled(aes(z = Delta_NLL), breaks = c(0, 2, 3, 4, 6, 10, 20, 50, seq(100, 600, 100))) +
    geom_point(aes (x = 0.108, y = 0.72),  size = 4, col = "white") +
    geom_point(aes (x = find[,1], y = find[,2]),  size = 4, col = "white") +
    annotate("text", x = find[,1] + 0.015, y = find[,2], label = "Lowest NLL", size =10, col = 'white') +
    annotate("text", x = 0.12, y = 0.72, label = "Base Model", size =10, col = 'white') +
    geom_text_contour(aes(z = Delta_NLL), 
       breaks = c(3, 5, 7, seq(10, 30, 10)), size = 7, color = 'white') +
    xlab("Natural Mortality (F)") +
    ylab("Steepness (h)") +
    scale_x_continuous(breaks = seq(0.05, 0.15, 0.01)) +
    scale_y_continuous(breaks = seq(0.20, 0.90, 0.1)) +
    theme(
      axis.text.y = element_text(size = 15, color = 1),
      axis.text.x = element_text(size = 15, color = 1), 
      axis.title.x = element_text(size = 20), 
      axis.title.y = element_text(size = 20),
      legend.text = element_text(size = 15), 
      legend.title = element_text(size = 15)) +
    guides(fill = guide_legend(title = "Change in NLL")) +
    theme_bw(base_size = 16)
ggsave(file.path(getwd(), "_plots", "joint_m_h_profile_ggsave.png"), width = 14, height = 12)
#dev.off()