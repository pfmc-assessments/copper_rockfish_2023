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
setwd("C:/Assessments/2023/copper_rockfish_2023/models/nca/_sensitivities/bivariat_m_h")

# set 1 #####
mydir = file.path(getwd(), "9.8_selex_fix")
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
  linenum = c(73, 122), #mortality, steepness
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
Sdgrid1=readxl::read_xlsx("bivariate_post-STAR/southbase.bivariate.table.xlsx",sheet = "longformat")
Sdgrid=rbind(Sdgrid1,Sdgrid)
Sdgrid=dplyr::arrange(Sdgrid,h,M)
write.csv(Sdgrid,"bivariate_post-STAR/south_bivariate_table.csv",row.names = F)

#base model
basemodel <- SS_output("C:/Assessments/2023/copper_rockfish_2023/models/nca/_sensitivities/9.8_selex_fix")
Mbase=basemodel$parameters["NatM_uniform_Fem_GP_1","Value"]
hbase=basemodel$parameters["SR_BH_steep","Value"]
nllbase=basemodel$likelihoods_used["TOTAL","values"]

#figures 

#model with min NLL
Sdgridmin=Sdgrid[which.min(Sdgrid$NLL),]
#models within 2 of min NLL
Sdgridminset=Sdgrid[which(abs(Sdgrid$NLL-nllbase)<1.386),]

p1=ggplot(Sdgrid,aes(x=M,y=h,fill=NLL)) +
  geom_tile() +
  geom_tile(data=Sdgridminset, color="tomato", lwd=1, fill=NA) +
  geom_point(data=data.frame(M=Mbase,h=hbase), fill="white", color="black",pch=21, size=2) +
  geom_text(aes(label=round(NLL,1)), color="white") +
  #geom_text(data=Sdgridmin, aes(label=round(NLL,1)), color="tomato") +
  theme_bw() + scale_fill_viridis_c(direction = -1) +
  scale_x_continuous(expand = c(0,0), breaks = unique(Sdgrid$M)) + 
  scale_y_continuous(expand = c(0,0), breaks = unique(Sdgrid$h))
#ggsave("bivariate_post-STAR/S_Mhgrid_nll.png",width = 5.5,height = 4)

p2=ggplot(Sdgrid,aes(x=M,y=h,fill=Depletion)) +
  geom_tile() +
  geom_tile(data=Sdgridminset, color="tomato", lwd=1, fill=NA) +
  geom_point(data=data.frame(M=Mbase,h=hbase), fill="white", color="black",pch=21, size=2) +
  geom_text(aes(label=round(Depletion,2)), color="white") +
  #geom_text(data=Sdgridmin, aes(label=round(Depletion,2)), color="tomato") +
  theme_bw() + scale_fill_viridis_c(direction = 1) +
  scale_x_continuous(expand = c(0,0), breaks = unique(Sdgrid$M)) + 
  scale_y_continuous(expand = c(0,0), breaks = unique(Sdgrid$h))
#ggsave("bivariate_post-STAR/S_Mhgrid_depletion.png",width = 5.5,height = 4)

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
