
########################################################
# Ageing Error Analysis
########################################################
devtools::install_github("pfmc-assessments/nwfscAgeingError")


# Load package
library(dplyr)
library(nwfscAgeingError)
SourceFile <- file.path(system.file("executables", package = "nwfscAgeingError"), .Platform$file.sep)


library(here)
dir = file.path(here(), "data", "ages","ageing_error_2023")
reads <- read.csv(file = file.path(dir,  'double_reads_a1=patrick_a2=tyler.csv'), header = TRUE)
SourceFile <- dir


#filter out the zero age fish
ind = which(reads[,1] == 0)
if(length(ind) > 0) { reads = reads[-ind,] }

Nreaders <- dim(reads)[2]

reads2 <- reads %>%
  group_by(.dots = names(reads)) %>%
  summarise(count = n())

for(a in 1:nrow(reads2)){
  find = which(is.na(reads2[a,1:ncol(reads2)]))
  reads2[a, find] = -999
}

# Re-organize the columns into the right order
format_reads <- data.frame(
  Count = reads2$count,
  A1 = reads2$A1,
  A2 = reads2$A2
)

MinAge <- 1
MaxAge <- max(ceiling(max(reads2[, 1:2], na.rm = TRUE) / 10) * 10)
KnotAges = list(NA, NA)

BiasOpt.mat = SigOpt.mat =matrix(0, 6, 2)
BiasOpt.mat[1,] =  c(0,0)
BiasOpt.mat[2,] =  c(0,1)
BiasOpt.mat[3,] =  c(0,2)
BiasOpt.mat[4,] =  c(1,0)
BiasOpt.mat[5,] =  c(2,0)
BiasOpt.mat[6,] =  c(1,1)


SigOpt.mat[1,] =c(1,-1)
SigOpt.mat[2,] =c(2,-1)
SigOpt.mat[3,] =c(3,-1)
SigOpt.mat[4,] =c(1,-1)
SigOpt.mat[5,] =c(2,-1)
SigOpt.mat[6,] =c(3,-1)


model.aic <- as.data.frame(matrix(NA, 6, 4))
colnames(model.aic)<-c("Run","AIC","AICc","BIC")
model.name<-c("B0_S1","B0_S2","B0_S3","B1_S1","B2_S2", "B1_S3")
rownames(model.aic) <- model.name[1:6]

#shell("agemat.exe > output.txt 2>&1")

for(i in 1:6){
  setwd(dir)
  DateFile = paste(getwd(),"/",model.name[i],"/",sep="")
  dir.create(DateFile, showWarnings = FALSE)
  BiasOpt =BiasOpt.mat[i,]
  SigOpt = SigOpt.mat[i,]
  
  RunFn(Data = format_reads, SigOpt = SigOpt, KnotAges = KnotAges, BiasOpt = BiasOpt,
        NDataSets = 1, MinAge = MinAge, MaxAge = MaxAge, RefAge = 10,
        MinusAge = 2, PlusAge = 50,
        SaveFile = DateFile,
        AdmbFile = SourceFile, EffSampleSize = 0, Intern = FALSE,
        JustWrite = FALSE, CallType = "system")
  
  PlotOutputFn(Data = format_reads, MaxAge = MaxAge,
               SaveFile = DateFile, PlotType = "PNG"
  )
  
  Df = as.numeric(scan(paste(DateFile,"agemat.par",sep=""),comment.char="%", what="character", quiet=TRUE)[6])
  Nll = as.numeric(scan(paste(DateFile,"agemat.par",sep=""),comment.char="%", what="character", quiet=TRUE)[11])
  n = sum(ifelse(reads2[,-1]==-999,0,1))
  Aic = 2*Nll + 2*Df
  Aicc = Aic + 2*Df*(Df+1)/(n-Df-1)
  Bic = 2*Nll + Df*log(n)
  run.name<-strsplit(DateFile,"/")[[1]][3]
  model.aic[i,]<-c(run.name, Aic, Aicc, Bic)  
  setwd(dir)
}

model.aic$delta_aic <- as.numeric(model.aic[,"AIC"]) - min(as.numeric(model.aic[,"AIC"]))
model.aic$delta_aicc <- as.numeric(model.aic[,"AICc"]) - min(as.numeric(model.aic[,"AICc"]))
model.aic$delta_bic <- as.numeric(model.aic[,"BIC"]) - min(as.numeric(model.aic[,"BIC"]))
save(model.aic, file = file.path(dir, "model_selection.dmp", sep = ""))
