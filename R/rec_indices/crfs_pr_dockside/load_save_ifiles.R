#########################################################################
### Load and save the original i3 and i1 files
### Copper assessment 2023
### Melissa Monk
#########################################################################
library(readxl)
library(dplyr)
library(tidyr)
library(here)

i3file <- read_xlsx("C:/Users/melissa.monk/Documents/CDFW data/CDFW_PR_2004_2012/i_Files/PR_i3_2004-2015_759607r.xlsx")
i1file <- read_xlsx("C:/Users/melissa.monk/Documents/CDFW data/CDFW_PR_2004_2012/i_Files/PR_i1_2004-2015_487087r.xlsx")
i2file <- read_xlsx("C:/Users/melissa.monk/Documents/CDFW data/CDFW_PR_2004_2012/i_Files/PR_i2_2004-2015_429673r.xlsx")

dir <- setwd("S:/copper_rockfish_2023/data/rec_indices/crfs_pr_dockside")
#dir <-file.path(here(),"data","rec_indices","crfs_pr_dockside")
setwd(dir)
save(i1file, i2file, i3file, file = "ifiles.RData")
     