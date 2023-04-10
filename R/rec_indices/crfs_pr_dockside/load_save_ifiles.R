#########################################################################
### Load and save the original i3 and i1 files
### Copper assessment 2023
### Melissa Monk
#########################################################################
library(readxl)
library(dplyr)
library(tidyr)

dir <- file.path(here(),"rec_indices", "crfs_pr_dockside")

i3file <- read_xlsx(file.path(dir, "i_files", "PR_i3_2004-2015_759607r.xlsx"))
i1file <- read_xlsx(file.path(dir, "i_files", "PR_i1_2004-2015_487087r.xlsx"))
i2file <- read_xlsx(file.path(dir, "i_files", "PR_i2_2004-2015_429673r.xlsx"))


save(i1file, i2file, i3file, file = "ifiles.RData")
     