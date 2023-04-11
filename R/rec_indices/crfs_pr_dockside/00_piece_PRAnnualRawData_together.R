###############################################################################
##### Piece back together the CDFW CRFS data
##### Each year and mode combination is in a separate Excel file
##### each with ~6 worksheets
##### Melissa Monk 6/21
##### Stitch PR back together
###############################################################################
#Remove all variables and turn off open graphics
rm(list=ls(all=TRUE))
graphics.off()



#load libraries
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(openxlsx)
library(readxl)
library(here)


###############################################################################
#setwd
dir <- file.path(here(),"data", "rec_indices", "crfs_pr_dockside")
setwd(dir)

##PR1 files by year
filePR1 = file.path(dir, "raw_data","PR1AnnualRawData", "PR1AnnualRawData_2015_PII removed.xlsx")
filePR2 = file.path(dir, "raw_data","PR1AnnualRawData", "PR1AnnualRawData_2016_PII removed.xlsx")
filePR3 = file.path(dir, "raw_data","PR1AnnualRawData", "PR1AnnualRawData_2017_PII removed.xlsx")
filePR4 = file.path(dir, "raw_data","PR1AnnualRawData", "PR1AnnualRawData_2018_PII removed.xlsx")
filePR5 = file.path(dir, "raw_data","PR1AnnualRawData", "PR1AnnualRawData_2019_PII removed.xlsx")
filePR6 = file.path(dir, "raw_data","PR1AnnualRawData", "PR1AnnualRawData_2020_PII removed.xlsx")
filePR7 = file.path(dir, "raw_data","PR1AnnualRawData", "PR1AnnualRawData_2021_PII removed.xlsx")
filePR8 = file.path(dir, "raw_data","PR1AnnualRawData", "PR1AnnualRawData_2022_PII removed.xlsx")

#Read in the 2015 sheets
sheetsPR1 = excel_sheets(filePR1)
#Put the sheets in a list as data frames
PR1 = lapply(excel_sheets(filePR1), read_excel, path = filePR1)
#Rename the sheets
names(PR1) = sheetsPR1

#Read in the 2016 sheets
sheetsPR2 = excel_sheets(filePR2)
#Put the sheets in a list as data frames
PR2 = lapply(excel_sheets(filePR2), read_excel, path = filePR2)
#Rename the sheets
names(PR2) = sheetsPR2


#Read in the 2017 sheets
sheetsPR3 = excel_sheets(filePR3)
#Put the sheets in a list as data frames
PR3 = lapply(excel_sheets(filePR3), read_excel, path = filePR3)
#Rename the sheets
names(PR3) = sheetsPR3


#Read in the 2018 sheets
sheetsPR4 = excel_sheets(filePR4)
#Put the sheets in a list as data frames
PR4 = lapply(excel_sheets(filePR4), read_excel, path = filePR4)
#Rename the sheets
names(PR4) = sheetsPR4


#Read in the 2019 sheets
sheetsPR5 = excel_sheets(filePR5)
#Put the sheets in a list as data frames
PR5 = lapply(excel_sheets(filePR5), read_excel, path = filePR5)
#Rename the sheets
names(PR5) = sheetsPR5


#Read in the 2020 sheets
sheetsPR6 = excel_sheets(filePR6)
#Put the sheets in a list as data frames
PR6 = lapply(excel_sheets(filePR6), read_excel, path = filePR6)
#Rename the sheets
names(PR6) = sheetsPR6

#Read in the 2021 sheets
sheetsPR7 = excel_sheets(filePR7)
#Put the sheets in a list as data frames
PR7 = lapply(excel_sheets(filePR7), read_excel, path = filePR7)
#Rename the sheets
names(PR7) = sheetsPR7

#Read in the 2021 sheets
sheetsPR8 = excel_sheets(filePR8)
#Put the sheets in a list as data frames
PR8 = lapply(excel_sheets(filePR8), read_excel, path = filePR8)
#Rename the sheets
names(PR8) = sheetsPR8

##Merge the lists together
PRa = map2(PR1, PR2, ~rbind(.x, setNames(.y, names(.x))))
PRb = map2(PRa, PR3, ~rbind(.x, setNames(.y, names(.x))))
PRc = map2(PRb, PR4, ~rbind(.x, setNames(.y, names(.x))))
PRd = map2(PRc, PR5, ~rbind(.x, setNames(.y, names(.x))))
PRe = map2(PRd, PR6, ~rbind(.x, setNames(.y, names(.x))))
PRf = map2(PRe, PR7, ~rbind(.x, setNames(.y, names(.x))))
PRfinal = map2(PRf, PR8, ~rbind(.x, setNames(.y, names(.x))))
#change names of tables
PRnew.names = paste0('PR_',sheetsPR1)
names(PRfinal) = PRnew.names

#Remove all of the extraneous blank columns in each dataframe in the list
PRfinal = map(PRfinal, ~select(.x, -starts_with('.')))

#Change the names of the table columns to try and match what's in SQL already  
#Remove # sign and replace with Num
PRfinal = map(PRfinal, ~setNames(.x, sub(' #', 'Num', names(.x))))
PRfinal = map(PRfinal, ~setNames(.x, sub('#', 'Num', names(.x))))

#Remove /
PRfinal = map(PRfinal, ~setNames(.x, sub('/', '', names(.x))))

#Remove any spaces
PRfinal = map(PRfinal, ~setNames(.x, sub(' ', '', names(.x))))

#Remove any spaces - have to do it twice??
PRfinal = map(PRfinal, ~setNames(.x, sub(' ', '', names(.x))))  

#Remove any spaces - have to do it thrice??
PRfinal = map(PRfinal, ~setNames(.x, sub(' ', '', names(.x)))) 


#Remove '- '
PRfinal = map(PRfinal, ~setNames(.x, sub('- ', '', names(.x))))  

#Remove '?' - didn't work - maybe differnt font issue
PRfinal = map(PRfinal, ~setNames(.x, sub('?', '', names(.x))))



#Write the final file to Excl
write.xlsx(PRfinal, file.path(dir, "raw_data","PR1Annual_Stitched_Together", "PRfinal.xlsx"))

#Also separate into dataframes and write as .csv
# Bring the dataframes to the global environment
list2env(PRfinal, .GlobalEnv)


write.csv(PR_BioData, file.path(dir, "raw_data","PR1Annual_Stitched_Together", "PR_Biodata.csv"), row.names=FALSE)
write.csv(PR_Catch,   file.path(dir, "raw_data","PR1Annual_Stitched_Together", "PR_Catch.csv"), row.names=FALSE)
write.csv(PR_Effort,  file.path(dir, "raw_data","PR1Annual_Stitched_Together", "PR_Effort.csv"), row.names=FALSE)
write.csv(PR_Header,  file.path(dir, "raw_data","PR1Annual_Stitched_Together", "PR_Header.csv"), row.names=FALSE)
write.csv(`PR_Time and Trailer Count`,  
          file.path(dir, "raw_data","PR1Annual_Stitched_Together", "PR_TimeTrailer.csv"), row.names=FALSE)



