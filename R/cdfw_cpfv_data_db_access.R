
library(DBI)
library(odbc)
library(dplyr)

con <- DBI::dbConnect(odbc::odbc(),
  driver = "SQL Server",
  server = "pinniger",
  database = "CDFW_CPFV_Logbook_Data",
  Trusted_Connection = "yes"
)
Effort2 <- DBI::dbSendQuery(con, "SELECT * from Logbook") #_Trips")
Effort1 <- DBI::dbFetch(Effort2)
DBI::dbClearResult(Effort2)

# Neither approach below works to filter the data
ind = which(Effort1$Species == "Rockfish_copper")
# 49106 copper observations
table(Effort1$LogYear[ind])

data  <- Effort1 %>%
  mutate_at("Species", as.character) %>%
  filter(
    Species == "Rockfish_copper"
  )



