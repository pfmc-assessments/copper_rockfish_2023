
library(r4ss)
area <- "sca"
area <- 'nca'

user <- Sys.getenv("USERNAME")
if( grepl("Chantel", user) ){
  user_dir <- "C:/Assessments/2023/copper_rockfish_2023"
} else {
  # Fill in Melissa's document directory below
  user_dir <- "C:/Assessments/2023/copper_rockfish_2023"
}

wd <- file.path(user_dir, "models", area)

model <- SS_output(file.path(wd, "4.0_base"))
fleets <- model$FleetNames

info1 <- SSplotSelex(model, fleets = 1, fleetnames = fleets[1], subplot = 1, year = c(2001, 2016, 2017))
#info1$infotable$col <- rich.colors.short(8)[c(1,1)]

info2 <- SSplotSelex(model, fleets = 2, fleetnames = fleets[2], subplot = 1, year = c(1916, 2022))
#info1$infotable$col <- rich.colors.short(8)[c(1,1)]

info3 <- SSplotSelex(model, fleets = 3, subplot = 1, year = c(1916, 2002, 2017, 2022))
info1$infotable$col <- rich.colors.short(8)[c(1,1)]

info4 <- SSplotSelex(model, fleets = 4, subplot = 1, year = c(1916, 2002, 2017, 2022))
info1$infotable$col <- rich.colors.short(8)[c(1,1)]

info5 <- SSplotSelex(model, fleets = 5, fleetnames = fleets[5], subplot = 1, year = c(2016, 2022))
info5$infotable$col <- rich.colors.short(8)[c(1,1)]

info3 <- SSplotSelex(model, fleets = 3, subplot = 1, year = c(1916, 2002, 2017))


info4 <- SSplotSelex(model, fleets = 4, subplot = 1, year = c(1916, 2002, 2017))


info3 <- SSplotSelex(model, fleets = 3, subplot = 1, year = c(1916, 2000))
info4 <- SSplotSelex(model, fleets = 4, subplot = 1, year = c(1916, 2000))

1916 1999 2000 2021 
info3 <- SSplotSelex(model, fleets = 3, subplot = 1, year = c(1916, 2000, 2022))
info4 <- SSplotSelex(model, fleets = 4, subplot = 1, year = c(1916, 2000, 2022))

