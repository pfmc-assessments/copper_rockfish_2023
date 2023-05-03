
library(r4ss)
area <- "sca"
base_model <- "11.0_francis_cpfv_dome"
#area <- 'nca'

user <- Sys.getenv("USERNAME")
if( grepl("Chantel", user) ){
  user_dir <- "C:/Assessments/2023/copper_rockfish_2023"
} else {
  # Fill in Melissa's document directory below
  user_dir <- "C:/Assessments/2023/copper_rockfish_2023"
}

wd <- file.path(user_dir, "models", area)

model <- SS_output(file.path(wd, base_model))

colors <- viridis::viridis(9)
#================================================================================
# Southern Figure
#================================================================================

info1 <- SSplotSelex(model, fleets = 1,  subplot = 1, year = c(2022))
#info1$infotable$longname = c("Com. Dead 1916-2016", "Com. Dead 2017-2022")
info1$infotable$col <- c(colors[1])

info2 <- SSplotSelex(model, fleets = 2, subplot = 1, year = c(2022))
info2$infotable$longname <- c("Com. Live 1916-2022")
info2$infotable$col <- colors[2]

info3 <- SSplotSelex(model, fleets = 3, subplot = 1, year = c(1999, 2003, 2022))
info3$infotable$longname = c("CPFV 1916-1999", "CPFV 2000-2003", "CPFV 2004-2022")
info3$infotable$col <- c(colors[3], colors[3], colors[3])

info4 <- SSplotSelex(model, fleets = 4, subplot = 1, year = c(1999, 2022))
info4$infotable$longname = c("PR 1916-1999", "PR 20000-2022")
info4$infotable$col <- c(colors[4], colors[4])

info5 <- SSplotSelex(model, fleets = 5,  subplot = 1, year = c(2022))
info5$infotable$col <- colors[5]

info6 <- SSplotSelex(model, fleets = 6, subplot = 1, year = c(2022))
info6$infotable$col <- colors[6]

info7 <- SSplotSelex(model, fleets = 7, subplot = 1, year = c(2022))
#info7$infotable$longname = c("NWFSC HKL 2004-2013", "NWFSC HKL 2014-2022")
info7$infotable$col <- c(colors[7])

info9 <- SSplotSelex(model, fleets = 9, agefactors = "Asel2", plot = 12, year = c(1916, 2022))
info9 <- SSplotSelex(model, fleets = 9, subplot = 1, year = c(1916, 2022))
#info9$infotable$col <- colors[9]

HandyCode::pngfun(wd = file.path(wd, base_model, "plots"), 'south_selectivity.png', w = 10, h = 12)
par(mfrow=c(4,2),mar=c(2,4,3,1))
SSplotSelex(model, fleets=1,  infotable=info1$infotable, 
            subplot=1, legendloc='topleft', year = c(2022))
grid()
SSplotSelex(model, fleets=2, infotable=info2$infotable, 
            subplot=1, legendloc='topleft', year = c(2022))
grid()
SSplotSelex(model, fleets=3, infotable=info3$infotable,subplot=1, 
            legendloc='topleft', year = c(1999, 2003, 2022))
grid()
SSplotSelex(model, fleets=4, infotable=info4$infotable,subplot=1, 
            legendloc='topleft', year = c(1999, 2022))
grid()
SSplotSelex(model, fleets=5, infotable=info5$infotable,subplot=1, 
            legendloc='topleft')
grid()
SSplotSelex(model, fleets=6, infotable=info6$infotable, subplot=1, 
            legendloc='bottomright')
grid()
SSplotSelex(model, fleets=7, infotable=info7$infotable, subplot=1, 
            legendloc='topleft', year = c(2013, 2022))
grid()
#SSplotSelex(model, fleets=9, infotable=info9$infotable, subplot=14, 
#            legendloc='topleft', agefactors = "Asel")
#grid()
dev.off()




info4 <- SSplotSelex(model, fleets = 4, subplot = 1, year = c(1916, 2002, 2017))


info3 <- SSplotSelex(model, fleets = 3, subplot = 1, year = c(1916, 2000))
info4 <- SSplotSelex(model, fleets = 4, subplot = 1, year = c(1916, 2000))

1916 1999 2000 2021 
info3 <- SSplotSelex(model, fleets = 3, subplot = 1, year = c(1916, 2000, 2022))
info4 <- SSplotSelex(model, fleets = 4, subplot = 1, year = c(1916, 2000, 2022))

