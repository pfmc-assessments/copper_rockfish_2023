###################################################################
# Create Map of Assessment Area
###################################################################
# load packages
require(maps)
require(mapdata)
library(HandyCode)
library(here)

user <- Sys.getenv("USERNAME")
if( grepl("Chantel", user) ){
  user_dir <- "C:/Assessments/2023/copper_rockfish_2023"
} else {
  # Fill in Melissa's document directory below
  user_dir <- "C:/Users/melissa.monk/Documents/GitHub/copper_rockfish_2023"
}

savedir = file.path(user_dir, "documents", "shared_figures")
# define names and colors for each area
mod.names <- c("Management  Range", 
               "Assessment Areas")
mod.cols <- viridis::viridis(3)

# open PNGfile
pngfun(wd =  savedir, file = 'map.png', w = 7, h = 10)
# map with Canada and Mexico (not sure how to add states on this one)
map('worldHires', regions=c("Canada","Mexico"),
    xlim=c(-130, -114), ylim=c(31, 51),
    col='grey', fill=TRUE, interior=TRUE, , lwd=1)
# map with US states
map('state', regions=c("Wash","Oreg","Calif","Idaho",
                       "Montana","Nevada","Arizona","Utah"),
    add=TRUE,
    col='grey', fill=TRUE, interior=TRUE, lwd=1)
axis(2, at=seq(32,50,2), lab=paste0(seq(32,50,2), "°N"), las=1)
axis(1, at=seq(-130,-114,4), lab=paste0(abs(seq(-130,-114,4)), "°W"))
#map.axes()

## add vertical lines indicating range for each stock
latrange <- c(32.5, 48.5) + c(.2, -.2)
lines(rep(-128,2), latrange, lwd=10, col=mod.cols[1])
text(-128-.8, mean(latrange), mod.names[1], srt=90)
latrange <- c(32.5, 34.5) + c(.2, -.2)
lines(rep(-126,2), latrange, lwd=10, col=mod.cols[2])
latrange <- c(34.5, 42.0) + c(.2, -.2)
lines(rep(-126,2), latrange, lwd=10, col=mod.cols[3])
#latrange <- c(42.0, 46.0) + c(.2, -.2)
#lines(rep(-126,2), latrange, lwd=10, col=mod.cols[4])
#latrange <- c(46.0, 48.5) + c(.2, -.2)
lines(rep(-126, 2), latrange, lwd=10, col=mod.cols[5])
text(-126-.8, 36.5, mod.names[2], srt=90)
#
text(-120, 50, "Canada")
text(-120, 47, "Washington")
text(-121, 44, "Oregon")
text(-119.5, 37, "California")
text(-115.5, 32.1, "Mexico")
#
box()
dev.off()