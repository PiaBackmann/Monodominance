########################    local_plot.R    ###################################
##   March 2016           von Pia Backmann                       Für Martin  ##
##                                                                           ##
##   Iteriert über gewünschte propagule pressures und Zeitschritte und       ##
##   plottet die jeweilige Weltansicht zur gegebenen Parameterkombination.   ##
##   Monodominante Art: schwarze Felder, andere Arten: Blauschattierungen    ##
##   Leere Felder: braun (werden aber nicht angezeigt)                       ##
##                                                                           ##
###############################################################################

library(ggplot2)
library(fields)


############### set path correctly:############################
# current working directory
path <- getwd()

# to read the data
# if you want to use the provided example data:
path_data <- sub("R_scripts", "Example_DATA", path)
# if you want to use your own generated data:
# path_data <- sub("R_scripts", "Data", path)

# to store the results
path_results <- sub("R_scripts", "Pictures", path)
################################################################
source(getwd(),"/FORMIND_colors.r")

local_plot <- function(radius)
{
  
  mortality = 0.015
  size = 20
  tick = 1
  prop = 9500
  seed = 10
  density_radius = radius
  
  string<-paste(path_data, "/Localmap/Localmap_size_", size,"_propagule_",prop,"_radius_20_mort_",mortality,"000_seed_",seed,"_tick_", tick, "_densrad_",density_radius,".out",sep="")
  world_view<-as.matrix(read.table(string, header=F))
  
 # center_x <- c(3.992809, 268.118681)
  # center_y <- c(252.916854, 253.599267)
  
  maximum = max(world_view)
  zusatz = floor(maximum/7)
  #meineFarben = terrain.colors(maximum)
  #meineFarben = cm.colors(maximum)
  #meineFarben = topo.colors(maximum)
  #meineFarben = rainbow(maximum, start = 0.15, end = 1, alpha = 1.0) 
  meineFarben = pft_color1(maximum+zusatz, alpha = 1.0) [zusatz : maximum + zusatz]
  #meineFarben = heat.colors(maximum)
  meineFarben = c(meineFarben)

  
  bild<-image(1:ncol(world_view), 1:nrow(world_view), t(t(world_view)),  cex.lab=2, cex.main = 2,
             col=meineFarben, axes=FALSE,zlim=c(0,maximum), xlab="",ylab = "", main ="")     
  #bild<- bild + points(center_x, center_y, pch=19, cex=1.6)
  
  return(bild)
}