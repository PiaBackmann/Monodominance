#######################    Local_density_map.R   ##############################
##  March  2016          von Pia Backmann                        Für Martin  ##
##                                                                           ##
##   produces a map showing how many seeds of the monodominant species       ##
##  are in the seedbank of each patch                                        ##
##                                                                           ##    
###############################################################################

library(ggplot2)
library(fields)


############### set path correctly:############################
# current working directory
path <- getwd()

# read functions needed:
source(paste0(getwd(),"/FORMIND_colors.r"))
source(paste0(getwd(), "/local_plot.R"))

# to read the data
# if you want to use the provided example data:
path_data <- sub("R_scripts", "Example_DATA", path)
# if you want to use your own generated data:
# path_data <- sub("R_scripts", "Data", path)

# to store the results
path_results <- sub("R_scripts", "Pictures", path)
################################################################
ini = 1
mortality = 0.015
size = 512
tick = 1
prop = 9500
seed = 1
density_radius = 1

string<-paste0(path_data,"/Localmap/Localmap_size_", size,"_seedmass_",prop,"_radius_20_mort_",mortality,"000_init_option_",ini,"_density",density_radius,"_seed_",seed,"_tick_", tick,".out",sep="")
world_view<-as.matrix(read.table(string, header=F))

#center_x <- c(3.992809, 268.118681)
#center_y <- c(252.916854, 253.599267)

maximum = max(world_view)+1
zusatz = floor(maximum/7)
#meineFarben = terrain.colors(maximum)
#meineFarben = cm.colors(maximum)
#meineFarben = topo.colors(maximum)
#meineFarben = rainbow(maximum, start = 0.15, end = 1, alpha = 1.0) 
meineFarben = pft_color1(maximum+zusatz, alpha = 1.0) [zusatz : maximum + zusatz]
#meineFarben = heat.colors(maximum)
meineFarben = c(meineFarben)


radius_v<- c(1)

filename=paste0(path_results, "/Localmap/Localmap_size_", size,"_seedmass_",prop,"_radius_20_mort_",mortality,"000_seed_",seed,"_tick_", tick,"_rainbow_multiple.png", sep="")

png(filename,width=1100,height=1000)
#par(mfrow=c(3,3), mar=c(0.1,0.1,0.1,0.1))


  localbild<-image(1:ncol(world_view), 1:nrow(world_view), t(t(world_view)),  cex.lab=2, cex.main = 2,
              col=meineFarben, axes=FALSE,zlim=c(0,maximum), xlab="",ylab = "", main ="") 
  localbild


dev.off()

