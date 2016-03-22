#######################    Picture_world.R   ##################################
##   Januar 2015           von Pia Backmann                      Für Martin  ##
##                                                                           ##
##   erzeugt Karte von Häfigkeit von Monodominanten Feldern in einem         ##
##   selbstgewählten Radius und codiert die Häufigkeit mit Farbe.            ##
##                                                                           ##
##   Speichert Bild im Ordner:                                               ##
##          Arbeitsfläche/Code_Server/Bilder/Localmaps/                      ##
##                      unter:                                               ##
##   Localmap_size_XXX_propagule_XXX_radius_20_mort_XXX_seed_XXX_tick_XXX_densrad_XXX_rainbow.png      ##
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
source(paste0(getwd(),"/FORMIND_colors.r"))
source(paste0(getwd(),"/local_plot.R"))
mortality = 0.015
size = 512
tick = 1
prop = 9500
seed = 11
ini = 3
density_radius = 1
rotate <- function(x) t(apply(x, 2, rev))

string<-paste(path_data, "/Localmap/Recruitement_prob_size_", size,"_seedmass_",prop,"_radius_20_mort_",mortality,"000_init_option_",ini,"_tick_", tick,".out",sep="")

world_view<-as.matrix(read.table(string, header=F))

rain = 200
maximum = max(world_view)
#maximum = 0.5691
zusatz = floor(maximum/7)
#meineFarben = terrain.colors(10)
#meineFarben = cm.colors(maximum)
#meineFarben = topo.colors(maximum)
meineFarben = rainbow(rain, start = 0.15, end = 0.7, alpha = 1.0) 
meineFarben = c("white", meineFarben)
#meineFarben = pft_color1(10, alpha = 1.0)# [zusatz : maximum + zusatz]
#meineFarben <- c("white", "grey90","grey80", "grey70", "grey60", "grey50", "grey40", "grey30", "grey20", "grey10", "black")
#meineFarben = heat.colors(10)
#meineFarben = c(meineFarben)
filename=paste(path_results, "/Localmap/Localmap_size_", size,"_seedmass_",prop,"_ini_",ini,"_seed_",seed,"_tick_",tick,"_rainbow_",rain,".png", sep="")

png(filename,width=1000,height=1000)
image.plot(1:ncol(world_view), 1:nrow(world_view), rotate(world_view),  cex.lab=2, cex.main = 2.7,
           col=meineFarben, axes=FALSE,legend.width=1.2, zlim=c(0,maximum), xlab="",ylab = "", main = "Knight's move")     
#points(center_x, center_y, pch=19, cex=1.6)

dev.off()
if(ini ==-1) {mean_streets = mean(world_view)
sd_streets = sd(world_view)}
if(ini ==1) {mean_grid = mean(world_view)
sd_grid = sd(world_view)}
if(ini ==3) {mean_springer = mean(world_view)
sd_springer = sd(world_view)}
if(ini ==0) {mean_random = mean(world_view)
sd_random = sd(world_view)}

bedeckt = sum(world_view > 0)/length(world_view)
