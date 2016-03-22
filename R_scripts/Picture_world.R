#######################    Picture_world.R   ##################################
##   March 2016         von Pia Backmann                         Für Martin  ##
##                                                                           ##
##   Plots a screenshot of the forest matrix                                 ##

##   black grid cells: monodominant species,                                 ##
##   blue/grey/white grid cells: identical species                           ##
##   brown grid cells: gaps                                                  ##
##                                                                           ##
###############################################################################

library(ggplot2)
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
mortality_v <- c(0.015)  #c(0.001, 0.005, 0.008, 0.009, 0.01, 0.011, 0.012) 
size = 20
#pia <- seq(from=10, by=10, to=10000)
pia <- c(1)
pia <- seq(from=500, by=500, to=30000)
seedmass_v<-(9450)
ticker<-c(1,pia)
current = 1000
factor = 1
seed = 1
rotate <- function(x) t(apply(x, 2, rev))

#center_x <- c(3.992809, 268.118681)
#center_y <- c(252.916854, 253.599267)
ini = 3
for(mortality in mortality_v)
{
  for(i in  seedmass_v)
  {
    for(j in ticker)
    {
      string<-paste(path_data, "/Weltview/Monodominanz_size_", size,"_seedmass_", i,"_radius_20_mort_",mortality,"000_init_option_",ini,"_seed_",seed,"_tick_", j, ".out",sep="")
      world_view<-as.matrix(read.table(string, header=F))
      if(j%%current == 0)
      {
        factor = factor+1
        current = 1000*factor
      }
      tickstr = paste("tick < ", current)
     # tickstr = paste("tick = ", j)
      meineFarben = c("brown", "black", "blue", "gray", "blue4", "white", "gray38", "blue2", "lightblue")#, "lightsteelblue")
     filename=paste(path_results,"/Worldview/Monodominanz_size_",size,"_seedmass_", i,"_radius_20_mort_",mortality,"_init_option_",ini,"_seed_",seed,"_tick_", j,  ".png", sep="")
     if(j < 10000)
      {    
        filename=paste(path_results,"/Worldview/Monodominanz_size_",size,"_seedmass_", i,"_radius_20_mort_",mortality,"_init_option_",ini,"_seed_",seed,"_tick_0", j,  ".png", sep="")
      }
      if(j < 1000)
      {
        filename=paste(path_results,"/Worldview/Monodominanz_size_",size,"_seedmass_", i,"_radius_20_mort_",mortality,"_init_option_",ini,"_seed_",seed,"_tick_00", j,  ".png", sep="")
      }
      if(j < 100)
      {
        filename=paste(path_results,"/Worldview/Monodominanz_size_",size,"_seedmass_", i,"_radius_20_mort_",mortality,"_init_option_",ini,"_seed_",seed,"_tick_000", j,  ".png", sep="")
      }
      if(j < 10)
      {
        filename=paste(path_results,"/Worldview/Monodominanz_size_",size,"_seedmass_", i,"_radius_20_mort_",mortality,"_init_option_",ini,"_seed_",seed,"_tick_0000", j,  ".png", sep="")
      }
      png(filename,width=1000,height=1000)
      par( mai=c(0,0,0,0), oma=c(0,0,0,0))
      bild<-image(1:ncol(world_view), 1:nrow(world_view), rotate(world_view), cex.lab=2, cex.main = 2,
            col=meineFarben, axes=FALSE, zlim=c(0,8), xlab=tickstr, ylab = "", main = paste("Monodominance: init: ",ini,"seedmass ", i,", mortality:",mortality))
     # bild<- bild + points(center_x, center_y, pch=5, cex=15, lwd=2)   
     bild
      dev.off()
    }
  }
}



# convert the .png files to one .gif file using ImageMagick. 
# The system() function executes the command as if it was done
# in the terminal. the -delay flag sets the time between showing
# the frames, i.e. the speed of the animation.
#system(convert -delay 10 *.png example_1.gif)