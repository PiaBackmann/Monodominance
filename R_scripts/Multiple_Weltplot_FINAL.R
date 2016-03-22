#######################    Picture_world.R   ##################################
##   March 2016           von Pia Backmann                       Für Martin  ##
##                                                                           ##
##   Creates a time-line of snapshots of the modeled forest matrix.          ##
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

plot_welt <- function(string, tick)
{
  world_view<-as.matrix(read.table(string, header=F))
  meineFarben = c("brown", "black", "blue", "gray", "blue4", "white", "gray38", "blue2", "lightblue")#, "lightsteelblue")
  bild<-image(1:ncol(world_view), 1:nrow(world_view), t(t(world_view)), cex.lab=2, cex.main = 2,
              col=meineFarben, axes=FALSE, zlim=c(0,8), xlab="",ylab = "") 
  return(bild) 
}
ini = 0
mortality = 0.015
size = 400
mass = 9500
seed = 11
sequence <- seq(from=500, by=500, to=9500)
tick_v = c(1,sequence)

filename=paste(path_results, "/Worldview/Timeline_world.png", sep="")

   png(filename,width=750,height=600)
    par(mfrow=c(4,5), mar=c(.1,.1,.1,.1))
    i = 1
    for(tick in tick_v)
    {
      string<-paste(path_data, "/Weltview/Monodominanz_size_", size,"_seedmass_", mass,"_radius_20_mort_",mortality,"000_init_option_",ini,"_seed_",seed,"_tick_", tick, ".out",sep="")
      weltbild<-plot_welt(string, tick)
      if(i==1)
      {
        weltbild<- weltbild + text(190, 450, paste(tick," year -->", sep=""), ps = 30, cex = 2.5, col = "white", font=2)
      }
      if(i==6||i==11||i==16)
      {
       weltbild<- weltbild + text(240, 450, paste(tick," years -->", sep=""), ps = 30, cex = 2.5, col = "white", font=2)
      }
      if(i %%5 == 0)
      {
        weltbild<- weltbild + text(200, 450, paste(tick," years ", sep=""), ps = 30, cex = 2.5, col = "white", font=2)
      }
      weltbild
      i = i+1
    }
    dev.off()


