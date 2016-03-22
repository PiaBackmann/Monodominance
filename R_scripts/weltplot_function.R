
library(ggplot2)

plot_welt <- function(string, tick)
{
    world_view<-as.matrix(read.table(string, header=F))
    meineFarben = c("brown", "black", "blue", "gray", "blue4", "white", "gray38", "blue2", "lightblue")#, "lightsteelblue")
    bild<-image(1:ncol(world_view), 1:nrow(world_view), t(t(world_view)), cex.lab=2, cex.main = 2,
          col=meineFarben, axes=FALSE, zlim=c(0,8), xlab="",ylab = "") 
    return(bild) 
}
