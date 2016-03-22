

#########################    Init_boxplot.R    ###########################
##                                                                      ##
##  Plots the relative abundance of the monodominant species            ##
## abainst the chosen initialization option                             ##
##                                                                      ##
##########################################################################

library(ggplot2)
require(scales)

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

size_w = 400
init_v<-c(0,1,-1,3)
prop = 9300
radius = 20
radius_vec = c(20) 
mortality = 0.015
radius = 20
zusammen<-data.frame(size = NA, seedmass = NA, seed = NA, init_opt = NA, rel_abundance = NA)

for(init in init_v)
{
  string<-paste0(path_data, "/Init/Monodominance_Stats_size_",size_w,"_seedmass_", prop,"_mort_0.015_init_option_",init,".txt",sep="")
  Statistik<-as.matrix(read.table(string, header=F, sep="\t"))
  init_option = Statistik[,5]
  if (init_option == -1)
  {
    init_option = 2
  }
  new<-data.frame(size = Statistik[,1], seedmass = Statistik[,2], seed = Statistik[,3], init_opt = init_option, rel_abundance = Statistik[,9])      
  zusammen<-rbind(zusammen,new)
}
zusammen<-na.omit(zusammen)
attach(zusammen)


abundance_v = zusammen$rel_abundance[size==size_w]
init_v = zusammen$init[size==size_w]


results<-data.frame(init=init_v, abundance=abundance_v*100)

attach(results)

#plot
filename=paste(path_results,"/Initialization/Monodominance_INIT_boxplotprop_",prop,".eps", sep="")
colors <- c("grey40","grey90","grey90","grey90")
achse <- c("random","grid","streets","knight's move") #rand = 0, grid1 = 1, streets = 2, springer = 3
x <- c(1,2,3,4)#,5,6,7,8,9,10)
setEPS()
postscript(filename)
bw=0.8
par(mar=c(5, 7, 1, 1)) 

bild<-boxplot(abundance~init, data=results, col=colors, boxwex=bw, las=1, xaxt="n", cex.axis=1.3, ylab = "Relative Abundance [%]" , xlab = "Init Option", cex.lab=1.7)
axis(side=1,labels=achse, at=x, tick=TRUE, las = 0, cex.axis=1.3)
bild <- bild + text(2.5, 28, paste("SM = ", prop, sep=""), ps = 20, cex = 1.8, col = "grey50", font=2)
dev.off()
