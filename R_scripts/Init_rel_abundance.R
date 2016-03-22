# Pia Backmann, 2015
# Plot of the Percolation probability with colour gradient
# instead of colour hue.
# For the Monodominance publication

library(ggplot2)
library(grid)

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

radius_vec = c(20) 
mortality = 0.015
prop_v <-  c(9300, 9400, 9500, 9600, 9700, 9800)
init_v <- c(-1, 0, 1, 3)
radius = 20
zusammen<-data.frame(size = NA, seedmass = NA, seed = NA, init_opt = NA, rel_abundance = NA)
size_vec <- c(128, 256, 400)
for(init in init_v)
{
  for(size_w in size_vec)
  { 
    for(prop in prop_v)
    {
      string<-paste(path_data,"/Init/Monodominance_Stats_size_",size_w,"_seedmass_", prop,"_mort_0.015_init_option_",init,".txt",sep="")
      Statistik<-as.matrix(read.table(string, header=F, sep="\t"))
      new<-data.frame(size = Statistik[,1], seedmass = Statistik[,2], seed = Statistik[,3], init_opt = Statistik[,5], rel_abundance = Statistik[,9])      
      zusammen<-rbind(zusammen,new)
    }
  }
}

zusammen<-na.omit(zusammen)
attach(zusammen)

size_v<-vector(length=6)
seedmass_v<-vector(length=6)
abundance_mean_v <- vector(length=6)
initOpt_v <- vector(length=6)
i = 1;
for(init in init_v)
{
  for(size_w in 400)
  {  
    for(prop in prop_v)
    { 
      count = length(zusammen$rel_abundance[zusammen$size==size_w & zusammen$seedmass==prop & zusammen$init_opt == init])
      abundance_sum = sum(zusammen$rel_abundance[zusammen$size==size_w & zusammen$seedmass==prop & zusammen$init_opt== init])
      show(abundance_sum)
      show(count)
      
      abundance_mean_v[i] = abundance_sum/count
      size_v[i] = size_w
      seedmass_v[i] = prop  
      initOpt_v[i] = init
      i = i+1;  
    }
  }
}
results<-data.frame(size=size_v, seedmass=seedmass_v, abundance=abundance_mean_v*100, init = initOpt_v)

# scale colours: 
#cols <- c( "128" = "#C6DBEF","178" = "skyblue", "256" = "#6BAED6", "400"= "#3182BD" , "512" = "#08519C", "64" = "cyan", "100" = "darkgreen", "768" = "black")

cols <- c( "-1" = "blue","0" = "green", "1" = "red", "3" = "violet")
#shapes <- c( "-1" = 1,"0" = 2, "1" = 3, "3" = 4)

################### relative abundance ################################


filename=paste(path_results,"/Initialization/Monodominance_INIT_relAbundance.eps", sep="")
setEPS()
postscript(filename,width=15, height=13)
par(mar=c(8.1, 4.1, 4.1, 2.1))
p<-ggplot(results, aes(x = seedmass, y = abundance, group = init, color = as.factor(init))) 


p <- p + geom_line(size=2) 
p <- p +  geom_point(size=5, shape = 1)
#p <- p + geom_errorbar(aes(x = seedmass, ymax = wahrscheinlichkeit+fehler/2, ymin = wahrscheinlichkeit-fehler/2, color = as.factor(size)))

####
p <- p  + xlab("Seed mass SM") + ylab(expression(paste( "Mean(relative abundance)", sep =""))) + theme_classic()
p <- p  + labs(title = "")
p <- p  + theme(axis.text.x = element_text(size = rel(3.6), color = "black"))
p <- p  + theme(axis.text.y = element_text(size = rel(3.6), color = "black"))
p <- p  + theme(axis.title.x  = element_text(size = rel(3.4), vjust=-1))
p <- p  + theme(axis.title.y  = element_text(size = rel(3.4), vjust= 1))
p <- p  + theme(legend.title  = element_text(size = rel(2.7))) + theme(plot.margin = unit(c(0,0,1,0.7), "cm"))
p <- p  + theme(legend.text  = element_text(size = rel(2.7))) + theme(legend.position=c(.2, .8))
p <- p + theme(legend.text=element_text(lineheight=1.8),legend.key.height=unit(1.7, "cm")) + theme(legend.background=element_rect(fill="white", colour="black"))
p <- p + scale_colour_manual("Init options",values = cols, labels=c("Streets", "Random", "Grid", "Knight"))
p
dev.off()

####################################################################################
