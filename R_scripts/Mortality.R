

#########################    Mortality.R    ##############################
##                                                                      ##
##  Plots the relative abundance of Monodominant species against        ##
##  the background mortality per year as boxplots.                      ##
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

radius_vec = c(20)
size = 512
mortality_v<-c(0.001, 0.005, 0.008, 0.01, 0.013, 0.015,0.017, 0.02, 0.025, 0.03)
prop = 9500
radius = 20
size_w = 512
zusammen<-data.frame(size_martin = NA, seedmass = NA, seed_martin = NA, rel_abundance = NA, mort=NA, cluster = NA)

for(mortality in  mortality_v)
{
  # if you use the provided example-dataset:
  string<-paste0(path_data, "/Mortality/Martins_Monodominance_Stats_size_",size_w,"_prop_", prop,"_mortality_",mortality,"_radius_",radius,".txt",sep="")
  
  # if you use your own generated data:
  #string<-paste0(path_data,"/Monodominance_Modell/Data/Mortality/Monodominance_Stats_size_",size_w,"_seedmass_", prop,"_mortality_",mortality,"_radius_",radius,".txt",sep="")
  Statistik<-as.matrix(read.table(string, header=F, sep="\t"))
  new<-data.frame(size_martin = Statistik[,1], seedmass = prop, seed_martin=Statistik[,3], rel_abundance = Statistik[,7], mort = mortality, cluster = Statistik[,6])
  zusammen<-rbind(zusammen, new)
}
zusammen<-na.omit(zusammen)
attach(zusammen)


abundance_v = zusammen$rel_abundance[size_martin==size_w]
cluster_v = zusammen$cluster[size_martin==size_w]
mort_v = zusammen$mort[size_martin==size_w]

results<-data.frame(mortality=mort_v*100, abundance=abundance_v, cluster = cluster_v)

attach(results)

achse <- c( 0.1, 0.5, 0.8, 1.0, 1.3, 1.5, 1.7, 2.0, 2.5, 3.0)
x <- c( 1,2,3,4,5,6,7,8,9,10)
setEPS()
postscript(paste0(path_results,"/Statistics/Monodominance_mortality_boxplot.eps", sep=""))
bw=0.8
par(mar=c(5, 5, 1, 1)) 
boxplot(cluster~mortality, data=results, col="grey90", boxwex=bw, las=1, xaxt="n", ylab = "Relative cluster size" , xlab = "Annual mortality [%]", cex.lab=1.7, cex.axis=1.5)
axis(side=1,labels=achse, at=x, tick=TRUE, las = 0, cex.axis=1.3)

dev.off()
