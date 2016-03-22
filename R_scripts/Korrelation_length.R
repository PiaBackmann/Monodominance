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


mortality = 0.015
radius = 20
prop_v <- c(9200, 9300, 9350, 9400, 9425, 9450, 9465, 9480, 9490, 9500, 9515, 9550, 9565, 9580, 9600, 9650, 9700)

size_vec <- c( 178, 400, 256, 512, 128 )


zusammen<-data.frame(size_martin = NA, propagule = NA, seed_martin=NA, size_big = NA, size_nonperko= NA, correlation = NA)

radius = 20

for(size_w in size_vec)
{ 
  for(prop in prop_v)
  {
      # If you want to use the provided example-data-package
      string<-paste(path_data, "/Ergebnisse/radius",radius,"/Martins_Monodominance_Stats_size_",size_w,"_prop_", prop,"_radius_",radius,"_mortality_0.015.txt",sep="")

      # I you want to use your own generated data:
      # string<-paste(path_data, "/Ergebnisse/radius",radius,"/Monodominance_Stats_size_",size_w,"_prop_", prop,"_radius_",radius,"_mortality_0.015.txt",sep="")
     
      Statistik<-as.matrix(read.table(string, header=F, sep="\t"))
      new<-data.frame(size_martin = Statistik[,1],propagule = Statistik[,2], seed_martin=Statistik[,3], size_big = Statistik[,6], size_nonperko= Statistik[,8], correlation = Statistik[,9])
      zusammen<-rbind(zusammen,new)
  }
}

zusammen<-na.omit(zusammen)
attach(zusammen)
  
  size_v<-vector(length=6)
  propagule_v<-vector(length=6)
  correlation_v<-vector(length = 6)
  fehler_v<-vector(length = 6)
  i = 1;
  for(prop in  prop_v) 
  {  
    for(size_w in size_vec)
    {  
      count = length(zusammen$correlation[zusammen$size_martin==size_w & zusammen$propagule==prop])
      correlation_v[i] = mean(correlation[size_martin==size_w & propagule==prop])
      size_v[i] = size_w
      propagule_v[i] = prop
      fehler_v[i] = sqrt(correlation_v[i]*(1-correlation_v[i])/count)
      i = i+1;
    }
  }

  results<-data.frame(size=size_v,propagule=propagule_v, correlation=correlation_v, fehler = fehler_v)

  crit_point = 9480
  
  ######################  Plot routines  ###############################
  attach(results)
  # scale colours: 
# blues: "#C6DBEF" "#9ECAE1" "#6BAED6" "#3182BD" "#08519C"
cols <- c( "128" = "#C6DBEF","178" = "skyblue", "256" = "#6BAED6", "400"= "#3182BD" , "512" = "#08519C", "64" = "cyan", "100" = "darkgreen", "768" = "black")

################### relative abundance #################################
filename=paste(path_results,"/Statistics/Monodominance_Korrelation_radius.eps", sep="")
setEPS()
postscript(filename,width=15, height=13)#
par(mar=c(8.1, 4.1, 4.1, 2.1))
  p<-ggplot(results, aes(x = propagule, y = correlation, group = as.factor(size), color = as.factor(size)))
  p <- p + scale_colour_manual("System size", values = cols) 
  p <- p +  geom_line(size = 1.4) 
  p <- p + geom_errorbar(aes(x = propagule, ymax = correlation+fehler/2, ymin = correlation-fehler/2, color = as.factor(size)))
  p <- p+ xlab("Seed mass SM") + ylab(expression(paste( "Correlation length ", zeta, sep =""))) + theme_classic()
  # make description of axes bigger and thus more readable
  p<- p + theme(axis.text.x = element_text(size = rel(3.6)))
  p<- p + theme(axis.text.y = element_text(size = rel(3.6)))
  p<- p + theme(axis.title.x  = element_text(size = rel(3.4), vjust = -1))
  p<- p + theme(axis.title.y  = element_text(size = rel(3.4), vjust = 1))
  p<- p + theme(legend.title  = element_text(size = rel(2.7)))
  p <- p   +  theme(plot.margin = unit(c(0,0,1,0.7), "cm"))
  p <- p  + theme(legend.text  = element_text(size = rel(2.7))) + theme(legend.position=c(.87, .5)) +  theme(legend.key.size = unit(2.0, "cm"))
  p <- p + theme(legend.text=element_text(lineheight=1.8),legend.key.height=unit(1.7, "cm")) + theme(legend.background=element_rect(fill="white", colour="black"))
  skala<-c(9200, 9300,  9400,  9500, 9600, 9700, 9800)
  p <- p + scale_x_continuous(breaks = skala)
  p <- p + geom_vline(xintercept = crit_point, size = 1.4) 
  p <- p + annotate("text", x = crit_point-70, y = 0.145, parse=TRUE, label=paste("SM[c]==", crit_point, sep=""), size = 14)
#p <- p + annotate("text", x = crit_point-85, y = 0.138, parse=TRUE, label=expression(paste(SM[c], "==", crit_point, sep="")), size = 7)

  p
  dev.off()
 


