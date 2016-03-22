#################   Binder_Transformation_BiggestCluster.R  ###################
##    August 2015          by Pia Backmann                       For Martin  ##
##                                                                           ##
##  Calculates the mean of the size of the biggest cluster (for each value   ##
##  of the seed mass SM). Plot the mean biggest cluster size against the     ##
##  seed masss (main plot) and the binder cumulant of the mean biggest       ##
##  cluster size (subplot), both against the seed mass (SM)                  ##
##  for different system sizes.                                              ##
##                                                                           ##
###############################################################################


library(grid)
library(ggplot2)
library(scales)   

radius_vec = c(20) 
size_vec <- c(178, 768, 64, 256, 100, 512, 400)
mortality = 0.015
prop_v2 <- c(9200, 9300, 9350, 9400, 9425, 9450, 9465, 9480, 9500, 9515, 9540, 9550, 9565, 9580, 9600, 9650, 9700)#, 9750, 9800)
prop_binder <- c(9350, 9400, 9425, 9450, 9465, 9480, 9500, 9515, 9540, 9550, 9600)

crit_PP = 9488
radius = 20

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

zusammen<-data.frame(size_martin = NA, seedmass = NA, seed_martin = NA, cluster = NA)

for(size_w in size_vec)
{ 
  for(prop in prop_v2)
  {
    string<-paste(path_data,"/Ergebnisse/radius",radius,"/Martins_Monodominance_Stats_size_",size_w,"_prop_", prop,"_radius_",radius,"_mortality_0.015.txt",sep="")     
    Statistik<-as.matrix(read.table(string, header=F, sep="\t"))
    
    new<-data.frame(size_martin = Statistik[,1],seedmass =Statistik[,2], seed_martin=Statistik[,3], cluster = Statistik[,7])
      werte <-new[order(new$cluster ),]
      count_werte = length(werte[,1])
      ein_prozent = floor(count_werte*0.05)
      werte_neu<- werte[(1+ein_prozent):(count_werte - ein_prozent),]
      length_neu = length(werte_neu[,1])
      zusammen<-rbind(zusammen, werte_neu)
  }
}

zusammen<-na.omit(zusammen)
attach(zusammen)

# Binder- transformation

#define variables
size_v<-vector(length=1)
seedmass_v<-vector(length=1)
cluster_v<-vector(length = 1)
binder_v<-vector(length = 1)
klammer_v<-vector(length = 1) # length(rel_abundance[size_martin==size_w & seedmass==prop]))
unten_v<-vector(length = 1)
fehler_v<-vector(length = 1)

i = 1;
for(prop in prop_v2) # prop_v
{  
  for(size_w in size_vec)
  {  
    size_v[i] = size_w
    seedmass_v[i] = prop
    cluster_v[i] = mean(cluster[size_martin==size_w & seedmass==prop])
    count = length(cluster[size_martin==size_w & seedmass==prop])
    
    oben = mean(cluster[size_martin==size_w & seedmass==prop]^4)
    unten = mean(cluster[size_martin==size_w & seedmass==prop]^2)
    binder_v[i] =1.5*( 1 - oben/(3*unten^2))
    fehler_v[i] = sqrt(cluster_v[i]*(1-cluster_v[i])/count)
    i = i+1
  }
}
results<-data.frame(size=size_v,seedmass=seedmass_v, cluster = cluster_v, binder = binder_v, fehler = fehler_v) 
attach(results)

#blues 
cols <- c( "128" = "#C6DBEF","178" = "#6BAED6", "256" = "#3182BD", "400"= "darkorchid4" , "512" = "#08519C", "64" = "lightblue", "100" = "skyblue", "768" = "black")
########  inlay for Binder Plot  #################################

filename=paste(path_results, "/Statistics/Monodominance_BiggestCluster.eps", sep="")
setEPS()
postscript(filename,width=15, height=13)
par(mar=c(8.1, 4.1, 4.1, 2.1))
gesamt<-ggplot(results, aes(x = seedmass, y = cluster, group = size))
gesamt<- gesamt + geom_errorbar(aes(x = seedmass, ymax = cluster+fehler/2, ymin = cluster-fehler/2, color = as.factor(size)))
gesamt <- gesamt + geom_line(aes(x = seedmass, y = cluster, color = as.factor(size)), size = 1.0) + scale_colour_manual("System size",values = cols)
#p <- p + geom_errorbar(aes(x = seedmass, ymax = wahrscheinlichkeit+fehler/2, ymin = wahrscheinlichkeit-fehler/2, color = as.factor(size)))
####
gesamt <- gesamt   + xlab("Seed Mass SM") + ylab(expression(paste("Size largest cluster ", P[infinity](SM), sep =""))) + theme_classic()
gesamt <- gesamt   + labs(title = "")#Ratio of Monodominant + biggest cluster size")
gesamt <- gesamt   + theme(axis.text.x = element_text(size = rel(3.6), color = "black"))
gesamt <- gesamt   + theme(axis.text.y = element_text(size = rel(3.6), color = "black"))
gesamt <- gesamt   + theme(axis.title.x  = element_text(size = rel(3.4), vjust = -0.3))
gesamt <- gesamt   + theme(axis.title.y  = element_text(size = rel(3.4), vjust = 0.3))
gesamt <- gesamt   + theme(legend.title  = element_text(size = rel(2.7))) + theme(plot.margin = unit(c(0,0,1,0.7), "cm"))
gesamt <- gesamt   + theme(legend.text  = element_text(size = rel(2.7))) + theme(legend.position=c(.87, .27))  +  theme(legend.key.size = unit(2.0, "cm"))
gesamt <- gesamt  + theme(legend.text=element_text(lineheight=1.8),legend.key.height=unit(1.7, "cm")) + theme(legend.background=element_rect(fill="white", colour="black"))

skala<-c(9200, 9300, 9400,9500, 9600, 9700, 9800)
gesamt <- gesamt  + scale_x_continuous(breaks = skala) 

mainvp <- viewport(width = 1, height = 1)  
subvp <- viewport(width = 0.35, height = 0.4, x = 0.35, y = 0.72)  
print(gesamt, vp = mainvp)   
print(p, vp = subvp)
dev.off()

