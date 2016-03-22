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
prop_v2 <-  c(9200, 9300, 9350, 9400, 9425, 9450, 9465, 9480, 9500, 9515, 9540, 9550, 9565, 9580, 9600, 9650, 9700)
crit_PP = 9488 
crit_exp <- 1.4
radius = 20
zusammen<-data.frame(size_martin = NA, seedmass = NA, seed_martin=NA, size_big = NA, size_nonperko= NA)
size_vec <- c(128, 178, 256, 400, 512)

for(size_w in size_vec)
{ 
  for(prop in prop_v2)
  {
    ##############################################################################
    # I #you want to use your own generated data:
    # string<-paste(path_data, "/Ergebnisse/radius",radius,"/Monodominance_Stats_size_",size_w,"_prop_", prop,"_radius_",radius,"_mortality_0.015.txt",sep="")
    ##############################################################################
    # If you want to use the provided example-data-package
      string<-paste(path_data,"/Ergebnisse/radius",radius,"/Martins_Monodominance_Stats_size_",size_w,"_prop_", prop,"_radius_",radius,"_mortality_0.015.txt",sep="")
      Statistik<-as.matrix(read.table(string, header=F, sep="\t"))
      new<-data.frame(size_martin = Statistik[,1], seedmass = Statistik[,2], seed_martin=Statistik[,3], size_big = Statistik[,7], size_nonperko= Statistik[,9])      

      zusammen<-rbind(zusammen,new)
  }
}

zusammen<-na.omit(zusammen)
attach(zusammen)

  size_v<-vector(length=6)
  seedmass_v<-vector(length=6)
  wahrscheinlichkeit_v<-vector(length = 6)
  fehler_v<-vector(length = 6)
  kollaps_v<-vector(length = 6)
  prop_prozent_v<-vector(length = 6)
  i = 1;
 
  for(size_w in size_vec)
  {  
    for(prop in prop_v2)
    { 
      count = length(zusammen$size_big[zusammen$size_martin==size_w & zusammen$seedmass==prop])
      gewonnen = length(subset(zusammen$size_big[zusammen$size_martin==size_w & zusammen$seedmass==prop],  zusammen$size_big[zusammen$size_martin==size_w & zusammen$seedmass==prop] != zusammen$size_nonperko[zusammen$size_martin==size_w & zusammen$seedmass==prop]))
      wahrscheinlichkeit_v[i] = gewonnen/count
      size_v[i] = size_w
      seedmass_v[i] = prop
      fehler_v[i] = sqrt(wahrscheinlichkeit_v[i]*(1-wahrscheinlichkeit_v[i])/count)
      kollaps_v[i] =  (seedmass_v[i] - crit_PP)*size_w^(1/crit_exp)  
   
      i = i+1;  
    }
  }
  

  
  ##################################################
  # (PP-PP_c)*L^(1/nu) # mit nu so angepasst, dass die Kurven aufeinander liegen
  ##################################################
  results<-data.frame(size=size_v, seedmass=seedmass_v, wahrscheinlichkeit=wahrscheinlichkeit_v, fehler = fehler_v, kollaps_x = kollaps_v)
  ######################  Plot routines  ###############################
  #attach(results)
  prop_vkollaps<-c( 9350, 9400, 9425, 9450, 9465, 9480, 9500, 9515, 9540, 9550, 9565, 9580, 9600, 9650)
  

  for(size_w in size_vec)
  {  
    for(prop in prop_vkollaps)
    { 
      count = length(zusammen$size_big[zusammen$size_martin==size_w & zusammen$seedmass==prop])
      gewonnen = length(subset(zusammen$size_big[zusammen$size_martin==size_w & zusammen$seedmass==prop],  zusammen$size_big[zusammen$size_martin==size_w & zusammen$seedmass==prop] != zusammen$size_nonperko[zusammen$size_martin==size_w & zusammen$seedmass==prop]))
      wahrscheinlichkeit_v[i] = gewonnen/count
      size_v[i] = size_w
      seedmass_v[i] = prop
      fehler_v[i] = sqrt(wahrscheinlichkeit_v[i]*(1-wahrscheinlichkeit_v[i])/count)
      kollaps_v[i] =  (seedmass_v[i] - crit_PP)*size_w^(1/crit_exp)  
      i = i+1;  
    }
  }
  results_kollaps<-data.frame(size=size_v, seedmass=seedmass_v, wahrscheinlichkeit=wahrscheinlichkeit_v, fehler = fehler_v, kollaps_x = kollaps_v)

  # scale colours: 
cols <- c( "128" = "#C6DBEF","178" = "skyblue", "256" = "#6BAED6", "400"= "#3182BD" , "512" = "#08519C", "64" = "cyan", "100" = "darkgreen", "768" = "black")

  ######################  Datenkollaps  #################################
  attach(results_kollaps)

  d<-ggplot(results_kollaps, aes(x = kollaps_x, y = wahrscheinlichkeit, group = size, color = as.factor(size))) + theme_classic()
  d <- d + geom_line() + geom_point(size=5)
  d <- d + scale_colour_manual("System size",values = cols)
 d <- d  + ylab(expression(P[L](SM)))  + ylim(0.05, 0.95) + xlim(-2500, 4000)
 d <- d + labs(title = "") +  xlab(expression(paste("", (SM-SM[c])," * ", L^{1/nu}, sep ="")))
  # make description of axes bigger and thus more readable
 d <- d + theme(axis.text.x = element_text(size = rel(2.9), color = "black"))
 d <- d + theme(axis.text.y = element_text(size = rel(2.9), color = "black"))
 d <- d + theme(axis.title.x  = element_text(size = rel(2.6)))
 d <- d + theme(axis.title.y  = element_text(size = rel(2.6)))
 d <- d +  theme(legend.position = "none")
  d
 attach(results)
 
  ################### relative abundance ################################

  filename=paste(path_results, "/Statistics/Monodominance_perkolation_phasetransition",crit_exp,"_critPP_",crit_PP,".eps", sep="")
  setEPS()
  postscript(filename,width=15, height=13)
  par(mar=c(8.1, 4.1, 4.1, 2.1))
  p<-ggplot(results, aes(x = seedmass, y = wahrscheinlichkeit, group = size,  colour = as.factor(size))) 
  p <- p + scale_colour_manual("System size",values = cols)

  p <- p + geom_line(size=1.2) # +  geom_point(size=5)
  p <- p + geom_errorbar(aes(x = seedmass, ymax = wahrscheinlichkeit+fehler/2, ymin = wahrscheinlichkeit-fehler/2, color = as.factor(size)))

####
p <- p  + xlab("Seed Mass SM") + ylab(expression(paste( "Percolation probability ", P[L](SM), sep =""))) + theme_classic()
p <- p  + labs(title = "")
p <- p  + theme(axis.text.x = element_text(size = rel(3.6), color = "black"))
p <- p  + theme(axis.text.y = element_text(size = rel(3.6), color = "black"))
p <- p  + theme(axis.title.x  = element_text(size = rel(3.4), vjust=-1))
p <- p  + theme(axis.title.y  = element_text(size = rel(3.4), vjust= 1))
p <- p  + theme(legend.title  = element_text(size = rel(2.7))) + theme(plot.margin = unit(c(0,0,1,0.7), "cm"))
p <- p  + theme(legend.text  = element_text(size = rel(2.7))) + theme(legend.position=c(.88, .2))
p <- p + theme(legend.text=element_text(lineheight=1.8),legend.key.height=unit(1.7, "cm")) + theme(legend.background=element_rect(fill="white", colour="black"))
p <- p + annotate("text", x = 9565, y = 0.42, parse=TRUE, label=paste("SM[c]==",crit_PP, sep=""), size = 14)
#p <- p +  geom_text(label = paste("nu", sep=""), x = 9390, y = 0.93 , size = 12)
p <- p + annotate("text", x = 9350, y = 0.98, parse=TRUE, label=paste("nu==",crit_exp, sep=""), size = 14)
#skala<-c(9200, 9250, 9300, 9350, 9400, 9450, 9500, 9550, 9600, 9650, 9700, 9750)
p <- p + geom_vline(xintercept = crit_PP, colour="black") 
mainvp <- viewport(width = 1, height = 1)  
subvp <- viewport(width = 0.35, height = 0.4, x = 0.36, y = 0.70)  
print(p, vp = mainvp)   
print(d, vp = subvp)
dev.off()

####################################################################################
