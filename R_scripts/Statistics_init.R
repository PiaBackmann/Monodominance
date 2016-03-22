

#####################    Statistik_init.R    #############################
##                                                                      ##
##  Plots the relative abundance of Monodominant over the mortality     ##
##  per year as boxplots. Plots as well the emergence of                ##
##  (non-percolating) clusters.                                         ##
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
size_w = 512
#init_v<-c(0,1,-1,3)
init_v<-c(0,-1, 1, 3)
prop = 9500
radius = 20
radius_vec = c(20) 
mortality = 0.015
prop_v <-  c(9500)
radius = 20
zusammen<-data.frame(size = NA, seedmass = NA, seed = NA, init_opt = NA, rel_abundance = NA)
size_vec <- c(512)

for(init in init_v)
{
  string<-paste(path_data, "/Init/Init_Stats_size_",size_w,"_seedmass_", prop,"_radius_20_mort_0.015000_init_option_",init, sep="")
  
  Statistik<-as.matrix(read.table(string, header=F, sep="\t"))
  init_option = Statistik[,5]

  new<-data.frame(size = Statistik[,1], seedmass = Statistik[,2], seed = Statistik[,3], init_opt = init_option, rel_abundance = Statistik[,9])      
  zusammen<-rbind(zusammen,new)
}
zusammen<-na.omit(zusammen)
attach(zusammen)

mean_0 <- mean(zusammen$rel_abundance[init_opt==0])
mean_1 <- mean(zusammen$rel_abundance[init_opt==1])
mean_3 <- mean(zusammen$rel_abundance[init_opt==3])
mean_streets <- mean(zusammen$rel_abundance[init_opt==-1])


sd_0 <- sd(zusammen$rel_abundance[init_opt==0])
sd_1 <- sd(zusammen$rel_abundance[init_opt==1])
sd_3 <- sd(zusammen$rel_abundance[init_opt==3])
sd_streets <- sd(zusammen$rel_abundance[init_opt==-1])

init_statistics <- data.frame(random.mean = mean_0, random.sd = sd_0, grid.mean = mean_1, grid.sd = sd_1, knight.mean = mean_3 ,  knight.mean = sd_3, streets.mean = mean_streets, streets.sd = sd_streets )
