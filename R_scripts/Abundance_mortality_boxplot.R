###################   Abundance_mortality_boxplot.R   #########################
##    March  2016          von Pia Backmann                      For Martin  ##
##                                                                           ##
##    Plots the relative abundance for different values of tree mortalities  ##                                                
##    against the seed mass of the Monodominant species   (boxplot)          ##
##                                                                           ##
##                                                                           ##
###############################################################################

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


zusammen<-data.frame(seedmass = NA, seed_martin = NA, rel_abundance = NA, cluster = NA, non_perko = NA, mortality = NA)
radius = 20
weltgroesse = 512

mass_v <- c(9000, 9200, 9300, 9350, 9400, 9450, 9500, 9550, 9600, 9650, 9700, 9800, 10000)
mass_v = mass_v
size = 512
mort_vec<-c(0.001, 0.005,  0.01, 0.025)

for(mort in mort_vec)
{
    for(mass in  mass_v)
    {
      # if you use the example-dataset:
      string<-paste0(path_data,"/Mortality/Martins_Monodominance_Stats_size_",size,"_prop_", mass,"_mortality_", mort,"_radius_",radius,".txt",sep="")
     
      # if you use your own produced data:
      # string<-paste0(path_data,"/Mortality/Monodominance_Stats_size_",size,"_seedmass_", mass,"_mortality_", mort,"_radius_",radius,".txt",sep="")
      
      Statistik<-as.matrix(read.table(string, header=F, sep="\t"))
      new<-data.frame(seedmass = Statistik[,2], seed_martin=Statistik[,3], rel_abundance = Statistik[,7], cluster = Statistik[,6], non_perko = Statistik[,8], mortality = mort)
      
      zusammen<-rbind(zusammen, new)
    }
}
zusammen<-na.omit(zusammen)
attach(zusammen)


mort_v<-vector(length=1)
seedmass_v<-vector(length=1)
abundance_v<-vector(length=1)
abundance_std_v<-vector(length=1)
cluster_v<-vector(length =1)
i = 1;
for(mass in  mass_v)
{  
  for(mort in mort_vec)
  {  
    abundance_v[i] = mean(rel_abundance[mortality==mort & seedmass==mass])
    abundance_std_v[i] = sd(rel_abundance[mortality==mort & seedmass==mass])
    mort_v[i] = mort
    seedmass_v[i] = mass
    cluster_v[i] = mean(non_perko[mortality==mort & seedmass==mass])
    i = i+1;
  }
}
ergebnisse<-data.frame(mortality=mort_v, mass=seedmass_v, rel_abundance=abundance_v, non_perko = cluster_v )



# hier sind noch alle Werte (ungemittelt) drin für Boxplot
non_perko_v = zusammen$non_perko
abundance_v = zusammen$rel_abundance
seedmass_v = zusammen$seedmass
mort_v = zusammen$mortality

line_mean_v<-vector(length = length(abundance_v))
for (item in 1:length(abundance))
{
  mort = mortality[item]
  pro = seedmass[item]
  
  line_mean_v[item] = mean(ergebnisse$rel_abundance[ergebnisse$mortality==mort & ergebnisse$mass == pro])
}
results<-data.frame(seedmass=seedmass_v,abundance=abundance_v, non_perko = non_perko_v, mortality = mort_v, line_mean = line_mean_v)

mass_v =  c( 9000, 9100, 9200, 9300, 9350, 9400, 9450, 9500, 9550, 9600, 9650, 9700, 9750, 9800, 9900, 10000)
mass_v = mass_v
for(mass in mass_v)
{
  for(mort in mort_vec)
  {
    new = data.frame(seedmass=mass, abundance=NA, non_perko = NA, mortality = mort, line_mean = NA)
    results2<-rbind(results, new)
  }
}
attach(results)


setEPS()
postscript(paste0(path_results,"/Statistics/Monodominance_boxplot.eps", sep=""))

#png(filename=paste0(path_results,"/Statistics/Monodominance_boxplot.png", sep=""))#, width=1200, height=1000)
bw=0.2
#xlaenge=length(unique(results$seedmass))
xlaenge=13
limi= c(1,length(unique(results$seedmass)))
par(mar=c(5, 5, 1, 1)) 
boxplot(abundance~seedmass, data=results, subset = mortality=="0.01", col="#984ea3", boxwex=bw, las=1, ylab = "Relative abundance of monodominant species", xlab = "Seed mass of monodominant species", cex.lab=1.3, cex.axis=1.3)
legend(1, 0.8, c("0.001", "0.005", "0.01", "0.025"), title = "mortality",
       fill = c("#e41a1c", "#4daf4a", "#984ea3", "#377eb8" ), text.width = 1.3, cex=1.2)
boxplot(abundance~seedmass, data=results, subset = mortality=="0.001", col="#e41a1c",add = TRUE, at=(1:xlaenge), boxwex=bw, xaxt="n", yaxt="n", las=1, xlim = limi)
boxplot(abundance~seedmass, data=results, subset = mortality=="0.005", col="#4daf4a",add = TRUE, at=(1:xlaenge)-0.15, boxwex=bw, xaxt="n", yaxt="n",las=1, xlim = limi)
boxplot(abundance~seedmass, data=results, subset = mortality=="0.025", col="#377eb8",add = TRUE,  at=(1:xlaenge)+0.45,boxwex=bw, xaxt="n", yaxt="n", las=1, xlim = limi)
mittel<-tapply(results2$abundance[results2$mortality=="0.01"], results2$seedmass[results2$mortality=="0.01"], mean, na.rm=TRUE)
lines(seq(1,13), mittel, type="l", col="#984ea3", lw=1.7)
mittel<-tapply(results2$abundance[results2$mortality=="0.005"], results2$seedmass[results2$mortality=="0.005"], mean, na.rm=TRUE)
lines(seq(1,13)-0.15, mittel, type="l", col="#4daf4a", lw=1.7)
mittel<-tapply(results2$abundance[results2$mortality=="0.001"], results2$seedmass[results2$mortality=="0.001"], mean, na.rm=TRUE)
lines(seq(1,13), mittel, type="l", col="#e41a1c", lw=1.7)
mittel<-tapply(results2$abundance[results2$mortality=="0.025"], results2$seedmass[results2$mortality=="0.025"], mean, na.rm=TRUE)
lines(seq(1,13)+0.45, mittel, type="l", col="#377eb8", lw=1.7)

dev.off()

