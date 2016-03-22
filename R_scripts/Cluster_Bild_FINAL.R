#######################    Cluster_Bild.R   ##################################
## March 2016           by  Pia Backmann                   For Martin       ##
##                                                                          ##
##   Plots all clusters of the monodominant species (each cluster is        ##
##   to another color which is randomly chosen. Plots a sequence of plots   ##
##   at different time steps. To ensure that for two consecutive pictures   ##
##   the same color codes are used for the same clusters, a special         ##
##   algorithm is used (to at least detect the four biggest clusters)       ##
##   !!!         Produces a large number of pictures              !!!       ##
##                                                                          ##
##############################################################################
library(ggplot2)
library(fields)

size = 512
propagule = 9450

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
current = 500
factor = 1
seed = 4

string<-paste(path_data, "/Cluster/MartinsClusterView_size_",size,"_propagule_",propagule,"_radius_20_mort_0.015000_seed_",seed,"_tick_1.out", sep="")
cluster_view<-as.matrix(read.table(string, header=F))
before<-cluster_view

initial_maximum = max(cluster_view)+1

# Definiere Farbpalette
meineFarben = rainbow(initial_maximum)
meineFarben = rep(meineFarben,500)
meineFarben = c("white", meineFarben) 

 #######   hier würde for-schleife anfangen   ########
increment<-seq(from=100, to=10000, by=100)
ticker<-c(1, increment)
for (tick in ticker)
{
    string<-paste(path_data, "/Cluster/MartinsClusterView_size_",size,"_propagule_",propagule,"_radius_20_mort_0.015000_seed_",seed,"_tick_",tick,".out", sep="")
    cluster_view<-as.matrix(read.table(string, header=F))
    aktuell<-cluster_view
    
    if(tick >= 2100) 
    {
         #finde größten cluster, zweitgrößten cluster und drittgrößten cluster (heuristischer ansatz)
        max_cluster = 0
        second = 0
        third = 0
        forth = 0
        fifth = 0
        sixth = 0
        seventh = 0
        y_biggest_cluster_old = -20
        y_second_cluster_old = -20
        y_third_cluster_old = -20
        y_forth_cluster_old = -20
        y_fifth_cluster_old = -20
        y_sixth_cluster_old = -20
        y_seventh_cluster_old = -20
        y_biggest_cluster_new = -20
        y_second_cluster_new = -20
        y_third_cluster_new = -20
        y_forth_cluster_new = -20
        y_fifth_cluster_new = -20
        y_sixth_cluster_new = -20
        y_seventh_cluster_new = -20
        
        max_size = max(before)
         for(i in 0:max_size)
         {
           count = length(which(before == i))
           if(max_cluster < count)
           {
             seventh = sixth
             y_seventh_cluster_old = y_sixth_cluster_old
             sixth = fifth
             y_sixth_cluster_old = y_fifth_cluster_old
             fifth = forth
             y_fifth_cluster_old = y_forth_cluster_old
             forth = third
             y_forth_cluster_old = y_third_cluster_old
             third = second 
             y_third_cluster_old = y_second_cluster_old
             second = max_cluster
             y_second_cluster_old = y_biggest_cluster_old
             y_biggest_cluster_old = i
             max_cluster = count
           }
           else if(second < count)
           {
             seventh = sixth
             y_seventh_cluster_old = y_sixth_cluster_old
             sixth = fifth
             y_sixth_cluster_old = y_fifth_cluster_old
             fifth = forth
             y_fifth_cluster_old = y_forth_cluster_old
             forth = third
             y_forth_cluster_old = y_third_cluster_old
             third = second 
             y_third_cluster_old = y_second_cluster_old    
             second = count
             y_second_cluster_old = i
           }
           else if(third < count)
           {
             seventh = sixth
             y_seventh_cluster_old = y_sixth_cluster_old
             sixth = fifth
             y_sixth_cluster_old = y_fifth_cluster_old
             fifth = forth
             y_fifth_cluster_old = y_forth_cluster_old
             forth = third
             y_forth_cluster_old = y_third_cluster_old
             third = count
             y_third_cluster_old = i             
           }
           else if(forth < count)
           {
             seventh = sixth
             y_seventh_cluster_old = y_sixth_cluster_old
             sixth = fifth
             y_sixth_cluster_old = y_fifth_cluster_old
             fifth = forth
             y_fifth_cluster_old = y_forth_cluster_old
             forth = count
             y_forth_cluster_old = i
           }
           else if(fifth < count)
           {
             seventh = sixth
             y_seventh_cluster_old = y_sixth_cluster_old
             sixth = fifth
             y_sixth_cluster_old = y_fifth_cluster_old
             fifth = count
             y_fifth_cluster_old = i

           }
           else if(sixth < count)
           {
             seventh = sixth
             y_seventh_cluster_old = y_sixth_cluster_old
             sixth = count
             y_sixth_cluster_old = i          
           }
           else if(seventh < count)
           {
             seventh = count
             y_seventh_cluster_old = i        
           }
         }
         
        
        if(max_cluster > 1)
        {
          biggest_x_before<-which(before == y_biggest_cluster_old)
          biggest_y_now<-aktuell[biggest_x_before]
          #finde größten cluster
          max = 0
          max_size_cluster = max(biggest_y_now)
          for(i in 0:max_size_cluster)
          {
            count = length(which(biggest_y_now == i))
            if( count > 0 && max < count)
            {
              max = count
              y_biggest_cluster_new = i
            }
          }
          biggest_x_now<-which(aktuell == y_biggest_cluster_new)
          cluster_view[biggest_x_now] = y_biggest_cluster_old 
        }

        
        if(second > 1)
        {
          second_x_before<-which(before == y_second_cluster_old)
          second_y_now<-aktuell[second_x_before]
          #finde zweit-größten cluster
          max_second = 0
          second_size_cluster = max(second_y_now)
          for(i in 0:second_size_cluster)
          {
            count = length(which(second_y_now == i))
            if(count > 0 && max_second < count && i != y_biggest_cluster_new)
            {
              max_second = count
              y_second_cluster_new = i
            }
          }
          second_x_now<-which(aktuell == y_second_cluster_new)
          cluster_view[second_x_now] = y_second_cluster_old
        }
        
        if(third > 1)
        { 
            third_x_before<-which(before == y_third_cluster_old)
            third_y_now<-aktuell[third_x_before]
            
            #finde dritt-größten cluster
            max_third = 0
            third_size_cluster = max(third_y_now)
            for(i in 0:third_size_cluster)
            {
              count = length(which(third_y_now == i))
              if(count > 0 && max_third < count && i != y_second_cluster_new && i != y_biggest_cluster_new)
              {
                max_third = count
                y_third_cluster_new = i
              }
            }
            third_x_now<-which(aktuell == y_third_cluster_new)
            cluster_view[third_x_now] = y_third_cluster_old        
        }   

        if(forth > 1)
        { 
          forth_x_before<-which(before == y_forth_cluster_old)
          forth_y_now<-aktuell[forth_x_before]
          
          #finde dritt-größten cluster
          max_forth = 0
          forth_size_cluster = max(forth_y_now)
          for(i in 0:forth_size_cluster)
          {
            count = length(which(forth_y_now == i))
            if(count > 0 && max_forth < count && i != y_third_cluster_new && i != y_second_cluster_new && i != y_biggest_cluster_new)
            {
              max_forth = count
              y_forth_cluster_new = i
            }
          }
          forth_x_now<-which(aktuell == y_forth_cluster_new)
          cluster_view[forth_x_now] = y_forth_cluster_old      
        }   
        
        if(fifth > 1)
        { 
          fifth_x_before<-which(before == y_fifth_cluster_old)
          fifth_y_now<-aktuell[fifth_x_before]
          
          #finde fünft-größten cluster
          max_fifth = 0
          fifth_size_cluster = max(fifth_y_now)
          for(i in 0:fifth_size_cluster)
          {
            count = length(which(fifth_y_now == i))
            if(count > 0 && max_fifth < count && i != y_forth_cluster_new && i != y_third_cluster_new  && i != y_second_cluster_new && i != y_biggest_cluster_new)
            {
              max_fifth = count
              y_fifth_cluster_new = i
            }
          }
          fifth_x_now<-which(aktuell == y_fifth_cluster_new)
          cluster_view[fifth_x_now] = y_fifth_cluster_old      
        }  
        
        
        if(sixth > 1)
        { 
          sixth_x_before<-which(before == y_sixth_cluster_old)
          sixth_y_now<-aktuell[sixth_x_before]
          
          #finde sechst-größten cluster
          max_sixth = 0
          sixth_size_cluster = max(sixth_y_now)
          for(i in 0:sixth_size_cluster)
          {
            count = length(which(sixth_y_now == i))
            if(count > 0 && max_sixth < count && i != y_fifth_cluster_new &&  i != y_forth_cluster_new && i != y_third_cluster_new  && i != y_second_cluster_new && i != y_biggest_cluster_new)
            {
              max_sixth = count
              y_sixth_cluster_new = i
            }
          }
          sixth_x_now<-which(aktuell == y_sixth_cluster_new)
          cluster_view[sixth_x_now] = y_sixth_cluster_old      
        }  
        
        if(seventh > 1)
        { 
          seventh_x_before<-which(before == y_seventh_cluster_old)
          seventh_y_now<-aktuell[seventh_x_before]
          
          #finde siebt-größten cluster
          max_seventh = 0
          seventh_size_cluster = max(seventh_y_now)
          for(i in 0:seventh_size_cluster)
          {
            count = length(which(seventh_y_now == i))
            if(count > 0 && max_seventh < count && i != y_sixth_cluster_new &&  i != y_fifth_cluster_new && i != y_forth_cluster_new && i != y_third_cluster_new  && i != y_second_cluster_new && i != y_biggest_cluster_new)
            {
              max_seventh = count
              y_seventh_cluster_new = i
            }
          }
          seventh_x_now<-which(aktuell == y_seventh_cluster_new)
          cluster_view[seventh_x_now] = y_seventh_cluster_old      
        }  
        
        show(paste("#############################   tick = ", tick, "   ###############################", sep=""))
        show(paste("First old = ", y_biggest_cluster_old, "  first new = ", y_biggest_cluster_new, " || size_old = ", max_cluster, "  size_new = ", max ,sep=""))
        show(paste("Second old = ", y_second_cluster_old, "  second new = ", y_second_cluster_new, " || size_old = ", second, "  size_new = ", max_second  ,sep=""))
        show(paste("Third old = ", y_third_cluster_old, "  third new = ", y_third_cluster_new, " || size_old = ", third , "  size_new = ", max_third , sep=""))
        show(paste("Forth old = ", y_forth_cluster_old, "  forth new = ", y_forth_cluster_new, " || size_old = ", forth, "  size_new = ", max_forth , sep=""))
        show("#####################################################################")
    }
    

     ########################################################################################
     
      # Speichere Plot ab
      if(tick < 10000)
      {    
        name=paste(path_results, "/Cluster/ClusterView_size_",size,"_propagule_",propagule,"_radius_20_mort_0.015000_seed_",seed,"_tick_0", tick,".png", sep="")
        name_alt=paste(path_results, "/ClusterView_size_",size,"_propagule_",propagule,"_radius_20_mort_0.015000_seed_",seed,"_tick_0", tick,"_alt.png", sep="")
      }
      if(tick < 1000)
      {
        name=paste(path_results, "/Cluster/ClusterView_size_",size,"_propagule_",propagule,"_radius_20_mort_0.015000_seed_",seed,"_tick_00", tick,".png", sep="")
      }
      if(tick < 100)
      {
        name=paste(path_results, "/Cluster/ClusterView_size_",size,"_propagule_",propagule,"_radius_20_mort_0.015000_seed_",seed,"_tick_000", tick,".png", sep="")
      }
      if(tick < 10)
      {
        name=paste(path_results, "/Cluster/ClusterView_size_",size,"_propagule_",propagule,"_radius_20_mort_0.015000_seed_",seed,"_tick_0000", tick,".png", sep="")
      }
      if(tick%%current == 0)
      {
        factor = factor+1
        current = 500*factor
      }
      tickstr = paste("tick < ", current)
      png(filename=name, width=1000,height=1000)
      #    bg="white")
      image( 1:ncol(cluster_view),1:nrow(cluster_view), cex.lab=2, cex.main = 2, t(t(cluster_view)), xlab=tickstr, ylab = "", main = "Monodominance cluster formation view"
             , col=meineFarben, axes=FALSE, zlim=c(-1, initial_maximum))
      dev.off()
      
    if(tick > 10000)
    {
      png(filename=name_alt, width=1000,height=1000)
      #    bg="white")
      image( 1:ncol(aktuell),1:nrow(aktuell), cex.lab=2, cex.main = 2, t(t(aktuell)), xlab=tickstr, ylab = "", main = "Monodominance cluster formation view"
             , col=meineFarben, axes=FALSE, zlim=c(-1, max_size))
      dev.off()
    }

        
      #speichere neue matrix als alte ab für nächstes bild
      before=cluster_view
}