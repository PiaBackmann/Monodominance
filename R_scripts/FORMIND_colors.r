## pallets for formind analysis
## Date:   9.7.2013
## brief:     different pallet-creators
## version:   13.7
## copyright: Creative Commons CC-BY-SA-NC
## deteils:

rm(list=ls()) # clean up workspace

library(colorspace)

pft_color1<-function(n,alpha=NA){
 # palette<-rainbow_hcl(n,c=100,l=(1:n)/n *70+15,start=245,end=605*(n-1)/n)
  palette<-rainbow_hcl(n,c=100,l=(1:n)/n *70+15,start=50,end=440*(n-1)/n)
  if(!is.na(alpha))palette<-paste(palette,substr(rgb(alpha,1,1),2,3),sep="")

  return(palette)
}

pft_color2<-function(n,alpha=NA){
  palette<-rainbow_hcl(n,start=0.4)
  if(!is.na(alpha))palette<-paste(palette,substr(rgb(alpha,1,1),2,3),sep="")
  
  return(palette)
}


seq_colorwb<-function(n,alpha=NA){
  palette<-sequential_hcl(n, h = 260, c. = c(100, 0), l = c(40, 90))
  if(!is.na(alpha))palette<-paste(palette,substr(rgb(alpha,1,1),2,3),sep="")
  
  return(palette)
}
seq_colorrg<-function(n,alpha=NA){
  palette<-rainbow_hcl(n,c=100,l=(1:n)/n *60+10,start=135,end=10*(n-1)/n)
  if(!is.na(alpha))palette<-paste(palette,substr(rgb(alpha,1,1),2,3),sep="")
  
  return(palette)
}

div_color<-function(n,alpha=NA){
  palette<-diverge_hcl(n, h = c(220, 355), c = 100, l = c(30, 95))
  if(!is.na(alpha))palette<-paste(palette,substr(rgb(alpha,1,1),2,3),sep="")
  
  return(palette)
}
#path <- getwd()

#save.image(paste0(path, "/FORMIND_colors.RData")
