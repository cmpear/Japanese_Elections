#!/usr/bin/env Rscript

library(shiny)
library(shinythemes)

path <<- 'R'

## ENCODE & DECODE FUNCTIONS #####################
kencode<-function(d){
  if (is.numeric(d)){return(d)}
  if (NROW(d)>1)
  {
    return(sapply(d,kencode))
  }
  return(ken.codex$prefid[ken.codex$pref==d])
}
kdecode<-function(d){
  if (NROW(d)>1)
  {
    return(sapply(d,kdecode))
  }
  if(d>100)
  {
    return(paste0(ken.codex$pref[ken.codex$prefid==trunc(d/100)],"-",d%%100))
  }
  return(ken.codex$pref[ken.codex$prefid==d])
}

jencode<-function(d,j=FALSE){
  if (is.numeric(d)){return(d)} # In case it is already encoded
  if (is.null(d)){return(d)}
  if(NROW(d)>1)
  {
    return(sapply(d,jencode))
  }
  if(j)
  {
    return(jcode$Party.ID[jcode[,4]==d])
  }
  else if (nchar(d)<7) #Abreviation
  {
    return(jcode$Party.ID[jcode[,2]==d])
  }
  else  # Full Name
  {
    return(jcode$Party.ID[jcode[,3]==d])
  }
}
jdecode<-function(d,full=FALSE,j=FALSE){
  if(NROW(d)>1)  {
    return(sapply(d,jdecode, full=full))
  }
  if (!is.numeric(d))  {
    return(d)
  }
  if(full)  {
    return(jcode$Full.Name[jcode[,1]==d])
  }
  else if (j)  {
    return(jcode$Japanese.Name[jcode[,1]==d])
  }
  else  {
    return(jcode$Name[jcode[,1]==d])
  }
}

block<-function(d){
  d<-kencode(d)
  if (NROW(d)>1)
  {
    return(sapply(d,block))
  }
  return(ken.codex$block[ken.codex$prefid==d])
}

## RETRIEVE DATA #############################################################################################
# having trouble with data-loading later on, so moving data-loading here
# original problem seems to have been some random characters that got inserted into a function, but still makes more sense to load data and use as globals.
ken.codex<-read.delim( "https://raw.githubusercontent.com/cmpear/Japanese_Elections/master/R/Data/kenCodex.csv",sep=",",stringsAsFactors=FALSE)
jElections<-read.csv( "https://raw.githubusercontent.com/cmpear/Japanese_Elections/master/R/Data/smdResults.csv",sep=",",stringsAsFactors=FALSE)
jpr<-read.delim( "https://raw.githubusercontent.com/cmpear/Japanese_Elections/master/R/Data/prResults.csv",sep=",",stringsAsFactors=FALSE)[,-1]
jcode<-read.delim( "https://raw.githubusercontent.com/cmpear/Japanese_Elections/master/R/Data/PartyCodex.csv",sep=",",stringsAsFactors=FALSE)
jcode<-rbind(jcode,c(100,"NOO","The FAKE Party","MU","",""))
jcode[,1]<-as.numeric(jcode[,1])
#jElections<-rbind(jElections,data.frame("id"=c(144794+1:5),"pref"=rep(47,5),"dist"= c(94+1:5),"n.can"= rep(1,5),"win"= rep(2,5),
#          "candidate"= rep("",5),"party"= rep(100,5),"inc"= rep(0,5),"dual"=rep(0,5),"votes"=rep(0,5),
#          "age"=rep(0,5),"turnout"=rep(0,5),"vshare"=rep(0,5),"place"=rep(1,5),"margin"=rep(0,5),"year"=rep(2014,5)))

# Adding dud data that should be automatically excluded to deal with a problem that seems unique to Shiny


# japgeo<-readOGR(dsn=file.path(path, "data/jpmap/jp_grid_ken_pgn.shp"),stringsAsFactors = FALSE)
# japgeo$NAME2[japgeo$NAME2=="Gumma"]<-"Gunma"
# japgeo$NAME2[japgeo$NAME2=="Okajama"]<-"Okayama"
# japgeo$NAME2[japgeo$NAME2=="Tshiba"]<-"Chiba"
# ken.japgeo<-unlist(kencode(japgeo$NAME2))
#file.path(getwd(), 'R' )
#print(path)

print("AAHHHHHHHHHHHHHHHHHHHHHHHHHHHH")

options(shiny.host = '0.0.0.0')
options(shiny.port = 8080)
shiny::runApp(appDir = path)


# Project Notes: Current the map functionality is disabled, as rgdal is difficult to install on linux and
# has compatibility issues with the latest version of R.  Possible solutions are to either to get it up and
# running, or use an alternative mapping solution.  Perhaps go with ggmap, but that would require ggplot.
# Maybe we can get around needing rgdal with the fortify function to transform a shapefile into a csv.


