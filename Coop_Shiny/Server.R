library(shiny)
library(RColorBrewer)
library(rgdal)
library(scales) # for alpha--transparent colors

## ALLIANCE HELPER FUNCTIONS ######################
alliance.depreciate.all<-function(vsmatrix,eff=1){
  return(apply(FUN="alliance.depreciate",X=vsmatrix,eff=eff,MARGIN=1))
}
alliance.depreciate<-function(junior,eff=1,senior=NA){
  # given a vector of votes and an efficiency, returns expected alliance effectiveness
  if (NROW(junior)==1){return(junior)}
  senior<-max(junior)
  return(sum(senior,sum(junior[junior!=senior])*eff))
}
alliance.shorten<-function(alliance){
  if (NROW(alliance)>1)
  {
    return(sapply(X=alliance,FUN="alliance.shorten"))
  }
  if (alliance=="Best Outsider"){return("Others")}
  if (substr(alliance,3,3)=="-")
  {
    return(paste0(substr(alliance,1,2),"+"))
  }
  if (substr(alliance,4,4)=="-")
  {
    return(paste0(substr(alliance,1,3),"+"))
  }
  return(alliance)
}
alliance.namer<-function(a,stub=NULL){
  if (NROW(a)==1) {return(paste0(stub,jdecode(a[1])))}
  if (NROW(a)>1) {
    return(alliance.namer(a[-1],paste0(stub,jdecode(a[1]),"-")))
  }
}
#returns the cc value associated with a ternary function
ternary.cc<-function(year,a1,a2,a3=NULL,hyp=TRUE,eff=1,mar=10){
#  if (exists(input$a3)){a3<-jencode(input$a3)}else{a3<-NULL}
  a1<-jencode(a1);a2<-jencode(a2); a3<-jencode(a3)
  #  a1<-c("LDP","CGP"); a2<-c("DPJ","PNP","SDP","NPN");a3<-NULL; year<-2009
  cc<-ppvs.matrix(PARTY=NULL,PR=FALSE,restrict=TRUE,blankcol=3,year=year)
  cc[,3]<-alliance.depreciate.all(ppvs.matrix(a1,year,PR=FALSE)[-1:-2],eff=eff)
  cc[,4]<-alliance.depreciate.all(ppvs.matrix(a2,year,PR=FALSE)[-1:-2],eff=eff)
  names(cc)[3:4]<-c(alliance.namer(a1),alliance.namer(a2))
  if (is.null(a3)){
    a3<-unique(jElections$party[jElections$year==year])
    for (p in c(a1,a2))    {
      a3<-a3[a3!=p]
    }
    cc[,5]<-apply(FUN="max",MARGIN=1,X=ppvs.matrix(a3,year,PR=FALSE)[-1:-2])
    names(cc)[5]<-"Best Outsider"
  }else  {
    cc[,5]<-alliance.depreciate.all(ppvs.matrix(a3,year,PR=FALSE)[-1:-2],eff=eff)
    names(cc)[5]<-alliance.namer(a3)
  }
  cc[,1]<-cc[,1]+mar%%10/10
#  cc[,3]<-cc[,3]+0.03;cc[,4]<-cc[,4]+0.04;cc[,5]<-cc[,5]+0.07
  return(cc)
}



## MAP FUNCTIONS ####
merge_map<-function(d,d.criteria,col=brewer.pal(9,"YlOrRd"),fixedPR=FALSE){
  # d is a dataframe, c1=criteria, c2=data for ken
  # originally had an input for map data, but decided this would be faster if I just used global variable japgeo
  # the return vaue for this should be a matrix with c1=data and c2=colors; should be sorted to match ken.japgeo)
  #NOTE: japgeo prefectures are not only out of order, but entries for the same prefecture are not even all next to each other
  
  #  d<-jElections[jElections$party==1 & jElections$year==2005 & jElections$dist==1,c(2,13)]
  u.rows<-NROW(unique(d[,2]))
  # c1=min, c2=color
  ncolors<-NROW(col)
  iro<-matrix(data=NA,nrow=ncolors,ncol=2)
  iro[,2]<-col
  if (fixedPR){
    iro[,1]<-c(0,0.10,0.20,5:10*5/100)
  }else if (u.rows==ncolors){
    start<-0
    for (i in 1:u.rows)
    {
      iro[i,1]<-as.numeric(unique(d[,2])[i])
    }
  }else if (u.rows>ncolors){
    start<-min(d[,2])
    gap<-(range(d[,2])[2]-range(d[,2])[1])/(ncolors)
    for (i in 1:ncolors)
    {
      iro[i,1]<-start + gap * (i-1)
    }
  }
  d.return<-as.data.frame(matrix(data=NA,ncol=2,nrow=NROW(japgeo)))
  for (i in 1:47)
  {
    logi<-(ken.japgeo==i)
    logi2<-d[,1]==i
    if (NROW(d[logi2,2])==0) {
      d.return[logi,1]<-0
      d.return[logi,2]<-    iro[sum (start>=iro[,1]),2]      
    }
    else {
      d.return[logi,1]<-d[logi2,2]
      d.return[logi,2]<-    iro[sum (d[logi2,2]>=iro[,1]),2]      
    }
  }
  return(d.return)
}

## FUNCTIONS FOR REARRANGING ELECTION DATA ################################
#Given a party, year, and list of IDs, returns 
ppvs.vector<-function(party,ID,PR=FALSE){
  if (PR){
    return(unlist(mapply(FUN="pvs.pr",party=party,id=ID)))
  }
  return(unlist(mapply(FUN="pvs",party=party,id=ID)))
}
ppvs.matrix<-function(PARTY,year,PR=TRUE,restrict=FALSE,blankcol=0){
  # restrict determines whether only districts where one of the parties won should be included
  if (is.null(PARTY)){
    cc<-unique(jElections$id[jElections$year==year])
    cc<-as.data.frame(matrix(data=cc,nrow=NROW(cc),ncol=(2+blankcol)))
    names(cc)[1:2]<-c("id","pref")
    cc[,2]<-trunc(cc[,2]%%10000/100)
    if (blankcol>0){
      cc[,3:(2+blankcol)]<-NA
    }
    return(cc)
  }
  if (restrict){
    ID<-jElections$party==PARTY[1]; if (NROW(PARTY)==1){break}
    for (p in PARTY[-1])
    {
      ID<-ID | jElections$party==p
    }
    ID<-unique(jElections$id[ID & jElections$year==year & jElections$win==2])
  }
  else{
    ID<-unique(jElections$id[jElections$year==year])    
  }
  
  cc<-as.data.frame(cbind(ID,trunc(ID%%10000/100),
                          matrix(data=mapply(party=rep(PARTY,each=NROW(ID)),
                                             id=   rep(ID,NROW(PARTY)),
                                             FUN="pvs"),ncol=NROW(PARTY),nrow=NROW(ID)))
  )
  names(cc)[1:2]<-c("id","pref")
  i<-3
  if (PR)
  {
    while(i<=NCOL(cc)){
      names(cc)[i]<-paste0(jdecode(PARTY[i-2]),"smd")
      i<-i+1
    }
    cc<-cbind(cc,matrix(data=mapply(party=rep(PARTY,each=NROW(ID)),
                                    id=   rep(ID,NROW(PARTY)),
                                    FUN="pvs.pr"),ncol=NROW(PARTY),nrow=NROW(ID)))
    while (i <=NCOL(cc)){
      names(cc)[i]<-paste0(jdecode(PARTY[i-NROW(PARTY)-2]),"pr")
      i<-i+1
    }
    return(cc)
  }
  while(i<=NCOL(cc)){
    names(cc)[i]<-paste0(jdecode(PARTY[i-2]))
    i<-i+1
  }
  return(cc)
}
pvs<-function(party,id){
  v<-jElections$vshare[jElections$id==id & jElections$party==party]
  #This is a temporary solution to cases where there are multiple "other" and independent candidates
  if (NROW(v)>1){v<-max(v)}
  if (NROW(v)>0){return(v)}
  return(0)
}
pvs.pr<-function(party,id){
  pref<-trunc(id%%10000/100)
  year<-trunc(id/10000)+2000
  v<-jpr$vshare[jpr$Year==year & jpr$Party==party & jpr$pref==pref]
  if (NROW(v)>0){return(v)}
  return(0)
}

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
ken.codex<-read.delim( paste0(getwd(), "/Data/kenCodex.csv"),sep=",",stringsAsFactors=FALSE)
jElections<-read.csv( paste0(getwd(), "/Data/smdResults.csv"),sep=",",stringsAsFactors=FALSE)
jpr<-read.delim( paste0(getwd(), "/Data/prResults.csv"),sep=",",stringsAsFactors=FALSE)[,-1]
jcode<-read.delim( paste0(getwd(), "/Data/PartyCodex.csv"),sep=",",stringsAsFactors=FALSE)
jcode<-rbind(jcode,c(100,"NOO","The FAKE Party","MU","",""))
jcode[,1]<-as.numeric(jcode[,1])
#jElections<-rbind(jElections,data.frame("id"=c(144794+1:5),"pref"=rep(47,5),"dist"= c(94+1:5),"n.can"= rep(1,5),"win"= rep(2,5),
#          "candidate"= rep("",5),"party"= rep(100,5),"inc"= rep(0,5),"dual"=rep(0,5),"votes"=rep(0,5),
#          "age"=rep(0,5),"turnout"=rep(0,5),"vshare"=rep(0,5),"place"=rep(1,5),"margin"=rep(0,5),"year"=rep(2014,5)))

# Adding dud data that should be automatically excluded to deal with a problem that seems unique to Shiny


japgeo<-readOGR(dsn=paste0(getwd(), "/jpmap/jp_grid_ken_pgn.shp"),stringsAsFactors = FALSE)
japgeo$NAME2[japgeo$NAME2=="Gumma"]<-"Gunma"
japgeo$NAME2[japgeo$NAME2=="Okajama"]<-"Okayama"
japgeo$NAME2[japgeo$NAME2=="Tshiba"]<-"Chiba"
ken.japgeo<-unlist(kencode(japgeo$NAME2))

## graph adjustment variables ####
bw<-0.3; tp<-0.25
# tbar

## alliance.defaults ####
a1.defaults<-list("1996"=c("Liberal Democratic Party","Social Democratic Party","New Party Sakigake"),
                  "2000"=c("Liberal Democratic Party","Komeito","New Conservative Party"),
                  "2003"=c("Liberal Democratic Party","Komeito","New Conservative Party"),
                  "2005"=c("Liberal Democratic Party","Komeito"),
                  "2009"=c("Liberal Democratic Party","Komeito"),
                  "2012"=c("Liberal Democratic Party","Komeito"),
                  "2014"=c("Liberal Democratic Party","Komeito"),
                  "2017"=c("Liberal Democratic Party","Komeito"))
a2.defaults<-list("1996"="New Frontier Party",
                  "2000"="Democratic Party of Japan",
                  "2003"="Democratic Party of Japan",
                  "2005"="Democratic Party of Japan",
                  "2009"=c("Democratic Party of Japan","Social Democratic Party","Peoples New Party","New Party Nippon"),
                  "2012"=c("Democratic Party of Japan","Peoples New Party"),
                  "2014"="Democratic Party of Japan",
                  "2017"=c("Constitutional Democratic Party","Social Democratic Party","Japanese Communist Party"))
## SHINY SECTION!!! ####

shinyServer(function(input, output, session) {
  observeEvent( input$year,{
    updateCheckboxGroupInput(session,"a1", label = NULL,
                             choices= jdecode(unique(jElections$party[jElections$year==input$year & jElections$party!=98 & jElections$party!=100]), full=TRUE),
                             selected = unlist(a1.defaults[as.character(input$year)]))
    updateCheckboxGroupInput(session,"a2", label = NULL,
                             choices= jdecode(unique(jElections$party[jElections$year==input$year & jElections$party!=98 & jElections$party!=100]), full=TRUE),
                             selected = unlist(a2.defaults[as.character(input$year)]))
    updateCheckboxGroupInput(session,"a3", label = NULL,
                             choices= jdecode(unique(jElections$party[jElections$year==input$year & jElections$party!=98 & jElections$party!=100]), full=TRUE),
                             selected = character(0))
  })

  cc<-reactive({
    ternary.cc(input$year,input$a1,input$a2,input$a3,hyp=input$eff!=0,eff=input$eff)
  })
  observeEvent( input$save,{
    saved      <<-cc()
    saved.title<<-paste0(input$year," District Race Results",rep(paste0("\n",input$eff*100,"% efficiency"),input$eff!=0))
    saved.plot.title<<-c(paste(input$year,"Average District Support for",names(cc())[3]),
                         paste(input$year,"Average District Support for",names(cc())[4]))
    })
  output$ternaryPlot <- renderPlot({
    main<-paste0(input$year," District Race Results",rep(paste0("\n",input$eff*100,"% efficiency"),input$eff!=0))
## ternary 2 butchered ####
#    ternary2<-function(cc,main="",legend=FALSE)
      #  x<-c(1,0,0);y<-c(0,1,0);z<-c(0,0,1)
      iro <- hcl(15 + 90 * 1:4, alpha = 0.2)
#      pch<-c(0,1,5,7,10,9)
#      cex<-c(0.9,1.2,1.0,1.0,1.3,1.0)
#      pch<-rep(19,6)
#      cex<-rep(1,6)
      # i=up; j=down and left; k = down and right

      a<-0.9
      i<-c(0,sqrt(3)/3)*a;j<-c(-0.5,-sqrt(3)/6)*a;k<-c(0.5,-sqrt(3)/6)*a
      mid<-c(0.5,sqrt(3)/6)
      abc<-matrix(data=c(mid + i,mid + j,mid + k),nrow=3,ncol=2,byrow=TRUE)
      def<-matrix(data=c(mid + (j + k)/2,mid + (i + j)/2,mid + (i + k)/2),nrow=3,ncol=2,byrow=TRUE)
      
      long.def<-matrix(data=NA,nrow=3,ncol=2)
      long.def[1,]<-def[1,]+(j+k)*0.05
      long.def[2,]<-def[2,]+(i+j)*0.05
      long.def[3,]<-def[3,]+(i+k)*0.05

      plot.new()
      text(0.5,1,labels=main,cex=1.5,col="black",font=2)

      lines(abc[c(1:3,1),],col="black")
      segments(x0=rep(mid[1],3),y0=rep(mid[2],3),
               x1=long.def[,1],y1=long.def[,2],
               col="grey")
      lines(def[c(1:3,1),],col="grey")
      par(col="black")
      text(abc[2,1],abc[2,2],labels=names(cc() )[3],pos=1)  # Bottom Left
      text(abc[3,1],abc[3,2],labels=names(cc() )[4],pos=1)  # Bottom Right
      text(abc[1,1],abc[1,2],labels=names(cc() )[5],pos=3)  # Top Middle--least important section
      par(col="grey",cex=.75)
      text(x=def[1,1],y=def[1,2],labels="50%",pos=1)
      text(x=def[2,1],y=def[2,2],labels="50%",pos=2)
      text(x=def[3,1],y=def[3,2],labels="50%",pos=4)
      for (n in 1:(NROW(cc() )))  {
        if (cc()[n,3]==0 && cc()[n,4]==0 && cc()[n,5]==0){
          next()
        }
        # i=up; j=down and left; k = down and right
        denom<-(cc()[n,3]+cc()[n,4]+cc()[n,5])
        p<-cc()[n,5]/denom*i + cc()[n,3]/denom*j + cc()[n,4]/denom*k + mid
        if (cc()[n,3]/denom>=.5 || cc()[n,4]/denom>=.5 || cc()[n,5]/denom>=.5)  {
          temp<-c(rep(FALSE,3),max(cc()[n,3:5])==cc()[n,3:5])
          if (sum(temp)>1){if (temp[6]){temp[6]<-FALSE}else{temp[5]<-FALSE}}
          par(col=iro[1])   # PURPLE
        }else if (cc()[n,3]>cc()[n,4] && cc()[n,3]>cc()[n,5]){  
          par(col=iro[2])        # RED
        }else if (cc()[n,4]>cc()[n,5]) {
          par (col=iro[3])       # BLUE
        }else {
          par (col=iro[4])      } # GREEN
        points(p[1],p[2],cex=2,pch=19)
      }
      ## THE LEGEND ####
      par (col=iro[1],pch=19)
      points(0.02,0.9)
      text(0.06,0.9,labels="Majority",pos=4)
      par (col=iro[2])
      points(0.02,0.85)
      text(0.06,0.85,labels=paste(names(cc() )[3],"Plurality"),pos=4)
      par (col=iro[3])
      points(0.02,0.8)
      text(0.06,0.8,labels=paste(names(cc() )[4],"Plurality"),pos=4)
      par (col=iro[4])
      points(0.02,0.75)
      text(0.06,0.75,labels=paste(names(cc() )[5],"Plurality"),pos=4)
      ## tbar butchered ####
      #    tbar<-function(cc=-1,party=-1,
      #                   col=c("purple","blue","red","green"),
      #                   frame=c(1,0.35,-0.15,2,-0.2,1.7),fixed=TRUE){
      #Frame: Quadrant, Scale, xmin,xmax,ymin,ymax
      frame<-c(1,0.35,0,1,0,0.85); fixed<-TRUE
      col<-iro
      iro<-matrix(data=NA,nrow=3,ncol=3)
      iro[1,]<-col[1];iro[2,]<-iro[3,]<-col[c(2:4)]
      
      wl<-as.data.frame(matrix(nrow=3,ncol=3))
      names(wl)<-names(cc()[-1:-2])
      
      for (i in (1:3)){
        i2<-i+2
        wl[1,i]<-sum(cc()[,i2]>apply(FUN="sum",cc()[,c(-1,-2,-i2)],MARGIN=1)) # Majority Victories
        wl[2,i]<-sum(apply(FUN="all",cc()[,i2]>cc()[,c(-1,-2,-i2)],MARGIN=1)) # All Victories
      }
      for (i in (1:3)){
        wl[3,i]<-sum(wl[2,]) - sum(wl[1,-i])  # All victories for all parties - Enemy Majorities
      }
      
      # frame =c(quadrant, shrink factor, x0, x1, y0, y1)
      #  frame=c(1,0.35,-0.15,2,-0.2,1.7)
      x.space<-(frame[4]-frame[3])*frame[2]
      y.space<-(frame[6]-frame[5])*frame[2]
      #Have yet to allow placement in other quadrants
      if (frame[1]==1)  {
        x.low <-frame[4]-x.space
        y.low <-frame[6]-y.space
      }
      x.reduce<-1/3    * frame[2]
      if (fixed) { y.reduce<-1/250    * y.space } else       { y.reduce<-1/max(wl)* y.space }
      # the champ in the middle, 2nd place on the left, third place on the right
      ord<-order(wl[2,])[c(2,3,1)]
      wl<-  wl[,ord]
      iro<-iro[,ord]
      ylab<-(unlist(wl))
      for (i in 3:1){
        for (j in 1:3){
          #      wl[i,j]
          polygon(x=x.low + (j + c(-bw,-bw,bw,bw)) * x.reduce,
                  y=y.low + c(0,wl[i,j],wl[i,j],0) * y.reduce,
                  col=iro[i,j],density=c(NA,NA,30)[i])
          if (i==3)    {
            #        flip<-i==3 && j==1
            text(x=x.low + (j + c(-tp,tp,tp)[j])*x.reduce,
                 y=y.low + wl[i,j] * y.reduce, 
                 labels=paste0(wl[i,j],"*"), pos=c(2,4,4)[j],cex=.7,col="black")
          }    else{
            text(x=x.low + (j + c(tp,-tp,tp)[i])*x.reduce,
                 y=y.low + wl[i,j] * y.reduce,
                 labels=wl[i,j], pos=c(4,2,4)[i],cex=.7,col="black")
          }
        }}
      text(x=x.low+(1:3)*x.reduce,y=y.low,labels=alliance.shorten(names(wl)),pos=1,cex=.9,col="black")
      text(x=x.low+mean(1:3)*x.reduce,y=y.low,labels="\n* With Winnable Losses",pos=1,cex=.7,col="black")
      text(x=x.low+.8*x.reduce,y=y.low,labels=0,pos=2,col="black",cex=0.7)
## END TERNARY FUNCTIONS ####
  })
  output$map.plot <-renderPlot({
    # show support levels for alliance 1
    iro<-brewer.pal(9,"YlOrRd")
    d<-merge_map(d=cbind(unique(cc()[,2]),
                         tapply(cc()[,3+input$map12%%2],cc()[,2],FUN="mean")),
                 "pref",col=iro,fixedPR=TRUE)
    #    start<-min(d[,2])
    #    gap<-(range(d[,2])[2]-range(d[,2])[1])/(ncolors)
    main<-paste(input$year,"Average District Support for",names(cc())[3+input$map12%%2])
    scale<-c("00%<=support<10%","10%<=support<20%","20%<=support<25%","25%<=support<30%","30%<=support<35%","35%<=support<40%","40%<=support<45%","45%<=support<50%","50%<=support")
    plot(japgeo,col=d[,2],main=main)
    text(121,46:38,pos=4,labels=scale)
    points(rep(120,9),46:38,pch=22,bg=iro,col="black",cex=1.5)
  })
  observeEvent(input$save,{
   output$ternaryPlot.saved <- renderPlot({
    ## ternary.enter2 butchered ####
#    cc<-saved
    main<-saved.title
    ## ternary 2 butchered ####
    #    ternary2<-function(cc,main="",legend=FALSE)
    #  x<-c(1,0,0);y<-c(0,1,0);z<-c(0,0,1)
    iro <- hcl (h = 15 + 90 * c(0:4), alpha = 0.2 )
    pch<-rep(19,6)
    cex<-rep(1,6)
#    pch<-c(0,1,5,7,10,9)
#    cex<-c(0.9,1.2,1.0,1.0,1.3,1.0)
    # i=up; j=down and left; k = down and right
    a<-0.9
    i<-c(0,sqrt(3)/3)*a;j<-c(-0.5,-sqrt(3)/6)*a;k<-c(0.5,-sqrt(3)/6)*a
    mid<-c(0.5,sqrt(3)/6)
    abc<-matrix(data=c(mid + i,mid + j,mid + k),nrow=3,ncol=2,byrow=TRUE)
    def<-matrix(data=c(mid + (j + k)/2,mid + (i + j)/2,mid + (i + k)/2),nrow=3,ncol=2,byrow=TRUE)
    
    long.def<-matrix(data=NA,nrow=3,ncol=2)
    long.def[1,]<-def[1,]+(j+k)*0.05
    long.def[2,]<-def[2,]+(i+j)*0.05
    long.def[3,]<-def[3,]+(i+k)*0.05
    
    plot.new()
    text(0.5,1,labels=main,cex=1.5,col="black",font=2)
    
    lines(abc[c(1:3,1),],col="black")
    segments(x0=rep(mid[1],3),y0=rep(mid[2],3),
             x1=long.def[,1],y1=long.def[,2],
             col="grey")
    lines(def[c(1:3,1),],col="grey")
    par(col="black")
    text(abc[2,1],abc[2,2],labels=names(saved)[3],pos=1)  # Bottom Left
    text(abc[3,1],abc[3,2],labels=names(saved)[4],pos=1)  # Bottom Right
    text(abc[1,1],abc[1,2],labels=names(saved)[5],pos=3)  # Top Middle--least important section
    par(col="grey",cex=.75)
    text(x=def[1,1],y=def[1,2],labels="50%",pos=1)
    text(x=def[2,1],y=def[2,2],labels="50%",pos=2)
    text(x=def[3,1],y=def[3,2],labels="50%",pos=4)
    for (n in 1:(NROW(saved)))  {
      if (saved[n,3]==0 && saved[n,4]==0 && saved[n,5]==0){
        next()
      }
      # i=up; j=down and left; k = down and right
      denom<-(saved[n,3]+saved[n,4]+saved[n,5])
      p<-saved[n,5]/denom*i + saved[n,3]/denom*j + saved[n,4]/denom*k + mid
      if (saved[n,3]/denom>=.5 || saved[n,4]/denom>=.5 || saved[n,5]/denom>=.5)  {
        temp<-c(rep(FALSE,3),max(saved[n,3:5])==saved[n,3:5])
        if (sum(temp)>1){if (temp[6]){temp[6]<-FALSE}else{temp[5]<-FALSE}}
        par(col=iro[1])   # PURPLE
      }else if (saved[n,3]>saved[n,4] && saved[n,3]>saved[n,5]){  
        par(col=iro[2])        # RED
      }else if (saved[n,4]>saved[n,5]) {
        par (col=iro[3])       # BLUE
      }else {
        par (col=iro[4])      } # GREEN
      points(p[1],p[2],pch=19,cex=2)
    }
    ## THE LEGEND ####
    par (col=iro[1],pch=19)
    points(0.02,0.9)
    text(0.06,0.9,labels="Majority",pos=4)
    par (col=iro[2])
    points(0.02,0.85)
    text(0.06,0.85,labels=paste(names(saved)[3],"Plurality"),pos=4)
    par (col=iro[3])
    points(0.02,0.8)
    text(0.06,0.8,labels=paste(names(saved)[4],"Plurality"),pos=4)
    par (col=iro[4])
    points(0.02,0.75)
    text(0.06,0.75,labels=paste(names(saved)[5],"Plurality"),pos=4)
    
    ## tbar butchered ####
    #    tbar<-function(cc=-1,party=-1,
    #                   col=c("purple","blue","red","green"),
    #                   frame=c(1,0.35,-0.15,2,-0.2,1.7),fixed=TRUE){
    frame<-c(1,0.35,0,1,0,0.85); fixed<-TRUE
    col<-iro
    iro<-matrix(data=NA,nrow=3,ncol=3)
    iro[1,]<-col[1];iro[2,]<-iro[3,]<-col[c(2:4)]
    wl<-as.data.frame(matrix(nrow=3,ncol=3))
    names(wl)<-names(saved)[-1:-2]
    for (i in (1:3)){
      i2<-i+2
      wl[1,i]<-sum(saved[,i2]>apply(FUN="sum",saved[,c(-1,-2,-i2)],MARGIN=1)) # Majority Victories
      wl[2,i]<-sum(apply(FUN="all",saved[,i2]>saved[,c(-1,-2,-i2)],MARGIN=1)) # All Victories
    }
    for (i in (1:3)){
      wl[3,i]<-sum(wl[2,]) - sum(wl[1,-i])  # All victories for all parties - Enemy Majorities
    }
    
    # frame =c(quadrant, shrink factor, x0, x1, y0, y1)
    #  frame=c(1,0.35,-0.15,2,-0.2,1.7)
    x.space<-(frame[4]-frame[3])*frame[2]
    y.space<-(frame[6]-frame[5])*frame[2]
    #Have yet to allow placement in other quadrants
    if (frame[1]==1)  {
      x.low <-frame[4]-x.space
      y.low <-frame[6]-y.space
    }
    x.reduce<-1/3    * frame[2]
    if (fixed) { y.reduce<-1/250    * y.space } else       { y.reduce<-1/max(wl)* y.space }
    # the champ in the middle, 2nd place on the left, third place on the right
    ord<-order(wl[2,])[c(2,3,1)]
    wl<-  wl[,ord]
    iro<-iro[,ord]
    ylab<-(unlist(wl))
    for (i in 3:1){
      for (j in 1:3){
        #      wl[i,j]
        polygon(x=x.low + (j + c(-bw,-bw,bw,bw)) * x.reduce,
                y=y.low + c(0,wl[i,j],wl[i,j],0) * y.reduce,
                col=iro[i,j],density=c(NA,NA,30)[i])
        if (i==3)    {
          #        flip<-i==3 && j==1
          text(x=x.low + (j + c(-tp,tp,tp)[j])*x.reduce,
               y=y.low + wl[i,j] * y.reduce, 
               labels=paste0(wl[i,j],"*"), pos=c(2,4,4)[j],cex=.7,col="black")
        }    else{
          text(x=x.low + (j + c(tp,-tp,tp)[i])*x.reduce,
               y=y.low + wl[i,j] * y.reduce,
               labels=wl[i,j], pos=c(4,2,4)[i],cex=.7,col="black")
        }
      }}
    text(x=x.low+(1:3)*x.reduce,y=y.low,labels=alliance.shorten(names(wl)),pos=1,cex=.9,col="black")
    text(x=x.low+mean(1:3)*x.reduce,y=y.low,labels="\n* With Winnable Losses",pos=1,cex=.7,col="black")
    text(x=x.low+.8*x.reduce,y=y.low,labels=0,pos=2,col="black",cex=0.7)
#    last.cc<<-cc
    ## END TERNARY FUNCTIONS ####
    })
   output$map.plot.saved <-renderPlot({
     iro<-brewer.pal(9,"YlOrRd")
     d<-merge_map(d=cbind(unique(saved[,2]),
                          tapply(saved[,3+input$map12%%2],saved[,2],FUN="mean")),
                  "pref",col=iro,fixedPR=TRUE)
     #    start<-min(d[,2])
     #    gap<-(range(d[,2])[2]-range(d[,2])[1])/(ncolors)
     scale<-c("00%<=support<10%","10%<=support<20%","20%<=support<25%","25%<=support<30%","30%<=support<35%","35%<=support<40%","40%<=support<45%","45%<=support<50%","50%<=support")
     plot(japgeo,col=d[,2],main=saved.plot.title[1+input$map12%%2])
     text(121,46:38,pos=4,labels=scale)
     points(rep(120,9),46:38,pch=22,bg=iro,col="black",cex=1.5)
          #  abline(h=24:46); abline(v=113:156)
   })
  })
})


