#########################################################
#PARAMETERS
#########################################################
path<-"/home/myname/images"  #your image directory, something like "C:/Users/myname/images" for windows or "/home/myname/images" for Ubuntu. Use / or \\ , never \
pathm<-"/home/myname/images/Mosaic.txt"  #the file with the coordinates of your images with colums "Image", "x", "y"

extensions=c("jpg", "tif", "tiff") #do not use svg
pxum=676.4536  #scale in pixel/micron. This is an example, write the scale of your images
pixelsize=1000*(1/pxum) #pixelsize in nm

#change here if you want to specify a windowsize and displacement. If left unchanged, the tool will calculate the optimum parameters based on the scale introduced in pxum
#use "recommended" if you want a recommended windowsize and/or displacement
windowsize="recommended" 
pxdisplacement= "recommended"

FindCellLimits=T #TRUE if you want the algorithm to find PM and LD
LDexpected=T #TRUE if you expect the images to contain a big LD, like in adipocytes
deleteENVwhenfinished = F  #leave this to T or TRUE unless you want to do troubleshooting. It may induce errors when set to F for a set of several images
#########################################################

t1<-(Sys.time())
par(cex=0.7, mai=c(0.5,0.5,0.5,0.5))
par(mfrow=c(1,1))
size=windowsize

#########################################################
###this is to calculate recommended windowsize and displacement
#########################################################
divisors <- function(x){
  y <- seq_len(x)
  y[ x%%y == 0 ]
  #function by Simon O'Hanlon in StackOverflow https://stackoverflow.com/questions/19465720/writing-a-function-to-calculate-divisors-in-r
}

if (is.numeric(windowsize)==FALSE) { #using the recommended windowsize
  if (windowsize !="recommended") {
    print("if you want to change windowsize, make sure you introduce an unquoted number")
  }
  if(is.numeric(pxdisplacement)==FALSE) { #using the recommended pxdisplacement
    if (windowsize !="recommended") {
      print("if you want to change pxdisplacement, make sure you introduce an unquoted number")
    }
    size100<-round((100/1000)*pxum)
    windowsize=ceiling(size100/7)*7
    print(paste("using recommended windowsize ", windowsize, sep=""))
    pxdisplacement=windowsize/7
    print(paste("using recommended displacement ", pxdisplacement, sep=""))
  } else { #using specified pxdisplacement
    size100<-round((100/1000)*pxum)
    windowsize=ceiling(size100/7)*7
    windowsize=round(windowsize/pxdisplacement)*pxdisplacement #finds the closer number to the recommended windowsize, that is also a multiple of specified displacement
    print(paste("using recommended windowsize ", windowsize, sep=""))
    print(paste("using specified pxdisplacement ", pxdisplacement, sep=""))
  }
  #print(paste(windowsize, " recommended windowsize and ", pxdisplacement, " recommended displacement", sep=""))
} else { #using specified windowsize
  print(paste("using specified windowsize ", windowsize, sep=""))
  if(is.numeric(pxdisplacement)==FALSE) { #using recommended pxdisplacement
    pxdisplacement=ceiling(windowsize/7)
    pxdisoptions<-divisors(windowsize)
    if(length(pxdisoptions) <3 ) {
      print("better not choose a prime number as windowsize")
    }
    thedifference=9999999999999999999999999999999
    for (div in pxdisoptions) { #we find the divisor of windowsize that is closer to recommended pxdisplacement
      tdif<- abs(div-pxdisplacement)
      if (tdif<thedifference) {
        thedifference = tdif
        newpxdisplacement=div
      }
    }
    pxdisplacement=newpxdisplacement
    print(paste("using recommended displacement ", pxdisplacement, sep=""))
  } else { #using specified pxdisplacement
    if ((round(windowsize/pxdisplacement, digits=0) == windowsize/pxdisplacement) == FALSE) {
      print("ERROR: specified windowsize must be a multiple of displacement")
    } else {
      print(paste("using specified displacement ", pxdisplacement, sep=""))
    }
  }
}
#########################################################


#FUNCTIONS
####################################################
library(plyr); library(dplyr)

importfiles<-function(directory, ending, ext=".txt"){
  #this function imports all files ending in ext and merges them in a single dataframe
  fileList <- dir(directory, recursive=TRUE, pattern = paste("\\" , ending , ext, "$", sep=""))
  if (length(fileList)>0) {
    files <- paste(directory, fileList, sep=.Platform$file.sep)
    myfiles <- lapply(files, read.delim, sep=" ") #, check.names = FALSE)
    dsizev <- vector()
    for(i in 1:(length(myfiles))){
      dsize<-nrow(myfiles[[i]])
      dsizev[i]<-dsize
    }
    dsizev2<-dsizev[dsizev>0]
    if (length(dsizev)>length(dsizev2)){ #if there are empty elements on the list
      target_list <- vector(mode = "list")#, length = desired_length)
      fileList2<-fileList
      for(i in 1:(length(myfiles))){
        lsize<-nrow(myfiles[[i]])
        if (lsize>0){
          target_list<-c(target_list, list(myfiles[[i]]))
        } else {
          fileList2[i]<-NA
        }
      }
      myfiles<-target_list
      fileList2 <- fileList2[!is.na(fileList2)]
      fileList<-fileList2
    }
    fileListnodot<-gsub(".txt","",fileList)
    myIDdfiles <- mapply(cbind, myfiles, "SampleID"=fileListnodot, SIMPLIFY=F)
    library(plyr)
    data<-do.call("rbind.fill",myIDdfiles)
    return(data)
  } else {
    data<-data.frame(NoDataFound=character())
    return(data)
  }
}

addtilecoords<-function(df, dir="~/Desktop/mosaic.txt"){
  #this function transforms tile coordinates (from 0 to 1024) to stitch coordinates
  #1: load file with mosaic coords, coords where every tile is located
  mosaicoord<-read.delim(dir)
  #2: coordinates in mosaicJ may not start at zero. We obtain new coordinates, tilex and tiley that do start at zero
  minx<-min(mosaicoord$x)
  miny<-min(mosaicoord$y)
  mosaicoord$tilex<-mosaicoord$x-min(mosaicoord$x)
  mosaicoord$tiley<-mosaicoord$y-min(mosaicoord$y)
  mosaicoord$Image<-as.character(mosaicoord$Image)
  #3: merge and add
  mosaicoord2<-mosaicoord[, c(1,4:5)]
  colnames(mosaicoord2)<-c("imageID", "tilex", "tiley")
  df<-(merge(df, mosaicoord2, by="imageID"))
  df$stitX<-df$xcoord+ df$tilex
  df$stitY<-df$ycoord+ df$tiley
  return(df)
}

rmoverlap<-function(dataf, sizerow=1024, sizecol=1024, checknearby=FALSE, hownearby=windowsize){
  #this function removes coordinates from a tile if they overlap with a previous tile (or nearby the overlap region)
  imlabels<-as.factor(dataf$imageID)
  levlabels<-levels(imlabels)
  outdf<-dataf[0,]
  donedf<-data.frame(tilex=rep(NA, length(levlabels)), tiley=rep(NA, length(levlabels)),
                     tilex2=rep(NA, length(levlabels)), tiley2=rep(NA, length(levlabels)))
  for (i in 1:length(levlabels)) {
    atile<-subset(dataf, imageID==levlabels[i])
    newtilex<-unique(atile$tilex); newtiley<-unique(atile$tiley)
    newtilex2<-unique(atile$tilex)+sizecol; newtiley2<-unique(atile$tiley)+sizerow
    if (i==1) {
      outdf<-rbind(outdf, atile)
      donedf$tilex[i]<-newtilex; donedf$tiley[i]<-newtiley
      donedf$tilex2[i]<-newtilex2; donedf$tiley2[i]<-newtiley2
    }
    if (i>1) {
      filled<-donedf[complete.cases(donedf), ]
      for(z in 1:nrow(filled)) {
        atileIN<-subset(atile, stitX>donedf$tilex[z] & stitX<donedf$tilex2[z] & stitY>donedf$tiley[z] & stitY<donedf$tiley2[z]) #these are present in the two tiles
        atileOUT<-subset(atile, stitX<donedf$tilex[z] | stitX>donedf$tilex2[z] | stitY<donedf$tiley[z] | stitY>donedf$tiley2[z]) #these are out of the overlap region
        atile<-atileOUT
        if(checknearby==TRUE) {
          if(nrow(atileIN)>0) {
            toinclude<-outdf[0,]
            for (j in 1:nrow(atileIN)) {
              inx<-atileIN$stitX[j]; iny<-atileIN$stitY[j]
              closeby<-subset(outdf, stitX>inx-hownearby & stitX<inx+hownearby & stitY>iny-hownearby & stitY<iny+hownearby) 
              if(nrow(closeby)==0) { #if there is nothing nearby
                toinclude<-rbind(toinclude, atileIN[j,])
              }
            }
            atile<-rbind(atile, toinclude)
          }
        }
      }
      outdf<-rbind(outdf, atile)
      donedf$tilex[i]<-newtilex; donedf$tiley[i]<-newtiley
      donedf$tilex2[i]<-newtilex2; donedf$tiley2[i]<-newtiley2
    }
  }
  return(outdf)
}


proposresidues<-function(ya, xa, yb, xb){
  #this function considers the segment as a regression line and the cytoplasmic coordinates as observations. It calculates the residues at both sides of the segment to see if the segment is surounded by cytopasmic coordinates
  gravx<-mean(c(xa,xb))
  gravy<-mean(c(ya,yb))
  lsg<-sqrt((xa-xb)^2+(ya-yb)^2)
  lsg<-round(lsg*0.5)
  difx<-abs(xa-xb);   dify<-abs(ya-yb)
  maxofx<-max(xa, xb); maxofy<-max(ya, yb); minofx<-min(xa, xb); minofy<-min(ya, yb)
  if (difx > dify) {
    env<-subset(predictions5,stitX<maxofx+lsg & stitX>minofx-lsg & stitY<gravy+nfila & stitY>gravy-nfila)
    if (nrow(env)==0) {
      env<-subset(predictions5,stitX<maxofx+2*lsg & stitX>minofx-2*lsg & stitY<gravy+nfila & stitY>gravy-nfila)
    }
  }
  if (dify > difx) {
    env<-subset(predictions5, stitX<gravx+ncolu & stitX>gravx-ncolu & stitY<maxofy+lsg & stitY>minofy-lsg)
    if (nrow(env)==0) {
      env<-subset(predictions5, stitX<gravx+ncolu & stitX>gravx-ncolu & stitY<maxofy+2*lsg & stitY>minofy-2*lsg)
    }
  }
  if (dify == difx) {
    env<-subset(predictions5, stitX<gravx+ncolu*0.3 & stitX>gravx-ncolu*0.3 & stitY<gravy+nfila*0.3 & stitY>gravy-nfila*0.3)
  }
  if (xa != xb) {
    #calculate line and residues
    slope<-(ya-yb)/(xa-xb)
    intrc<-ya-slope*xa
    countpos<-0
    countneg<-0
    for(nr in 1:nrow(env)) {
      nrcase<-env[nr, ]
      xpunto<-nrcase$stitX; ypunto<-nrcase$stitY
      ypred<-intrc + slope*xpunto
      resid<-ypunto-ypred
      if (resid >0) {countpos = countpos+1}
      if (resid <0) {countneg = countneg+1}
    }
    propos<-countpos/(countneg+countpos)
  }
  if (xa == xb) {
    #this would be a perfectly vertical residue
    countpos<-0
    countneg<-0
    for(nr in 1:nrow(env)) {
      nrcase<-env[nr, ]
      xpunto<-nrcase$stitX; 
      xpred<-xa
      resid<-xpunto-xpred
      if (resid >0) {countpos = countpos+1}
      if (resid <0) {countneg = countneg+1}
    }
    propos<-countpos/(countneg+countpos)
  }
  return(propos)
}

matchColClasses <- function(df1, df2) {
  #this function makes sure that column classess in two dataframes are the same so there are no problems when merging
  #this comes from GGAnderson in StackOverflow <https://stackoverflow.com/questions/49215193/r-error-cant-join-on-because-of-incompatible-types>
  sharedColNames <- names(df1)[names(df1) %in% names(df2)]
  sharedColTypes <- sapply(df1[,sharedColNames], class)
  for (n in sharedColNames) {
    class(df2[, n]) <- sharedColTypes[n]
  }
  return(df2)
}
####################################################


#################
#CAVEOLAE
#################
TOTALCAVDF<-importfiles(path, "CAVDFDISTANCES")
TOTALCAVDF$imageID<-as.character(TOTALCAVDF$imageID)
TOTALCAVDF<-addtilecoords(TOTALCAVDF, dir=pathm)
TOTALCAVDF<-rmoverlap(TOTALCAVDF)
write.table(TOTALCAVDF, paste(path, "/CAVDFTOTAL.txt", sep=""),sep = '\t',quote = FALSE,row.names = FALSE)
#################


#################
#PLASMA MEMBRANE
#################
if (FindCellLimits==TRUE) {
  FINALPM<-importfiles(path, "intPM")
  if (nrow(FINALPM)>0) {
    FINALPM$imageID<-FINALPM$SampleID
    FINALPM$imageID<-gsub(".*/", "",FINALPM$imageID)
    FINALPM$imageID<-gsub("tif_.*$", "tif",FINALPM$imageID)
    FINALPM$imageID<-as.character(FINALPM$imageID)
    FINALPM<-addtilecoords(FINALPM, dir=pathm) 
    write.table(FINALPM, paste(path, "/FINALPM.txt", sep=""))
  }
  if (nrow(FINALPM)==0 & ncol(FINALPM)==1)  {
    colnames(FINALPM)<-"imageID"
    FINALPM1<-FINALPM
    cl=1
    while (cl<15) {
      FINALPM<-cbind(FINALPM, FINALPM1)
      cl<-cl+1
    } #add empty columns
    colnames(FINALPM)<-c("imageID", "xcoord",  "ycoord", "cluster", "colorcodes2.y", "surfacetype", "belong",
                         "DistToLD" ,     "LDx" ,          "LDy",
                         "SampleID",  "tilex", "tiley","stitX", "stitY"  )  
  }
  
} else {
  FINALPM<-importfiles(path, "intPM")
  colnames(FINALPM)<-"imageID"
  FINALPM1<-FINALPM
  cl=1
  while (cl<15) {
    FINALPM<-cbind(FINALPM, FINALPM1)
    cl<-cl+1
  } #add empty columns
  colnames(FINALPM)<-c("imageID", "xcoord",  "ycoord", "cluster", "colorcodes2.y", "surfacetype", "belong",
                       "DistToLD" ,     "LDx" ,          "LDy",
                       "SampleID",  "tilex", "tiley","stitX", "stitY"  ) 
}
#################


#################
#LIPID DROPLET
#################
if (LDexpected ==TRUE & FindCellLimits==TRUE) {
  FINALLD<-importfiles(path, "intLD")
  if (nrow(FINALLD)>0) {
    FINALLD$imageID<-FINALLD$SampleID
    FINALLD$imageID<-gsub(".*/", "",FINALLD$imageID)
    FINALLD$imageID<-gsub("tif_.*$", "tif",FINALLD$imageID)
    FINALLD$imageID<-as.character(FINALLD$imageID)
    FINALLD<-addtilecoords(FINALLD, dir=pathm)
    write.table(FINALLD, paste(path, "/FINALLD.txt", sep=""))
  }
  if (nrow(FINALLD)==0 & ncol(FINALLD)==1)  {
    colnames(FINALLD)<-"imageID"
    FINALLD1<-FINALLD
    cl=1
    while (cl<12) {
      FINALLD<-cbind(FINALLD, FINALLD1)
      cl<-cl+1
    } #esto es un truco para a??adir m??s columnas vac??as
    colnames(FINALLD)<-c("imageID", "xcoord",  "ycoord", "cluster", "colorcodes2.y", "surfacetype", "belong",       
                          "SampleID",  "tilex", "tiley","stitX", "stitY"  )  
  }
} else {
  FINALLD<-importfiles(path, "intLD")
  colnames(FINALLD)<-"imageID"
  FINALLD1<-FINALLD
  cl=1
  while (cl<12) {
    FINALLD<-cbind(FINALLD, FINALLD1)
    cl<-cl+1
  } 
  colnames(FINALLD)<-c("imageID", "xcoord",  "ycoord", "cluster", "colorcodes2.y", "surfacetype", "belong",       
                       "SampleID",  "tilex", "tiley","stitX", "stitY"  )  
}
#################


#################
#PLOTS
#################
if (nrow(FINALPM)>0){
  minxforplot<-min(FINALPM$stitX,FINALLD$stitX); maxxforplot<-max(FINALPM$stitX,FINALLD$stitX);
  minyforplot<-min(FINALPM$stitY,FINALLD$stitY); maxyforplot<-max(FINALPM$stitY,FINALLD$stitY);
  plot(FINALPM$stitX, FINALPM$stitY, pch=16, col="black", cex=0.1, xlim=c(minxforplot, maxxforplot), ylim=c(minyforplot, maxyforplot))
  points(TOTALCAVDF$stitX, TOTALCAVDF$stitY, pch=16, col=as.character(TOTALCAVDF$colorcodes), cex=0.3)
  points(FINALLD$stitX, FINALLD$stitY, pch=16, col="orange", cex=0.1)
}
if (nrow(FINALPM)==0 & nrow(FINALLD)>0){
  minxforplot<-min(FINALPM$stitX,FINALLD$stitX); maxxforplot<-max(FINALPM$stitX,FINALLD$stitX);
  minyforplot<-min(FINALPM$stitY,FINALLD$stitY); maxyforplot<-max(FINALPM$stitY,FINALLD$stitY);
  plot(FINALLD$stitX, FINALLD$stitY, pch=16, col="black", cex=0.1, xlim=c(minxforplot, maxxforplot), ylim=c(minyforplot, maxyforplot))
  points(TOTALCAVDF$stitX, TOTALCAVDF$stitY, pch=16, col=as.character(TOTALCAVDF$colorcodes), cex=0.3)
  points(FINALPM$stitX, FINALPM$stitY, pch=16, col="orange", cex=0.1)
}
if (nrow(FINALPM)==0 & nrow(FINALLD)==0){
  plot(TOTALCAVDF$stitX, TOTALCAVDF$stitY, pch=16, col=as.character(TOTALCAVDF$colorcodes), cex=0.3)
}
#################

#################
#PREDICTIONS (cell pseudosegmentation)
#################
PREDICTIONS<-importfiles(path, "FINALPREDTHRIMAGE") 
PREDICTIONS$imageID<-PREDICTIONS$SampleID
PREDICTIONS$imageID<-gsub(".*/", "",PREDICTIONS$imageID)
PREDICTIONS$imageID<-gsub("tif_.*$", "tif",PREDICTIONS$imageID)
PREDICTIONS$imageID<-as.character(PREDICTIONS$imageID)
PREDICTIONS<-addtilecoords(PREDICTIONS, dir=pathm)
write.table(PREDICTIONS, paste(path, "/PREDICTIONS.txt", sep=""))
predictions5<-subset(PREDICTIONS, prediction==5)
#################


#################
#JOIN SURFACE SEGMENTS
#################
if(FindCellLimits==TRUE & (nrow(FINALPM)>0 | nrow(FINALLD)>0 )) {
  #################
  #cluster here means a grouping of surface coordinates that we need to join to make a continuous surface
  #a segment is the connection that we use to join them
  PMDF<-data.frame(FINALPM$stitX, FINALPM$stitY, FINALPM$imageID, FINALPM$cluster, FINALPM$surfacetype, FINALPM$tilex, FINALPM$tiley)
  LDDF<-data.frame(FINALLD$stitX, FINALLD$stitY, FINALLD$imageID, FINALLD$cluster, FINALLD$surfacetype, FINALLD$tilex, FINALLD$tiley)
  colnames(PMDF)<-c("stitX", "stitY", "imageID", "cluster", "surfacetype", "tilex", "tiley")
  colnames(LDDF)<-c("stitX", "stitY", "imageID", "cluster", "surfacetype", "tilex", "tiley")
  PMDF$clu<-paste(PMDF$imageID, PMDF$cluster, PMDF$surfacetype, sep="-")
  LDDF$clu<-paste(LDDF$imageID, LDDF$cluster, LDDF$surfacetype, sep="-")
  PLDF<-rbind(PMDF, LDDF)
  #count common characters at beginning of the image name. this is to find a shorter name for the segments
  if(length(unique(nchar(PLDF$clu)))==1) {#if all cluster numbers same length
    longcl<-unique(nchar(PLDF$clu)) #get string length
    countequal<-0
    for (ch in 1:longcl) { #this loop counts the number of initial characters that are the same in all strings
      testequal<-length(unique(substring(PLDF$clu, 1, ch)))
      if (testequal == 1) {countequal<-countequal+1} #testequal +1 would be the character from which strings change
    }
  }
  PLDF$clu<-substring(PLDF$clu, countequal) 
  PLDF<-rmoverlap(PLDF, checknearby=TRUE)
  #write.table(PLDF, paste(path, "/PLDF.txt", sep=""),sep = '\t',quote = FALSE,row.names = FALSE)
  PLDFall<-PLDF
  FINALPMNO<-subset(PLDF, surfacetype=="PM")
  FINALLDNO<-subset(PLDF, surfacetype=="LD")

  #PLDF is a df with all surfaces
  #ideally these three lines should be determined automatically, but let's leave it like this for now
  ncell<-1; nfila<-1024; ncolu<-1024
  
  #now we will join the clusters that are close to each other, by order of proximity
  #to a maximum of 2 joints per cluster
  #and exclude those that run through cytoplasmic prediction area
  
  #1: a loop to find, for each cluster, the two closest clusters and the corresponding minimum distances
  #I think we can shorten this loop further, but it works like this
  proxdfinal<-data.frame(cltile=numeric(),clprox=numeric(), distance=numeric(),xprox=numeric(), yprox=numeric(), xpoint=numeric(), ypoint=numeric())
  for (imID in levels(as.factor(PLDF$imageID))) {
    itile<-subset(PLDF, imageID == imID)
    centerX<-mean(itile$stitX)
    centerY<-mean(itile$stitY)
    entorno<- subset(PLDF, stitX>(centerX-1.2*ncolu) & stitX<(centerX+1.2*ncolu) &  stitY>(centerY-1.2*nfila) & stitY<(centerY+1.2*nfila))
    proxdf<-data.frame(cltile=numeric(),clprox=numeric(), distance=numeric(),xprox=numeric(), yprox=numeric(), xpoint=numeric(), ypoint=numeric())
    for (clt in unique(itile$clu)) { #loop for each tile cluster
      iclt<-subset(itile, clu == clt)
      theclu<-unique(iclt$clu) #same as clt
      entorno<-subset(entorno, clu !=theclu)
      for(prclt in unique(entorno$clu)) { #loop por each nearby cluster
        ientorno<-subset(entorno, clu==prclt)
        eudist<-999999999999999999999999999999999999999999999999999999999999
        for (ipt in 1:nrow(iclt)) {
          if (round(ipt/5) == ipt/5) { #only one each 5 points, or it would take too long
            for (jpt in 1:nrow(ientorno)) {
              if (round(jpt/5) == jpt/5) {
                ijdist<-sqrt((iclt$stitX[ipt]-ientorno$stitX[jpt])^2 + (iclt$stitY[ipt]-ientorno$stitY[jpt])^2)
                if(ijdist < eudist) {
                  eudist <- ijdist
                  cltile<-iclt$clu[ipt]; clprox<-ientorno$clu[jpt]
                  distance<-eudist
                  xprox<-ientorno$stitX[jpt]; yprox<-ientorno$stitY[jpt]
                  ypoint<-iclt$stitY[ipt]; xpoint<-iclt$stitX[ipt]
                }
              }
            }
          }
        }
        proxv<-data.frame(cltile, clprox, distance, xprox, yprox, ypoint, xpoint)
        proxdf<-rbind(proxdf, proxv)
      }
    } #end loop for every tile cluster
    #we now have a table with the minimum distance for each combination of tile cluster and nearby cluster, proxdf
    #we get the two minimum distance for each  cluster
    proxdf2<-data.frame(cltile=numeric(),clprox=numeric(), distance=numeric(),xprox=numeric(), yprox=numeric(), xpoint=numeric(), ypoint=numeric())
    for (eachcl in unique(proxdf$cltile)) {
      clproxdf<-subset(proxdf, cltile == eachcl)
      sortclproxdf<-clproxdf[order(clproxdf$distance),]
      sortclproxdf<-sortclproxdf[1:2, ]
      proxdf2<-rbind(proxdf2, sortclproxdf)
    }
    proxdfinal<-rbind(proxdfinal, proxdf2)
  }
  #write.table(proxdfinal, paste(path, "/proxdfinal.txt", sep=""),sep = '\t',quote = FALSE,row.names = FALSE)
  
  #we order the list of paired clusters by increasing distance
  sortproxdf<-proxdfinal[order(proxdfinal$distance),]
  #we remove combinations of clusters that are equivalent
  #for example, clusters a-b and clusters b-a
  trimdf<-sortproxdf[0,]
  for (rr in 1:nrow(sortproxdf)) {
    rdf<-sortproxdf[rr, ]
    rcltile<-rdf$cltile
    rclprox<-rdf$clprox
    testrim<-subset(trimdf, cltile == rclprox & clprox == rcltile)
    if (nrow(testrim)==0){
      trimdf<-rbind(trimdf, rdf)
    }
  }
  sortproxdf<-trimdf
  alldf<-sortproxdf
  alldf<-alldf[complete.cases(alldf), ]
  
  #2: We select pairs of clusters with increasing distance, limiting to 2 max. interactions
  #we create a table to count each time a clusters is paired
  resultdf<-alldf[0,]
  cachitos<-unique(c(as.character(alldf$cltile), as.character(alldf$clprox)))
  count<-rep(0, length(cachitos))
  countable<-data.frame(cachitos, count)

  #this loop gets each clusters pair, checks if any member of the pair has been used before
  #and if both did not reach the maximum number of usages, it passes the pair to a new df, resultdf
  #it also rejects pairs if the clusters joining them crossess through cytoplasmic predictions based on the proportion of negative and positive residuals
  for (r in 1:nrow(alldf)) {
    cl1<-alldf$cltile[r]; cl2<-alldf$clprox[r]
    c1<-subset(countable, cachitos ==cl1)
    c2<-subset(countable, cachitos ==cl2)
    count1<-c1$count; count2<-c2$count
    if (count1<2 & count2<2) {
      if (count1 ==1){ #if there is one, check that the point is not too close
        sametile<-subset(resultdf, cltile==cl1)
        if (nrow(sametile)>0) {
          sametilex<-sametile$xpoint
          sametiley<-sametile$ypoint
          distosametile<-sqrt((alldf$xpoint[r]-sametilex)^2+(alldf$ypoint[r]-sametiley)^2)
          distosameprox<-9999999999999999999999999999
        }
        if (nrow(sametile)==0) {
          sameprox<-subset(resultdf, clprox==cl1)
          sameproxx<-sameprox$xprox
          sameproxy<-sameprox$yprox
          distosameprox<-sqrt((alldf$xpoint[r]-sameproxx)^2+(alldf$ypoint[r]-sameproxy)^2)
          distosametile<-9999999999999999999999999999
        }
      }
      if (count2 ==1){
        sametile<-subset(resultdf, cltile==cl2)
        if (nrow(sametile)>0) {
          sametilex<-sametile$xpoint
          sametiley<-sametile$ypoint
          distosametile<-sqrt((alldf$xprox[r]-sametilex)^2+(alldf$yprox[r]-sametiley)^2)
          distosameprox<-9999999999999999999999999999
        }
        if (nrow(sametile)==0) {
          sameprox<-subset(resultdf, clprox==cl2)
          sameproxx<-sameprox$xprox
          sameproxy<-sameprox$yprox
          distosameprox<-sqrt((alldf$xprox[r]-sameproxx)^2+(alldf$yprox[r]-sameproxy)^2)
          distosametile<-9999999999999999999999999999
        }
      }
      if (count1==0 & count2==0) {
        distosameprox<-9999999999999999999999999999
        distosametile<-9999999999999999999999999999
      }
      ya<-alldf$yprox[r];     xa<-alldf$xprox[r]
      yb<-alldf$ypoint[r];    xb<-alldf$xpoint[r]
      pr<-proposresidues(ya, xa, yb, xb)
      if ((pr>0.60 | pr<0.40) & (distosametile > ncolu/20 & distosameprox > ncolu/20)){  #test different pr here if needed
        resultdf<-rbind(resultdf, alldf[r , ])
        countable[countable$cachitos == cl1, 2] <- count1+1
        countable[countable$cachitos == cl2, 2] <- count2+1
      }
    }
  }
  
  plot(FINALPMNO$stitX, FINALPMNO$stitY, pch=16, col="black", cex=0.1, xlim=c(minxforplot, maxxforplot), ylim=c(minyforplot, maxyforplot))
  points(predictions5$stitX, predictions5$stitY, col="yellow", pch=16, cex=0.1)
  points(FINALLDNO$stitX, FINALLDNO$stitY, pch=16, col="orange", cex=0.1)
  points(FINALPMNO$stitX, FINALPMNO$stitY, pch=16, col="black", cex=0.1)
  segments(resultdf$xpoint, resultdf$ypoint, x1 = resultdf$xprox, y1 = resultdf$yprox, col = "green", lwd=4)
  
  #3: we get the remaining segments and recalculate the minimum distances between them
  remaining<-subset(countable, count <2)
  remaining<-droplevels(remaining)
  proxdfinal2<-data.frame(cltile=numeric(),clprox=numeric(), distance=numeric(),xprox=numeric(), yprox=numeric(), xpoint=numeric(), ypoint=numeric())
  for (imID in levels(as.factor(remaining$cachitos))) {
    itile<-subset(PLDF, clu == imID)
    centerX<-mean(itile$stitX)
    centerY<-mean(itile$stitY)
    entorno<- subset(PLDF, stitX>(centerX-1.2*ncolu) & stitX<(centerX+1.2*ncolu) &  stitY>(centerY-1.2*nfila) & stitY<(centerY+1.2*nfila))
    entorno<-subset(entorno, clu %in% remaining$cachitos)
    proxdf<-data.frame(cltile=numeric(),clprox=numeric(), distance=numeric(),xprox=numeric(), yprox=numeric(), xpoint=numeric(), ypoint=numeric())
    iclt<-itile  
    theclu<-unique(iclt$clu)
    entorno<-subset(entorno, clu !=theclu)
    for(prclt in unique(entorno$clu)) { #loop for each environment cluster
      ientorno<-subset(entorno, clu==prclt)
      if (nrow(ientorno)==0) {print("nothing nearby")}
      eudist<-999999999999999999999999999999999999999999999999999999999999
      for (ipt in 1:nrow(iclt)) {
        if (round(ipt/5) == ipt/5) { #one each 5 or it'll take too long
          for (jpt in 1:nrow(ientorno)) {
            if (round(jpt/5) == jpt/5) {
              ijdist<-sqrt((iclt$stitX[ipt]-ientorno$stitX[jpt])^2 + (iclt$stitY[ipt]-ientorno$stitY[jpt])^2)
              if(ijdist < eudist) {
                eudist <- ijdist
                cltile<-iclt$clu[ipt]
                clprox<-ientorno$clu[jpt]
                distance<-eudist
                xprox<-ientorno$stitX[jpt]
                yprox<-ientorno$stitY[jpt]
                ypoint<-iclt$stitY[ipt]
                xpoint<-iclt$stitX[ipt]
              }
            } 
          }
        }
      }
      proxv<-data.frame(cltile, clprox, distance, xprox, yprox, ypoint, xpoint)
      proxdf<-rbind(proxdf, proxv)
    }
    proxdf2<-data.frame(cltile=numeric(),clprox=numeric(), distance=numeric(),xprox=numeric(), yprox=numeric(), xpoint=numeric(), ypoint=numeric())
    for (eachcl in unique(proxdf$cltile)) {
      clproxdf<-subset(proxdf, cltile == eachcl)
      sortclproxdf<-clproxdf[order(clproxdf$distance),]
      sortclproxdf<-sortclproxdf[1:2, ]
      proxdf2<-rbind(proxdf2, sortclproxdf)
    }
    proxdfinal2<-rbind(proxdfinal2, proxdf2)
  }
  #Some NAs appear because some segments can only find one distance in their environment
  
  #we order by distances and eliminate equivalent pairs
  sortproxdf2<-proxdfinal2[order(proxdfinal2$distance),]
  trimdf<-sortproxdf2[0,]
  for (rr in 1:nrow(sortproxdf2)) {
    rdf<-sortproxdf2[rr, ]
    rcltile<-rdf$cltile
    rclprox<-rdf$clprox
    testrim<-subset(trimdf, cltile == rclprox & clprox == rcltile)
    if (nrow(testrim)==0){
      trimdf<-rbind(trimdf, rdf)
    }
  }
  sortproxdf2<-trimdf
  
  #4: We select pairs of segments with increasing distance, limiting to 2 max, taking into account the findings of previous steps
  alldf2<-sortproxdf2
  alldf2<-alldf2[complete.cases(alldf2), ]
  resultdf2<-resultdf
  countable2<-countable
  for (r in 1:nrow(alldf2)) {
    cl1<-alldf2$cltile[r]; cl2<-alldf2$clprox[r]
    cl1<-as.character(cl1); cl2<-as.character(cl2)
    c1<-subset(countable2, cachitos ==cl1)
    c2<-subset(countable2, cachitos ==cl2)
    count1<-c1$count; count2<-c2$count
    if (count1<2 & count2<2) {
      if (count1 ==1){ 
        sametile<-subset(resultdf2, cltile==cl1)
        if (nrow(sametile)>0) {
          sametilex<-sametile$xpoint
          sametiley<-sametile$ypoint
          distosametile<-sqrt((alldf2$xpoint[r]-sametilex)^2+(alldf2$ypoint[r]-sametiley)^2)
          distosameprox<-9999999999999999999999999999
        }
        if (nrow(sametile)==0) {
          sameprox<-subset(resultdf2, clprox==cl1)
          sameproxx<-sameprox$xprox
          sameproxy<-sameprox$yprox
          distosameprox<-sqrt((alldf2$xpoint[r]-sameproxx)^2+(alldf2$ypoint[r]-sameproxy)^2)
          distosametile<-9999999999999999999999999999
        }
      }
      if (count2 ==1){
        sametile<-subset(resultdf2, cltile==cl2)
        if (nrow(sametile)>0) {
          sametilex<-sametile$xpoint
          sametiley<-sametile$ypoint
          distosametile<-sqrt((alldf2$xprox[r]-sametilex)^2+(alldf2$yprox[r]-sametiley)^2)
          distosameprox<-9999999999999999999999999999
        }
        if (nrow(sametile)==0) {
          sameprox<-subset(resultdf2, clprox==cl2)
          sameproxx<-sameprox$xprox
          sameproxy<-sameprox$yprox
          distosameprox<-sqrt((alldf2$xprox[r]-sameproxx)^2+(alldf2$yprox[r]-sameproxy)^2)
          distosametile<-9999999999999999999999999999
        }
      }
      if (count1==0 & count2==0) {
        distosameprox<-9999999999999999999999999999
        distosametile<-9999999999999999999999999999
      }
      ya<-alldf2$yprox[r];     xa<-alldf2$xprox[r]
      yb<-alldf2$ypoint[r];    xb<-alldf2$xpoint[r]
      pr<-proposresidues(ya, xa, yb, xb)
      if ((pr>0.90 | pr<0.10) & (distosametile > ncolu/20 & distosameprox > ncolu/20)){ #test pr here if necessary. This second round, because distances are larger, we are more restrictive just in case
        resultdf2<-rbind(resultdf2, alldf2[r , ])
        countable2[countable2$cachitos == cl1, 2] <- count1+1
        countable2[countable2$cachitos == cl2, 2] <- count2+1
      }
    }
  }
  
  resultdf3<-resultdf2[(nrow(resultdf)+1):nrow(resultdf2),]
  #this has only the new ones. resultdf2 has all segments
  par(cex=0.8, mai=c(0.7,0.7,0.7,0.7))
  plot(FINALPMNO$stitX, FINALPMNO$stitY, pch=16, col="black", cex=0.1, xlim=c(minxforplot, maxxforplot), ylim=c(minyforplot, maxyforplot))
  points(FINALLDNO$stitX, FINALLDNO$stitY, pch=16, col="orange", cex=0.1)
  segments(resultdf$xpoint, resultdf$ypoint, x1 = resultdf$xprox, y1 = resultdf$yprox, col = "green", lwd=4)
  segments(resultdf3$xpoint, resultdf3$ypoint, x1 = resultdf3$xprox, y1 = resultdf3$yprox, col = "red", lwd=4)
  points(TOTALCAVDF$stitX, TOTALCAVDF$stitY, pch=16, col=as.character(TOTALCAVDF$colorcodes), cex=0.3)
  #################
}
#################


#################
#EXCLUDE POSSIBLE BORDERS (erroneous segments that connect clusters in same tile)
#################
if (LDexpected ==TRUE & FindCellLimits==TRUE & (nrow(FINALLD)>0) & nrow(FINALPM)>0) {
  
    #################
    #we first get segments joined within same imageID
    pairsg<-resultdf2
    pairsg$imageIDtile<-substr(pairsg$cltile, 1, 3)   
    pairsg$imageIDprox
    for (ext in 1:length(extensions)) {
      if (unique(nchar(as.character(pairsg$cltile))) > unique(nchar(gsub(paste(".*.", extensions[ext], sep=""), "", pairsg$cltile)))) {
        pairsg$imageIDtile<-gsub(paste("\\.", extensions[ext], ".*", sep=""), "", pairsg$cltile)
      } #this loop gets the image name without cluster o surfacetype by taking a substring at the extension
    }
    for (ext in 1:length(extensions)) {
      if (unique(nchar(as.character(pairsg$clprox))) > unique(nchar(gsub(paste(".*.", extensions[ext], sep=""), "", pairsg$clprox)))) {
        pairsg$imageIDprox<-gsub(paste("\\.", extensions[ext], ".*", sep=""), "", pairsg$clprox)
      } #this loop gets the image name without cluster o surfacetype by taking a substring at the extension
    }
    pairsg$sameimg<-(pairsg$imageIDtile==pairsg$imageIDprox)
    pairsgwithin<-subset(pairsg, sameimg ==TRUE)
    
    if(nrow(pairsgwithin)>0){
      #now we check if in these images there is PM and LD
      pairsgwithin$typesintile<-rep(NA, nrow(pairsgwithin))
      for (eachsg in 1:nrow(pairsgwithin)) {
        segmentID<-pairsgwithin$cltile[eachsg]
        subPLDF<-subset(PLDF, clu==segmentID)
        eachimageID<-unique(subPLDF$imageID)
        subimage<-subset(PLDF, imageID==eachimageID)
        nstypes<-length(unique(subimage$surfacetype))
        #print(nstypes)
        pairsgwithin$typesintile[eachsg]<-nstypes
      }
      #and if pair of segments involves PM-LD
      pairsgwithin$typetile<-substr(pairsgwithin$cltile, nchar(as.character(pairsgwithin$cltile))-1, nchar(as.character(pairsgwithin$cltile)))
      pairsgwithin$typeprox<-substr(pairsgwithin$clprox, nchar(as.character(pairsgwithin$clprox))-1, nchar(as.character(pairsgwithin$clprox)))
      pairsgwithin$score1<-as.numeric((pairsgwithin$typesintile==2 & (pairsgwithin$typetile != pairsgwithin$typeprox)))
      #now we check if the distance of the connecting segment is >= minimum PM-LD distance, only on those that have 2 segment types in tile
      if("DistToLD" %in% colnames(FINALPM)) {#just in case this option was not checked
        pairsgwithin$mindistLD<-rep(NA, nrow(pairsgwithin))
        for (eachsg in 1:nrow(pairsgwithin)) {
          segmentID<-pairsgwithin$cltile[eachsg]
          subPLDF<-subset(PLDF, clu==segmentID)
          eachimageID<-unique(subPLDF$imageID)
          subimage<-subset(FINALPM, imageID==eachimageID)
          mindistoLD<-min(subimage$DistToLD)
          pairsgwithin$mindistLD[eachsg]<-mindistoLD
        }
      }
      pairsgwithin$score2<-as.numeric(pairsgwithin$distance >= pairsgwithin$mindistLD)
      pairsgwithin$score2[is.na(pairsgwithin$score2)] <- 0
      #score 1 awards segments connecting PM-LD that are in in tiles with two segment types
      #score 2 awards segments where the connecting segment distance is equal or bigger than minimum PM-LD distance
      #now we calculate regression lines of each segment of the pair, and of the combination of both
      #and we calculate several parameters to estimate if both segments are more or less parallel
      pairsgwithin$score3<-rep(NA, nrow(pairsgwithin))
      for (eachsg in 1:nrow(pairsgwithin)) {
        countscore=0
        asegment<-subset(PLDF, clu==as.character(pairsgwithin$cltile[eachsg]))
        lma<-lm(asegment$stitY~asegment$stitX)
        inta<-coefficients(lma)[[1]]
        
        bsegment<-subset(PLDF, clu==as.character(pairsgwithin$clprox[eachsg]))
        lmb<-lm(bsegment$stitY~bsegment$stitX)
        intb<-coefficients(lmb)[[1]]
        
        absegment<-subset(PLDF, clu==as.character(pairsgwithin$cltile[eachsg]) | clu==as.character(pairsgwithin$clprox[eachsg]))
        lmab<-lm(absegment$stitY~absegment$stitX)
        intab<-coefficients(lmab)[[1]]
        #is rab intermediate to ra or rb, instead of using intercept, use value in x=minx of the segments
        minxsegments<-min(absegment$stitX)
        inta2<-coefficients(lma)[[1]]+coefficients(lma)[[2]]*minxsegments
        intb2<-coefficients(lmb)[[1]]+coefficients(lmb)[[2]]*minxsegments
        intab2<-coefficients(lmab)[[1]]+coefficients(lmab)[[2]]*minxsegments
        isinterm<-((intab2 < inta2 & intab2 > intb2) | (intab2 > inta2 & intab2 < intb2))
        if(isinterm==TRUE){
          countscore<-countscore+1
        }
        #find if connecting segment crosses rab
        slpab<-coefficients(lmab)[[2]]
        sya<-pairsgwithin$ypoint[eachsg]
        syb<-pairsgwithin$yprox[eachsg]
        sxa<-pairsgwithin$xpoint[eachsg]
        sxb<-pairsgwithin$xprox[eachsg]
        segments(sxa, sya, sxb, syb, col="red")
        if(sxa==sxb){
          predd<-intab+slpab*sxa
          crossess<-((predd < sya & predd >syb) | (predd > sya & predd < syb))
          if(crossess==TRUE){
            countscore<-countscore+1
          }
        }
        if (sxa!=sxb){
          slsg<-(sya-syb)/(sxa-sxb)
          insg<-sya-slsg*sxa
          xcross<-(intab-insg)/(slsg-slpab)
          ycross<-intab+slpab*xcross
          points(xcross, ycross, col="yellow", cex=0.5, pch=19)
          ifcrossx<-((round(xcross,1) <= round(sxa,1) & round(xcross,1) >=round(sxb,1)) | (round(xcross,1) >= round(sxa,1) & round(xcross,1) <= round(sxb,1)))
          ifcrossy<-((round(ycross,1)<=round(sya, 1) & round(ycross,1)>=round(syb, 1)) | (round(ycross,1)>=round(sya, 1) & round(ycross,1)<=round(syb, 1)))
          if(ifcrossx==TRUE & ifcrossy==TRUE){
            countscore<-countscore+1
          }
        }
        #we now test if ra and rb cross within the range of the segments
        slpa<-coefficients(lma)[[2]]
        slpb<-coefficients(lmb)[[2]]
        xcross2<-(inta-intb)/(slpb-slpa)
        ycross2<-inta+slpa*xcross2
        points(xcross2, ycross2, col="green", cex=0.5, pch=19)
        minysegments<-min(absegment$stitY)
        maxxsegments<-max(absegment$stitX)
        maxysegments<-max(absegment$stitY)
        if(xcross2<maxxsegments & xcross2>minxsegments & ycross2<maxysegments & ycross2>minysegments){
        } else {
          countscore<-countscore+1}
        pairsgwithin$score3[eachsg]<-countscore-1
        countscore<-0
        
        #now we check the residues of every segment to rab
        preA<-coefficients(lmab)[[1]]+coefficients(lmab)[[2]]*asegment$stitX
        resA<-preA-asegment$stitY
        preB<-coefficients(lmab)[[1]]+coefficients(lmab)[[2]]*bsegment$stitX
        resB<-preB-bsegment$stitY
        
        proposresA<-sum(resA>0)/length(resA)
        proposresB<-sum(resB>0)/length(resB)
        difprop<-max(c(proposresA, proposresB))-min(c(proposresA, proposresB))
        pairsgwithin$score3[eachsg]<-(pairsgwithin$score3[eachsg]+3*difprop)
      }
      
      pairsgwithin$scoreT1<-pairsgwithin$score1+pairsgwithin$score2+pairsgwithin$score3
      PLDFG<-PLDF
      PLDFG$grouping<-rep(NA, nrow(PLDFG))
      PLDFG$chosen<-rep(NA, nrow(PLDFG))
      PLDFG$ip<-rep(NA, nrow(PLDFG))
      PLDFG$jp<-rep(NA, nrow(PLDFG))
      PLDFG<-PLDFG[0,]
      certv <- vector()
      scoreT1.1 <- vector()
      scoreT1.2 <- vector()
      cltilev1 <- vector()
      clproxv1 <- vector()
      cltilev2 <- vector()
      clproxv2 <- vector()
      ipv<- vector()
      jpv<- vector()
      counting=0
      #calculate times that this will loop
      nf<-length(1:nrow(pairsgwithin)) ;  rf<-2
      factcalc<-factorial(nf+rf-1)/(factorial(rf)*factorial(nf-1))
      totalloops<-factcalc+1
      todivide<-ceiling(sqrt(totalloops))
      par(cex=0.6, mai=c(0.05,0.05,0.05,0.05))
      par(mfrow=c(todivide, todivide))
      for(ip in 0:nrow(pairsgwithin)){
        for(jp in ip:nrow(pairsgwithin)){
          if((ip != 0 & jp != 0) |( ip==0 & jp==0)){
            if( ip==0 & jp==0 ) {
              borderpairs<-pairsgwithin[, 1:7]
              resultdfnoborder<-resultdf2
              remainingpairs<-borderpairs
            } else {
              borderpairs<-unique(pairsgwithin[c(ip,jp), 1:7])
              resultdfnoborder<-dplyr::setdiff(resultdf2, borderpairs) #all segments but the two of the loop round
              remainingpairs<-dplyr::setdiff(pairsgwithin[,1:7], borderpairs) #all segments from same tile but the two of the loop round
            }
   
            mycount=0 
            resultdfnoborder$group<-rep(888, dim(resultdfnoborder)[1])
            inlocs2<-resultdfnoborder
            grouping<-inlocs2[0,]
            for (z in 1:dim(resultdfnoborder)[1]) {
              if ((nrow(merge(resultdfnoborder[z,]  ,grouping))>0 ) == FALSE) {
                grouping<-rbind( grouping, resultdfnoborder[z, ]) 
                end<-dim(grouping)[1]; 
                counter=dim(grouping)[1] -1; 
                dimdif<-10000000
                while(dimdif>0) {
                  i=counter+1
                  for (p in i:end) { 
                    if (dim(inlocs2)[1]>0) { #while rows remain in inlocs2
                      group<-z
                      sg1<-grouping$cltile[p]
                      sg2<-grouping$clprox[p]
                      neighlist<-subset(inlocs2, cltile==sg1 | clprox==sg1 | cltile==sg2 | clprox==sg2)
                      neighlist$group<-rep(group, (dim(neighlist)[1]))
                      mycount=mycount+(dim(neighlist)[1])
                      grouping<-rbind(grouping, neighlist)
                      grouping<-matchColClasses(inlocs2, grouping)
                      library(dplyr)
                      inlocs2<-dplyr::setdiff(inlocs2[,1:7], grouping[,1:7])
                      inlocs2$group<-rep(888, dim(inlocs2)[1])
                      counter<-counter+1 
                    }
                  }
                  dimdif<-dim(grouping)[1]-end;  end<-dim(grouping)[1] 
                }
              }
            }
            grouping2<-subset(grouping, group < 888)
            #segments that are in resultdf2 and not in grouping
            segmentsgrouping2<-c(grouping2$cltile, grouping2$clprox) 
            segmentsresultdf2<-c(resultdf2$cltile, resultdf2$clprox) 
            lonesegments<-dplyr::setdiff(segmentsresultdf2, segmentsgrouping2) #vector
            grouplone<-as.factor(max(as.numeric(as.character(grouping2$group)))+1)
            
            ######################################################################

            
            ######################################################################
            
            grouping2$group<-as.factor( grouping2$group)
            grouping2$colorcodes2<-colors()[as.numeric(grouping2$group)*3]
            
            PLDFINAL<-PLDF[0,]
            count=0
            for(grp in levels(grouping2$group)){
              count=count+1
              agroup<-subset(grouping, group==grp)
              interpolaciones<-data.frame(stitX=numeric(), stitY=numeric(), grouping=character())
              for(fila in 1:nrow(agroup)){
                if(agroup$xpoint[fila] == agroup$xprox[fila]) {
                  n=round(distance/5)
                  they<-seq(agroup$ypoint[fila],agroup$yprox[fila], length.out=n)
                  thex<-rep(agroup$xpoint[fila], n)
                  listint<-list(thex, they)
                  names(listint) <- c("x", "y")
                  int<-listint
                } else {
                  int<-approx(c(agroup$xpoint[fila], agroup$xprox[fila]), c(agroup$ypoint[fila], agroup$yprox[fila]), n=round(distance/5))
                }
                ints<-data.frame(int$x, int$y)
                ints$grouping<-rep(grp, nrow(ints))
                colnames(ints)<-c("stitX", "stitY", "grouping")
                interpolaciones<-rbind(interpolaciones, ints)
              }
              includedsg<-unique(c(as.character(agroup$cltile), as.character(agroup$clprox)))
              includedcoords<-subset(PLDF, clu %in% includedsg)
              includedcoords$grouping<-rep(grp, nrow(includedcoords))
              PLDFINAL<-rbind(PLDFINAL, includedcoords)
              library(plyr)
              PLDFINAL<-rbind.fill(PLDFINAL, interpolaciones)
            }
            #we get the lone segments from PLDF and add them to PLDFINAL
            
            #############################################
            lonecoords<-subset(PLDF, clu %in% lonesegments)
            lonecoords$grouping<-rep(grouplone, nrow(lonecoords))
            PLDFINAL<-rbind(PLDFINAL, lonecoords)
            #############################################
            
            #we reclassify surface according to the predominant type
            clugr<-data.frame(PLDFINAL$clu, PLDFINAL$surfacetype, PLDFINAL$group)
            clugr<-(unique(clugr))
            clugr<-clugr[complete.cases(clugr), ]
            colnames(clugr)<-c("clu", "surfacetype", "group")
            clugrtable<-as.data.frame(table(clugr$group, clugr$surfacetype))
            clugrtablewide<-reshape(clugrtable, idvar = "Var1", timevar = "Var2", direction = "wide")
            colnames(clugrtable)<-c("group", "surfacetype", "Freq")
            colnames(clugrtablewide)<-c("group", "PM", "LD")
            clugrtablewide$total<-clugrtablewide$PM+clugrtablewide$LD
            
            clugrtablewide$chosen<-rep(NA, nrow(clugrtablewide))
            for (iii in 1:nrow(clugrtablewide)){
              vmax<-(which.max(c(clugrtablewide$PM[iii], clugrtablewide$LD[iii])))
              if(vmax==1){
                clugrtablewide$chosen[iii]<-"PM"
              }
              if(vmax==2){
                clugrtablewide$chosen[iii]<-"LD"
              }
              if(clugrtablewide$PM[iii]==clugrtablewide$LD[iii]){
                clugrtablewide$chosen[iii]<-"LD" #LD tends to get more errors
              }
            }
            
            clugrtablewide$discrepancies<-rep(NA, nrow(clugrtablewide))
            for (iii in 1:nrow(clugrtablewide)){
              if(clugrtablewide$chosen[iii]=="LD"){
                clugrtablewide$discrepancies[iii]<-clugrtablewide$PM[iii]
              }
              if(clugrtablewide$chosen[iii]=="PM"){
                clugrtablewide$discrepancies[iii]<-clugrtablewide$LD[iii]
              }
            }
            DiscrepancyIndex<-sum(clugrtablewide$discrepancies)/sum(clugrtablewide$total)
            #correctly classified surfaces should have a clearly predominant surface type. If each type is close to 50%, we must have made a mistake
            clugrtablewide$propOK<-((clugrtablewide$total-clugrtablewide$discrepancies)/clugrtablewide$total)
            clugrtablewide$certainty1<-rep(NA, nrow(clugrtablewide))
            for (iii in 1:nrow(clugrtablewide)){
              clugrtablewide$certainty1[iii]<--log(binom.test(clugrtablewide$PM[iii], clugrtablewide$total[iii], p = 0.5, alternative = "two.sided")$p.value,10)
            }
            CertaintyIndex<-sum(clugrtablewide$certainty1)

            colnames(clugrtablewide)[1]<-"grouping"
            PLDFINALg<-merge(PLDFINAL, clugrtablewide[, c(1,5)], by="grouping")
            plot(PLDFINALg$stitX, PLDFINALg$stitY, pch=16, col=colors()[as.numeric(as.factor(PLDFINALg$chosen))*10+400], cex=0.3,axes=FALSE)
            segments(remainingpairs$xpoint, remainingpairs$ypoint, x1 = remainingpairs$xprox, y1 = remainingpairs$yprox, col = "blue", lwd=3)
            box(lwd=2)
            midplotpointx<-mean(PLDFINALg$stitX); midplotpointy<-mean(PLDFINALg$stitY); 
            midplotpointx2<-quantile(PLDFINALg$stitX,0.6); midplotpointy2<-quantile(PLDFINALg$stitY,0.4)
            text(midplotpointx,midplotpointy, labels=round(CertaintyIndex,3))
            text(midplotpointx,midplotpointy2, labels=ip); text(midplotpointx2,midplotpointy2, labels=jp)
            counting=counting+1
            certv[counting]<-CertaintyIndex
            ipv[counting]<- ip
            jpv[counting]<- jp
            if( ip==0 & jp==0 ) {
              scoreT1.1[counting] <- 0
              scoreT1.2[counting]  <- 0
              cltilev1[counting] <- NA
              clproxv1[counting] <- NA
              cltilev2[counting] <- NA
              clproxv2[counting] <- NA
            } else {
              scoreT1.1[counting] <- pairsgwithin$scoreT1[ip]
              scoreT1.2[counting]  <- pairsgwithin$scoreT1[jp]
              cltilev1[counting] <- as.character(pairsgwithin$cltile[ip])
              clproxv1[counting] <- as.character(pairsgwithin$clprox[ip])
              cltilev2[counting] <- as.character(pairsgwithin$cltile[jp])
              clproxv2[counting] <- as.character(pairsgwithin$clprox[jp])
            }
            PLDFINALg$ip<-rep(ip, nrow(PLDFINALg))
            PLDFINALg$jp<-rep(jp, nrow(PLDFINALg))
            PLDFG<-rbind(PLDFG, PLDFINALg)
          }
        }
      }
      par(mfrow=c(1, 1))
      par(cex=0.7, mai=c(0.3,0.3,0.3,0.3))
      decisionsg<-data.frame(cltilev1,clproxv1,scoreT1.1,cltilev2,clproxv2,scoreT1.2,certv,ipv,jpv)
      decisionsg$scoreTT<-0.5*decisionsg$scoreT1.1+0.5*decisionsg$scoreT1.2+decisionsg$certv
      chosenrow<-which.max(decisionsg$scoreTT)
      PLDFINALg<-subset(PLDFG, ip==decisionsg$ipv[chosenrow] & jp==decisionsg$jpv[chosenrow])
      
      plot(PLDFINALg$stitX, PLDFINALg$stitY, pch=16, col=colors()[as.numeric(as.factor(PLDFINALg$chosen))*10+400], cex=0.3)
      rm(PLDFG) #I remove it because it is huge
      PMcoords<-subset(PLDFINALg, chosen=="PM")
      LDcoords<-subset(PLDFINALg, chosen=="LD")
    } else {
      print("No joining segments within image found. There should be no borders")
      mycount=0 
      resultdfnoborder<-resultdf2
      resultdfnoborder$group<-rep(888, dim(resultdfnoborder)[1])
      inlocs2<-resultdfnoborder
      grouping<-inlocs2[0,]
      for (z in 1:dim(resultdfnoborder)[1]) {
        if ((nrow(merge(resultdfnoborder[z,]  ,grouping))>0 ) == FALSE) {
          grouping<-rbind( grouping, resultdfnoborder[z, ]) 
          end<-dim(grouping)[1]; 
          counter=dim(grouping)[1] -1; 
          dimdif<-10000000
          while(dimdif>0) {
            i=counter+1
            for (p in i:end) { 
              if (dim(inlocs2)[1]>0) { #while rows remain in inlocs2
                group<-z
                sg1<-grouping$cltile[p]
                sg2<-grouping$clprox[p]
                neighlist<-subset(inlocs2, cltile==sg1 | clprox==sg1 | cltile==sg2 | clprox==sg2)
                neighlist$group<-rep(group, (dim(neighlist)[1]))
                mycount=mycount+(dim(neighlist)[1])
                grouping<-rbind(grouping, neighlist)
                grouping<-matchColClasses(inlocs2, grouping)
                library(dplyr)
                inlocs2<-dplyr::setdiff(inlocs2[,1:7], grouping[,1:7])
                inlocs2$group<-rep(888, dim(inlocs2)[1])
                counter<-counter+1 
              }
            }
            dimdif<-dim(grouping)[1]-end;  end<-dim(grouping)[1] 
          }
        }
      }
      grouping2<-subset(grouping, group < 888)
      grouping2$group<-as.factor( grouping2$group)
      grouping2$colorcodes2<-colors()[as.numeric(grouping2$group)*3]
      
      PLDFINAL<-PLDF[0,]
      count=0
      for(grp in levels(grouping2$group)){
        count=count+1
        agroup<-subset(grouping, group==grp)
        interpolaciones<-data.frame(stitX=numeric(), stitY=numeric(), grouping=character())
        for(fila in 1:nrow(agroup)){
          int<-approx(c(agroup$xpoint[fila], agroup$xprox[fila]), c(agroup$ypoint[fila], agroup$yprox[fila]), n=round(distance/5))
          ints<-data.frame(int$x, int$y)
          ints$grouping<-rep(grp, nrow(ints))
          colnames(ints)<-c("stitX", "stitY", "grouping")
          interpolaciones<-rbind(interpolaciones, ints)
        }
        includedsg<-unique(c(as.character(agroup$cltile), as.character(agroup$clprox)))
        includedcoords<-subset(PLDF, clu %in% includedsg)
        includedcoords$grouping<-rep(grp, nrow(includedcoords))
        PLDFINAL<-rbind(PLDFINAL, includedcoords)
        library(plyr)
        PLDFINAL<-rbind.fill(PLDFINAL, interpolaciones)
      }

      #classify according to predominant type
      clugr<-data.frame(PLDFINAL$clu, PLDFINAL$surfacetype, PLDFINAL$group)
      clugr<-(unique(clugr))
      clugr<-clugr[complete.cases(clugr), ]
      colnames(clugr)<-c("clu", "surfacetype", "group")
      clugrtable<-as.data.frame(table(clugr$group, clugr$surfacetype))
      clugrtablewide<-reshape(clugrtable, idvar = "Var1", timevar = "Var2", direction = "wide")
      colnames(clugrtable)<-c("group", "surfacetype", "Freq")
      colnames(clugrtablewide)<-c("group", "PM", "LD")
      clugrtablewide$total<-clugrtablewide$PM+clugrtablewide$LD
      
      clugrtablewide$chosen<-rep(NA, nrow(clugrtablewide))
      for (iii in 1:nrow(clugrtablewide)){
        vmax<-(which.max(c(clugrtablewide$PM[iii], clugrtablewide$LD[iii])))
        if(vmax==1){
          clugrtablewide$chosen[iii]<-"PM"
        }
        if(vmax==2){
          clugrtablewide$chosen[iii]<-"LD"
        }
        if(clugrtablewide$PM[iii]==clugrtablewide$LD[iii]){
          clugrtablewide$chosen[iii]<-"LD" #LD tends to have more erroneous classifications
        }
      }
      
      clugrtablewide$discrepancies<-rep(NA, nrow(clugrtablewide))
      for (iii in 1:nrow(clugrtablewide)){
        if(clugrtablewide$chosen[iii]=="LD"){
          clugrtablewide$discrepancies[iii]<-clugrtablewide$PM[iii]
        }
        if(clugrtablewide$chosen[iii]=="PM"){
          clugrtablewide$discrepancies[iii]<-clugrtablewide$LD[iii]
        }
      }
      DiscrepancyIndex<-sum(clugrtablewide$discrepancies)/sum(clugrtablewide$total)
      
      clugrtablewide$propOK<-((clugrtablewide$total-clugrtablewide$discrepancies)/clugrtablewide$total)
      clugrtablewide$certainty1<-rep(NA, nrow(clugrtablewide))
      for (iii in 1:nrow(clugrtablewide)){
        clugrtablewide$certainty1[iii]<--log(binom.test(clugrtablewide$PM[iii], clugrtablewide$total[iii], p = 0.5, alternative = "two.sided")$p.value,10)
      }
      CertaintyIndex<-sum(clugrtablewide$certainty1)

      colnames(clugrtablewide)[1]<-"grouping"
      PLDFINALg<-merge(PLDFINAL, clugrtablewide[, c(1,5)], by="grouping")
      plot(PLDFINALg$stitX, PLDFINALg$stitY, pch=16, col=colors()[as.numeric(as.factor(PLDFINALg$chosen))*10+400], cex=0.3,axes=FALSE)
      box(lwd=2)
      PMcoords<-subset(PLDFINALg, chosen=="PM")
      LDcoords<-subset(PLDFINALg, chosen=="LD")
    }
    
    ######################
}
write.table(PMcoords, paste(path, "_PMCORRECTED.txt", sep="/"),sep = '\t',quote = FALSE,row.names = FALSE)
write.table(LDcoords, paste(path, "_LDCORRECTED.txt", sep="/"),sep = '\t',quote = FALSE,row.names = FALSE)
#################




#We interpolate connecting segments for the case that there is no LD
#################
if ((LDexpected ==FALSE | (nrow(FINALLD)==0))  & nrow(FINALPM)>0 & FindCellLimits==TRUE) {
  #################
  PLDFINAL<-PLDF[0,]
    interpolaciones<-data.frame(stitX=numeric(), stitY=numeric())
    for(fila in 1:nrow(resultdf2)){
      if(resultdf2$xpoint[fila] == resultdf2$xprox[fila]) {
        n=round(distance/5)
        they<-seq(resultdf2$ypoint[fila],resultdf2$yprox[fila], length.out=n)
        thex<-rep(resultdf2$xpoint[fila], n)
        listint<-list(thex, they)
        names(listint) <- c("x", "y")
        int<-listint
      } else {
        int<-approx(c(resultdf2$xpoint[fila], resultdf2$xprox[fila]), c(resultdf2$ypoint[fila], resultdf2$yprox[fila]), n=round(distance/5))
      }
      ints<-data.frame(int$x, int$y)
      colnames(ints)<-c("stitX", "stitY")
      interpolaciones<-rbind(interpolaciones, ints)
    }
    includedsg<-unique(c(as.character(resultdf2$cltile), as.character(resultdf2$clprox)))
    includedcoords<-subset(PLDF, clu %in% includedsg)
    PLDFINAL<-rbind(PLDFINAL, includedcoords)
    library(plyr)
    PLDFINAL<-rbind.fill(PLDFINAL, interpolaciones)
    PMcoords<-PLDFINAL
    LDcoords<-PLDFINAL[0,]
  #################
} 
#################


t2<-(Sys.time())
print(t1)
print(t2)

if (deleteENVwhenfinished == TRUE) {
  print("Deleting all things in environment now")
}