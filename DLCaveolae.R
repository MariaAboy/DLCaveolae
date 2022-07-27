#########################################################
#PARAMETERS
#########################################################
mode="PREDICT"
path<-"C:/Users/myname/images" #your image directory, something like "C:/Users/myname/images" for windows or "/home/myname/images" for Ubuntu. Use / or \\ , never \
confidence=0.75
extensions=c("jpg", "tif", "tiff") #better not use svg
pxum=676.4536  #scale in pixel/micron. This is an example, write the scale of your images
pixelsize=1000*(1/pxum) #pixelsize in nm
#minimum, maximum and average values for caveolae diameter in nm from the literature. Modify if needed.
mincavnm=50
maxcavnm=100
avcavnm=75

#change here if you want to specify a windowsize and displacement. If left unchanged, the tool will calculate the optimum parameters based on the scale introduced in pxum
#use "recommended" if you want a recommended windowsize and/or displacement
windowsize="recommended"
pxdisplacement= "recommended"

FindCellLimits=T #TRUE if you want the algorithm to find PM and LD
LDexpected=T #TRUE if you expect the images to contain a big LD, like in adipocytes
specifyfilelist=F #if you only want to analyse a subset of the images contained in the directory, set it to TRUE and specify the filenames in the vector below. Remember to remove the # at the begining ot the line if you do so
#listrequired=c("Fig1", "Fig2")
deleteENVwhenfinished = F  #leave this to T or TRUE unless you want to do troubleshooting. It may induce errors when set to F for a set of several images
measurecavdistances = T #set to F if you don't need Caveolae-PM and caveolae-LD distances
measurePMLDdistances = T #set to F if you don't need PM-LD distances
#########################################################



#########################################################
###this is to calculate recommended windowsize and displacement
#########################################################
timeinit<-(Sys.time())
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



#########################################################
###FUNCTIONS
#########################################################
if(FindCellLimits==FALSE) {
  measurecavdistances = FALSE
  LDexpected=FALSE
  measurePMLDdistances = FALSE
}
if(LDexpected==FALSE) {
  measurePMLDdistances = FALSE 
}

repaint_predictiondf2<-function() {
  #this function assigns a color to coordinates in plots depending on prediction
  predictiondf2$colorcodes<-ifelse(grepl( 0 ,predictiondf2$prediction ),"black",0)
  predictiondf2$colorcodes<-ifelse(grepl( 1 ,predictiondf2$prediction ),"black",predictiondf2$colorcodes)
  predictiondf2$colorcodes<-ifelse(grepl( 2 ,predictiondf2$prediction ),"black",predictiondf2$colorcodes)
  predictiondf2$colorcodes<-ifelse(grepl( 3 ,predictiondf2$prediction ),"cyan",predictiondf2$colorcodes)
  predictiondf2$colorcodes<-ifelse(grepl( 4 ,predictiondf2$prediction ),"magenta",predictiondf2$colorcodes)
  predictiondf2$colorcodes<-ifelse(grepl( 5 ,predictiondf2$prediction ),"yellow",predictiondf2$colorcodes)
  predictiondf2$colorcodes<- ifelse(grepl(6,predictiondf2$prediction),"pink",predictiondf2$colorcodes)
  predictiondf2$colorcodes<- ifelse(grepl(7,predictiondf2$prediction),"steelblue3",predictiondf2$colorcodes)
  predictiondf2$colorcodes<- ifelse(grepl(8,predictiondf2$prediction),"khaki",predictiondf2$colorcodes)
  return (predictiondf2)
}
repaint<-repaint_predictiondf2

classplot<-function(election) {
  #this function produces a prediction plot
  if (election==1) {
    #plot with initial cell prediction
    par(cex=0.7, mai=c(0.01,0.01,0.01,0.01))
    layout(matrix(c(1, 1, 2,
                    1, 1, 2,
                    1, 1, 2), nrow=3, byrow=TRUE))
    plot(as.raster(img_tensor[1,,,])) 
    for (i in 1:(dim(predictiondf)[1])) {
      if (predictiondf$pchcodes[i] == 0) {
        points(predictiondf$xcoord[i],  nrows-(predictiondf$ycoord[i]), col= predictiondf$colorcodes[i], pch=16, cex=0.8)
      } else { if ( sample(0:1, 1, replace=T,prob=c(0,1)) == 1) { #to paint only a random 10% prob=c(0.9,0.1)) == 1)
        points(predictiondf$xcoord[i],  nrows-(predictiondf$ycoord[i]), col= predictiondf$colorcodes[i], pch=16, cex=0.8)
      }
      }
    }
    plot(0,type='n',axes=FALSE,ann=FALSE)
    legend("center", legend=c("Caveolae", "LD/empty", "ECM", "cytoplasm", "er", "mito", "ves"),
           col=c("black", "cyan", "magenta","yellow", "pink", "steelblue3", "khaki"), pch=c(16,16,16,16,16,16), cex=1.7, box.lty=0)
  } else {
    if (election==2) {
      
      repaint_predictiondf2()
      #plot with corrected cell prediction
      par(cex=0.7, mai=c(0.01,0.01,0.01,0.01))
      layout(matrix(c(1, 1, 2,
                      1, 1, 2,
                      1, 1, 2), nrow=3, byrow=TRUE))
      plot(as.raster(img_tensor[1,,,])) 
      for (i in 1:(dim(predictiondf2)[1])) {
        if (predictiondf2$pchcodes[i] == 0) {
          points(predictiondf2$xcoord[i],  nrows-(predictiondf2$ycoord[i]), col= predictiondf2$colorcodes[i], pch=16, cex=0.8)
        } else { if ( sample(0:1, 1, replace=T,prob=c(0,1)) == 1) { #paint only a random 10% #antes prob=c(0.9,0.1)) == 1)
          points(predictiondf2$xcoord[i],  nrows-(predictiondf2$ycoord[i]), col= predictiondf2$colorcodes[i], pch=16, cex=0.8)
        }
        }
      }
      plot(0,type='n',axes=FALSE,ann=FALSE)
      legend("center", legend=c("Caveolae", "LD/empty", "ECM", "cytoplasm", "er", "mito", "ves"),
             col=c("black", "cyan", "magenta","yellow", "pink", "steelblue3", "khaki"), pch=c(16,16,16,16,16,16), cex=1.7, box.lty=0)
    } else {
      print("PLEASE, select 1 for initial plot and 2 for corrected plot")
    }
  } 
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

flatsummaryby<-function(operacion, variable, grupo){
  #this function performs a by operation and then transforms the output list in a dataframe
  lista<-by(variable, grupo, operacion)
  listaflat<-data.frame((vapply(lista, unlist, unlist(lista[[1]]))))
  library(data.table)
  setDT(listaflat, keep.rownames = TRUE)[]
  name1 <- as.character(match.call()[[4]][3])
  name2 <- as.character(match.call()[[3]][3])
  name3 <- as.character(match.call()[[2]])
  name4 <- as.character(match.call()[[3]][2])
  colnames(listaflat)<-c(name1, paste(name2, name3,name4, sep="_"))
  return(listaflat)
}

sortingroups<-function(targetdf, locsinit, inlocs, lastcol, steps=1) {
  #this function groups coordinates by spatial continuity
  mycount=0
  ###########locsinit has the borders with inner contacts. inlocs has these borders and inners
  locsinit$xcoord<-as.numeric(locsinit$xcoord); locsinit$ycoord<-as.numeric(locsinit$ycoord)
  if(length(locsinit$X)>0) {locsinit$X<-as.numeric(locsinit$X)}
  if(length(locsinit$Y)>0) {locsinit$Y<-as.numeric(locsinit$Y)}
  inlocs$xcoord<-as.numeric(inlocs$xcoord); inlocs$ycoord<-as.numeric(inlocs$ycoord)
  if(length(inlocs$X)>0) {inlocs$X<-as.numeric(inlocs$X)}
  if(length(inlocs$Y)>0) {inlocs$Y<-as.numeric(inlocs$Y)}
  
  locsinit$group<-rep(999999999999, dim(locsinit)[1])
  inlocs$group<-rep(888, dim(inlocs)[1])
  
  for (z in 1:dim(locsinit)[1]) {
    if ((nrow(merge(locsinit[z,]  ,groupdf))>0 ) == FALSE) {
      groupdf<-rbind( groupdf, locsinit[z, ]) 
      end<-dim(groupdf)[1]; 
      counter=dim(groupdf)[1] -1; 
      dimdif<-10000000
      while(dimdif>0) {
        i=counter+1
        for (p in i:end) { 
          if (dim(inlocs)[1]>0) { #while rows remain in inlocs
            group<-z
            neighlist<-getneighbours(p, groupdf, inlocs, nstep= steps, ifsame=TRUE)
            neighlist$group<-rep(group, (dim(neighlist)[1]))
            mycount=mycount+(dim(neighlist)[1])
            groupdf<-rbind(groupdf, neighlist)
            groupdf<-matchColClasses(inlocs, groupdf)
            library(dplyr)
            inlocs<-setdiff(inlocs[,1:lastcol], groupdf[,1:lastcol]) #leaves in inlocs only what is in groupdf
            inlocs$group<-rep(888, dim(inlocs)[1])
            counter<-counter+1 
          }
        }
        dimdif<-dim(groupdf)[1]-end;  end<-dim(groupdf)[1] 
      }  
    }
  } 
  groupdf2<-subset(groupdf, group < 999999999999)
  groupdf2$group<-as.factor( groupdf2$group)
  levels(groupdf2$group)<-seq(1,length(levels(groupdf2$group)), by=1)
  groupdf2$group<-as.numeric(groupdf2$group)
  groupdf2$colorcodes2<-colors()[groupdf2$group*3]
  return(groupdf2)
}

getneighbours<-function(instance, df1, df2, nstep=1, ifsame=TRUE) {
  #this function gets an instance row in df1 and searches all adjacent neighbours in df2
  if (round(nstep, digits=0) == nstep) { 
    dfneighlist <- data.frame(matrix(NA, nrow =0 , ncol = dim(df2)[2]))
    colnames(dfneighlist)<-colnames(df2)
    stepvector<-seq(from=nstep, to=1)
    for (i in stepvector)  {
      step=i*pxdisplacement
      ex<-df1[instance,]$xcoord; ey<-df1[instance,]$ycoord;
      topleft<-df2[(df2$xcoord == ex-step & df2$ycoord == ey+step),]
      top<-df2[(df2$xcoord == ex & df2$ycoord == ey+step),]
      topright<-df2[(df2$xcoord == ex+step & df2$ycoord == ey+step),]
      right<-df2[(df2$xcoord == ex-step & df2$ycoord == ey),]
      left<-df2[(df2$xcoord == ex+step & df2$ycoord == ey),]
      bottomright<-df2[(df2$xcoord == ex+step & df2$ycoord == ey-step),]
      bottom<-df2[(df2$xcoord == ex & df2$ycoord == ey-step),]
      bottomleft<-df2[(df2$xcoord == ex-step & df2$ycoord == ey-step),]
      if (ifsame == TRUE) { 
        same<-df2[(df2$xcoord == ex & df2$ycoord == ey),]
        neighlist<-rbind(same, topleft, top, topright, right, left, bottomright, bottom, bottomleft)
        dfneighlist<-rbind(dfneighlist, neighlist)
      } else {
        if (ifsame == FALSE) {
          neighlist<-rbind(topleft, top, topright, right, left, bottomright, bottom, bottomleft)
          dfneighlist<-rbind(dfneighlist, neighlist)
        } else { print("same argument must be TRUE or FALSE") }
      }
    }
    return (dfneighlist)
  } else {print("step must be a whole number")}
}

Cgetneighbours<-function(instance, df1, df2, nstep=1, ifsame=TRUE, cond=TRUE) {
  #this function is similar to getneighbours. But if no inmiediately adjacent neighbours are found, it looks for neighbours from a greater distance
  if (round(nstep, digits=0) == nstep) { 
    dfneighlist <- data.frame(matrix(NA, nrow =0 , ncol = dim(df2)[2]))
    colnames(dfneighlist)<-colnames(df2)
    stepvector<-seq(from=nstep, to=1)
    for (i in stepvector)  {
      step=i*pxdisplacement
      ex<-df1[instance,]$xcoord; ey<-df1[instance,]$ycoord;
      
      top<-df2[(df2$xcoord == ex & df2$ycoord == ey+step),]
      right<-df2[(df2$xcoord == ex-step & df2$ycoord == ey),]
      bottom<-df2[(df2$xcoord == ex & df2$ycoord == ey-step),]
      left<-df2[(df2$xcoord == ex+step & df2$ycoord == ey),]
      
      directos<-rbind(top, right, bottom, left)
      if ((dim(directos)[1]) < 2) {
        topleft<-df2[(df2$xcoord == ex-step & df2$ycoord == ey+step),]
        topright<-df2[(df2$xcoord == ex+step & df2$ycoord == ey+step),]
        bottomright<-df2[(df2$xcoord == ex+step & df2$ycoord == ey-step),]
        bottomleft<-df2[(df2$xcoord == ex-step & df2$ycoord == ey-step),]
        
        if (ifsame == TRUE) { 
          same<-df2[(df2$xcoord == ex & df2$ycoord == ey),]
          neighlist<-rbind(same, topleft, top, topright, right, left, bottomright, bottom, bottomleft)
          dfneighlist<-rbind(dfneighlist, neighlist)
          
          if ((dim(neighlist)[1]) == 0 & nstep == 1 & cond == TRUE) {
            step=2
            ex<-df1[instance,]$xcoord; ey<-df1[instance,]$ycoord;
            
            top<-df2[(df2$xcoord == ex & df2$ycoord == ey+step),]
            right<-df2[(df2$xcoord == ex-step & df2$ycoord == ey),]
            bottom<-df2[(df2$xcoord == ex & df2$ycoord == ey-step),]
            left<-df2[(df2$xcoord == ex+step & df2$ycoord == ey),]
            
            directos2<-rbind(top, right, bottom, left)
            
            if ((dim(directos2)[1]) < 2) {
              topleft<-df2[(df2$xcoord == ex-step & df2$ycoord == ey+step),]
              topright<-df2[(df2$xcoord == ex+step & df2$ycoord == ey+step),]
              bottomright<-df2[(df2$xcoord == ex+step & df2$ycoord == ey-step),]
              bottomleft<-df2[(df2$xcoord == ex-step & df2$ycoord == ey-step),]
              
              neighlist<-rbind(same, topleft, top, topright, right, left, bottomright, bottom, bottomleft)
              dfneighlist<-rbind(dfneighlist, neighlist)
            } else {
              neighlist<-rbind(same, top, right, left, bottom)
              dfneighlist<-rbind(dfneighlist, neighlist)
            }
          }
        } else {
          if (ifsame == FALSE) {
            neighlist<-rbind(topleft, top, topright, right, left, bottomright, bottom, bottomleft)
            dfneighlist<-rbind(dfneighlist, neighlist)
            
            if ((dim(neighlist)[1]) == 0 & nstep == 1 & cond == TRUE) {
              step=2
              ex<-df1[instance,]$xcoord; ey<-df1[instance,]$ycoord;
              
              top<-df2[(df2$xcoord == ex & df2$ycoord == ey+step),]
              right<-df2[(df2$xcoord == ex-step & df2$ycoord == ey),]
              bottom<-df2[(df2$xcoord == ex & df2$ycoord == ey-step),]
              left<-df2[(df2$xcoord == ex+step & df2$ycoord == ey),]
              
              directos2<-rbind(top, right, bottom, left)
              
              if ((dim(directos2)[1]) < 2) {
                topleft<-df2[(df2$xcoord == ex-step & df2$ycoord == ey+step),]
                topright<-df2[(df2$xcoord == ex+step & df2$ycoord == ey+step),]
                bottomright<-df2[(df2$xcoord == ex+step & df2$ycoord == ey-step),]
                bottomleft<-df2[(df2$xcoord == ex-step & df2$ycoord == ey-step),]
                
                neighlist<-rbind(topleft, top, topright, right, left, bottomright, bottom, bottomleft)
                dfneighlist<-rbind(dfneighlist, neighlist)
              } else {
                neighlist<-rbind(top, right, left, bottom)
                dfneighlist<-rbind(dfneighlist, neighlist)
              }
            }
          } else { print("same argument must be TRUE or FALSE") }
        }
      } else {
        
        if (ifsame == TRUE) { 
          same<-df2[(df2$xcoord == ex & df2$ycoord == ey),]
          neighlist<-rbind(same, top, right, left, bottom)
          dfneighlist<-rbind(dfneighlist, neighlist)
        } else {
          if (ifsame == FALSE) {
            neighlist<-rbind(top, right, left, bottom)
            dfneighlist<-rbind(dfneighlist, neighlist)
          } else { print("same argument must be TRUE or FALSE") }
        }
      }
    }
    return (dfneighlist)
  } else {print("step must be a whole number")}
}

disorderinterpolate<-function (dfr, st=10) {
  #this function interpolates coordinates from PM and LD segments
  resultdf<-data.frame(seqx=numeric(), seqy=numeric(),
                       cluster=factor(), colorcodes2.y=character(),  surfacetype=character() )
  for (i in 1:(dim(dfr)[1])) {
    yyy<-dfr$ycoord[i]; xxx<-dfr$xcoord[i]
    nlist<-Cgetneighbours(i, dfr, dfr, nstep=1, ifsame=FALSE)
    othercolumns<-dfr[i, 10:12]
    if ((dim(nlist)[1]) > 0) {
      for (j in 1:(dim(nlist)[1])) { 
        ypoint1<-yyy; xpoint1<-xxx
        ypoint2<-nlist$ycoord[j]; xpoint2<-nlist$xcoord[j]
        if (xpoint1 == xpoint2) {
          seqx<-rep(xpoint1, st)
          seqy<-seq(ypoint1, ypoint2, length.out=st)
          resultdf<-rbind(resultdf, cbind(data.frame(seqx, seqy), othercolumns))
        } else {
          if (ypoint1 == ypoint2) {
            seqy<-rep(ypoint1, st)
            seqx<-seq(xpoint1, xpoint2, length.out=st)
            resultdf<-rbind(resultdf, cbind(data.frame(seqx, seqy), othercolumns))
          } else {
            intr<-approx(c(xpoint1, xpoint2), c(ypoint1, ypoint2), method="linear", n=st, ties = mean)
            intrdf<-data.frame(intr$x, intr$y); colnames(intrdf)<-c("seqx", "seqy")
            intrdf<-cbind(intrdf, othercolumns)
            resultdf<-rbind(resultdf, intrdf)
          }
        }
        
      } #loop por neighbours
    }
  }  #loop por rows df
  colnames(resultdf) <- c( "xcoord", "ycoord",
                           "cluster", "colorcodes2.y", "surfacetype")
  resultdf<-unique(resultdf)
  return(resultdf)
}

expandcyto<-function (dfcyto, dfobject, expands) {
  #this function checks if expanding the cytoplasmic coordinates in dfcyto for a number of times (expands) gets the cytoplasm in contact with a surface in dfobject
  #and assigns the surface to the cytoplasm to which it makes contact (to which it belongs)
  if (exists("belong")) {rm(belong)}
  belong <- vector()
  for (clt in levels(dfobject$cluster)) { #for each of the membranes
    obj<-subset (dfobject, cluster == clt) #subset that membrane
    donexpands = 0
    while (donexpands < (expands+1)) {
      donexpands = donexpands +1
      for (grp in levels(dfcyto$group)) { #for each of the cells
        if (donexpands < (expands+1)) {
          cll<- subset(dfcyto, group == grp) #subset that cell
          counter = 0
          for (ff in 1:(dim(cll)[1])) {
            xt<-cll$xcoord[ff]; yt<-cll$ycoord[ff]
            range <- donexpands * displacement
            vicinity <- subset(obj, xcoord < xt + range & xcoord > xt - range & ycoord < yt +range & ycoord > yt -range )  
            if ((dim(vicinity)[1]) > 0) {
              counter = counter +1
            } 
          }
          if (counter == 0) {
            donexpands = donexpands 
          } else {
            currentexpand = donexpands
            donexpands = expands+1
            belongc <- rep (grp, dim(obj)[1])
            belong<-c(belong, belongc)
          }
        }#if a cell makes contact with the membrane, it won't look in other cells
      }
    } #if it finds contact in a round, it won't expand anymore
  }
  if (length(belong) > 0) { 
    dfobject$belong <- belong
    return(dfobject)
  } else {
    print("Seems that this surface is far away from the cell. Try maybe a wider range?")
    dfobject$belong <- rep(NA, (dim(dfobject)[1]))
    return(dfobject)
  }
}

#########################################################



#########################################################
###Loading models
#########################################################
library(keras)
modelclascav <- load_model_hdf5("TEMCavCNN.h5")
modelcavnocav <- load_model_hdf5("TEMCellCNN.hdf5")
#########################################################



#########################################################
###Load and analyse images from path
#########################################################
library(keras)
size=windowsize; certainty=confidence
filelist<-list.files(path)
newcropsize= round (148*(pxum/1000))
#load images and reshape to 1024x1024
if(specifyfilelist==TRUE){
  folderfiles<-length(filelist)
  filelist<-intersect(filelist,listrequired)
  finalfiles<-length(filelist)
  ignoredfiles<-folderfiles- finalfiles
  print(paste(ignoredfiles, "files from folder will be ignored because specifylelist is set to TRUE", sep=" "))
}

for (element in filelist) { #loop through all the images in path. Closes at the end of the script
  if ( any(endsWith(element, extensions))) { #checks if "element" ends with any of the extensions from the list
  print(element)
    if(specifyfilelist==TRUE){
      print(paste(ignoredfiles, "files from folder will be ignored because specifylelist is set to TRUE", sep=" "))
      print(paste(finalfiles, "remaining files to analyse", sep=" "))
    }
  img_path<-paste(path, element, sep="/")
  img<-image_load(img_path, target_size=c(1024,1024), grayscale = TRUE)
  img_tensor<-image_to_array(img)
  original_tensor<-img_tensor
  #we add a padding to minimize border effect
  padx<-matrix(data = 0, nrow = dim(img_tensor)[1], ncol = round(size/2))  
  padx<-array_reshape(padx, c(1024,round(size/2,1),1))
  dim(padx)
  dim(img_tensor)
  library(abind)
  img_tensor<-abind(padx, img_tensor, along=2); img_tensor<-abind(img_tensor, padx, along=2)
  pady<-matrix(data = 0, nrow = round(size/2), ncol = dim(img_tensor)[2])  
  pady<-array_reshape(pady, c(round(size/2),dim(img_tensor)[2],1))
  img_tensor<-abind(pady, img_tensor, along=1); img_tensor<-abind(img_tensor, pady, along=1)
  #reshape and normalize
  nrows<-dim(img_tensor)[1]; ncols<-dim(img_tensor)[2]
  img_tensor<-array_reshape(img_tensor, c(1,nrows,ncols,1))
  img_tensor<-img_tensor/255
  original_tensor<-array_reshape(original_tensor, c(1,1024,1024,1))
  original_tensor<-original_tensor/255
  #We transpose the matrix so zero is at the top left corner like in the image 
  mtimage<-drop(img_tensor)
  tmtimage<-t(mtimage)
  tmtimage<-array_reshape(tmtimage, c(1,nrows,ncols,1))
  #categories learned:
  #type_names<-c("cavicle", "rosette","typical", "empty", "ecm", "cytop", "er", "mito", "ves")
  #0 cavicle ; #1 rosette ; #2 typical ; #3 empty ; #4 ecm; #5 cytop; #6 er; #7 mito; #8 ves;
  
  #########################################################
  #########################################################
  #########################################################
  #########################################################
  ###PART1: DEEP LEARNING MODEL APPLICATION
  #########################################################
  #########################################################
  #########################################################
  #########################################################
  
  #########################################################
  ###Divide image in grids and obtain predictions
  #########################################################
  library(keras)
  cuadricula<-function(displacement, size){
    #size must be a multiple of displacement
    npxseqs<-size/displacement
    if (round(npxseqs, digits=0) == npxseqs)  { #if size/displacement is a whole number without decimals
      secuencia<-seq(1,nrows, by=1)
      multiplo <- rep(NA, length(secuencia))
      for (i in 1:length(secuencia)) {
        multiplo[i]<-size*secuencia[i]
      }
      multiplo<-multiplo[multiplo <(nrows+1)]
      max<-max(multiplo)
      lateral<-(nrows-max)/2  #searches for the bigger multple of size that is smaller than image size, to divide the image in equally sized grids
  
      thepoints<-seq(lateral-displacement,(nrows-lateral-size+displacement), by=displacement)
      totaltst<-length(thepoints)*length(thepoints)
      print(paste("total number of tests ", totaltst, sep=""))
       par(cex=0.7, mai=c(0.01,0.01,0.01,0.01))
       par(mfrow=c(1,1))
       plot(as.raster(img_tensor[1,,,]))
       plot(as.raster(img_tensor[1,,,])) #paints the image
       for (j in seq(lateral-displacement,(nrows-lateral-size+displacement), by=displacement)) {
         for (i in seq(lateral-displacement,(ncols-lateral-size+displacement), by=displacement)) {
           points( ((i+size - i)/2)+i ,    (((nrows-(j+size))  - (ncols-j))/2)+ (ncols-j)  , col="magenta", cex=0.01)
        }
       } #paints central points of the grid
      
      predictiondf <- data.frame(prediction=character(), xcoord=character(), 
                                 ycoord=character(), stringsAsFactors=FALSE) 
      allprobdf <- data.frame(cavicle=numeric(), typical=numeric(),rosette=numeric(),empty=numeric(),
                              ecm=numeric(),cytoplasm=numeric(),xcoord=character(), 
                                 ycoord=character(), stringsAsFactors=FALSE) 
      acount=0
      for (i in thepoints){
        for (j in thepoints) {
          acount=acount+1
          if( (round(acount/100) - acount/100)  ==0) {print(paste(acount, " tests of a total ", totaltst, sep=""))}
          miniimg<-tmtimage[ , i:(i+(size-1)), j:(j+(size-1)) , ]
          if (size==100){
            miniimg<-array_reshape(miniimg, c(1,size,size,1))
            allprob<-modelcavnocav %>% predict(miniimg)
          } else {
            miniimg<-tmtimage[ , i:(i+(size-1)), j:(j+(size-1)) , ]
            miniimg<-array_reshape(miniimg, c(size,size,1))
            miniimg<-image_array_resize(miniimg, 100,100)
            miniimg<-array_reshape(miniimg, c(1,100,100,1))
            allprob<-modelcavnocav %>% predict(miniimg)
          }
          xcoord<-i+(size/2); ycoord<-j+(size/2)
          datpr<-c(allprob, xcoord, ycoord)
          allprobdf <- rbind(datpr, allprobdf)
        }
      } #Makes a crop in every grid center and applies a model. If size is different from 100, it reshapes the image so the model is applied in 100x100 images
      colnames(allprobdf)<-c("cavicle", "typical","rosette","empty","ecm","cytoplasm","er", "mito", "ves",  "xcoord", "ycoord")
      return(allprobdf)
    } else {message("ERROR: size must be a multiple of displacement")}
  }
  size=windowsize; displacement=pxdisplacement
  
  if (mode == "PREDICT") {
    predictionlist<-cuadricula(displacement,size)
    write.table(predictionlist, paste(path, paste(element, "_PREDICTIONLIST.txt", sep=""), sep="/"))
  } else {
    predictionlist<-read.csv(paste(path, paste(element, "_PREDICTIONLIST.txt", sep=""), sep="/"), sep="")
  }

  allprobdf<-predictionlist
  allprobdf$class<-apply( allprobdf[,1:9], 1, which.max)
  allprobdf$class<-allprobdf$class-1
  allprobdf$pcav<-allprobdf$cavicle+allprobdf$typical+allprobdf$rosette
  allprobdf$pnocav<-1-allprobdf$pcav
  allprobdf$pcav_pnocav=allprobdf$pcav-allprobdf$pnocav
  #reassign to caveolae if cavicle+rosette+typical more likely than any other category
  for (rr in 1:nrow(allprobdf)) {
    pc_pnoc<- allprobdf$pcav_pnocav[rr]
    pcav <-allprobdf$pcav[rr]
    pcyto<-allprobdf$cytoplasm[rr]
    pecm<-allprobdf$ecm[rr]
    pld<-allprobdf$empty[rr]
    per<-allprobdf$er[rr]
    pmito<-allprobdf$mito[rr]
    pendo<-allprobdf$ves[rr]
    if (pcav > pcyto & pcav > pecm &  pcav > pld & pcav > per & pcav > pmito & pcav > pendo ) {
      mostlikelycav<-which.max(allprobdf[rr,1:3])[[1]]
      allprobdf$class[rr]<-mostlikelycav-1
    }
  }
  #write.table(allprobdf, paste(path, paste(element, "_OUTPUT.txt", sep=""), sep="/"))
  predictiondf<-data.frame(allprobdf$class, allprobdf$xcoord, allprobdf$ycoord)
  
  #we modify the prediction dataframe and create interesting variables for plots
  colnames(predictiondf)<-c("prediction", "xcoord", "ycoord") #xcoord and ycoord are the coordinates of the grid center

  predictiondf$pchcodes<-ifelse(grepl(0,predictiondf$prediction),0,0)
  predictiondf$pchcodes<-ifelse(grepl(1,predictiondf$prediction),0,predictiondf$pchcodes)
  predictiondf$pchcodes<-ifelse(grepl(2,predictiondf$prediction),0,predictiondf$pchcodes)
  predictiondf$pchcodes<-ifelse(grepl(3,predictiondf$prediction),1,predictiondf$pchcodes)
  predictiondf$pchcodes<- ifelse(grepl(4,predictiondf$prediction),1,predictiondf$pchcodes)
  predictiondf$pchcodes<- ifelse(grepl(5,predictiondf$prediction),1,predictiondf$pchcodes)
  predictiondf$pchcodes<- ifelse(grepl(6,predictiondf$prediction),1,predictiondf$pchcodes)
  predictiondf$pchcodes<- ifelse(grepl(7,predictiondf$prediction),1,predictiondf$pchcodes)
  predictiondf$pchcodes<- ifelse(grepl(8,predictiondf$prediction),1,predictiondf$pchcodes)
  
  predictiondf$colorcodes<-ifelse(grepl(0,predictiondf$prediction),"black",0)
  predictiondf$colorcodes<-ifelse(grepl(1,predictiondf$prediction),"black",predictiondf$colorcodes)
  predictiondf$colorcodes<-ifelse(grepl(2,predictiondf$prediction),"black",predictiondf$colorcodes)
  predictiondf$colorcodes<-ifelse(grepl(3,predictiondf$prediction),"cyan",predictiondf$colorcodes)
  predictiondf$colorcodes<- ifelse(grepl(4,predictiondf$prediction),"magenta",predictiondf$colorcodes)
  predictiondf$colorcodes<- ifelse(grepl(5,predictiondf$prediction),"yellow",predictiondf$colorcodes)
  predictiondf$colorcodes<- ifelse(grepl(6,predictiondf$prediction),"pink",predictiondf$colorcodes)
  predictiondf$colorcodes<- ifelse(grepl(7,predictiondf$prediction),"steelblue3",predictiondf$colorcodes)
  predictiondf$colorcodes<- ifelse(grepl(8,predictiondf$prediction),"khaki",predictiondf$colorcodes)
  
  predictiondf$cexcodes<-ifelse(grepl(0,predictiondf$prediction),5,0)
  predictiondf$cexcodes<-ifelse(grepl(1,predictiondf$prediction),5,predictiondf$cexcodes)
  predictiondf$cexcodes<-ifelse(grepl(2,predictiondf$prediction),5,predictiondf$cexcodes)
  predictiondf$cexcodes<-ifelse(grepl(3,predictiondf$prediction),1,predictiondf$cexcodes)
  predictiondf$cexcodes<- ifelse(grepl(4,predictiondf$prediction),1,predictiondf$cexcodes)
  predictiondf$cexcodes<- ifelse(grepl(5,predictiondf$prediction),1,predictiondf$cexcodes)
  predictiondf$cexcodes<- ifelse(grepl(6,predictiondf$prediction),1,predictiondf$cexcodes)
  predictiondf$cexcodes<- ifelse(grepl(7,predictiondf$prediction),1,predictiondf$cexcodes)
  predictiondf$cexcodes<- ifelse(grepl(8,predictiondf$prediction),1,predictiondf$cexcodes)
  
  predictiondf$X<-predictiondf$xcoord-(size/2)
  predictiondf$Y<-predictiondf$ycoord-(size/2) #X and Y are the corners of the grids
  
  predictiondf<-subset(predictiondf, xcoord>min(predictiondf$xcoord) & xcoord < max(predictiondf$xcoord) &
                         ycoord > min(predictiondf$ycoord) & ycoord < max(predictiondf$xcoord))
  #plot with initial cell prediction
  par(cex=0.7, mai=c(0.01,0.01,0.01,0.01))
  layout(matrix(c(1, 1, 2,
                  1, 1, 2,
                  1, 1, 2), nrow=3, byrow=TRUE))
  plot(as.raster(img_tensor[1,,,])) 
  for (i in 1:(dim(predictiondf)[1])) {
    if (predictiondf$pchcodes[i] == 0) {
      points(predictiondf$xcoord[i],  nrows-(predictiondf$ycoord[i]), col= predictiondf$colorcodes[i], pch=16, cex=0.7)
    } else { if ( sample(0:1, 1, replace=T,prob=c(0,1)) == 1) { #paint only a random 10% #antes prob=c(0.9,0.1)) == 1)
      points(predictiondf$xcoord[i],  nrows-(predictiondf$ycoord[i]), col= predictiondf$colorcodes[i], pch=16, cex=0.7)
    }
    }
  }
  plot(0,type='n',axes=FALSE,ann=FALSE)
  legend("center", legend=c("Caveolae", "LD/empty", "ECM", "cytoplasm", "er", "mito", "ves"),
         col=c("black", "cyan", "magenta","yellow", "pink", "steelblue3", "khaki"), pch=c(16,16,16,16,16,16), cex=1.7, box.lty=0)
  dev.print(svg, paste(path, paste(element, "_PREDPLOT.svg", sep=""), sep="/"))
  
  
  #########################################################
  #########################################################
  #########################################################
  #########################################################
  ###PART2: CAVEOLAE
  #########################################################
  #########################################################
  #########################################################
  #########################################################
  
  
  #########################################################
  ###We classifiy caveolae predictions by neighbours
  #########################################################
  precavlocs<-subset(predictiondf, pchcodes == 0) #only caveolar predictions
  if (FindCellLimits ==TRUE){#if cell limits required, we obtain other dataframes with predictions of ecm, cytoplasm and ld
    precytolocs<-subset(predictiondf, prediction == 0 | prediction ==1 | prediction == 2 | prediction == 5| prediction == 6| prediction == 7| prediction == 8) 
    preemptylocs<-subset(predictiondf, prediction == 3) 
    preecmlocs<-subset(predictiondf, prediction == 4) 
  }
  
  #calculate neighbour number for each caveolae coordinate and classify
  #caveolar coordinates into inner (8 neighbours), borders (<8 and >=3 neighbours), or isolated (<3 neighbours)

  precavlocs$neigh<-rep(NA, dim(precavlocs)[1])
  precavlocs$type<-rep(NA, dim(precavlocs)[1])
  for (i in 1:(dim(precavlocs)[1])) {
    neighlist<-getneighbours(i, precavlocs, precavlocs)
    neighn=(dim(neighlist)[1])
    precavlocs$neigh[i]<-neighn
    if (neighn  < 4 ) { precavlocs$type[i]<-"solos" }
    if (neighn  < 9 & neighn >= 4 ) { precavlocs$type[i]<-"borders" }
    if (neighn  >= 9 ) { precavlocs$type[i]<-"in" }
  }
  precavlocsborders<-subset(precavlocs, neigh < 9 & neigh >= 4)
  precavlocsIN<-subset(precavlocs, neigh >=9)
  
  precavlocs$colorcodes<-ifelse(grepl("in",precavlocs$type),"chocolate4",0)
  precavlocs$colorcodes<-ifelse(grepl("borders",precavlocs$type),"lightseagreen",precavlocs$colorcodes)
  precavlocs$colorcodes<-ifelse(grepl("solos",precavlocs$type),"coral",precavlocs$colorcodes)
  par(cex=0.7, mai=c(0.01,0.01,0.01,0.01))
  layout(matrix(c(1), nrow=1, byrow=TRUE))
  plot(as.raster(img_tensor[1,,,])) 
  points(precavlocs$xcoord, nrows-precavlocs$ycoord, col="black", pch=15,cex=0.4)
  plot(as.raster(img_tensor[1,,,])) 
  points(precavlocs$xcoord, nrows-precavlocs$ycoord, col=precavlocs$colorcodes, pch=15,cex=0.4)
  
  #we do some modifications to remove noisy caveolae predicitons, to leave a caveolae area more compact and roundish
  #we eliminate borders that only contact other borders or isolated. Leave only borders that have at least one inner contact
  precavlocs$type<-as.factor(precavlocs$type)
  precavlocsborders$type2<-rep(NA, dim(precavlocsborders)[1])
  for (i in 1:(dim(precavlocsborders)[1])) {
    neighlist<-getneighbours(i, precavlocsborders, precavlocs, ifsame=FALSE)
    neighlisttable<- table (neighlist$type)
    neighlisttabledf<-as.data.frame(neighlisttable)
    if (neighlisttabledf$Freq[2] == 0 ) { precavlocsborders$type2[i]<-"noINcontact" } else { precavlocsborders$type2[i]<-"INcontact"}
    }
  
  precavlocsbordersfinal<-subset(precavlocsborders, type2 == "INcontact")
  precavlocsbordersfinals<-precavlocsbordersfinal[1:9]
  precavlocsIN<-precavlocsIN[1:9]
  precavlocsbinfinal <- rbind(precavlocsbordersfinals, precavlocsIN)
  precavlocs2<-precavlocs
  precavlocs<-precavlocs[1:9]
  
  plot(as.raster(img_tensor[1,,,])) 
  points(precavlocsbinfinal$xcoord, nrows-precavlocsbinfinal$ycoord, col=precavlocsbinfinal$colorcodes, pch=15,cex=0.4)
  
  #########################################################
  ###Finding caveolae clusters
  #########################################################
  groupdf <- data.frame(prediction=character(), xcoord=numeric(), ycoord=numeric(),
                        pchcodes=character(), colorcodes=character(), cexcodes=character(),
                        X=numeric(), Y=numeric(),
                        neigh=character(), group=numeric(), stringsAsFactors=FALSE) 
  
  groupdf2<-sortingroups(groupdf, precavlocsbordersfinals, precavlocsbinfinal, 9)
  groupdf2$group<-as.factor(groupdf2$group)
  
  
  #we further separate bigger groups. We separate in groups using only the inner points, ignoring the borders
  ngroupsinit<-length(unique(groupdf2$group))
  onlyIN<-subset(groupdf2, neigh == 9)
  onlyBORDER<-subset(groupdf2, neigh <9)
  
  if (nrow(onlyIN) >0 & nrow(onlyBORDER) >0) {
    groupdf <- data.frame(prediction=character(), xcoord=numeric(), ycoord=numeric(),
                          pchcodes=character(), colorcodes=character(), cexcodes=character(),
                          X=numeric(), Y=numeric(),
                          neigh=character(), group=numeric(), stringsAsFactors=FALSE) 
    
    groupdf3<-sortingroups(groupdf, onlyIN[, 1:10], onlyIN[, 1:10], 9)
    groupdf3$group<-as.factor(groupdf3$group)
    
    #now we sort the borders into the corresponding groups
    newgroups <- groupdf3 
    for (case in 1:(dim(onlyBORDER)[1])) {
      rowtobind<-onlyBORDER[case,]
      neighs<-getneighbours(case, onlyBORDER, groupdf3, ifsame=FALSE)
      neighs$group<-as.character(neighs$group)
      finalgroup <- unique(neighs$group)[1]
      finalcolor<- unique(neighs$colorcodes2)[1]
      rowtobind$group <-finalgroup
      rowtobind$colorcodes2 <-finalcolor
      newgroups<-rbind(newgroups, rowtobind)
    }
    groupdf2<-newgroups
  }
  plot(as.raster(img_tensor[1,,,])) 
  points(groupdf2$xcoord, nrows-groupdf2$ycoord, col=groupdf2$colorcodes2, pch=15,cex=0.4)
  
  
  #########################################################
  ###Finding isolated caveolae
  #########################################################

  mincavsize=floor(mincavnm/pixelsize)
  mincav<- floor((mincavsize/displacement)^2)
  mincav2<-0.8*mincav
  maxcavsize=floor(maxcavnm/pixelsize)
  maxcav<- floor((maxcavsize / displacement)^2)
  avcavsize<-floor(avcavnm/pixelsize)
  avcav<-floor((avcavsize / displacement)^2)

  if (nrow(onlyIN) >0 & nrow(onlyBORDER) >0) {
    grtable<-table(groupdf2$group)
    grtable<-as.data.frame(grtable)
    grtableindiv<-subset(grtable, Freq > mincav2 & Freq <maxcav)
    sizindiv<-mean(grtableindiv$Freq)
    if (is.na(sizindiv)) {
      sizindiv = avcav
      print ("We could not find any isolated caveolae to estimate caveolae size. We use average of 70 nm")
    }

    #we separate groups with individual caveolae
    grtableindiv2<-subset(grtable,  Freq > 0.8*mincav & Freq < maxcav) #we allow it to be slightly smaller than minimum in case some caveolae are not identified through their whole area
    library(dplyr)
    groupdfindiv<-filter(groupdf2, group %in% grtableindiv2$Var1) #we filter caveolar coordinates possibly belonging to individual caveolae
    
    circvector<-rep(NA, dim(grtableindiv2)[1])
    countcirc<-1
    for (gr in grtableindiv2$Var1) {
      prueba<-subset(groupdfindiv, group==gr)
      prueba2<-precavlocs2[, c("xcoord","ycoord","type")] #precavlocs2 is the df with caveolae classified in inner, isolated and borders
      prueba3<-merge(prueba, prueba2) #merge coordinates of group gr and information about neighbours
      bprueba3<-subset(prueba3, type=="borders") #get only border coordinates
      indexmin=1
      remainingrows<-dim(bprueba3)[1]
      vectordist<-rep(NA, dim(bprueba3)[1]-1)
      roundcount<-1
      while (remainingrows >1) {
        case<-bprueba3[indexmin, ]
        bprueba3<-bprueba3[ -c(indexmin), ]
        minim=9999999999999999999999999999999999999999999999999999999999
        for (bb in 1:(dim(bprueba3)[1])){
          distc<-sqrt ((case$xcoord-bprueba3$xcoord[bb])^2 + (case$ycoord-bprueba3$ycoord[bb])^2)
          if (distc < minim) {
            minim=distc
            indexmin<-bb
          }
        }
        vectordist[roundcount]<-minim
        roundcount<-roundcount+1
        remainingrows<-dim(bprueba3)[1]
      }#this loop gets one border coordinate, then finds the minimum distance with the nearest neighour coordinate. And then the nearest of the nearest. And so on.
      #adding all distances estimates the perimeter
      perim=sum(vectordist)
      area<-dim(prueba)[1]*100
      circ<-4*pi*(area/(perim^2))
      circvector[countcirc]<-circ
      countcirc<-countcirc+1
    }
    
    grtableindiv2$circ<-circvector
    
    avsizetocompare<-c(sizindiv, avcav)[which.min(c(sizindiv, avcav))]
    
    grtableindiv2.2<-subset(grtableindiv2, Freq<=avsizetocompare | (Freq>avsizetocompare & circ>=0.96  )) # we also tried 0.85
    grtableNOindiv2.2<-subset(grtableindiv2, Freq>avsizetocompare & circ<0.96  )
    groupdfindiv<-filter(groupdf2, group %in% grtableindiv2.2$Var1)
    #groupdfindiv has the coordinates of caveolae that are likely individual
    
    par(cex=0.7, mai=c(0.01,0.01,0.01,0.01))
    plot(as.raster(img_tensor[1,,,])) 
    points(precavlocsbinfinal$xcoord, nrows-precavlocsbinfinal$ycoord, col=precavlocsbinfinal$colorcodes, pch=15,cex=0.5)
    points(groupdfindiv$xcoord, nrows-groupdfindiv$ycoord, col="magenta", pch=15,cex=0.5)
    
    grtablemulti<-subset(grtable, Freq >= maxcav) 
    grtablemulti<-rbind(grtablemulti, grtableNOindiv2.2[,1:2]) #big and small non circular
    
    groupdfindivaverageX<-flatsummaryby(mean, groupdfindiv$xcoord, groupdfindiv$group)
    groupdfindivaverageY<-flatsummaryby(mean, groupdfindiv$ycoord, groupdfindiv$group)
    groupdfindivaverage<-merge(groupdfindivaverageX,groupdfindivaverageY)
    groupdfindivaverage<-groupdfindivaverage[complete.cases(groupdfindivaverage), ]
    colnames(groupdfindivaverage)<-c("group", "xcoord", "ycoord") #groupdfindivaverage has central coordinates of individual caveolae
    
    #confirm caveolae prediction from central coordinate
    #recalculate sizindiv only with confirmed
    precoordsandsize<-grtableindiv2.2
    colnames(precoordsandsize)[1]<-"group"
    coordsandsize<-merge(precoordsandsize, groupdfindivaverage, by="group")
    
    coordsandsize2<-coordsandsize
    if (nrow(coordsandsize2) >0) {
      newcropsize= round (148*(pxum/1000))
      for (xxxx in 1:length(coordsandsize$xcoord)) {
        ix<-coordsandsize$xcoord[xxxx]
        jx<-coordsandsize$ycoord[xxxx]
        newcrop<-tmtimage[ , (ix-((newcropsize-1)/2)):(ix+((newcropsize+1)/2)), (jx-((newcropsize-1)/2)):(jx+((newcropsize+1)/2)) , ]
        newcrop<-array_reshape(newcrop, c((newcropsize+1) ,(newcropsize+1),1))
        newcrop<-image_array_resize(newcrop, 100,100)
        newcrop<-array_reshape(newcrop, c(1,100,100,1))
        probnewcrop<-modelcavnocav %>% predict(newcrop)
        probcaveolae<- probnewcrop[1]+ probnewcrop[2]+probnewcrop[3] 
        if(probcaveolae < probnewcrop[4] & probcaveolae < probnewcrop[5]& probcaveolae < probnewcrop[6]& probcaveolae < probnewcrop[7]& probcaveolae < probnewcrop[8]& probcaveolae < probnewcrop[9]) {
          coordsandsize2[xxxx, ] <-NA
        }
      }
      coordsandsize2<-coordsandsize2[complete.cases(coordsandsize2), ]
      
      if (nrow(coordsandsize2)>0){
        newsizindiv<-mean(coordsandsize2$Freq)
      } else { newsizindiv <- avcav}
      
    } else { newsizindiv <- avcav}

    par(cex=0.7, mai=c(0.01,0.01,0.01,0.01))
    plot(as.raster(img_tensor[1,,,])) 
    points(groupdfindivaverage$xcoord, nrows-groupdfindivaverage$ycoord, col="magenta", pch=16,cex=1.5)
    #get probabilities of corrected caveolae
    probscav <- allprobdf[0,]
    for (i in 1:nrow(groupdf2)) {
      coinciden<-subset(allprobdf, xcoord==groupdf2$xcoord[i] & ycoord==groupdf2$ycoord[i])
      probscav<-rbind(probscav, coinciden)
    }

    probscav$PCAV<-probscav$cavicle + probscav$typical + probscav$rosette
    probscav$PNOCAV<-1-probscav$PCAV
    probscav$PNOCAV[probscav$PNOCAV == 0] <-1.0e-10
    probscav$PNOCAV[probscav$PNOCAV < 0] <-1.0e-10
    probscav$GridY2<-(nrows)-(probscav$ycoord)
    rbPal <- colorRampPalette(c('yellow','magenta'))
    probscav$Colors<- rbPal(20)[as.numeric(cut(log(probscav$PNOCAV),breaks = 20))]
    probscav<-probscav[complete.cases(probscav), ]

    probscav$GridX2<-probscav$xcoord
    par(cex=0.7, mai=c(0.01,0.01,0.01,0.01))
    layout(matrix(c(1, 1, 1, 1, 1, 2,
                    1, 1, 1, 1, 1,  2,
                    1, 1, 1, 1, 1, 2), nrow=3, byrow=TRUE))
    plot(as.raster(img_tensor[1,,,])) 
    points(probscav$GridY2~probscav$GridX2, col=probscav$Colors, cex=1, pch=16)
    coord <- par("usr")
    par(cex=0.4, mai=c(1,0.3,0.5,0.2))
    color.bar <- function(lut, min, max=-min, nticks=11, ticks=seq(min, max, len=nticks), title='', x1=0.1, x2=1, cex.axis=2) {
      scale = (length(lut)-1)/(max-min)
      plot(c(x1,x2), c(min,max), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='', main=title)
      ticks2<-format(round(ticks, 1), nsmall = 1)
      axis(2, ticks2, las=1, cex.axis=cex.axis)
      for (i in 1:(length(lut)-1)) {
        y = (i-1)/scale + min
        rect(0.9,y,1,y+1/scale, col=lut[i], border=NA)
      }
    }
    mmi<-min(log(probscav$PNOCAV));   mma<-max(log(probscav$PNOCAV))
    color.bar(colorRampPalette(c('yellow','magenta'))(50), min=mmi, max=mma, x1=1, x2=0.9, nticks=5)
    mtext("log(P(noCAV))", side = 3, line = -0.5, cex = 0.8)
  }
  

  #########################################################
  ###We run cluster analysis to further separate caveolae
  #########################################################

  if (nrow(onlyIN) >0 & nrow(onlyBORDER) >0) {
    library(dplyr)
    groupdfmulti<-filter(groupdf2, group %in% grtablemulti$Var1) #caveolae that are likely clustered
    newcropsize= round (148*(pxum/1000))
    cx<-round(newcropsize/2); cy<-round(newcropsize/2)
    cornerx<-round(1); cornery<-round(1)
    mat <- matrix( , nrow = newcropsize, ncol = newcropsize)
    for (i in cornerx:(cornerx+(newcropsize-1))) {
      for (j in cornery:(cornery+(newcropsize-1))){
        distance<-sqrt ((cx-i)^2 + (cy-j)^2)
        mat[i,j]<-distance
      }
    }
    
    distancemat<-mat #distance matrix centered in zero
    #cluster analysis to find caveolae centres
    centerpoints<-groupdfindivaverage
    centerpoints$cls<-rep(0, nrow(centerpoints)) #NEW
    cluspoints<-data.frame(groupdfindiv$xcoord, groupdfindiv$ycoord, groupdfindiv$group)
    colnames(cluspoints)<-c("xcoord", "ycoord", "group")
    cluspoints$cls<-rep(NA, nrow(cluspoints))
    cluspoints$option<-rep(NA , nrow(cluspoints))
    for (i in levels(droplevels(groupdfmulti$group))) { #LOOP through each caveolae cluster
      groupdfmulti2<-subset(groupdfmulti, group == i)
      groupdfmulti2simple<-groupdfmulti2[, c("xcoord", "ycoord")]
      #we decide number of clusters depending on area size
      karea<-(dim(groupdfmulti2simple)[1])/newsizindiv #new
      nclusters<-round(0.44+0.66*karea) 
      #print(c("nclusters: ", nclusters))
      nclvector<-c(nclusters-1, nclusters, nclusters+1) #possible k k
      nclvector<-nclvector[nclvector >0]#zero clusters not possible
      if (length(nclvector) < 3) {
        nclvector[3]<-nclvector[1] #we repeat one option or the code breaks
      }
      nclvector<-rep(nclvector, 10)
      
      probsgroup <- probscav[0,]
      for (tt in 1:nrow(groupdfmulti2simple)) {
        coinciden<-subset(probscav, xcoord==groupdfmulti2simple$xcoord[tt] & ycoord==groupdfmulti2simple$ycoord[tt])
        probsgroup<-rbind(probsgroup, coinciden)
      }

      #we duplicate rows proportional to probability
      probsgroup$repetitions<-round(1*-log(probsgroup$PNOCAV))
      wgroup<- probsgroup[0,]
      for (abc in 1:nrow(probsgroup)) {
        wrow<-probsgroup[abc,]
        timesrep<-wrow$repetitions
        if (is.na(timesrep) == TRUE) { timesrep = 50}
        while (timesrep > 0) {
          wgroup<-rbind(wgroup, wrow)
          timesrep <- timesrep-1
        }
      }
      wgroupsimple<-data.frame(wgroup$xcoord, wgroup$ycoord)
      colnames(wgroupsimple)<-c("xcoord", "ycoord")
      wgroupsimplebu<-groupdfmulti2simple
      groupdfmulti2simple<-wgroupsimple
      
      #print(paste("group:", i, "; nclvector:", nclvector[1], nclvector[2], nclvector[3], sep=" "))
      resultvector<-rep(NA, length(nclvector))
      sdsizevector<-rep(NA, length(nclvector))
      circvector<-rep(NA, length(nclvector))
      probvector<-rep(NA, length(nclvector))
      checkbordervector<-rep(NA, length(nclvector))
      opcenterdf<-data.frame(xcoord=numeric(), ycoord=numeric(), option=numeric())
      opclusterpointsdf<-data.frame(xcoord=numeric(), ycoord=numeric(), cls=numeric(), option=numeric())
      for (option in 1:(length(nclvector))) { #loop through options of k
        #wgroupsimplebu is the df without row duplicaton
        kinit<-kmeans(wgroupsimplebu, nclvector[option])
        cl<-kmeans(groupdfmulti2simple, kinit$centers)
        centers<-as.data.frame(cl$centers)
        datcl<-groupdfmulti2simple; datcl$cls<-cl$cluster; datcl$cls <-as.factor(datcl$cls)
        datcl<-unique(datcl)
        #circularity
        circclvector<-rep(NA, length(levels(datcl$cls)))
        counterbb=0
        for (bb in levels(datcl$cls)) {
          counterbb=counterbb+1
          clus<-subset(datcl, cls == bb) #subset each cluster
          classvector<-rep(NA, dim(clus)[1]) 
          for (rw in 1:(dim(clus)[1])) { #for each of the cluster points
            neighL<-getneighbours(rw, clus, clus, ifsame=FALSE)
            if ((dim(neighL)[1]) < 8) { #is a borde
              classvector[rw] <- "BORDER"
            }
            if ((dim(neighL)[1]) == 8) { #is an INNER
              classvector[rw] <- "INNER"
            }
          }
          clus$types<-classvector; clus$types<-as.factor(clus$types)
          clusB<-subset(clus, types == "BORDER"); clusI<-subset(clus, types == "INNER")
          #circularity
          clperim = dim(clusB)[1] # pxdisplacement is an estimation, calculating the exact distance takes too long

          clarea <- dim(clus)[1] 
          clcirc<-(clarea/(clperim^2))
          circclvector[counterbb] <-clcirc
        } #loop circularity
        medcirc<-median(circclvector)
        circvector[option]<-medcirc

        #sd area
        sdsize<-sd(as.data.frame(table(datcl$cls))$Freq)
        sdsizevector[option]<-sdsize
        centers$group<-rep(i, dim(centers)[1])
        sumresultvector<- rep(NA, dim(centers)[1])
        for (ii in 1:(dim(centers)[1])) { #intensity
          cx2<-centers$xcoord[ii]
          cy2<-centers$ycoord[ii]
          cornerx2<-cx2-((newcropsize/2)-1)
          cornery2<-cy2-((newcropsize/2)-1)
          surimg<-tmtimage[ , cornerx2:(cornerx2+(newcropsize-1)), cornery2:(cornery2+(newcropsize-1)) , ]
          intensitymat<-surimg
          resultmat<-intensitymat/(0.1+2*distancemat)
          sumresult<-sum(resultmat) #we want this for every cluster centre
          sumresultvector[ii]<-sumresult
        }
        avresult<-median(sumresultvector)
        resultvector[option]<-avresult
        probsv<-rep(NA, dim(centers)[1])
        if (exists("probsgroup") == TRUE) {
          for (ii in 1:(dim(centers)[1])) { #probability
            cx2<-centers$xcoord[ii];         cy2<-centers$ycoord[ii]
            probabs<-subset(probsgroup, xcoord > cx2-(2*displacement) & xcoord < cx2+(2*displacement) &
                              ycoord > cy2-(2*displacement) & ycoord < cy2+(2*displacement))
            sumpr<-sum(-log(probabs$PNOCAV))
            probsv[ii]<-sumpr
          }
          meanpr<-median(probsv)
          probvector[option]<-meanpr
        } else {probvector[option]<-0}
        
        #save center options and cluster points
        centerdf<-data.frame(centers$xcoord, centers$ycoord)
        colnames(centerdf)<-c("xcoord", "ycoord")
        centerdf$option<-rep(option, nrow(centerdf))
        opcenterdf<-rbind(opcenterdf, centerdf)
        
        pointsdf<-datcl
        pointsdf$option<-rep(option, nrow(pointsdf))
        opclusterpointsdf<-rbind(opclusterpointsdf, pointsdf)
        
        #border proximity
        groupborders<-subset(groupdfmulti2, neigh <9)
        tableforcl<-as.data.frame(table(datcl$cls))
        tableforcl$bordercontacts<-rep(NA, nrow(tableforcl))
        for (uu in 1:nrow(centerdf)) {
          closeborders<-subset(groupborders, xcoord < centerdf$xcoord[uu]+(displacement+1) & xcoord > centerdf$xcoord[uu]- (displacement+1) &
                                 ycoord < centerdf$ycoord[uu]+(displacement+1) & ycoord > centerdf$ycoord[uu]- (displacement+1) )
          tableforcl$bordercontacts[uu]<-nrow(closeborders)
        }
        frcontacts<-sum(tableforcl$bordercontacts, na.rm = TRUE)/length(tableforcl$bordercontacts)
        checkbordervector[option]<-frcontacts
      } #loop through k options ends
      
      #get a dataframe also with the coordinates belonging to each cluster
      colnames(opcenterdf)<-c("xcoord", "ycoord", "option") #dataframe with all possible sets of cluster centres
      
      #calculate score. select k that obtains higher score
      ncircvector<-circvector/max(circvector)
      nsdsizevector<-(-1*(sdsizevector/max(sdsizevector)))
      nresultvector<-resultvector/max(resultvector)
      if (max(checkbordervector) == 0 ) {ncheckbordervector<- rep(0, length(checkbordervector))} else {
        ncheckbordervector<-(-1*(checkbordervector/max(checkbordervector)))
      }
      if(max(probvector) >0) {nprobvector<-probvector/max(probvector)} else {
        nprobvector<-rep(0, length(probvector))
      }
      scorevector<-1*ncircvector+1*nsdsizevector+1*nresultvector+1*nprobvector+1*ncheckbordervector
      scorevectorA<-scorevector[seq(1, length(scorevector), 3)]
      scorevectorB<-scorevector[seq(2, length(scorevector), 3)]
      scorevectorC<-scorevector[seq(3, length(scorevector), 3)]
      
      comparescore<-c(sum(scorevectorA), sum(scorevectorB), sum(scorevectorC))
      tmax<-which.max(comparescore)
      vectorlist<-list(scorevectorA, scorevectorB, scorevectorC)
      
      if (length(tmax)>0) {
        election<-which.max(vectorlist[[tmax]])
        vvv<-seq(tmax, length(scorevector), 3); elegircl<-vvv[election]
      }
      scorevector2<-1*ncircvector+1*nresultvector+1*nprobvector+1*ncheckbordervector
      scorevector2A<-scorevector2[seq(1, length(scorevector2), 3)]
      scorevector2B<-scorevector2[seq(2, length(scorevector2), 3)]
      scorevector2C<-scorevector2[seq(3, length(scorevector2), 3)]

      comparescore2<-c(sum(scorevector2A), sum(scorevector2B), sum(scorevector2C))
      tmax2<-which.max(comparescore2)
      vectorlist2<-list(scorevector2A, scorevector2B, scorevector2C)
      
      if (length(tmax2)>0) {
        election<-which.max(vectorlist2[[tmax2]])
        vvv2<-seq(tmax2, length(scorevector2), 5); elegircl2<-vvv2[election]
      }
      
      # if (nclusters > 2) {print(paste("scorevector: ", scorevector, sep=""))} else {
      #   print(paste("scorevector2: ", scorevector2, sep=""))
      # }
      
      if (nclusters>2) {
        centers<-subset(opcenterdf, option == elegircl)
        centers<-centers[, 1:2]
        centers$group<-rep(i, dim(centers)[1])
        centers$cls<-seq(1, nrow(centers)) #NEW
        centerpoints<-rbind(centerpoints, centers)
        
        clusterpoints<-subset(opclusterpointsdf, option == elegircl)
        clusterpoints$group<-rep(i, dim(clusterpoints)[1])
        cluspoints<-rbind(cluspoints, clusterpoints)
        
      } else {
        centers<-subset(opcenterdf, option == elegircl2)
        centers<-centers[, 1:2]
        centers$group<-rep(i, dim(centers)[1])
        centers$cls<-seq(1, nrow(centers)) #NEW
        centerpoints<-rbind(centerpoints, centers)
        
        clusterpoints<-subset(opclusterpointsdf, option == elegircl2)
        clusterpoints$group<-rep(i, dim(clusterpoints)[1])
        cluspoints<-rbind(cluspoints, clusterpoints)
      } 
      
    } #LOOP through each caveolae group ends
    #centerpoints has the centres of clustered caveolae joined to those of individual caveolae. The ones from individual caveolae are at the beginning

    cluspoints$cls[is.na(cluspoints$cls)] <- 0
    cluspoints$caveolae<-paste(cluspoints$group, cluspoints$cls, sep="_")
    cluspoints$colorcodes<- colors()[as.numeric(as.factor(cluspoints$caveolae))*3]
    
    par(cex=0.7, mai=c(0.01,0.01,0.01,0.01))
    layout(matrix(c(1, nrow=1, byrow=TRUE)))
    plot(as.raster(img_tensor[1,,,])) 
    points(cluspoints$xcoord,  nrows-(cluspoints$ycoord), col=cluspoints$colorcodes , pch=16, cex=0.7)
    points(centerpoints$xcoord,  nrows-(centerpoints$ycoord), col="darkblue" , pch=16, cex=1)
    #dev.print(svg, paste(path, paste(element, "_CLUSTERPOINTS_CAVEOLAEPSEUDOSEGMENTATION.svg", sep=""), sep="/"))
    
    par(cex=0.7, mai=c(0.01,0.01,0.01,0.01))
    layout(matrix(c(1, 1, 1, 1, 1, 2,
                    1, 1, 1, 1, 1,  2,
                    1, 1, 1, 1, 1, 2), nrow=3, byrow=TRUE))
    plot(as.raster(img_tensor[1,,,])) 
    points(probscav$GridY2~probscav$GridX2, col=probscav$Colors, cex=0.4, pch=16)
    points(centerpoints$xcoord,  nrows-(centerpoints$ycoord), col="darkblue" , pch=16, cex=0.7)
    text(centerpoints$xcoord-30,  nrows-(centerpoints$ycoord-30), centerpoints$group, col="white", cex=0.7, font=2)
    par(cex=0.4, mai=c(1,0.3,0.5,0.2))
    color.bar(colorRampPalette(c('yellow','magenta'))(50), min=mmi, max=mma, x1=1, x2=0.9, nticks=5)
    mtext("log(P(noCAV))", side = 3, line = -0.5, cex = 0.8)
    dev.print(svg, paste(path, paste(element, "_PROBSANDCOORDS.svg", sep=""), sep="/"))
    
    probscavhigh<-probscav
    probscavhigh$logPNOCAV<- -log(probscavhigh$PNOCAV)
    probscavhigh<-subset(probscavhigh, logPNOCAV < 5)

    par(cex=0.7, mai=c(0.01,0.01,0.01,0.01))
    par(mfrow=c(1,1))
    plot(as.raster(img_tensor[1,,,])) 
    points(groupdf2$xcoord,  nrows-(groupdf2$ycoord), col=groupdf2$colorcodes2 , pch=16, cex=0.5)
    points(centerpoints$xcoord,  nrows-(centerpoints$ycoord), col="magenta" , pch=16, cex=1)
    points(centerpoints$xcoord,  nrows-(centerpoints$ycoord), col="magenta" , pch=1, cex=0.5)
    text(centerpoints$xcoord-30,  nrows-(centerpoints$ycoord-30), centerpoints$group, col="white", cex=0.7, font=2)
    text(centerpoints$xcoord-30,  nrows-(centerpoints$ycoord-30), centerpoints$group, col="white", cex=0.7, font=2)
    dev.print(svg, paste(path, paste(element, "_CLUSTERPLOT.svg", sep=""), sep="/"))
    
    centerpoints$X<-centerpoints$xcoord-50
    centerpoints$Y<-centerpoints$ycoord-50
    centerpoints$X[centerpoints$X<0] <- 1
    centerpoints$Y[centerpoints$Y<0] <- 1
    
    #NOW WE FIND CAVEOLAE TYPES WITH TEMCavCNN
    FINALDF <- data.frame(prediction=character(), xcoord=character(), ycoord=character(), 
                          X=character(), Y=character(), stringsAsFactors=FALSE) 
    PROBSDF<-data.frame(cavicle=character(), rosette=character(),typical=character(),empty=character(),
                        ecm=character(),mitop=character(),xcoord=character(), ycoord=character(), 
                        X=character(), Y=character(), stringsAsFactors=FALSE) 
    if (dim(groupdfindivaverage)[1] > 0 ) { #extract crops from individual caveolae centres and predict cavelae categories
      for (i in 1:(dim(groupdfindivaverage)[1])) {   #remember groupdfindivaverage  centerpoints { 
        X<-centerpoints$xcoord[i]
        Y<-centerpoints$ycoord[i]
        distx<-ncols-X
        disty<-nrows-Y
        if (distx> newcropsize/2 | disty > newcropsize/2 ) { cropsize = newcropsize-1 } else { cropsize = windowsize-1}   
        crop<-tmtimage[ , (X-((cropsize-1)/2)):(X+((cropsize+1)/2)), (Y-((cropsize-1)/2)):(Y+((cropsize+1)/2)) , ]
        crop<-array_reshape(crop, c((cropsize+1) ,(cropsize+1),1))
        crop<-image_array_resize(crop, 100,100)
        crop<-array_reshape(crop, c(1,100,100,1))
        
        xcoord<-centerpoints$xcoord[i]
        ycoord<-centerpoints$ycoord[i]
        probabilities<-modelcavnocav %>% predict(crop)
        DAT2<-c(probabilities, xcoord, ycoord, X, Y)
        PROBSDF<-rbind(PROBSDF, DAT2)
        if ((DAT2[1]+DAT2[2]+DAT2[3]) > certainty) { #filter predictions by certainty
          prediction<-modelclascav %>% predict(crop)
          prediction<-which.max(prediction)-1
          X<-X+size/2; Y<-Y+size/2
          DAT<-c(prediction, xcoord, ycoord, X, Y)
          FINALDF <- rbind(FINALDF, DAT)
        }
      }
    }

    if (dim(centerpoints)[1] > dim(groupdfindivaverage)[1]  ) { 
      for (i in ((dim(groupdfindivaverage)[1])+1):((dim(centerpoints)[1]))) { #repeats same but for multicaveolae groups
        X<-centerpoints$xcoord[i]
        Y<-centerpoints$ycoord[i]
        xcoord<-centerpoints$xcoord[i]
        ycoord<-centerpoints$ycoord[i]
        
        distx<-ncols-X
        disty<-nrows-Y
        if (distx>= newcropsize/2 | disty >=newcropsize/2 ) { cropsize = newcropsize-1 } else { cropsize = windowsize-1}
        crop<-tmtimage[ , (X-((cropsize-1)/2)):(X+((cropsize+1)/2)), (Y-((cropsize-1)/2)):(Y+((cropsize+1)/2)) , ]
        crop<-array_reshape(crop, c((cropsize+1) ,(cropsize+1),1))
        crop<-image_array_resize(crop, 100,100)
        crop<-array_reshape(crop, c(1,100,100,1))
        
        probabilities<-modelcavnocav %>% predict(crop)  #it is going to be reassigned to rosette
        X<-X+size/2; Y<-Y+size/2
        DAT2<-c(probabilities, xcoord, ycoord, X, Y)
        PROBSDF<-rbind(PROBSDF, DAT2)
        if ((DAT2[1]+DAT2[2]+DAT2[3]) > certainty) {
          prediction<-1
          DAT<-c(prediction, xcoord, ycoord, X, Y)
          FINALDF <- rbind(FINALDF, DAT)
        }
      }
    }
    ##end of loop. clustered caveolae classified as rosettes
    colnames(FINALDF)<-c("prediction", "xcoord", "ycoord", "X", "Y")
    colnames(PROBSDF)<-c("cavicle","rosette","typical","empty","ecm","cytop", "er", "mito", "ves",  "xcoord", "ycoord", "X", "Y")
    PROBSDF$PCAV<-PROBSDF$cavicle+PROBSDF$rosette+PROBSDF$typical
    PROBSDF$PNOCAV<-PROBSDF$empty+PROBSDF$ecm+PROBSDF$cytop
    PROBSDF$xcoord<-PROBSDF$xcoord-round(size/2); PROBSDF$ycoord<-PROBSDF$ycoord-round(size/2); 
    PROBSDF$X<-PROBSDF$X-round(size/2); PROBSDF$Y<-PROBSDF$Y-round(size/2); 
    
    FINALDF$colorcodes<-ifelse(grepl(0,FINALDF$prediction),"magenta",0)
    FINALDF$colorcodes<- ifelse(grepl(1,FINALDF$prediction),"cyan",FINALDF$colorcodes)
    FINALDF$colorcodes<- ifelse(grepl(2,FINALDF$prediction),"yellow",FINALDF$colorcodes)
    if(nrow(FINALDF)>0) {
      FINALDF$xcoord<-FINALDF$xcoord-round(size/2); FINALDF$ycoord<-FINALDF$ycoord-round(size/2); 
      FINALDF$X<-FINALDF$X-round(size/2); FINALDF$Y<-FINALDF$Y-round(size/2); 
    }

    par(cex=0.7, mai=c(0.01,0.01,0.01,0.01))
    layout(matrix(c(1, 1, 2,
                    1, 1, 2,
                    1, 1, 2), nrow=3, byrow=TRUE))
    plot(as.raster(original_tensor[1,,,]))
    if(nrow(FINALDF)>0) {points(FINALDF$xcoord,  1024-(FINALDF$ycoord), col= FINALDF$colorcodes, pch=16, cex=1)}
    plot(0,type='n',axes=FALSE,ann=FALSE)
    legend("center", legend=c("Typical", "rosette", "cavicle"),
           col=c("yellow", "cyan", "magenta"), pch=c(16,16,16), cex=1.5, box.lty=0)
    dev.print(svg, paste(path, paste(element, "_FINALPLOT.svg", sep=""), sep="/"))
    
    imagenamevector<-rep(element, dim(FINALDF)[1])
    FINALDF$imageID<-imagenamevector
    #FINALDF saved as CAVDF has the central predictions of caveolae, one row per caveolae
    
    centerpoints$caveolae<-paste(centerpoints$group, centerpoints$cls, sep="_")
    cluspoints2<-merge(cluspoints, centerpoints, by="caveolae")
    cluspoints2[,8]<-NULL
    cluspoints2[,10]<-NULL
    colnames(cluspoints2)<-c("caveolae", "xcoord", "ycoord", "group", "cls", "option", "colorcodes", "xcoordcentre", "ycoordcentre", "Xcentre", "Ycentre")
    
    par(cex=0.7, mai=c(0.01,0.01,0.01,0.01))
    layout(matrix(c(1, nrow=1, byrow=TRUE)))
    plot(as.raster(img_tensor[1,,,])) 
    points(cluspoints2$xcoord,  nrows-(cluspoints2$ycoord), col=cluspoints2$colorcodes , pch=16, cex=0.7)
    points(cluspoints2$xcoordcentre,  nrows-(cluspoints2$ycoordcentre), col="black", bg=cluspoints2$colorcodes, pch=21, cex=2)
    dev.print(svg, paste(path, paste(element, "_CLUSTERPOINTS_CAVEOLAEPSEUDOSEGMENTATION.svg", sep=""), sep="/"))
    
    write.table(FINALDF, paste(path, paste(element, "_CAVDF.txt", sep=""), sep="/"))
    write.table(PROBSDF, paste(path, paste(element, "_PROBSCAVDF.txt", sep=""), sep="/"))
    FINALDFtomerge<-FINALDF
    colnames(FINALDFtomerge)[4]<-"xcoordcentre"; colnames(FINALDFtomerge)[5]<-"ycoordcentre"
    cluspoints3<-merge(cluspoints2, FINALDFtomerge, by=c("xcoordcentre", "ycoordcentre"))
    colnames(cluspoints3)<-c("xcoordcentre", "ycoordcentre", "caveolae","xcoordcluspoints", "ycoordcluspoints", "group", "cls", "option", "colorcodescluster", "Xcentre",     
    "Ycentre", "prediction", "xcoordcentre2", "ycoordcentre2", "colorcodescentre", "imageID")
    
    write.table(cluspoints3, paste(path, paste(element, "_CLUSTERPOINTS_CAVEOLAEPSEUDOSEGMENTATION.txt", sep=""), sep="/"))
    
  } else {
    FINALDF <- data.frame(prediction=character(), xcoord=character(), ycoord=character(), 
                          X=character(), Y=character(), colorcodes=character(),imageID=character(),
                          stringsAsFactors=FALSE) 
  }
  
  ###################
  #########################################################
  
  
  
  #########################################################
  #########################################################
  #########################################################
  #########################################################
  ###PART2: ECM
  #########################################################
  #########################################################
  #########################################################
  #########################################################
  
  if (FindCellLimits ==TRUE){
    if ((dim(preecmlocs)[1])>0){
      predictiondf2<-predictiondf
      ncolscenters<-length(unique(predictiondf2$xcoord))
      nrowscenters<-length(unique(predictiondf2$ycoord))
      
      preecmlocs<-subset(predictiondf2, prediction == 4)
      #classify by neighbours
      ecmneighdf <- data.frame(xcoord=numeric(), ycoord=numeric(),
                               vemp=character(), vecm=character(), vcytocav=character(), stringsAsFactors=FALSE)
      for (i in 1:(dim(preecmlocs)[1])) {
        ex<-preecmlocs[i,]$xcoord; ey<-preecmlocs[i,]$ycoord;
        neighlist<-getneighbours(i, preecmlocs, predictiondf, ifsame=FALSE)
        neighlisttable<- table (neighlist$prediction)
        neighlisttabledf<-as.data.frame(neighlisttable)
        vempdf<-neighlisttabledf[(neighlisttabledf$Var1==3),];  if ((dim(vempdf)[1])==0) { vemp = 0} else {vemp<-vempdf$Freq}
        vecmdf<-neighlisttabledf[(neighlisttabledf$Var1==4),]; if ((dim(vecmdf)[1])==0) { vecm = 0} else {vecm<-vecmdf$Freq}
        vcytocavdf<-neighlisttabledf[(neighlisttabledf$Var1==0 | neighlisttabledf$Var1==1 |neighlisttabledf$Var1==2 |neighlisttabledf$Var1==5|neighlisttabledf$Var1==6|neighlisttabledf$Var1==7|neighlisttabledf$Var1==8),]
        #ER, mito and vesicles count here as Cytop
        if ((dim(vcytocavdf)[1])==0) { vcytocav = 0} else {vcytocav<-sum(vcytocavdf$Freq)}
        vectorneigh<-c(ex, ey, vemp, vecm, vcytocav)
        ecmneighdf<-rbind(ecmneighdf, vectorneigh)
        colnames(ecmneighdf)<-c("xcoord", "ycoord", "vemp", "vecm", "vcytocav")
      }
      
      #reclassify coordinates surounded by LD or cytoplasm/caveolae to smooth borders
      ecmneighdfrealcyto<-subset(ecmneighdf, vcytocav > 4)
      ecmneighdfrealcyto$prediction<-rep(5, dim(ecmneighdfrealcyto)[1])
      for(row1 in 1:nrow(ecmneighdfrealcyto)){
        predictiondf2$prediction[predictiondf2$xcoord %in% ecmneighdfrealcyto$xcoord[row1] & predictiondf2$ycoord %in% ecmneighdfrealcyto$ycoord[row1]] <- ecmneighdfrealcyto$prediction[row1]
      }
      ecmneighdfrealld<-subset(ecmneighdf, vemp > 4)
      ecmneighdfrealld$prediction<-rep(3, dim(ecmneighdfrealld)[1])
      for(row1 in 1:nrow(ecmneighdfrealld)){
        predictiondf2$prediction[predictiondf2$xcoord %in% ecmneighdfrealld$xcoord[row1] & predictiondf2$ycoord %in% ecmneighdfrealld$ycoord[row1]] <- ecmneighdfrealld$prediction[row1]
      }
      
      ecmneighdf<-setdiff(ecmneighdf, ecmneighdfrealcyto[,1:5])
      ecmneighdf<-setdiff(ecmneighdf, ecmneighdfrealld[,1:5])
      ecmneighdf$type<-ifelse(ecmneighdf$vecm ==8,"INecm",0)
      ecmneighdf$type<-ifelse(ecmneighdf$vecm < 8 ,"BORDERecm",ecmneighdf$type)
      ecmneighdf$type2<-ifelse(ecmneighdf$vemp >0,"EMPcontact","NOcontact")
      ecmneighdf$type2<-ifelse(ecmneighdf$vcytocav >0  ,"CYTOCAVcontact",ecmneighdf$type2) #important, if contact with cytoplasm rewrites the LD
      #its ok, ecm-LD contacts all erroneous
      ecmnosolos<-subset(ecmneighdf, type=="INecm" | type == "BORDERecm") #remove isolated
      ecmborders<-subset(ecmneighdf, type=="BORDERecm") #onlyborders
      ecmin<-subset(ecmneighdf, type=="INecm") #only in
      
      #we find isolated ECM groups
      groupdfEC <- data.frame(xcoord=numeric(), 
                              ycoord=numeric(),vemp=character(),vecm=character(),vcytocav=character(),
                              type=character(),type2=character(),group=numeric(), stringsAsFactors=FALSE) 
      groupdfEC2<-sortingroups(groupdfEC, ecmborders, ecmnosolos, 7)
      
      groupdfEC2$group<-as.factor(groupdfEC2$group)
      library(dplyr)
      #reclassify as LD if surounded by LD, or as cytopl, if surounded by cytoplasm or caveolae
      grtableEC<-table(groupdfEC2$group)
      grtableEC<-as.data.frame(grtableEC)
      grtableECrec<-subset(grtableEC, Freq<5*maxcav) #we leave ecm that are at least as 5 small caveolae
      truegroupdfEC2<-filter(groupdfEC2, group %in% grtableECrec$Var1) #removed coordinates of isolated ecm
      errgroupdfEC2<-filter(groupdfEC2, group %in% grtableECrec$Var1)#we get the erroneous
      errgroupdfEC2<-droplevels(errgroupdfEC2)
      
      berrgroupdfEC2<-subset(errgroupdfEC2, type == "BORDERecm") #only borders of erroneous
      
      #reassign small ecm areas to surounding type
      for (ec in (levels(as.factor(berrgroupdfEC2$group)))) {
        onecm<-subset(berrgroupdfEC2, group == ec)
        totalvemp<-sum(onecm$vemp); totalvcytocav<-sum(onecm$vcytocav)
        if (totalvemp > totalvcytocav) {realpred = 3}
        if (totalvcytocav > totalvemp) {realpred = 5}
        if (totalvcytocav == totalvemp) {realpred = 3} 
        allgroup<- subset(groupdfEC2, group == ec)
        allgroup$prediction <-rep(realpred, dim(allgroup)[1])
        for(rowi in 1:nrow(allgroup)){
          predictiondf2$prediction[predictiondf2$xcoord %in% allgroup$xcoord[rowi] & predictiondf2$ycoord %in% allgroup$ycoord[rowi]] <- allgroup$prediction[rowi]
        }
      }
      predictiondf2<-repaint_predictiondf2()
      predictiondf2$colorcodes<-as.character(predictiondf2$colorcodes)
    }
    } #end of ecm
  #########################################################
  
  
  
  #########################################################
  #########################################################
  #########################################################
  #########################################################
  ###PART3: LD
  #########################################################
  #########################################################
  #########################################################
  #########################################################
  
  if (FindCellLimits ==TRUE & LDexpected == TRUE){
    if((dim(preemptylocs)[1])>0){
      preemptylocs<-subset(predictiondf2, prediction == 3)
      emptyneighdf <- data.frame(xcoord=numeric(), ycoord=numeric(),
                                 vemp=character(), vecm=character(), vcytocav=character(), stringsAsFactors=FALSE)
      #get LD coordinate neighbours
      for (i in 1:(dim(preemptylocs)[1])) {
        ex<-preemptylocs[i,]$xcoord; ey<-preemptylocs[i,]$ycoord;
        neighlist<-getneighbours(i, preemptylocs, predictiondf2, ifsame=FALSE)
        if (dim(neighlist)[1] > 0) {
          neighlisttable<- table (neighlist$prediction)
          neighlisttabledf<-as.data.frame(neighlisttable)
          vempdf<-neighlisttabledf[(neighlisttabledf$Var1==3),];  if ((dim(vempdf)[1])==0) { vemp = 0} else {vemp<-vempdf$Freq}
          vecmdf<-neighlisttabledf[(neighlisttabledf$Var1==4),]; if ((dim(vecmdf)[1])==0) { vecm = 0} else {vecm<-vecmdf$Freq}
          vcytocavdf<-neighlisttabledf[(neighlisttabledf$Var1==0 | neighlisttabledf$Var1==1 |neighlisttabledf$Var1==2 |neighlisttabledf$Var1==5|neighlisttabledf$Var1==6|neighlisttabledf$Var1==7|neighlisttabledf$Var1==8),]
          #we consider ER, mito and vesciles as cytop neighbours
          if ((dim(vcytocavdf)[1])==0) { vcytocav = 0} else {vcytocav<-sum(vcytocavdf$Freq)}
          vectorneigh<-c(ex, ey, vemp, vecm, vcytocav)
          emptyneighdf<-rbind(emptyneighdf, vectorneigh)
          colnames(emptyneighdf)<-c("xcoord", "ycoord", "vemp", "vecm", "vcytocav")
        } else {
          vectorneigh<-c(ex, ey, 0, 0, 0)
          emptyneighdf<-rbind(emptyneighdf, vectorneigh)
          colnames(emptyneighdf)<-c("xcoord", "ycoord", "vemp", "vecm", "vcytocav")
        }
      }
      
      #if coordinates surounded by LD or cytoplsm/caveolae, we reclassify as cytoplasm or caveolae to smooth borders
      emptyneighdfrealcyto<-subset(emptyneighdf, vcytocav > 4)
      emptyneighdfrealcyto$prediction<-rep(5, dim(emptyneighdfrealcyto)[1])
      for(row1 in 1:nrow(emptyneighdfrealcyto)){
        predictiondf2$prediction[predictiondf2$xcoord %in% emptyneighdfrealcyto$xcoord[row1] & predictiondf2$ycoord %in% emptyneighdfrealcyto$ycoord[row1]] <- emptyneighdfrealcyto$prediction[row1]
      }
      emptyneighdfrealecm<-subset(emptyneighdf, vecm > 4)
      emptyneighdfrealecm$prediction<-rep(4, dim(emptyneighdfrealecm)[1])
      for(row1 in 1:nrow(emptyneighdfrealecm)){
        predictiondf2$prediction[predictiondf2$xcoord %in% emptyneighdfrealecm$xcoord[row1] & predictiondf2$ycoord %in% emptyneighdfrealecm$ycoord[row1]] <- emptyneighdfrealecm$prediction[row1]
      }
      
      predictiondf2<-repaint() 
      emptyneighdf<-setdiff(emptyneighdf, emptyneighdfrealcyto[,1:5])
      emptyneighdf<-setdiff(emptyneighdf, emptyneighdfrealecm[,1:5])
      
      emptyneighdf$type<-ifelse(emptyneighdf$vemp ==8,"INempty",0)
      emptyneighdf$type<-ifelse(emptyneighdf$vemp < 8,"BORDERempty",emptyneighdf$type)
      emptyneighdf$type2<-ifelse(emptyneighdf$vecm >0,"ECMcontact","NOcontact")
      emptyneighdf$type2<-ifelse(emptyneighdf$vcytocav >0  ,"CYTOCAVcontact",emptyneighdf$type2) #LD-ECM contact are erroneous, cytoplasm contact rewrites ECM
      #we can put >2 to consider a real contact, to avoid contacts with isolated points
      emptynosolos<-subset(emptyneighdf, type=="INempty" | type == "BORDERempty") 
      emptyborders<-subset(emptyneighdf, type=="BORDERempty") #only borders
      emptyin<-subset(emptyneighdf, type=="INempty") #only in
      
      #we find isolated groups
      if(nrow(emptyneighdf)>0 & nrow(emptyborders)>0 & nrow(emptynosolos)>0 ) {
        groupdfE <- data.frame(xcoord=numeric(), 
                               ycoord=numeric(),vemp=character(),vecm=character(),vcytocav=character(),
                               type=character(),type2=character(),group=numeric(), stringsAsFactors=FALSE)
        groupdfE2<-sortingroups(groupdfE, emptyborders, emptynosolos, 7)
        
        groupdfE2$group<-as.factor(groupdfE2$group)
        groupdfLD<-groupdfE2
        grtableE<-table(groupdfE2$group)
        grtableE<-as.data.frame(grtableE)
        grtableE<-subset(grtableE, Freq>10*mincav) #we get only LD that are at least as 10 small caveolae
        LDnumber<-dim(grtableE)[1]
      } else {LDnumber<-0}

      print(paste("number of LDs: ", LDnumber, sep=""))
      library(dplyr)
      
      if(LDnumber > 0) {
        truegroupdfE2<-filter(groupdfE2, group %in% grtableE$Var1) #remove coordinates of excluded LDs
       
          surfacegroupdfE2<-subset(truegroupdfE2, type2 == "ECMcontact" | type2 =="CYTOCAVcontact" ) #leave only coordinates of surface
          truegroupdfLD<-truegroupdfE2
          surfacegroupdfE2<-droplevels(surfacegroupdfE2); truegroupdfE2<-droplevels(truegroupdfE2)
          LDcenterX<-flatsummaryby(median, truegroupdfE2$xcoord, truegroupdfE2$group)
          LDcenterY<-flatsummaryby(median, truegroupdfE2$ycoord, truegroupdfE2$group) #we get central coordinates of LDs
          #calculate cytoplasm center to approximate LD surface to the cell. Otherwise LD surface too far
          cytox<-mean(precytolocs$xcoord); cytoy<-mean(precytolocs$ycoord)
          dfLDsurfaces <- data.frame(xcoord=numeric(), 
                                     ycoord=numeric(),vemp=character(),vecm=character(),vcytocav=character(),
                                     type=character(),type2=character(),group=numeric() ,colorcodes2=character(),stringsAsFactors=FALSE) 
          for (ld in 1:LDnumber) {
            rowx<-LDcenterX[ld,]; rowy<-LDcenterY[ld,]
            if (rowx$xcoord_median_truegroupdfE2 - cytox > 0) {correctionx = -2*pxdisplacement}
            if (rowx$xcoord_median_truegroupdfE2 - cytox == 0) {correctionx = 0}
            if (rowx$xcoord_median_truegroupdfE2 - cytox < 0) {correctionx = +2*pxdisplacement}
            if (rowy$ycoord_median_truegroupdfE2 - cytoy > 0) {correctiony = -2*pxdisplacement}
            if (rowy$ycoord_median_truegroupdfE2 - cytoy == 0) {correctiony = 0}
            if (rowy$ycoord_median_truegroupdfE2 - cytoy < 0) {correctiony = +2*pxdisplacement}
            groupLD<-rowx$group
            oneLD<-subset(surfacegroupdfE2, group==groupLD)
            oneLD$xcoord<-oneLD$xcoord+correctionx
            oneLD$ycoord<-oneLD$ycoord+correctiony
            oneLD$group<-rep(groupLD, dim(oneLD)[1])
            dfLDsurfaces<-rbind(dfLDsurfaces, oneLD)
          }
          
          dfLDsurfaces$xcoord<-dfLDsurfaces$xcoord-round(size/2); dfLDsurfaces$ycoord<-dfLDsurfaces$ycoord-round(size/2); 
         #1st LD>0
      } #2nd LD>0

    }
  } else {print("No LD expected or found")
    LDnumber = 0
    #reclassify erroneous LD as surounding type
    if(exists("predictiondf2")==FALSE){
      predictiondf2<-predictiondf
    }
    
    preemptylocs<-subset(predictiondf2, prediction == 3) 
    if (dim(preemptylocs)[1] > 0) { #get neighbours of LD predictions
    emptyneighdf <- data.frame(xcoord=numeric(), ycoord=numeric(),
                             vemp=character(), vempty=character(), vcytocav=character(), stringsAsFactors=FALSE)
    for (i in 1:(dim(preemptylocs)[1])) {
      ex<-preemptylocs[i,]$xcoord; ey<-preemptylocs[i,]$ycoord;
      neighlist<-getneighbours(i, preemptylocs, predictiondf, ifsame=FALSE)
      neighlisttable<- table (neighlist$prediction)
      neighlisttabledf<-as.data.frame(neighlisttable)
      vempdf<-neighlisttabledf[(neighlisttabledf$Var1==3),];  if ((dim(vempdf)[1])==0) { vemp = 0} else {vemp<-vempdf$Freq}
      vecmdf<-neighlisttabledf[(neighlisttabledf$Var1==4),]; if ((dim(vecmdf)[1])==0) { vecm = 0} else {vecm<-vecmdf$Freq}
      vcytocavdf<-neighlisttabledf[(neighlisttabledf$Var1==0 | neighlisttabledf$Var1==1 |neighlisttabledf$Var1==2 |neighlisttabledf$Var1==5|neighlisttabledf$Var1==6|neighlisttabledf$Var1==7|neighlisttabledf$Var1==8),]
      if ((dim(vcytocavdf)[1])==0) { vcytocav = 0} else {vcytocav<-sum(vcytocavdf$Freq)}
      vectorneigh<-c(ex, ey, vemp, vecm, vcytocav)
      emptyneighdf<-rbind(emptyneighdf, vectorneigh)
      colnames(emptyneighdf)<-c("xcoord", "ycoord", "vemp", "vecm", "vcytocav")
    }
    
    #if surounded by LD or cytoplsm/caveolae, reclassified as cytoplasm or caveolae, to make borders smoother
    emptyneighdfrealcyto<-subset(emptyneighdf, vcytocav > 4)
    emptyneighdfrealcyto$prediction<-rep(5, dim(emptyneighdfrealcyto)[1])
    for(row1 in 1:nrow(emptyneighdfrealcyto)){
      predictiondf2$prediction[predictiondf2$xcoord %in% emptyneighdfrealcyto$xcoord[row1] & predictiondf2$ycoord %in% emptyneighdfrealcyto$ycoord[row1]] <- emptyneighdfrealcyto$prediction[row1]
    }
    emptyneighdfrealecm<-subset(emptyneighdf, vecm > 4)
    emptyneighdfrealecm$prediction<-rep(4, dim(emptyneighdfrealecm)[1])
    for(row1 in 1:nrow(emptyneighdfrealecm)){
      predictiondf2$prediction[predictiondf2$xcoord %in% emptyneighdfrealecm$xcoord[row1] & predictiondf2$ycoord %in% emptyneighdfrealecm$ycoord[row1]] <- emptyneighdfrealecm$prediction[row1]
    }
    
    emptyneighdf<-setdiff(emptyneighdf, emptyneighdfrealcyto[,1:5])
    emptyneighdf<-setdiff(emptyneighdf, emptyneighdfrealecm[,1:5])
    emptyneighdf$type<-ifelse(emptyneighdf$vemp ==8,"INempty",0)
    emptyneighdf$type<-ifelse(emptyneighdf$vemp < 8 ,"BORDERempty",emptyneighdf$type)
    emptyneighdf$type2<-ifelse(emptyneighdf$vecm >0,"ECMcontact","NOcontact")
    emptyneighdf$type2<-ifelse(emptyneighdf$vcytocav >0  ,"CYTOCAVcontact",emptyneighdf$type2) #important, contact with cytoplasm overwrites contact with LD
    #ok, contact LD-ECM always wrong
    emptynosolos<-subset(emptyneighdf, type=="INempty" | type == "BORDERempty") 
    emptyborders<-subset(emptyneighdf, type=="BORDERempty") #only borders
    emptyin<-subset(emptyneighdf, type=="INempty") #only in
 
    #find isolated LD
    groupdfEC <- data.frame(xcoord=numeric(),
                            ycoord=numeric(),vemp=character(),vempty=character(),vcytocav=character(),
                            type=character(),type2=character(),group=numeric(), stringsAsFactors=FALSE)
    groupdfEC2<-sortingroups(groupdfEC, emptyborders, emptynosolos, 7)
  
    berrgroupdfEC2<-subset(groupdfEC2, type == "BORDERempty") #only erroneous borders, which are all, if no LD expected
      for (ec in (levels(as.factor(berrgroupdfEC2$group)))) {
        onempty<-subset(berrgroupdfEC2, group == ec)
        totalvecm<-sum(onempty$vecm); totalvcytocav<-sum(onempty$vcytocav)
        if (totalvecm > totalvcytocav) {realpred = 4}
        if (totalvcytocav > totalvecm) {realpred = 5}
        if (totalvcytocav == totalvecm) {realpred = 4} #seems more likely to find erroneous LD among ECM predictions than among cyto predictions
        allgroup<- subset(groupdfEC2, group == ec)
        allgroup$prediction <-rep(realpred, dim(allgroup)[1])
        for(rowi in 1:nrow(allgroup)){
          predictiondf2$prediction[predictiondf2$xcoord %in% allgroup$xcoord[rowi] & predictiondf2$ycoord %in% allgroup$ycoord[rowi]] <- allgroup$prediction[rowi]
        }
      }
    predictiondf2<-repaint() 
   
    }
    
     }
  ######################################################### 
  
  
  
  #########################################################
  #########################################################
  #########################################################
  #########################################################
  ###PART4: CYTOPLASM AND SURFACES
  #########################################################
  #########################################################
  #########################################################
  #########################################################

  if (exists("precytolocs")==FALSE){
    precytolocs<-subset(predictiondf, prediction == 0 | prediction ==1 | prediction == 2 | prediction == 5| prediction == 6| prediction == 7| prediction == 8)
  }
  if (FindCellLimits ==TRUE & (dim(precytolocs)[1])>0){
    
    #we reassign erroneous mito, er or ves to predominant in environment
    #########################################################
    #I would say a mito should be at least like 5 caveolae, er or endosome like at least  3
    mitomin<-avcav*5; ermin<-avcav*3; endomin<-avcav*3
    #make mito groups by continuity. check most frequent neighbour. reassign to most frequent neighbour if less than mitomin
    premitolocs<-subset(predictiondf, prediction == 7 ); preerlocs<-subset(predictiondf, prediction == 6 ); preENDOlocs<-subset(predictiondf, prediction == 8 )
    #########################################################
    #for mito
    #########################################################
    
    if (nrow(premitolocs)>0){
      mitoneighdf <- data.frame(xcoord=numeric(), ycoord=numeric(),
                                vemp=character(), vecm=character(), vmitocav=character(), stringsAsFactors=FALSE)
      #we first get neighbours. Should probably make a function for this
      for (i in 1:(dim(premitolocs)[1])) {
        ex<-premitolocs[i,]$xcoord; ey<-premitolocs[i,]$ycoord;
        neighlist<-getneighbours(i, premitolocs, predictiondf2, ifsame=FALSE)
        if (dim(na.omit(neighlist))[1] > 0) {
          neighlisttable<- table (neighlist$prediction)
          neighlisttabledf<-as.data.frame(neighlisttable)
          vempdf<-neighlisttabledf[(neighlisttabledf$Var1==3),];  if ((dim(vempdf)[1])==0) { vemp = 0} else {vemp<-vempdf$Freq}
          vecmdf<-neighlisttabledf[(neighlisttabledf$Var1==4),]; if ((dim(vecmdf)[1])==0) { vecm = 0} else {vecm<-vecmdf$Freq}
          vcitocavdf<-neighlisttabledf[(neighlisttabledf$Var1==0 | neighlisttabledf$Var1==1 |neighlisttabledf$Var1==2 |neighlisttabledf$Var1==5),]
          verdf<-neighlisttabledf[(neighlisttabledf$Var1==6) ,];  if ((dim(verdf)[1])==0) { ver = 0} else {ver<-verdf$Freq}
          vendodf<-neighlisttabledf[(neighlisttabledf$Var1==6) ,];  if ((dim(vendodf)[1])==0) { vendo = 0} else {vendo<-vendodf$Freq}
          if ((dim(vcitocavdf)[1])==0) { vcitocav = 0} else {vcitocav<-sum(vcitocavdf$Freq)}
          vectorneigh<-c(ex, ey, vemp, vecm, vcitocav, ver, vendo)
          mitoneighdf<-rbind(mitoneighdf, vectorneigh)
          colnames(mitoneighdf)<-c("xcoord", "ycoord", "vemp", "vecm", "vcitocav", "ver", "vendo")
        } else {
          vectorneigh<-c(ex, ey, 0, 0, 0, 0, 0)
          mitoneighdf<-rbind(mitoneighdf, vectorneigh)
          colnames(mitoneighdf)<-c("xcoord", "ycoord", "vemp", "vecm", "vcitocav", "ver", "vendo")
        }
      }
      if (dim(na.omit(neighlist))[1] > 0) {
      mitoneighdf$type<-ifelse(mitoneighdf$vcitocav ==8,"INmito",0)
      mitoneighdf$type<-ifelse(mitoneighdf$vcitocav < 8,"BORDERmito",mitoneighdf$type)
      mitoneighdf$type2<-ifelse(mitoneighdf$vecm >0,"ECMcontact","NOcontact")
      mitoneighdf$type2<-ifelse(mitoneighdf$vemp >0  ,"EMPTYcontact",mitoneighdf$type2) #if contact with LD and ECM, LD remains
      mitonosolos<-subset(mitoneighdf, type=="INmito" | type == "BORDERmito") #All
      mitoborders<-subset(mitoneighdf, type=="BORDERmito") #only borders
      mitoin<-subset(mitoneighdf, type=="INmito") #only inners
      mitosolos<-subset(mitoneighdf, vemp+vecm+vcitocav+ver+vendo==8)
      
      if (nrow(mitosolos)>0) { #reassign isolated
        for (var1 in 1:nrow(mitosolos)){
          allv<-c(mitosolos$vemp[var1], mitosolos$vecm[var1], mitosolos$vcitocav[var1], mitosolos$ver[var1], NA, mitosolos$vendo[var1]) #class is max +2
          mm<-which.max(allv)
          reassignedclass<-mm+2
          predictiondf2$prediction[predictiondf2$xcoord %in% mitosolos$xcoord[var1] & predictiondf2$ycoord %in% mitosolos$ycoord[var1]] <- reassignedclass
        }
      }
      
      if(nrow(na.omit(mitoborders)) > 0 & nrow(na.omit(mitonosolos)) >0 ) {
      #now we make groups by continuity
      groupdfM <- data.frame(xcoord=numeric(), 
                             ycoord=numeric(),vemp=character(),vecm=character(),vmitocav=character(),
                             type=character(),type2=character(),group=numeric(), stringsAsFactors=FALSE)
      groupdfM2<-sortingroups(groupdfM, mitoborders, mitonosolos, 9, steps=1)
      
      groupdfM2$group<-as.factor(groupdfM2$group);mitodf<-groupdfM2
      mitodf$xcoord<-mitodf$xcoord-round(size/2);  mitodf$ycoord<-mitodf$ycoord-round(size/2);
      grtableMi<-table(groupdfM2$group); grtableMi<-as.data.frame(grtableMi)
      grtableNoMi<-subset(grtableMi, Freq<mitomin | Freq==mitomin) #groups too small to be mitochondria
      grtableMi<-subset(grtableMi, Freq>mitomin) 
      mitoNUMBER<-dim(grtableMi)[1]
      
      if (nrow(grtableNoMi)>0) { #reassign small groups
        for (var1 in grtableNoMi$Var1){
          agroup<-subset(groupdfM2, group== var1)
          totalvemp<-sum(agroup$vemp); totalvecm<-sum(agroup$vecm); totalvcito<-sum(agroup$vcitocav); totalver<-sum(agroup$ver); totalvendo<-sum(agroup$vendo)
          allv<-c(totalvemp, totalvecm, totalvcito, totalver, NA, totalvendo)#class is max +2
          mm<-which.max(allv)
          reassignedclass<-mm+2
          for(row1 in 1:nrow(agroup)){
            predictiondf2$prediction[predictiondf2$xcoord %in% agroup$xcoord[row1] & predictiondf2$ycoord %in% agroup$ycoord[row1]] <- reassignedclass
          }
        }
      }
      }
      }
    }
    
    #########################################################
    #for er
    #########################################################
    if (nrow(preerlocs)>0){
      erneighdf <- data.frame(xcoord=numeric(), ycoord=numeric(),
                              vemp=character(), vecm=character(), vercav=character(), stringsAsFactors=FALSE)
      #we first get neighbours. Should probably make a function for this
      for (i in 1:(dim(preerlocs)[1])) {
        ex<-preerlocs[i,]$xcoord; ey<-preerlocs[i,]$ycoord;
        neighlist<-getneighbours(i, preerlocs, predictiondf2, ifsame=FALSE)
        if (dim(na.omit(neighlist))[1] > 0) {
          neighlisttable<- table (neighlist$prediction)
          neighlisttabledf<-as.data.frame(neighlisttable)
          vempdf<-neighlisttabledf[(neighlisttabledf$Var1==3),];  if ((dim(vempdf)[1])==0) { vemp = 0} else {vemp<-vempdf$Freq}
          vecmdf<-neighlisttabledf[(neighlisttabledf$Var1==4),]; if ((dim(vecmdf)[1])==0) { vecm = 0} else {vecm<-vecmdf$Freq}
          vcitocavdf<-neighlisttabledf[(neighlisttabledf$Var1==0 | neighlisttabledf$Var1==1 |neighlisttabledf$Var1==2 |neighlisttabledf$Var1==5),]
          vmitodf<-neighlisttabledf[(neighlisttabledf$Var1==6) ,];  if ((dim(vmitodf)[1])==0) { vmito = 0} else {vmito<-vmitodf$Freq}
          vendodf<-neighlisttabledf[(neighlisttabledf$Var1==6) ,];  if ((dim(vendodf)[1])==0) { vendo = 0} else {vendo<-vendodf$Freq}
          if ((dim(vcitocavdf)[1])==0) { vcitocav = 0} else {vcitocav<-sum(vcitocavdf$Freq)}
          vectorneigh<-c(ex, ey, vemp, vecm, vcitocav, vmito, vendo)
          erneighdf<-rbind(erneighdf, vectorneigh)
          colnames(erneighdf)<-c("xcoord", "ycoord", "vemp", "vecm", "vcitocav", "vmito", "vendo")
        } else {
          vectorneigh<-c(ex, ey, 0, 0, 0, 0, 0)
          erneighdf<-rbind(erneighdf, vectorneigh)
          colnames(erneighdf)<-c("xcoord", "ycoord", "vemp", "vecm", "vcitocav", "vmito", "vendo")
        }
      }
      
      if (dim(na.omit(neighlist))[1] > 0) {
      erneighdf$type<-ifelse(erneighdf$vcitocav ==8,"INer",0)
      erneighdf$type<-ifelse(erneighdf$vcitocav < 8,"BORDERer",erneighdf$type)
      erneighdf$type2<-ifelse(erneighdf$vecm >0,"ECMcontact","NOcontact")
      erneighdf$type2<-ifelse(erneighdf$vemp >0  ,"EMPTYcontact",erneighdf$type2) # if contact with LD and ECM, remains LD
      ernosolos<-subset(erneighdf, type=="INer" | type == "BORDERer") #all
      erborders<-subset(erneighdf, type=="BORDERer") 
      erin<-subset(erneighdf, type=="INer") 
      ersolos<-subset(erneighdf, vemp+vecm+vcitocav+vmito+vendo==8)
      
      if (nrow(ersolos)>0) { #reassign isolated predictions
        for (var1 in 1:nrow(ersolos)){
          allv<-c(ersolos$vemp[var1], ersolos$vecm[var1], ersolos$vcitocav[var1], NA, ersolos$vmito[var1],  ersolos$vendo[var1]) #la clase es el max +2
          mm<-which.max(allv)
          reassignedclass<-mm+2
          predictiondf2$prediction[predictiondf2$xcoord %in% ersolos$xcoord[var1] & predictiondf2$ycoord %in% ersolos$ycoord[var1]] <- reassignedclass
        }
      }
      
      #now we make groups by continuity
      if(nrow(na.omit(erborders)) > 0 & nrow(na.omit(ernosolos)) >0 ) {
        matchColClasses <- function(df1, df2) {
          sharedColNames <- names(df1)[names(df1) %in% names(df2)]
          sharedColTypes <- sapply(df1[,sharedColNames], class)
          for (n in sharedColNames) {
            class(df2[, n]) <- sharedColTypes[n]
          }
          return(df2)
        }
        
        groupdfM <- data.frame(xcoord=numeric(), 
                               ycoord=numeric(),vemp=character(),vecm=character(),vercav=character(),
                               type=character(),type2=character(),group=numeric(), stringsAsFactors=FALSE)
        groupdfM2<-sortingroups(groupdfM, erborders, ernosolos, 9, steps=1)
        
        groupdfM2$group<-as.factor(groupdfM2$group);erdf<-groupdfM2
        erdf$xcoord<-erdf$xcoord-round(size/2);  erdf$ycoord<-erdf$ycoord-round(size/2);
        grtableER<-table(groupdfM2$group); grtableER<-as.data.frame(grtableER)
        grtableNoER<-subset(grtableER, Freq<ermin | Freq==ermin)
        grtableER<-subset(grtableER, Freq>ermin) #filter by size
        erNUMBER<-dim(grtableER)[1]
        
        if (nrow(grtableNoER)>0) { #reassign those too small
          for (var1 in grtableNoER$Var1){
            agroup<-subset(groupdfM2, group== var1)
            totalvemp<-sum(agroup$vemp); totalvecm<-sum(agroup$vecm); totalvcito<-sum(agroup$vcitocav); totalvmito<-sum(agroup$vmito); totalvendo<-sum(agroup$vendo)
            allv<-c(totalvemp, totalvecm, totalvcito, NA, totalvmito, totalvendo)#class is max +2
            mm<-which.max(allv)
            reassignedclass<-mm+2
            for(row1 in 1:nrow(agroup)){
              predictiondf2$prediction[predictiondf2$xcoord %in% agroup$xcoord[row1] & predictiondf2$ycoord %in% agroup$ycoord[row1]] <- reassignedclass
            }
          }
        }
      }
      }
    }
    
    
    #########################################################
    #for endosomes
    #########################################################
    
    if (nrow(preENDOlocs)>0){
      ENDOneighdf <- data.frame(xcoord=numeric(), ycoord=numeric(),
                                vemp=character(), vecm=character(), vercav=character(), stringsAsFactors=FALSE)
      #we first get neighbours. Should probably make a function for this
      for (i in 1:(dim(preENDOlocs)[1])) {
        ex<-preENDOlocs[i,]$xcoord; ey<-preENDOlocs[i,]$ycoord;
        neighlist<-getneighbours(i, preENDOlocs, predictiondf2, ifsame=FALSE)
        if (dim(na.omit(neighlist))[1] > 0) {
          neighlisttable<- table (neighlist$prediction)
          neighlisttabledf<-as.data.frame(neighlisttable)
          vempdf<-neighlisttabledf[(neighlisttabledf$Var1==3),];  if ((dim(vempdf)[1])==0) { vemp = 0} else {vemp<-vempdf$Freq}
          vecmdf<-neighlisttabledf[(neighlisttabledf$Var1==4),]; if ((dim(vecmdf)[1])==0) { vecm = 0} else {vecm<-vecmdf$Freq}
          vcitocavdf<-neighlisttabledf[(neighlisttabledf$Var1==0 | neighlisttabledf$Var1==1 |neighlisttabledf$Var1==2 |neighlisttabledf$Var1==5),]
          vmitodf<-neighlisttabledf[(neighlisttabledf$Var1==6) ,];  if ((dim(vmitodf)[1])==0) { vmito = 0} else {vmito<-vmitodf$Freq}
          verdf<-neighlisttabledf[(neighlisttabledf$Var1==6) ,];  if ((dim(verdf)[1])==0) { ver = 0} else {ver<-verdf$Freq}
          if ((dim(vcitocavdf)[1])==0) { vcitocav = 0} else {vcitocav<-sum(vcitocavdf$Freq)}
          vectorneigh<-c(ex, ey, vemp, vecm, vcitocav, ver, vmito)
          ENDOneighdf<-rbind(ENDOneighdf, vectorneigh)
          colnames(ENDOneighdf)<-c("xcoord", "ycoord", "vemp", "vecm", "vcitocav","ver", "vmito")
        } else {
          vectorneigh<-c(ex, ey, 0, 0, 0, 0, 0)
          ENDOneighdf<-rbind(ENDOneighdf, vectorneigh)
          colnames(ENDOneighdf)<-c("xcoord", "ycoord", "vemp", "vecm", "vcitocav","ver",  "vmito")
        }
      }
      if (dim(na.omit(neighlist))[1] > 0) {
      ENDOneighdf$type<-ifelse(ENDOneighdf$vcitocav ==8,"INENDO",0)
      ENDOneighdf$type<-ifelse(ENDOneighdf$vcitocav < 8,"BORDERENDO",ENDOneighdf$type)
      ENDOneighdf$type2<-ifelse(ENDOneighdf$vecm >0,"ECMcontact","NOcontact")
      ENDOneighdf$type2<-ifelse(ENDOneighdf$vemp >0  ,"EMPTYcontact",ENDOneighdf$type2) #if contact with LD and ECM, ld remains
      ENDOnosolos<-subset(ENDOneighdf, type=="INENDO" | type == "BORDERENDO") #all
      ENDOborders<-subset(ENDOneighdf, type=="BORDERENDO") 
      ENDOin<-subset(ENDOneighdf, type=="INENDO") 
      ENDOsolos<-subset(ENDOneighdf, vemp+vecm+vcitocav+ver+vmito==8) 
      
      if (nrow(ENDOsolos)>0) { #reassign isolated
        for (var1 in 1:nrow(ENDOsolos)){
          allv<-c(ENDOsolos$vemp[var1], ENDOsolos$vecm[var1], ENDOsolos$vcitocav[var1],  ENDOsolos$ver[var1], ENDOsolos$vmito[var1]) #class is max +2
          mm<-which.max(allv)
          reassignedclass<-mm+2
          predictiondf2$prediction[predictiondf2$xcoord %in% ENDOsolos$xcoord[var1] & predictiondf2$ycoord %in% ENDOsolos$ycoord[var1]] <- reassignedclass
        }
      }
      
      if(nrow(na.omit(ENDOborders)) > 0 & nrow(na.omit(ENDOnosolos)) >0 ) {
      #now we make groups by continuity
      matchColClasses <- function(df1, df2) {
        sharedColNames <- names(df1)[names(df1) %in% names(df2)]
        sharedColTypes <- sapply(df1[,sharedColNames], class)
        for (n in sharedColNames) {
          class(df2[, n]) <- sharedColTypes[n]
        }
        return(df2)
      }
      
      groupdfEndo <- data.frame(xcoord=numeric(), 
                                ycoord=numeric(),vemp=character(),vecm=character(),vercav=character(),
                                type=character(),type2=character(),group=numeric(), stringsAsFactors=FALSE)
      groupdfEndo2<-sortingroups(groupdfEndo, ENDOborders, ENDOnosolos, 9, steps=1)
      
      groupdfEndo2$group<-as.factor(groupdfEndo2$group);ENDOdf<-groupdfEndo2
      ENDOdf$xcoord<-ENDOdf$xcoord-round(size/2);  ENDOdf$ycoord<-ENDOdf$ycoord-round(size/2);
      grtableENDO<-table(groupdfEndo2$group); grtableENDO<-as.data.frame(grtableENDO)
      grtableNoENDO<-subset(grtableENDO, Freq<endomin | Freq==endomin)
      grtableENDO<-subset(grtableENDO, Freq>endomin) #filter by size
      ENDONUMBENDO<-dim(grtableENDO)[1]
      
      if (nrow(grtableNoENDO)>0) { #reassign small
        for (var1 in grtableNoENDO$Var1){
          agroup<-subset(groupdfEndo2, group== var1)
          totalvemp<-sum(agroup$vemp); totalvecm<-sum(agroup$vecm); totalvcito<-sum(agroup$vcitocav); totalver<-sum(agroup$ver); totalvmito<-sum(agroup$vmito)
          allv<-c(totalvemp, totalvecm, totalvcito, totalver, totalvmito)#class is max +2
          mm<-which.max(allv)
          reassignedclass<-mm+2
          for(row1 in 1:nrow(agroup)){
            predictiondf2$prediction[predictiondf2$xcoord %in% agroup$xcoord[row1] & predictiondf2$ycoord %in% agroup$ycoord[row1]] <- reassignedclass
          }
        }
      }
      }
      }
    }
    
    classplot(1)
    classplot(2)
    #########################################################
  
    precytolocs<-subset(predictiondf2, prediction == 0 | prediction ==1 | prediction == 2 | prediction == 5 | prediction == 6 | prediction == 7 | prediction == 8) 
    
    cytoneighdf <- data.frame(xcoord=numeric(), ycoord=numeric(),
                               vemp=character(), vecm=character(), vcytocav=character(), stringsAsFactors=FALSE)
    for (i in 1:(dim(precytolocs)[1])) { #find neighbours
      ex<-precytolocs[i,]$xcoord; ey<-precytolocs[i,]$ycoord;
      neighlist<-getneighbours(i, precytolocs, predictiondf2, ifsame=FALSE)
      if (dim(neighlist)[1] > 0) {
        neighlisttable<- table (neighlist$prediction)
        neighlisttabledf<-as.data.frame(neighlisttable)
        vempdf<-neighlisttabledf[(neighlisttabledf$Var1==3),];  if ((dim(vempdf)[1])==0) { vemp = 0} else {vemp<-vempdf$Freq}
        vecmdf<-neighlisttabledf[(neighlisttabledf$Var1==4),]; if ((dim(vecmdf)[1])==0) { vecm = 0} else {vecm<-vecmdf$Freq}
        vcytocavdf<-neighlisttabledf[(neighlisttabledf$Var1==0 | neighlisttabledf$Var1==1 |neighlisttabledf$Var1==2 |neighlisttabledf$Var1==5),]
        if ((dim(vcytocavdf)[1])==0) { vcytocav = 0} else {vcytocav<-sum(vcytocavdf$Freq)}
        vectorneigh<-c(ex, ey, vemp, vecm, vcytocav)
        cytoneighdf<-rbind(cytoneighdf, vectorneigh)
        colnames(cytoneighdf)<-c("xcoord", "ycoord", "vemp", "vecm", "vcytocav")
      } else {
        vectorneigh<-c(ex, ey, 0, 0, 0)
        cytoneighdf<-rbind(cytoneighdf, vectorneigh)
        colnames(cytoneighdf)<-c("xcoord", "ycoord", "vemp", "vecm", "vcytocav")
      }
    }
    #classify according to neighbours
    cytoneighdf$type<-ifelse(cytoneighdf$vcytocav ==8,"INcyto",0)
    cytoneighdf$type<-ifelse(cytoneighdf$vcytocav < 8,"BORDERcyto",cytoneighdf$type)
    cytoneighdf$type2<-ifelse(cytoneighdf$vecm >0,"ECMcontact","NOcontact")
    cytoneighdf$type2<-ifelse(cytoneighdf$vemp >0  ,"EMPTYcontact",cytoneighdf$type2) #if contact with LD and ECM, LD remains
    cytonosolos<-subset(cytoneighdf, type=="INcyto" | type == "BORDERcyto") #all
    cytoborders<-subset(cytoneighdf, type=="BORDERcyto") 
    cytoin<-subset(cytoneighdf, type=="INcyto")
    
    #find isolated groups
    groupdfE <- data.frame(xcoord=numeric(), 
                           ycoord=numeric(),vemp=character(),vecm=character(),vcytocav=character(),
                           type=character(),type2=character(),group=numeric(), stringsAsFactors=FALSE)
    groupdfE2<-sortingroups(groupdfE, cytoborders, cytonosolos, 7, steps=1)
    groupdfE2$group<-as.factor(groupdfE2$group)
    cytodf<-groupdfE2
    cytodf$xcoord<-cytodf$xcoord-round(size/2);  cytodf$ycoord<-cytodf$ycoord-round(size/2);
    grtableE<-table(groupdfE2$group)
    grtableE<-as.data.frame(grtableE)
    grtableE<-subset(grtableE, Freq>10*mincav) #filter by size
    CYTONUMBER<-dim(grtableE)[1]
    
    groupdfETEST <- data.frame(xcoord=numeric(), 
                           ycoord=numeric(),vemp=character(),vecm=character(),vcytocav=character(),
                           type=character(),type2=character(),group=numeric(), stringsAsFactors=FALSE)
    groupdfE2TEST<-sortingroups(groupdfETEST, cytoborders, cytonosolos, 7, steps=2) #steps 2 is the key
    groupdfE2TEST$group<-as.factor(groupdfE2TEST$group)
    cytodfTEST<-groupdfE2TEST
    grtableETEST<-table(groupdfE2TEST$group)
    grtableETEST<-as.data.frame(grtableETEST)
    grtableETEST<-subset(grtableETEST, Freq>10*mincav) 
    CYTONUMBERTEST<-dim(grtableETEST)[1]
    #if grouping by two steps gives less cytoplasm, then it is probably one cytoplasm that is fragmented because the groups are really close
    if (CYTONUMBERTEST < CYTONUMBER) { 
      print("cytoplasm seems quite segmented. I will try to fix it")
      BCYTO2 <- data.frame(prediction=character(), xcoord=numeric(), 
                             ycoord=numeric(),pchcodes=character(),colorcodes=character(),cexcodes=character(),
                             X=character(),Y=character())
      BCYTO<-subset(groupdfE2, type == "BORDERcyto")
      BCYTO<-filter(BCYTO, group %in% grtableE$Var1)
      BCYTO<-droplevels(BCYTO)
      for (g in 1:(dim(BCYTO)[1])) {
        step=pxdisplacement
        xxxx<-BCYTO$xcoord[g]; yyyy<-BCYTO$ycoord[g];
        topleft<-predictiondf2[(predictiondf2$xcoord == xxxx-step & predictiondf2$ycoord == yyyy+step),]
        top<-predictiondf2[(predictiondf2$xcoord == xxxx & predictiondf2$ycoord == yyyy+step),]
        topright<-predictiondf2[(predictiondf2$xcoord == xxxx+step & predictiondf2$ycoord == yyyy+step),]
        right<-predictiondf2[(predictiondf2$xcoord == xxxx-step & predictiondf2$ycoord == yyyy),]
        left<-predictiondf2[(predictiondf2$xcoord == xxxx+step & predictiondf2$ycoord == yyyy),]
        bottomright<-predictiondf2[(predictiondf2$xcoord == xxxx+step & predictiondf2$ycoord == yyyy-step),]
        bottom<-predictiondf2[(predictiondf2$xcoord == xxxx & predictiondf2$ycoord == yyyy-step),]
        bottomleft<-predictiondf2[(predictiondf2$xcoord == xxxx-step & predictiondf2$ycoord == yyyy-step),]
        around<-rbind(topleft, top, topright, right, left, bottomright, bottom, bottomleft)
        BCYTO2<-rbind(BCYTO2, around)
      }
      #what do we have in BCYTO2 and not in groupdfE2
      difsxy<-setdiff(BCYTO2[,2:3], groupdfE2[,1:2])
      predictiondf3<-predictiondf2
      for(row1 in 1:nrow(difsxy)){
        predictiondf2$prediction[predictiondf2$xcoord %in% difsxy$xcoord[row1] & predictiondf2$ycoord %in% difsxy$ycoord[row1]] <- 5
      }
      predictiondf2<-repaint() 
      precytolocs<-subset(predictiondf2, prediction == 0 | prediction ==1 | prediction == 2 | prediction == 5) 
      
      cytoneighdf <- data.frame(xcoord=numeric(), ycoord=numeric(),
                                vemp=character(), vecm=character(), vcytocav=character(), stringsAsFactors=FALSE)
      for (i in 1:(dim(precytolocs)[1])) { #get neighbours again
        ex<-precytolocs[i,]$xcoord; ey<-precytolocs[i,]$ycoord;
        neighlist<-getneighbours(i, precytolocs, predictiondf2, ifsame=FALSE)
        if (dim(neighlist)[1] > 0) {
          neighlisttable<- table (neighlist$prediction)
          neighlisttabledf<-as.data.frame(neighlisttable)
          vempdf<-neighlisttabledf[(neighlisttabledf$Var1==3),];  if ((dim(vempdf)[1])==0) { vemp = 0} else {vemp<-vempdf$Freq}
          vecmdf<-neighlisttabledf[(neighlisttabledf$Var1==4),]; if ((dim(vecmdf)[1])==0) { vecm = 0} else {vecm<-vecmdf$Freq}
          vcytocavdf<-neighlisttabledf[(neighlisttabledf$Var1==0 | neighlisttabledf$Var1==1 |neighlisttabledf$Var1==2 |neighlisttabledf$Var1==5),]
          if ((dim(vcytocavdf)[1])==0) { vcytocav = 0} else {vcytocav<-sum(vcytocavdf$Freq)}
          vectorneigh<-c(ex, ey, vemp, vecm, vcytocav)
          cytoneighdf<-rbind(cytoneighdf, vectorneigh)
          colnames(cytoneighdf)<-c("xcoord", "ycoord", "vemp", "vecm", "vcytocav")
        } else {
          vectorneigh<-c(ex, ey, 0, 0, 0)
          cytoneighdf<-rbind(cytoneighdf, vectorneigh)
          colnames(cytoneighdf)<-c("xcoord", "ycoord", "vemp", "vecm", "vcytocav")
        }
      }
      #classify according to neighbours again
      cytoneighdf$type<-ifelse(cytoneighdf$vcytocav ==8,"INcyto",0)
      cytoneighdf$type<-ifelse(cytoneighdf$vcytocav < 8,"BORDERcyto",cytoneighdf$type)
      cytoneighdf$type2<-ifelse(cytoneighdf$vecm >0,"ECMcontact","NOcontact")
      cytoneighdf$type2<-ifelse(cytoneighdf$vemp >0  ,"EMPTYcontact",cytoneighdf$type2) 
      cytonosolos<-subset(cytoneighdf, type=="INcyto" | type == "BORDERcyto") 
      cytoborders<-subset(cytoneighdf, type=="BORDERcyto") 
      cytoin<-subset(cytoneighdf, type=="INcyto") 
      
      #find isolated groups again
      groupdfE <- data.frame(xcoord=numeric(), 
                             ycoord=numeric(),vemp=character(),vecm=character(),vcytocav=character(),
                             type=character(),type2=character(),group=numeric(), stringsAsFactors=FALSE)
      groupdfE2<-sortingroups(groupdfE, cytoborders, cytonosolos, 7, steps=1)
      groupdfE2$group<-as.factor(groupdfE2$group)
      cytodf<-groupdfE2
      cytodf$xcoord<-cytodf$xcoord-round(size/2);  cytodf$ycoord<-cytodf$ycoord-round(size/2);
      grtableE<-table(groupdfE2$group)
      grtableE<-as.data.frame(grtableE)
      grtableE<-subset(grtableE, Freq>10*mincav) #filter by size again
      CYTONUMBER<-dim(grtableE)[1]
    }
    
    if (CYTONUMBER > 1 & CYTONUMBER < 4) {
      grtableE<-grtableE[order(-grtableE$Freq),] 
      grtableE<-grtableE[1:3,]
      #it is very difficult than a TEM image taken with a proper resoluton to show caveolae contains more than three cells. In general we will find one or two
      if (CYTONUMBER == 2) {if (grtableE$Freq[1]/grtableE$Freq[2] > 5) { grtableE<-grtableE[1,]}} #we remove the small if size difference is too big
      if (CYTONUMBER == 3) {
      if (grtableE$Freq[1]/grtableE$Freq[2] > 2) { grtableE<-grtableE[1,]} else { 
        if (grtableE$Freq[2]/grtableE$Freq[3] > 2) { grtableE<-grtableE[2,]}   }
      }
    }
    
    if (CYTONUMBER > 3) {
      print("Detecting more than 3 cytoplasms")
      #it is very difficult than a TEM image taken with a proper resoluton to show caveolae contains more than three cells. In general we will find one or two
      grtableE<-grtableE[order(-grtableE$Freq),] 
      grtableE<-grtableE[1:3,]
      if (grtableE$Freq[1]/grtableE$Freq[2] > 2) { grtableE<-grtableE[1,]} else { 
        if (grtableE$Freq[2]/grtableE$Freq[3] > 2) { grtableE<-grtableE[2,]}   }
    }
    
    CYTONUMBER<-dim(grtableE)[1]
    groupsofcyto<-unique(grtableE$Var1)
    print(paste("number of Cytoplasms: ", CYTONUMBER, sep=""))
    library(dplyr)
    truegroupdfE2<-filter(groupdfE2, group %in% grtableE$Var1) #remove coordinates of excluded cytoplasm
    surfacegroupdfE2<-subset(truegroupdfE2, type2 == "ECMcontact" | type2 =="EMPTYcontact" ) #only surface coordinates
    if (dim(surfacegroupdfE2)[1] >0) {
    surfacegroupdfE2<-droplevels(surfacegroupdfE2); truegroupdfE2<-droplevels(truegroupdfE2)
    #calculate cytoplasm centers
    CYTOcenterX<-flatsummaryby(median, truegroupdfE2$xcoord, truegroupdfE2$group)
    CYTOcenterY<-flatsummaryby(median, truegroupdfE2$ycoord, truegroupdfE2$group)

    dfCYTOsurfaces<-surfacegroupdfE2 
    dfCYTOsurfaces$xcoord<-dfCYTOsurfaces$xcoord-round(size/2); dfCYTOsurfaces$ycoord<-dfCYTOsurfaces$ycoord-round(size/2); 
    }
    #remember predictiondf2 was not updated with excluded cytoplasms

    if (dim(surfacegroupdfE2)[1] >0) {
    par(mfrow=c(1,1))
    par(cex=0.7, mai=c(0.01,0.01,0.01,0.01))
    plot(as.raster(original_tensor[1,,,]))
    if (nrow(FINALDF) > 0) {
    points(FINALDF$xcoord,  1024-(FINALDF$ycoord), col= FINALDF$colorcodes, pch=1, cex=1)
    }
    if (LDnumber > 0) {
    points(dfLDsurfaces$xcoord,  1024-(dfLDsurfaces$ycoord), col= "black", pch=16, cex=0.8)
    text(( LDcenterX$xcoord_median_truegroupdfE2 - round(size/2)), (1024-(LDcenterY$ycoord_median_truegroupdfE2-round(size/2))), cex=2, labels="EMPTY", col="yellow")
    }
    points(dfCYTOsurfaces$xcoord,  1024-(dfCYTOsurfaces$ycoord), col= "white", pch=16, cex=0.8)
    dev.print(svg, paste(path, paste(element, "_FINALPLOTlimitsandcav.svg", sep=""), sep="/"))

    
    #########################################################
    #surfaces
    ########################################################
    #group membrane coordinates
    sortmembranes<-dfCYTOsurfaces[,1:2]
    dfmembraneclusters <- data.frame(xcoord=numeric(), 
                                     ycoord=numeric(),group=numeric(), stringsAsFactors=FALSE)
    membraneclusters<-sortingroups(dfmembraneclusters, sortmembranes, sortmembranes, 2) #we group surfaces
    membraneclusters$group<-as.factor(membraneclusters$group)
    #remove small surface clusters
    tablelimitclusters<-table(membraneclusters$group)
    tablelimitclusters<-as.data.frame(tablelimitclusters)
    tablelimitclusters<- subset(tablelimitclusters,  Freq > 30) 
    library(dplyr)
    membraneclusters<-filter(membraneclusters, group %in% tablelimitclusters$Var1) 
    }

    #membrane classification and interpolation
    if (exists("membraneclusters")) {
    if (dim(membraneclusters)[1] > 0) {
    membraneclusters<-droplevels(membraneclusters)
    
    sortmembranes<-membraneclusters
    colnames(sortmembranes)<-c("xcoord", "ycoord", "cluster", "colorcodes2")
    dfCYTOsurfaces<-merge(dfCYTOsurfaces, sortmembranes, by=c("xcoord", "ycoord"))
    limitmeansx<-flatsummaryby(mean, sortmembranes$xcoord, sortmembranes$cluster)
    limitmeansy<-flatsummaryby(mean, sortmembranes$ycoord, sortmembranes$cluster)
    
    if (LDexpected == TRUE & (dim(preemptylocs)[1])>0 & LDnumber > 0 ) {
    #group ld surfaces
    sortLDs<-dfLDsurfaces[,1:2]
    dfldclusters <- data.frame(xcoord=numeric(), 
                                     ycoord=numeric(),group=numeric(), stringsAsFactors=FALSE)
    ldclusters<-sortingroups(dfldclusters, sortLDs, sortLDs, 2, steps=3) 
    ldclusters$group<-as.factor(ldclusters$group)
    
    tableldclusters<-table(ldclusters$group)
    tableldclusters<-as.data.frame(tableldclusters)
    tableldclusters<- subset(tableldclusters,  Freq > 30) #filter small ld surfaces
    library(dplyr)
    ldclusters<-filter(ldclusters, group %in% tableldclusters$Var1)
    
    sortLDs<-ldclusters
    colnames(sortLDs)<-c("xcoord", "ycoord", "cluster", "colorcodes2")
    dfLDsurfaces<-merge(dfLDsurfaces, sortLDs, by=c("xcoord", "ycoord"))
    ldmeansx<-flatsummaryby(mean, sortLDs$xcoord, sortLDs$cluster)
    ldmeansy<-flatsummaryby(mean, sortLDs$ycoord, sortLDs$cluster)
    #find distance between cytoplasmic surfaces and LD surfaces. The cytoplasmic surface that is closer to the LD surface will in principle correspond to the LD membrane and the other wil be the PM
    distancetoLD <- data.frame(distance=character(), limitcluster=character() , ldcluster=character() )
      for (i in 1:(length(limitmeansx$cluster))) {
        for (j in 1:(length(ldmeansx$cluster))) {
        distance<-sqrt((limitmeansx[[i,2]] - ldmeansx[[j,2]])^2 + (limitmeansy[[i,2]]-ldmeansy[[j,2]])^2)
        clpm<-limitmeansx$cluster[i]
        clld<-ldmeansx$cluster[j]
          distancedf<-data.frame(distance=distance, limitcluster=clpm, ldcluster=clld)
          distancetoLD<-rbind(distancedf, distancetoLD)
      }
    }
    colnames(distancetoLD)<- c("distance", "limitcluster", "ldcluster")
    
    dfCYTOsurfaces$surfacetype<-rep("PM_", dim(dfCYTOsurfaces)[1]) #for the moment all are assigned PM
    dfLDsurfaces$surfacetype<-rep("EMPTY_", dim(dfLDsurfaces)[1])
    #this loop finds the cytoplasmic surface that is LD based on proximity to LD surface and other criteria. The other cytoplasmic surfaces remain as PM

    for (i in (unique(distancetoLD$ldcluster))) {
      aLD<-subset(distancetoLD, ldcluster == i)
      aLD<-aLD[order(aLD$distance),]
      closercytolimit<-aLD$limitcluster[1] #get cytoplasmic surface that is closer to LD surface
      #we calculate all distances and average LD surface-nearest cytoplasmic surface distance
      theld<-subset(dfLDsurfaces, cluster == i); theld<-theld[,1:2]
      if (dim(theld)[1] > 0) {
      thelimit<-subset(dfCYTOsurfaces, cluster == closercytolimit); thelimit<-thelimit[,1:2]
      #calculate minimum distance between all LD surface coordinates and nearest cytoplasmic surface coordinates
      theld$mindist<-rep(NA, dim(theld)[1])
      for (j in 1:(dim(theld)[1])) {
        ldist<-999999999999999999999999999999999999999999999
        theldx<-theld$xcoord[j]
        theldy<-theld$ycoord[j]
        for (k in 1:(dim(thelimit)[1])) {
          thelimitx<-thelimit$xcoord[k]
          thelimity<-thelimit$ycoord[k]
          distc<-sqrt((theldx - thelimitx)^2 + (theldy-thelimity)^2)
          if (distc < ldist) { ldist <- distc}
        }
        theld$mindist[j]<-ldist
      }
      medist<-median(theld$mindist)
      
      if (medist <41) { #quite arbitrary, this worked ok for our images. If cytoplasmic surface close to LD surface, then cytoplasmic surface is LD
        dfCYTOsurfaces$surfacetype<-ifelse(grepl( aLD$limitcluster[1] ,dfCYTOsurfaces$cluster),"LD",dfCYTOsurfaces$surfacetype)
        dfLDsurfaces$surfacetype<-ifelse(grepl( aLD$ldcluster[1] ,dfLDsurfaces$cluster),"LD",dfLDsurfaces$surfacetype)
        
      }
      if (medist >newcropsize) { #if distance bigger than 148 nm, it depends. It will be assigned to PM only if it has not been previously classified as LD (because it is closer to another LD surface, for example)
        subcytosurf<-subset(dfCYTOsurfaces, cluster== aLD$limitcluster[1] )
        if (unique(subcytosurf$surfacetype)=="PM_") { #only if it was not previously labeled as LD in a previous loop round
          dfCYTOsurfaces$surfacetype<-ifelse(grepl( aLD$limitcluster[1] ,dfCYTOsurfaces$cluster),"PM",dfCYTOsurfaces$surfacetype)
          dfLDsurfaces$surfacetype<-ifelse(grepl( aLD$ldcluster[1] ,dfLDsurfaces$cluster),"EMPTY",dfLDsurfaces$surfacetype) #if the cytoplasmic surface is a PM, then the closest "LD" should not be a real LD, maybe an empty ECM space...
        }
      }
      
      if (medist > 0.4*newcropsize & medist < (newcropsize+1)) { #if we find a medium distance, then we have uncertainty. Let's use other criteria to decide
        bordersLD<-subset(truegroupdfLD, type == "BORDERempty") #this has the coordinates of LD (small wrong LD filtered) that are borders
        bordersLD<-droplevels(bordersLD)
        nldclusters<-length(unique(ldclusters$group))
        circvectorLD<-rep(NA, nldclusters) #we will assess ld circularity. LD tend to be circular. If LD droplets are not circular, they are either very marginal in the image or empty areas of ECM
        countcirc<-1
          nld=i
          clusld<-subset(dfLDsurfaces, cluster == nld)
          groupld<-unique(clusld$group)
          if (dim(clusld)[1] >0) {
            perimld<-subset(bordersLD, group == groupld)
            totalperim = dim(perimld)[1] * pxdisplacement #estimation
            areald <- subset(truegroupdfLD, group == groupld)
            totalareald<-dim(areald)[1]*100
            circ<-4*pi*(totalareald/(totalperim^2))
            circvectorLD[countcirc]<-circ 
            #we assess now surface intrincacy. If the surface adjust well to a line, then it is most likely a LD. PM are usually more intrincate
            modelimit<-lm(thelimit$ycoord ~ thelimit$xcoord)
            modelimitrotate<-lm(thelimit$xcoord ~ thelimit$ycoord)
            r2<-summary(modelimit)$r.squared
            sse<-sum((modelimit$residuals)^2)
            sqsse<-sqrt(sse)
            sseR<-sum((modelimitrotate$residuals)^2)
            sqsseR<-sqrt(sseR)
            #now we take a decision
            if (circ > 0.54 | sqsse<1000) { #circular LD area and quite linear surface --> likely a LD
              if (CYTONUMBER ==1 & LDnumber>1) { #this is usually not expected in adipocytes, one cytoplasm and several LD
                #thisLD<- aLD$limitcluster[i]
                thisLD<- i
                if(is.na(thisLD)==FALSE) {
                  ldsize<-as.data.frame(table(ldclusters$group))
                  ldsize<-ldsize[order(-ldsize$Freq),]
                  ldbig<-ldsize$Var1[1] #we get the biggest one
                  if(as.character(ldbig) == as.character(thisLD)) { #if the LD in the loop round is the bigger, then we classify it as LD
                    dfCYTOsurfaces$surfacetype<-ifelse(grepl( aLD$limitcluster[1] ,dfCYTOsurfaces$cluster),"LD",dfCYTOsurfaces$surfacetype)
                    dfLDsurfaces$surfacetype<-ifelse(grepl( aLD$ldcluster[1] ,dfLDsurfaces$cluster),"LD",dfLDsurfaces$surfacetype)
                  } else {
                    subcytosurf<-subset(dfCYTOsurfaces, cluster== aLD$limitcluster[1] )
                    if (unique(subcytosurf$surfacetype)=="PM_") { #only if it was not previously labeled as LD
                      dfCYTOsurfaces$surfacetype<-ifelse(grepl( aLD$limitcluster[1] ,dfCYTOsurfaces$cluster),"PM",dfCYTOsurfaces$surfacetype)
                      dfLDsurfaces$surfacetype<-ifelse(grepl( aLD$ldcluster[1] ,dfLDsurfaces$cluster),"EMPTY",dfLDsurfaces$surfacetype)
                    }
                  }
                }
              } else { #this is the expected, one cytoplasm and one LD. We reassign surface to LD
                dfCYTOsurfaces$surfacetype<-ifelse(grepl( aLD$limitcluster[1] ,dfCYTOsurfaces$cluster),"LD",dfCYTOsurfaces$surfacetype)
                dfLDsurfaces$surfacetype<-ifelse(grepl( aLD$ldcluster[1] ,dfLDsurfaces$cluster),"LD",dfLDsurfaces$surfacetype)
              }
            } else { #low circularity or high intrincancy. we reassign to PM if it was not previously assigned to LD in a previous loop run
              subcytosurf<-subset(dfCYTOsurfaces, cluster== aLD$limitcluster[1] )
              
              if (unique(subcytosurf$surfacetype)=="PM_") { #only if it was not previously labeled as LD
                dfCYTOsurfaces$surfacetype<-ifelse(grepl( aLD$limitcluster[1] ,dfCYTOsurfaces$cluster),"PM",dfCYTOsurfaces$surfacetype)
                dfLDsurfaces$surfacetype<-ifelse(grepl( aLD$ldcluster[1] ,dfLDsurfaces$cluster),"EMPTY",dfLDsurfaces$surfacetype)
              }
            }
          }
      } # end if medium distance
      
      }
    }#end loop for every LD surface
    
    #those that remain unclassified
    dfCYTOsurfaces$surfacetype<-ifelse(grepl( "PM_" ,dfCYTOsurfaces$surfacetype ),"PM",dfCYTOsurfaces$surfacetype)
    dfLDsurfaces$surfacetype<-ifelse(grepl( "EMPTY_" ,dfLDsurfaces$surfacetype ),"EMPTY",dfLDsurfaces$surfacetype)
    
    PM<-subset(dfCYTOsurfaces, surfacetype == "PM") 
    #we interpolate PM surfaces
    if ((exists("membraneclusters"))  & exists("PM")) {
      if (dim(PM)[1] > 0) {
        allPMs<-data.frame(xcoord=numeric(), ycoord=numeric(),
                           cluster=factor(), colorcodes2.y=character(),  surfacetype=character() )
        for ( gg in levels(PM$cluster)) {
          clPM<-subset(PM, cluster == gg)
          if ((dim(clPM)[1]) > 0) {
            intPMcl<-disorderinterpolate(clPM, st=5)
            allPMs<-rbind(allPMs, intPMcl)
          }
        }
        lPMcenterX<-flatsummaryby(median, PM$xcoord, PM$cluster)
        lPMcenterY<-flatsummaryby(median, PM$ycoord, PM$cluster)
      }
    }
    #we interpolate LD surfaces
    if (exists("membraneclusters")  &  exists("dfLDsurfaces")) {
      if(nrow(na.omit(membraneclusters))>0 & nrow(na.omit(dfLDsurfaces))>0){
      LD<-subset(dfLDsurfaces, surfacetype == "LD")
      if(exists("LD")) {
        if ((dim(LD)[1]) > 0) {
          allLDs<-data.frame(xcoord=numeric(), ycoord=numeric(),
                             cluster=factor(), colorcodes2.y=character(),  surfacetype=character() )
          for ( gg in levels(LD$cluster)) {
            clLD<-subset(LD, cluster == gg)
            if ((dim(clLD)[1]) > 0) {
              intLDcl<-disorderinterpolate(clLD, st=5)
              allLDs<-rbind(allLDs, intLDcl)
            }
          }
        }
      }
      
      if(exists("lLDcenterX") & exists("lLDcenterY")) {
      lLDcenterX<-flatsummaryby(median, LD$xcoord, LD$cluster)
      lLDcenterY<-flatsummaryby(median, LD$ycoord, LD$cluster)
      }
      }
    }

    } else {
          PM<-dfCYTOsurfaces #if no LD expected, all cytoplasmic surfaces are PM
          PM$surfacetype<-rep("PM", dim(PM)[1])
          lPMcenterX<-flatsummaryby(median, PM$xcoord, PM$cluster)
          lPMcenterY<-flatsummaryby(median, PM$ycoord, PM$cluster)
          #interpolate PM
          if ((exists("membraneclusters")) & exists("PM")) {
            allPMs<-data.frame(xcoord=numeric(), ycoord=numeric(),
                               cluster=factor(), colorcodes2.y=character(),  surfacetype=character() )
            for ( gg in levels(PM$cluster)) {
              clPM<-subset(PM, cluster == gg)
              if ((dim(clPM)[1]) > 0) {
                intPMcl<-disorderinterpolate(clPM, st=5)
                allPMs<-rbind(allPMs, intPMcl)
              }
            }
          }
    } #end else if no LD
    
    } #end if there are membrane clusters
    } else {print("No PM found") #this is possible, no membrane at all
      PM<-data.frame(xcoord=numeric(), ycoord=numeric(),vemp=numeric(), vecm=numeric(),
                     vcytocav=numeric(), type=character(),type2=character(),group=factor(),
                     colorcodes2.x=character(), cluster=factor(), colorcodes2.y=character(),  surfacetype=character() )
      LD<-data.frame(xcoord=numeric(), ycoord=numeric(),vemp=numeric(), vecm=numeric(),
                     vcytocav=numeric(), type=character(),type2=character(),group=factor(),
                     colorcodes2.x=character(), cluster=factor(), colorcodes2.y=character(),  surfacetype=character() )
    }
    
 
    #reassing erroneous single pit predictions to second most likely tipe if they are far away from PM or if there is no PM in the image
    T1<-subset(FINALDF, prediction == 2) #subset the single pits
    if ((dim(T1)[1]) > 0 & exists("PM")) {
      if (nrow(PM)>0) {
        distmax<-(pxum/1000)*150 #150nm
        for (each in 1:(dim(T1)[1])) {
          xc<- T1$xcoord[each]; yc<- T1$ycoord[each]
          cornersx<-c(xc-distmax, xc-distmax,xc+distmax,xc+distmax )
          cornersy<-c(yc-distmax, yc+distmax,yc-distmax,yc+distmax )
          nearby<-subset(allPMs, xcoord > cornersx[1] & xcoord < cornersx[3] & ycoord > cornersy[1] & ycoord < cornersy[2]) #subset nearby PM coordinates
          if ((dim(nearby)[1]) == 0) {
            probsitem<-subset(PROBSDF, xcoord ==xc & ycoord ==yc)
            secondbest<-which.max(probsitem[, -which.max(probsitem[1:3])][1:3]) # find the maximum, exclude it, and find the maximum of the remaining
            if (names(secondbest) == "typical") {
              secondbest<-which.max(probsitem[1:3])
            }
            if (names(secondbest) == "cavicle") {newpred <- 0; newcol <- "magenta"}
            if (names(secondbest) == "rosette") {newpred <- 1; newcol <- "cyan"}
            FINALDF[FINALDF$xcoord == xc & FINALDF$ycoord == yc,1] <- newpred
            FINALDF[FINALDF$xcoord == xc & FINALDF$ycoord == yc,6] <- newcol
          }
        }
      }
    }
    
    if((dim(T1)[1]) > 0 & (exists("PM") == TRUE)   ) {
      if (dim(PM)[1] == 0 ) { #PM exists but has zero rows, then we reassign single pits
        for (each in 1:(dim(T1)[1])) {
          xc<- T1$xcoord[each]; yc<- T1$ycoord[each]
          probsitem<-subset(PROBSDF, xcoord ==xc & ycoord ==yc)
          secondbest<-which.max(probsitem[, -which.max(probsitem[1:3])][1:3]) # find the maximum, exclude it, and find the maximum of the remaining
          if (names(secondbest) == "typical") {
            secondbest<-which.max(probsitem[1:3])
          }
          if (names(secondbest) == "cavicle") {newpred <- 0; newcol <- "magenta"}
          if (names(secondbest) == "rosette") {newpred <- 1; newcol <- "cyan"}
          
          FINALDF[FINALDF$xcoord == xc & FINALDF$ycoord == yc,1] <- newpred
          FINALDF[FINALDF$xcoord == xc & FINALDF$ycoord == yc,6] <- newcol
        }
      }
    }
    
    if((dim(T1)[1]) > 0 & (exists("PM") == FALSE)   ) { #if no PM, we also change the single pits
      for (each in 1:(dim(T1)[1])) {
        xc<- T1$xcoord[each]; yc<- T1$ycoord[each]
        probsitem<-subset(PROBSDF, xcoord ==xc & ycoord ==yc)
        secondbest<-which.max(probsitem[, -which.max(probsitem[1:3])][1:3]) # find the maximum, exclude it, and find the maximum of the remaining
        if (names(secondbest) == "typical") {
          secondbest<-which.max(probsitem[1:3])
        }
        if (names(secondbest) == "cavicle") {newpred <- 0; newcol <- "magenta"}
        if (names(secondbest) == "rosette") {newpred <- 1; newcol <- "cyan"}
        FINALDF[FINALDF$xcoord == xc & FINALDF$ycoord == yc,1] <- newpred
        FINALDF[FINALDF$xcoord == xc & FINALDF$ycoord == yc,6] <- newcol
      }
    }
    
    
    #we remove caveolae outside of cytoplasmic bounds
    belong<-rep(NA, (dim(FINALDF)[1]))
    if ( exists("cytodf") == TRUE & nrow(FINALDF)>0){
      for (m in 1:(dim(FINALDF)[1])) {
        xcc<-FINALDF$xcoord[m]; ycc<-FINALDF$ycoord[m]
        cysamexy<-subset(cytodf, xcoord < (xcc+50) & xcoord > (xcc -50) & ycoord < (ycc+50) & ycoord > (ycc -50)) #check if cytoplasm predictions in vicinity of caveolae
        cysamexy<-cysamexy[cysamexy$group %in% groupsofcyto,]
        belong[m]<-unique(cysamexy$group)[1]
        if ((dim(cysamexy)[1]) == 0) {
          print("removing caveolae prediction outside of cytoplasm bounds")
          FINALDF[FINALDF$xcoord == xcc & FINALDF$ycoord == ycc,1] <- NA
          FINALDF[FINALDF$xcoord == xcc & FINALDF$ycoord == ycc,6] <- NA
        }
      }
    }
    FINALDF$cell<-belong #to know to which cell each caveolae belongs. Important to calculate PM-LD distances
    write.table(FINALDF, paste(path, paste(element, "_CAVDF.txt", sep=""), sep="/"))
    
    #we save
    if (LDnumber > 0) {
    if ((exists("membraneclusters"))  & (exists("allPMs"))) {
      write.table(allPMs, paste(path, paste(element, "_intPM.txt", sep=""), sep="/"))
      }
      if(exists("LD")) {
        if ((dim(LD)[1]) > 0) {
          write.table(allLDs, paste(path, paste(element, "_intLD.txt", sep=""), sep="/"))
        }
      }
    } else {
      # if (exists("membraneclusters") & (exists("PM"))) {points(PM$xcoord,  1024-(PM$ycoord), col= "white", pch=16, cex=0.8)
      #   }
      if (exists("membraneclusters")  & (exists("allPMs"))) {
        write.table(allPMs, paste(path, paste(element, "_intPM.txt", sep=""), sep="/"))
        }
    }
    
    #we plot
    par(mfrow=c(1,1))
    par(cex=0.7, mai=c(0.01,0.01,0.01,0.01))
    plot(as.raster(original_tensor[1,,,]))
    if(nrow(FINALDF)>0) {points(FINALDF$xcoord,  1024-(FINALDF$ycoord), col= FINALDF$colorcodes, pch=16, cex=1)}
    if (LDnumber > 0) {
      if(exists("LD")){
      if ((dim(LD)[1]) > 0) {points(allLDs$xcoord,  1024-(allLDs$ycoord), col= "black", pch=16, cex=0.8)}
      }
      if (exists("membraneclusters") & (exists("allPMs"))) { points(allPMs$xcoord,  1024-(allPMs$ycoord), col= "white", pch=16, cex=0.8) }
      if(exists("lLDcenterX") & exists("lLDcenterY")) {
        text(( lLDcenterX$xcoord_median_LD ), (1024-(lLDcenterY$ycoord_median_LD)), cex=2, labels="LD", col="darkviolet", font=2)
        text(( lLDcenterX$xcoord_median_LD + 75 ), (1024-(lLDcenterY$ycoord_median_LD)), cex=2, labels=lLDcenterX$cluster, col="darkviolet", font=2)
      }
      if (exists("membraneclusters") & (exists("allPMs"))) {
        points(allPMs$xcoord,  1024-(allPMs$ycoord), col= "white", pch=16, cex=0.8)
        text(( lPMcenterX$xcoord_median_PM ), (1024-(lPMcenterY$ycoord_median_PM)), cex=2, labels="PM", col="darkviolet", font=2) }
        dev.print(svg, paste(path, paste(element, "_FINALPLOTINTERPOLATED.svg", sep=""), sep="/"))
    } else {
      if (exists("membraneclusters")  & (exists("allPMs"))) {
        points(allPMs$xcoord,  1024-(allPMs$ycoord), col= "white", pch=16, cex=0.8)
        text(( lPMcenterX$xcoord_median_PM ), (1024-(lPMcenterY$ycoord_median_PM)), cex=2, labels="PM", col="darkviolet", font=2) }
      dev.print(svg, paste(path, paste(element, "_FINALPLOTINTERPOLATED.svg", sep=""), sep="/"))
    }
    
  } else {print("No CYTOP expected or found or cell limits were not requested")}
  
  FINALDF<-subset(FINALDF, !is.na(FINALDF[,1]))
  
  #assign each surface to a cytoplasmic group to see to which cell each surface belongs 
  if (FindCellLimits ==TRUE) {
    cytodfb<-subset(cytodf, type =="BORDERcyto")
    cytodfb<-cytodfb[cytodfb$group %in% groupsofcyto,]
    cytodfb<-droplevels(cytodfb)
    if (exists("allPMs") == TRUE) {allPMstry<-allPMs}
    if (exists("allLDs") == TRUE) {allLDstry<-allLDs}
    if (exists("allPMs") == TRUE ) {
      if ((dim(allPMs)[1]) > 0) {
        allPMs<- expandcyto(cytodfb, allPMstry, 3)
      }
    }
    if (exists("allLDs") == TRUE ) {
      if ( (dim(allLDs)[1]) > 0) { 
        allLDs<- expandcyto(cytodfb, allLDstry, 20)
      }
    }
  }
  
  if (FindCellLimits ==TRUE) {FINALDF$cell <-as.factor(FINALDF$cell)}
  
  #find minimum distances between caveolae and PM and caveolae and LD for each cell  
  if ((measurecavdistances == TRUE )  & nrow(FINALDF)>0){
    if ( measurecavdistances == TRUE ) {
      if (exists("allPMs") == TRUE) {
        #find minimum distances between caveolae and PM for each cell  
        if ( (dim(allPMs)[1]) > 0) {
        distPM<-rep(NA, (dim(FINALDF)[1]))
        PMX<-rep(NA, (dim(FINALDF)[1]))
        PMY<-rep(NA, (dim(FINALDF)[1]))
        for (cc in levels(FINALDF$cell)) {
          eachPM <- subset(allPMs, belong == cc)
          if(nrow(eachPM)>0){
            for (ii in 1:(dim(FINALDF)[1])){
              if (FINALDF$cell[ii] == cc) {
                minds=9999999999999999999999999999999999999999999999999999999999
                for (jj in 1:(dim(eachPM)[1])) {
                  ds<-sqrt ((FINALDF$xcoord[ii]-eachPM$xcoord[jj])^2 + (FINALDF$ycoord[ii]-eachPM$ycoord[jj])^2)
                  if (ds < minds) {
                    minds=ds
                    pmx<-eachPM$xcoord[jj]; pmy<-eachPM$ycoord[jj]
                  }
                }
                distPM[ii] <- minds
                PMX[ii] <- pmx
                PMY[ii] <- pmy
              }
            } #ends loop for each caveolae of the cell
          }
        } #ends loop for each cell
        FINALDF$distPM<-distPM
        FINALDF$PMX<-PMX
        FINALDF$PMY<-PMY
        } 
      } else{ #if no PM , we complete with NA, we may need that the dataframes have similar rows to combine them later
        FINALDF$distPM<-rep(NA, (dim(FINALDF)[1]))
        FINALDF$PMX<-rep(NA, (dim(FINALDF)[1]))
        FINALDF$PMY<-rep(NA, (dim(FINALDF)[1]))
      }
      
      #find minimum distances between caveolae and LD for each cell
      if (exists("allLDs") == TRUE ) {
        if ( (dim(allLDs)[1]) > 0) {
        distLD<-rep(NA, (dim(FINALDF)[1]))
        LDX<-rep(NA, (dim(FINALDF)[1]))
        LDY<-rep(NA, (dim(FINALDF)[1]))
        for (cc in levels(FINALDF$cell)) {
          eachLD <- subset(allLDs, belong == cc)
          if(nrow(eachLD)>0) {
            for (ii in 1:(dim(FINALDF)[1])){
              if (FINALDF$cell[ii] == cc) {
                minds=9999999999999999999999999999999999999999999999999999999999
                for (jj in 1:(dim(eachLD)[1])) {
                  ds<-sqrt ((FINALDF$xcoord[ii]-eachLD$xcoord[jj])^2 + (FINALDF$ycoord[ii]-eachLD$ycoord[jj])^2)
                  if (ds < minds) {
                    minds=ds
                    LDx<-eachLD$xcoord[jj]; LDy<-eachLD$ycoord[jj]
                  }
                }
                distLD[ii] <- minds
                LDX[ii] <- LDx
                LDY[ii] <- LDy
              }
            } #ends loop for each caveolae
          }
        } #ends loop for each cell
        FINALDF$distLD<-distLD
        FINALDF$LDX<-LDX
        FINALDF$LDY<-LDY
        } 
      } else{
        FINALDF$distLD<-rep(NA, (dim(FINALDF)[1]))
        FINALDF$LDX<-rep(NA, (dim(FINALDF)[1]))
        FINALDF$LDY<-rep(NA, (dim(FINALDF)[1]))
      }
      
      FINALDF$distancia<- FINALDF$distPM+FINALDF$distLD #sum both distances to get PM-LD distance through caveolae
      #plot
      par(mfrow=c(1,1))
      par(cex=0.7, mai=c(0.01,0.01,0.01,0.01))
      plot(as.raster(original_tensor[1,,,]))
      points(FINALDF$xcoord,  1024-(FINALDF$ycoord), col= FINALDF$colorcodes, pch=16, cex=1)
      segments(FINALDF$xcoord, 1024-FINALDF$ycoord, FINALDF$PMX, 1024- FINALDF$PMY, col="yellow")
      segments(FINALDF$xcoord, 1024-FINALDF$ycoord, FINALDF$LDX, 1024- FINALDF$LDY, col="green")
      if (LDnumber > 0) {
        if(exists("LD")) {
          if ((dim(LD)[1]) > 0) {points(allLDs$xcoord,  1024-(allLDs$ycoord), col= "black", pch=16, cex=0.8)}
        }
        if (exists("membraneclusters") & (exists("allPMs"))) { points(allPMs$xcoord,  1024-(allPMs$ycoord), col= "white", pch=16, cex=0.8) }
        if(exists("lLDcenterX") & exists("lLDcenterY")) {
        text(( lLDcenterX$xcoord_median_LD ), (1024-(lLDcenterY$ycoord_median_LD)), cex=2, labels="LD", col="darkviolet", font=2)
        text(( lLDcenterX$xcoord_median_LD + 75 ), (1024-(lLDcenterY$ycoord_median_LD)), cex=2, labels=lLDcenterX$cluster, col="darkviolet", font=2)
        }
        if (exists("membraneclusters") & (exists("allPMs"))) {
          points(allPMs$xcoord,  1024-(allPMs$ycoord), col= "white", pch=16, cex=0.8)
          text(( lPMcenterX$xcoord_median_PM ), (1024-(lPMcenterY$ycoord_median_PM)), cex=2, labels="PM", col="darkviolet", font=2) }
        dev.print(svg, paste(path, paste(element, "_FINALPLOTDIST.svg", sep=""), sep="/"))
      } else {
        if (exists("membraneclusters") & (exists("allPMs"))) {
          points(allPMs$xcoord,  1024-(allPMs$ycoord), col= "white", pch=16, cex=0.8)
          text(( lPMcenterX$xcoord_median_PM ), (1024-(lPMcenterY$ycoord_median_PM)), cex=2, labels="PM", col="darkviolet", font=2) }
        dev.print(svg, paste(path, paste(element, "_FINALPLOTDIST.svg", sep=""), sep="/"))
      }
    }
  }
  
  #find mimum PM-LD distances for each cell
  if (( measurePMLDdistances == TRUE) ){
    if (exists("allLDs") == TRUE & exists("allPMs") == TRUE) {
      if ( (dim(allLDs)[1]) > 0 &  (dim(allPMs)[1]) > 0 ) {
        allPMs$belong<-as.factor(allPMs$belong)
        allLDs$belong<-as.factor(allLDs$belong)
        DistToLD<-rep(NA, (dim(allPMs)[1]))
        LDx<-rep(NA, (dim(allPMs)[1]))
        LDy<-rep(NA, (dim(allPMs)[1]))
        for (cc in levels(allPMs$belong)) {
          eachLD <- subset(allLDs, belong == cc)
          if(nrow(eachLD)>0){
            for (ii in 1:(dim(allPMs)[1])){
              if (allPMs$belong[ii] == cc) {
                minds=9999999999999999999999999999999999999999999999999999999999
                for (jj in 1:(dim(eachLD)[1])) {
                  ds<-sqrt ((allPMs$xcoord[ii]-eachLD$xcoord[jj])^2 + (allPMs$ycoord[ii]-eachLD$ycoord[jj])^2)
                  if (ds < minds) {
                    minds=ds
                    ldx<-eachLD$xcoord[jj]; ldy<-eachLD$ycoord[jj]
                  }
                }
                DistToLD[ii] <- minds
                LDx[ii] <- ldx; LDy[ii] <- ldy
              }
            } 
          }
        } 
        allPMs$DistToLD<-DistToLD
        allPMs$LDx<-LDx; allPMs$LDy<-LDy
        #plot
        par(mfrow=c(1,1))
        par(cex=0.7, mai=c(0.01,0.01,0.01,0.01))
        plot(as.raster(original_tensor[1,,,]))
        if(nrow(FINALDF)>0) {points(FINALDF$xcoord,  1024-(FINALDF$ycoord), col= FINALDF$colorcodes, pch=1, cex=1)}
        if (exists("membraneclusters") & (exists("allPMs"))) {segments(allPMs$xcoord, 1024-allPMs$ycoord, allPMs$LDx, 1024- allPMs$LDy, col="yellow")}
        if (LDnumber > 0) {
          if(exists("LD")) {
            if ((dim(LD)[1]) > 0) {points(allLDs$xcoord,  1024-(allLDs$ycoord), col= "black", pch=16, cex=0.8)}
          }
          if (exists("membraneclusters") & (exists("allPMs"))) { points(allPMs$xcoord,  1024-(allPMs$ycoord), col= "white", pch=16, cex=0.8) }
          if(exists("lLDcenterX") & exists("lLDcenterY")) {
          text(( lLDcenterX$xcoord_median_LD ), (1024-(lLDcenterY$ycoord_median_LD)), cex=2, labels="LD", col="darkviolet", font=2)
          text(( lLDcenterX$xcoord_median_LD + 75 ), (1024-(lLDcenterY$ycoord_median_LD)), cex=2, labels=lLDcenterX$cluster, col="darkviolet", font=2)
          }
          if (exists("membraneclusters") & (exists("allPMs"))) {points(allPMs$xcoord,  1024-(allPMs$ycoord), col= "white", pch=16, cex=0.8)}
            text(( lPMcenterX$xcoord_median_PM ), (1024-(lPMcenterY$ycoord_median_PM)), cex=2, labels="PM", col="darkviolet", font=2) }
          dev.print(svg, paste(path, paste(element, "_FINALPLOTDISTPMLD.svg", sep=""), sep="/"))
          write.table(allPMs, paste(path, paste(element, "_intPM.txt", sep=""), sep="/"))
          write.table(allLDs, paste(path, paste(element, "_intLD.txt", sep=""), sep="/"))
      }
    }
  }
  
  #save
  if (exists("membraneclusters")  & (exists("allPMs"))) {
    write.table(allPMs, paste(path, paste(element, "_intPM.txt", sep=""), sep="/"))
  }
  if ((exists("allLDs"))) {
    write.table(allLDs, paste(path, paste(element, "_intLD.txt", sep=""), sep="/"))
  }
  write.table(FINALDF, paste(path, paste(element, "_CAVDFDISTANCES.txt", sep=""), sep="/"))
  
  predictiondf2$colorcodes<-as.character(predictiondf2$colorcodes)
  predictiondf2<-repaint() 
  #write.table(predictiondf2, paste(path, paste(element, "_FINALPREDTHRIMG.txt", sep=""), sep="/"))
  #more plots and saves
  predictiondf2nopad<-predictiondf2
  predictiondf2nopad$xcoord<-predictiondf2nopad$xcoord-round(size/2)
  predictiondf2nopad$ycoord<-predictiondf2nopad$ycoord-round(size/2)
  par(mfrow=c(1,1))
  par(cex=0.7, mai=c(0.01,0.01,0.01,0.01))
  plot(as.raster(original_tensor[1,,,]))
  for (i in 1:(dim(predictiondf2nopad)[1])) {
    if (predictiondf2nopad$pchcodes[i] == 0) {
      points(predictiondf2nopad$xcoord[i], 1024-(predictiondf2nopad$ycoord[i]), col= predictiondf2nopad$colorcodes[i], pch=16, cex=0.6)
    } else { if ( sample(0:1, 1, replace=T,prob=c(0,1)) == 1) { #antes prob=c(0.9,0.1)
      points(predictiondf2nopad$xcoord[i],  1024-(predictiondf2nopad$ycoord[i]), col= predictiondf2nopad$colorcodes[i], pch=16, cex=0.6) #antes cex=0.5
    }
    }
  }
  if(exists("allPMs")){ if(nrow(allPMs)>1) {points(allPMs$xcoord,  1024-(allPMs$ycoord), col= "white", pch=16, cex=0.8)}}
  if(exists("allLDs")){if(nrow(allLDs)>1) {points(allLDs$xcoord,  1024-(allLDs$ycoord), col= "black", pch=16, cex=0.8)}}
  if(exists("FINALDF")) {if(nrow(FINALDF)>1) {points(FINALDF$xcoord,  1024-(FINALDF$ycoord), col= FINALDF$colorcodes, pch=16, cex=1)}}
  write.table(predictiondf2nopad, paste(path, paste(element, "_FINALPREDTHRIMAGE.txt", sep=""), sep="/"))
  if(exists("cytodf")){write.table(cytodf, paste(path, paste(element, "_CYTOPREDICTIONS.txt", sep=""), sep="/"))}
 
  dev.print(svg, paste(path, paste(element, "_PREDICTIONSFINAL.svg", sep=""), sep="/"))
  print("Image done. I'll go for the next one.")
  if (deleteENVwhenfinished == TRUE) {
    print("Deleting all things in environment now")
    rm(list=setdiff(ls(), c("modelcavnocav", "modelclascav", "windowsize", "pxdisplacement",
                            "FindCellLimits", "LDexpected", "certainty", "extensions", "timeinit", "deleteENVwhenfinished",
                            "measurecavdistances", "measurePMLDdistances",  "pxum", "size",
                            "path", "filelist", "listrequired","specifyfilelist", "ignoredfiles","finalfiles", "mode", 
                            "pixelsize", "pxum", "mincavnm", "maxcavnm", "avcavnm", "size100",
                            "repaint_predictiondf2", "repaint", "classplot")))

  }

  }
}

print("All done. I hope I did a good job ^^")
print(paste("started ", timeinit, sep=""))
print(paste("ended " , Sys.time(), sep=""))
