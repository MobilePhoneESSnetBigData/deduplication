# 
# s1<-0
# for(i in 1:90) {
#   mdx[[i]]<-buildDeltaProb(centrs, postLoc[[43]], postLoc[[44]], path_root, 1, i)
#   mdy[[i]]<-buildDeltaProb(centrs, postLoc[[43]], postLoc[[44]], path_root, 2, i)
#   mm<-max(dr249[i], dr250[i])
#   s1<-s1+(modeDelta(mdx[[i]])<mm & modeDelta(mdy[[i]])<mm)
#   print(paste0(modeDelta(mdx[[i]]), ":", mm ,"\n"))
#   print(paste0(modeDelta(mdx[[i]]), ":", mm ,"\n"))
# }



devices <- sort(as.numeric(getDeviceIDs(events)))
postLoc <- list() 
centerOfProbs <- list()
dr<-list()
mdy<-list(length = 90)
mdx<-list(length = 90)
for( i in 1:length(devices)) {
  postLoc[[i]] <- readPostLocProb(path_root, devices[i]) 
  centerOfProbs[[i]] <- centerOfProbabilities(centrs, postLoc[[i]])
  dr[[i]] <- dispersionRadius(centrs,postLoc[[i]], centerOfProbs[[i]])
}

P1 <- aprioriDuplicityProb(simParams$prob_sec_mobile_phone, length(devices))
P2 <- 1 - P1
alpha<-P1/P2
n<-length(devices)
pmoded1d2<-Matrix(nrow = n, ncol = n )
pd1d2<-Matrix(nrow = n, ncol = n )
for(i in 1:4) {
  if(i<4) {
    for(j in (i+1):4) {
      s1<-0
      mm<-max(dr[[i]], dr[[j]])
      for(t in 1:90) {
        mdx[[t]]<-buildDeltaProb(centrs, postLoc[[i]][,t], postLoc[[j]][,t], path_root, 1, t)
        mdy[[t]]<-buildDeltaProb(centrs, postLoc[[i]], postLoc[[j]], path_root, 2, t)
        print(paste0(modeDelta(mdx[[t]]), ":", mm ,"\n"))
        print(paste0(modeDelta(mdy[[t]]), ":", mm ,"\n"))
        s1<-s1+(modeDelta(mdx[[t]])<mm & modeDelta(mdy[[t]])<mm)
      }
      pmoded1d2[i,j] <- s1/90
      pd1d2[i,j] <- 1- 1/(1+alpha*pmoded1d2[i,j]/(1-pmoded1d2[i,j]))
      print(paste0(pmoded1d2[i,j], " : ", pd1d2[i,j], "\n"))
      print(paste0(modeDelta(mdx[[t]]), ":", modeDelta(mdy[[t]])))
    }
  }
}


