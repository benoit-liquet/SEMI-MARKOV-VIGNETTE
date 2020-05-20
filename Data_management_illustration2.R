library("SemiMarkov")
library(flexsurv)
data("asthma", package = "SemiMarkov")
head(asthma)
table.state(asthma)
require(mstate)

##########################################################
###### functions to expand the data from SemiMarkov model
###########################################################
expand.row.first <- function(data.row){
  if(data.row$state.h!=data.row$state.j){
    res <- data.frame(id=data.row$id,from=data.row$state.h,to=data.row$state.j,Tsart=0,Tstop=data.row$time,status=1,trans=paste(data.row$state.h,data.row$state.j,sep=""),data.row[,5:7]) 
    transC <- setdiff(c(1,2,3),c(data.row$state.h,data.row$state.j))
    res1 <- data.frame(id=data.row$id,from=data.row$state.h,to=transC[1],Tsart=0,Tstop=data.row$time,status=0,trans=paste(data.row$state.h,transC[1],sep=""),data.row[,5:7]) 
    #res2 <- data.frame(id=data.row$id,from=data.row$state.h,to=transC[2],Tsart=0,Tstop=data.row$time,status=0,trans=paste(data.row$state.h,transC[2],sep=""),data.row[,5:7]) 
    res <- rbind(res,res1)
  }else{
    transC <- setdiff(c(1,2,3),data.row$state.h)
    res1 <- data.frame(id=data.row$id,from=data.row$state.h,to=transC[1],Tsart=0,Tstop=data.row$time,status=0,trans=paste(data.row$state.h,transC[1],sep=""),data.row[,5:7]) 
    res2 <- data.frame(id=data.row$id,from=data.row$state.h,to=transC[2],Tsart=0,Tstop=data.row$time,status=0,trans=paste(data.row$state.h,transC[2],sep=""),data.row[,5:7]) 
    res <- rbind(res1,res2)
  }
  result <- list(Tstart=data.row$time,res=res)
}

expand.row <- function(data.row,Tstart){
  if(data.row$state.h!=data.row$state.j){
    res <- data.frame(id=data.row$id,from=data.row$state.h,to=data.row$state.j,Tsart=Tstart,Tstop=Tstart+data.row$time,status=1,trans=paste(data.row$state.h,data.row$state.j,sep=""),data.row[,5:7]) 
    transC <- setdiff(c(1,2,3),c(data.row$state.h,data.row$state.j))
    res1 <- data.frame(id=data.row$id,from=data.row$state.h,to=transC[1],Tsart=Tstart,Tstop=Tstart+data.row$time,status=0,trans=paste(data.row$state.h,transC[1],sep=""),data.row[,5:7]) 
    #res2 <- data.frame(id=data.row$id,from=data.row$state.h,to=transC[2],Tsart=Tstart,Tstop=Tstart+data.row$time,status=0,trans=paste(data.row$state.h,transC[2],sep=""),data.row[,5:7]) 
    res <- rbind(res,res1)
  }else{
    transC <- setdiff(c(1,2,3),data.row$state.h)
    res1 <- data.frame(id=data.row$id,from=data.row$state.h,to=transC[1],Tsart=Tstart,Tstop=Tstart+data.row$time,status=0,trans=paste(data.row$state.h,transC[1],sep=""),data.row[,5:7]) 
    res2 <- data.frame(id=data.row$id,from=data.row$state.h,to=transC[2],Tsart=Tstart,Tstop=Tstart+data.row$time,status=0,trans=paste(data.row$state.h,transC[2],sep=""),data.row[,5:7]) 
    res <- rbind(res1,res2)
  }
  result <- list(Tstart=Tstart+data.row$time,expand=res)
  
} 
expand.data <- function(id,data){
  data.id <- data[which(data$id==id),]
  n.id <- dim(data.id)[1]
  result <- expand.row.first(data.id[1,])
  Tstart <- result$Tstart
  if(n.id==1){
    resfinal <- result$res
  }else{ 
    res <- result$res
    for(j in 1:(n.id-1)){
      res1 <- expand.row(data.id[j+1,],Tstart=Tstart)
      res <- rbind(res,res1$expand)
      Tstart <- res1$Tstart
    }
    resfinal <- res  
  }
  result <- list(resfinal=resfinal)
}

############################################################################


#newA <- data.frame(asthma,trans=interaction(asthma$state.h,asthma$state.j))
#head(newA)
#table(newA$trans)

name.data <-c("id","from","to","Tstart","Tstop","status","trans")
ind <- unique(asthma$id)
nbid <- table(asthma$id)
id.data <- rep(ind,times=nbid+1)
data <- asthma
res <- NULL
for (j in ind) {
  test <- expand.data(j,data) 
  res <- rbind(res,test$resfinal)
}

asthma.wide <- res

save(asthma.wide,file="asthma-wide-article.Rdata")




 


