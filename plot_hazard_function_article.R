plot.hazard.benoit<-function(x,x2=NULL,x3=NULL,x4=NULL,x5=NULL,x6=NULL,x7=NULL,
                      x8=NULL,x9=NULL,x10=NULL,transitions=NULL,names=NULL,legend=TRUE,legend.pos=NULL,cex=NULL,colors=NULL,xlab="Time",ylab="Hazard function",
                      lwd=3,lty=lty,type="p",ylim,title_paper="Semi-Markov process hazard rate",...){
  
  if(missing(x))stop("Argument 'x' is missing with no default")
  
  if (!inherits(x, "hazard")) 
    stop("expected object to be a result of the 'hazard' function")
  if(legend==FALSE && length(legend.pos>0))
    warning("No legend displayed")
  
  k<-length(x$call)
  x1<-x$vector
  if(k>1){
    for(i in 2:k){
      x1<-cbind(x1,x$vector)}}
  #boundaries for Time
  #they need to be equal for all the arguments
  t<-max(x$Time)
  s<-min(x$Time)
  length<-length(x$Time)
  if(missing(names)){
    N<-0
    Names<-x$call
    cov<-x$cova
  }
  else{
    N<-1
    Names<-names}
  #type of hazard (alpha/lambda)
  Type<-x$Type
  if(length(x2[[1]])>1){
    if (!inherits(x2, "hazard")) 
      stop("expected object [[2]] to be a result of the 'hazard' function")
    t1<-max(x2$Time)
    s1<-min(x2$Time)
    length1<-length(x2$Time)
    if(t!=t1 || s!=s1 || length!=length1)
      stop("The compatibility of Time is required")
    if(missing(names)){
      Names<-c(Names,x2$call)
      cov<-c(cov,x2$cova)}
    Type1<-x2$Type
    if(Type!=Type1)
      stop("All arguments need to represent the same type of risk")
    if(dim(x2$vector)[2]!=dim(x$vector)[2])
      stop("All arguments need to have the same number of transitions considered")
    k_temp<-length(x2$call)
    k<-k+k_temp
    x1<-cbind(x1,x2$vector)
    if(k_temp>1){
      for(i in 2:k_temp){x1<-cbind(x1,x2$vector)}}
  }
  if(length(x3)>1){
    if (!inherits(x3, "hazard")) 
      stop("expected object [[3]]  to be a result of the 'hazard' function")
    t1<-max(x3$Time)
    s1<-min(x3$Time)
    length1<-length(x3$Time)
    if(t!=t1 || s!=s1 || length!=length1)
      stop("The compatibility of Time is required")
    if(missing(names)){
      Names<-c(Names,x3$call)
      cov<-c(cov,x3$cova)}
    Type1<-x3$Type
    if(Type!=Type1)
      stop("All arguments need to represent the same type of risk")
    if(dim(x3$vector)[2]!=dim(x$vector)[2])
      stop("All arguments need to have the same number of transitions considered")
    k_temp<-length(x3$call)
    k<-k+k_temp
    x1<-cbind(x1,x3$vector)
    if(k_temp>1){ 
      for(i in 2:k_temp){x1<-cbind(x1,x3$vector)}}
  }
  if(length(x4)>1){
    if (!inherits(x4, "hazard")) 
      stop("expected object [[4]] to be a result of the 'hazard' function")
    t1<-max(x4$Time)
    s1<-min(x4$Time)
    length1<-length(x4$Time)
    if(t!=t1 || s!=s1 || length!=length1)
      stop("The compatibility of Time is required")
    if(missing(names)){
      Names<-c(Names,x4$call)
      cov<-c(cov,x4$cova)}
    Type1<-x4$Type
    if(Type!=Type1)
      stop("All arguments need to represent the same type of risk")
    if(dim(x4$vector)[2]!=dim(x$vector)[2])
      stop("All arguments need to have the same number of transitions considered")
    k_temp<-length(x4$call)
    k<-k+k_temp
    x1<-cbind(x1,x4$vector)
    if(k_temp>1){
      for(i in 2:k_temp){x1<-cbind(x1,x4$vector)}}
  }
  if(length(x5)>1){
    if (!inherits(x5, "hazard")) 
      stop("expected object [[5]] to be a result of the 'hazard' function")
    t1<-max(x5$Time)
    s1<-min(x5$Time)
    length1<-length(x5$Time)
    if(t!=t1 || s!=s1 || length!=length1)
      stop("The compatibility of Time is required")
    if(missing(names)){
      Names<-c(Names,x5$call)
      cov<-c(cov,x5$cova)}
    Type1<-x5$Type
    if(Type!=Type1)
      stop("All arguments need to represent the same type of risk")
    if(dim(x5$vector)[2]!=dim(x$vector)[2])
      stop("All arguments need to have the same number of transitions considered")
    k_temp<-length(x5$call)
    k<-k+k_temp
    x1<-cbind(x1,x5$vector)
    if(k_temp>1){
      for(i in 2:k_temp){x1<-cbind(x1,x5$vector)}}
  }
  if(length(x6)>1){
    if (!inherits(x6, "hazard")) 
      stop("expected object [[6]] to be a result of the 'hazard' function")
    t1<-max(x6$Time)
    s1<-min(x6$Time)
    length1<-length(x6$Time)
    if(t!=t1 || s!=s1 || length!=length1)
      stop("The compatibility of Time is required")
    if(missing(names)){
      Names<-c(Names,x6$call)
      cov<-c(cov,x6$cova)}
    Type1<-x6$Type
    if(Type!=Type1)
      stop("All arguments need to represent the same type of risk")
    if(dim(x6$vector)[2]!=dim(x$vector)[2])
      stop("All arguments need to have the same number of transitions considered")
    k_temp<-length(x6$call)
    k<-k+k_temp
    x1<-cbind(x1,x6$vector)
    if(k_temp>1){
      for(i in 2:k_temp){x1<-cbind(x1,x6$vector)}}
  }
  if(length(x7)>1){
    if (!inherits(x7, "hazard")) 
      stop("expected object [[7]] to be a result of the 'hazard' function")
    t1<-max(x7$Time)
    s1<-min(x7$Time)
    length1<-length(x7$Time)
    if(t!=t1 || s!=s1 || length!=length1)
      stop("The compatibility of Time is required")
    if(missing(names)){
      Names<-c(Names,x7$call)
      cov<-c(cov,x7$cova)}
    Type1<-x7$Type
    if(Type!=Type1)
      stop("All arguments need to represent the same type of risk")
    if(dim(x7$vector)[2]!=dim(x$vector)[2])
      stop("All arguments need to have the same number of transitions considered")
    k_temp<-length(x7$call)
    k<-k+k_temp
    x1<-cbind(x1,x7$vector)
    if(k_temp>1){
      for(i in 2:k_temp){x1<-cbind(x1,x7$vector)}}
  }
  if(length(x8)>1){
    if (!inherits(x8, "hazard")) 
      stop("expected object [[8]] to be a result of the 'hazard' function")
    t1<-max(x8$Time)
    s1<-min(x8$Time)
    length1<-length(x8$Time)
    if(t!=t1 || s!=s1 || length!=length1)
      stop("The compatibility of Time is required")
    if(missing(names)){
      Names<-c(Names,x8$call)
      cov<-c(cov,x8$cova)}
    Type1<-x8$Type
    if(Type!=Type1)
      stop("All arguments need to represent the same type of risk")
    if(dim(x8$vector)[2]!=dim(x$vector)[2])
      stop("All arguments need to have the same number of transitions considered")
    k_temp<-length(x8$call)
    k<-k+k_temp
    x1<-cbind(x1,x8$vector)
    if(k_temp>1){
      for(i in 2:k_temp){x1<-cbind(x1,x8$vector)}}
  }
  if(length(x9)>1){
    if (!inherits(x9, "hazard")) 
      stop("expected object [[9]] to be a result of the 'hazard' function")
    t1<-max(x9$Time)
    s1<-min(x9$Time)
    length1<-length(x9$Time)
    if(t!=t1 || s!=s1 || length!=length1)
      stop("The compatibility of Time is required")
    if(missing(names)){
      Names<-c(Names,x9$call)
      cov<-c(cov,x9$cova)}
    Type1<-x9$Type
    if(Type!=Type1)
      stop("All arguments need to represent the same type of risk")
    if(dim(x9$vector)[2]!=dim(x$vector)[2])
      stop("All arguments need to have the same number of transitions considered")
    k_temp<-length(x9$call)
    k<-k+k_temp
    x1<-cbind(x1,x9$vector)
    if(k_temp>1){
      for(i in 2:k_temp){x1<-cbind(x1,x9$vector)}}
  }
  if(length(x10)>1){
    if (!inherits(x10, "hazard")) 
      stop("expected object [[10]] to be a result of the 'hazard' function")
    t1<-max(x10$Time)
    s1<-min(x10$Time)
    length1<-length(x10$Time)
    if(t!=t1 || s!=s1 || length!=length1)
      stop("The compatibility of Time is required")
    if(missing(names)){
      Names<-c(Names,x10$call)
      cov<-c(cov,x10$cova)}
    Type1<-x10$Type
    if(Type!=Type1)
      stop("All arguments need to represent the same type of risk")
    if(dim(x10$vector)[2]!=dim(x$vector)[2])
      stop("All arguments need to have the same number of transitions considered")
    k_temp<-length(x10$call)
    k<-k+k_temp
    x1<-cbind(x1,x10$vector)
    if(k_temp>1){
      for(i in 2:k_temp){x1<-cbind(x1,x10$vector)}}
  }
  if(length(names)>0 && length(names)!=k)stop("The length of vector 'names' does not correspond to the number of arguments")
  if(length(colors)>0 && length(colors)!=k)stop("The number of colors must be the same as number of arguments")
  name<-colnames(x1)[1]
  len<-length(which(colnames(x1)==name))
  if(missing(transitions)){
    tr<-dim(x$vector)[2]
    names<-colnames(x$vector)
  }else{
    if(!all(transitions%in%colnames(x$vector)))
      stop("Wrong format of argument 'transitions'")
    tr<-length(transitions)
    which<-which(colnames(x$vector)%in%transitions)
    temp<-which
    
    if(len>1){
      for(i in 1:(len-1)){
        which<-c(which,temp+i*dim(x$vector)[2])}
    }
    x1<-data.frame(x1[,unique(which)])
    names<-transitions
  }
  if(tr>1 && ceiling(tr/2)<5){
    par(mfrow=c(ceiling(tr/2),2), oma = c(0, 0, 4, 0))}
  else{
    #if(tr==1){par( oma = c(0, 0, 4, 0))}
    if(ceiling(tr/2)>=5){
      par(mfrow=c(1,1), oma = c(0, 0, 4, 0),ask=TRUE)
    }}
  col<-c(1,2)
  l<-1
  for(i in 1:tr){
    col<-c(1)
    plot_colors<-c(col)
    Time<-as.data.frame(x$Time)
    y<-x1[,i]
    if(k>1){
      for(w in 1:(k-1)){
        Time[,dim(Time)[2]+1]<-x$Time
        y<-cbind(y,x1[,i+w*tr])
        plot_colors<-c(plot_colors,w+1)
      }}
    max<-max(y)
    if(missing(colors)){
      matplot(Time, y,xlab=xlab,ylab=ylab,lwd=lwd,xlim=c(s,t),ylim=ylim,type=type,lty=lty,pch=".",col=plot_colors)}
    else{
      matplot(Time, y,xlab=xlab,ylab=ylab,lwd=lwd,xlim=c(s,t),ylim=ylim,type=type,lty=lty,pch=".",col=colors)}
    if(tr>1){	title(paste("Transition",names[i],collapse=" "))}
    else{
      if(Type=="alpha"){
        title(paste("Sojourn time hazard rate \n Transition",names[i],collapse=" "),font = 2, line = 1, cex = 1.2,  outer = F) 
      }else{
        #title(paste("Semi-Markov process hazard rate \n Transition",names[i],collapse=" "),font = 2, line = 1, cex = 1.2,  outer = F) 
        title(title_paper,font = 2, line = 1, cex = 1.2,  outer = F)
      }
    }
    if(legend==TRUE){
      #if(N==0){
      Legend<-c()
      for(i in 1:length(Names)){
        if(is.vector(cov)){
          Legend[i]<-paste(Names[i],", cov=",cov[i])}
        else{
          Legend[i]<-paste(Names[i])
        }
      }
      if(missing(cex))cex=0.5
      #}
      if(missing(legend.pos)){
        if(missing(colors)){
          legend("topleft", "(x,y)",legend = Legend, 
                 col=plot_colors, lwd=2, cex=.8, horiz = FALSE)}
        else{legend("topleft", "(x,y)",legend = Legend, 
                    col=colors, lwd=2, cex=cex, horiz = FALSE)}
      }else if(is.vector(legend.pos) && length(legend.pos)==2*tr){
        pos<-legend.pos[l]
        max<-legend.pos[l+1]
        if(missing(colors)){
          legend(pos,max,legend = Legend, 
                 col=plot_colors, lwd=2, cex=cex, horiz = FALSE)
        }else{
          legend(pos,max,legend = Legend, 
                 col=colors, lwd=2, cex=cex, horiz = FALSE)}
      }else{
        stop("Wrong format of the argument legend.pos")}
      
    }
    l<-l+2
    if(tr>1){
      if(Type=="alpha"){
        mtext("Sojourn time hazard rate",font = 2, line = 1, cex = 1.2,  outer = TRUE) 
      }else{
        mtext("Semi-Markov process hazard rate",font = 2, line = 1, cex = 1.2,  outer = TRUE) }}
  }
}