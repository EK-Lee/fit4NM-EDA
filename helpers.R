
find.bin.equal.n<-function(x,nbin)
{ cutpoints<-quantile(x,(0:nbin)/nbin)
  return(cut(x,cutpoints,include.lowest=TRUE))
}  


makeCOVbin<-function(COV.data,N.covbin)
{ if(N.covbin<length(table(COV.data)))
{  COV.bin<-find.bin.equal.n(COV.data,N.covbin)
   COV.bin.ID<-names(table(COV.bin))
} else
{
  COV.bin<-cut(COV.data,as.numeric(names(table(COV.data))))
  COV.bin.ID<-names(table(COV.bin))
}  
return(list(COV.bin=COV.bin,COV.bin.ID=COV.bin.ID))
}


read.PKPDdata<-function(file.name)
{ 
  temp.data<-read.csv(file.name,na.strings=".")
  colnames(temp.data)<-toupper(colnames(temp.data))
  return(temp.data)
}


XYplot.orig<-function(orig.data,X.name,Y.name,ID.name,x.lim,y.lim)
{  
    X.t<-orig.data[,X.name]
    Y.t<-orig.data[,Y.name]
    ID.t<-orig.data[,ID.name]
    new.data<-data.frame(X.t=X.t,Y.t=Y.t,ID.t=ID.t)
    ggplot( data=new.data, aes(x=X.t,y=Y.t))+
     geom_point()+labs(x=X.name,y=Y.name)+xlim(x.lim)+ylim(y.lim)
}   

XYplot.orig.with.COV<-function(orig.data,X.name,Y.name,ID.name,x.lim,y.lim,cov.bin)
{ X.t<-orig.data[,X.name]
  Y.t<-orig.data[,Y.name]
  ID.t<-orig.data[,ID.name]
  new.data<-data.frame(X.t=X.t,Y.t=Y.t,ID.t=ID.t,cov.bin=cov.bin$COV.bin)
  ggplot( data=new.data, aes(x=X.t,y=Y.t))+
    geom_point()+labs(x=X.name,y=Y.name)+xlim(x.lim)+ylim(y.lim) + facet_wrap(~cov.bin)
  
}   

### Xyplot with ID profile
XYplotwithID.orig<-function(orig.data,X.name,Y.name,ID.name,x.lim,y.lim)
{ X.t<-orig.data[,X.name]
  Y.t<-orig.data[,Y.name]
  ID.t<-orig.data[,ID.name]
  new.data<-data.frame(X.t=X.t,Y.t=Y.t,ID.t=ID.t)
  ggplot( data=new.data, aes(x=X.t,y=Y.t,group=ID.t))+
    geom_point()+labs(x=X.name,y=Y.name)+xlim(x.lim)+ylim(y.lim)+geom_line()
  
}   

XYplotwithID.orig.with.COV<-function(orig.data,X.name,Y.name,ID.name,x.lim,y.lim,cov.bin)
{ 
  X.t<-orig.data[,X.name]
  Y.t<-orig.data[,Y.name]
  ID.t<-orig.data[,ID.name]
  new.data<-data.frame(X.t=X.t,Y.t=Y.t,ID.t=ID.t,cov.bin=cov.bin$COV.bin)
  ggplot( data=new.data, aes(x=X.t,y=Y.t,group=ID.t))+
    geom_point()+labs(x=X.name,y=Y.name)+xlim(x.lim)+ylim(y.lim)+geom_line()+ facet_wrap(~cov.bin)
  
}   
