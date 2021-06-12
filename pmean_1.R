pmean<-function(directory, pollutant,id=1:332){
  setwd('C://Users/Public/R/')
 directory<-getwd()
   sum_poll=0
 count_poll=0
 i=id
for (i in id){
  fl<-paste0(directory,'/',i, '.csv')
  df<-read.csv(fl)
  sum_poll=sum_poll+colSums(df[pollutant],na.rm = TRUE)
  count_poll=count_poll+colSums(df[pollutant],na.rm = TRUE)/colMeans(df[pollutant],na.rm = TRUE)
 }
 return(sum_poll/count_poll)
  
}

complete<-function(directory,id){
  setwd('C://Users/Public/R/')
  nob1<-c()
  l=1
  for (i in id){
    file1=paste0(directory,'/',i,'.csv')
    df<-read.csv(file=file1)
    sum_poll=0
    sum_poll=colSums(df[2],na.rm = TRUE)
    nob1[l]=colSums(df[2],na.rm = TRUE)/colMeans(df[2],na.rm = TRUE)
    l=l+1
  }
  return(data.frame(id,nob1))
}
correl=function(directory, threshold = 0) {
  setwd('C://Users/Public/R/')
  correl=numeric()
  for (i in 1:332){
    file1=paste0(directory,'/',i,'.csv')
    df<-na.omit(read.csv(file=file1))
    if (nrow(df) > threshold){
      correl=c(correl,cor(df['sulfate'], df['nitrate']))
    }
  }
  return(correl)
}