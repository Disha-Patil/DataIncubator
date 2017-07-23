#tower of hanoi

#initialize
hanoi<-function(n,m)
{
  stacks<-vector("list",m)
  stacks[[1]]<-c(n:1)
  return(stacks)
}

hanoi.move<-function(stacks,moves,n,m)
{
  for(t in 1:moves){
  #first elements only in each stack
    first<-sapply(stacks,"[[", 1)
    valid<-vector("list");loop=1
      for(i in 1:m){
        if(i==1){if(is.null(first[[i+1]]) | first[[i]]<first[[i+1]]){
        valid[[loop]]<-c(i,i+1); loop=loop+1}}
        
        if(is.null(first[[i+1]]) | first[[i]]<first[[i+1]]){
        valid[[loop]]<-c(i,i+1); loop=loop+1}
        
        if(is.null(first[[i-1]]) | first[[i]]<first[[i-1]]){
        valid[[loop]]<-c(i,i-1); loop=loop+1}
        
        if(i==m){if(is.null(first[[i-1]]) | first[[i]]<first[[i-1]]){
        valid[[loop]]<-c(i,i-1); loop=loop+1}}
        
      }
    
    
    
  }
}
