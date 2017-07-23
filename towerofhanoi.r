#tower of hanoi

#initialize
hanoi<-function(n,m)
{
  stacks<-vector("list",m)
  stacks[[1]]<-c(n:1)
  #for(s in 2:m){stacks[[s]]<-0}
  return(stacks)
}

hanoi.move<-function(stacks,n,m)
{
  #for(t in 1:moves){
    #print(t)
      #first elements only in each stack
        first<-sapply(stacks,"[[", 1)
        valid<-vector("list");loop=1
       # print(first)
                  for(i in 1:m){
                   # print(i);
                            if(i==1){
                              if(is.null(first[[i]])){next}
                              
                              if(is.null(first[[i+1]])){
                                  valid[[loop]]<-c(i,i+1); loop=loop+1}
                              else if(first[[i]]<first[[i+1]]){
                                  valid[[loop]]<-c(i,i+1); loop=loop+1}
                            }
                            else if(i==m){
                                    if(is.null(first[[i]])){next}
                              
                                    if(is.null(first[[i-1]])){
                                        valid[[loop]]<-c(i,i-1); loop=loop+1}
                                    else if(first[[i]]<first[[i-1]]){
                                        valid[[loop]]<-c(i,i-1); loop=loop+1}
                            }
                            else {
                                  if(is.null(first[[i]])){next}
                                  
                                  if(is.null(first[[i+1]])){
                                        valid[[loop]]<-c(i,i+1); loop=loop+1}
                                  else if(first[[i]]<first[[i+1]]){
                                        valid[[loop]]<-c(i,i+1); loop=loop+1}

                                  if(is.null(first[[i-1]])){
                                        valid[[loop]]<-c(i,i-1); loop=loop+1}
                                  else if(first[[i]]<first[[i-1]]){
                                  valid[[loop]]<-c(i,i-1); loop=loop+1}
                                    
                            }

                }
        
    
    
    return(valid)
  #}
}
 
  hanoi.all.moves<-function(stacks,moves,n,m)
  {
    for(t in 1:moves)
      {
      #print(t)
        valid.moves<-hanoi.move(stacks,n,m)
        select.move<-sample(1:length(valid.moves),1)
        #print(select.move);print(valid.moves)
        from<-valid.moves[[select.move]][1]
        to<-valid.moves[[select.move]][2]
        #to.top<-length(stack[[to]]) + 1
        stacks[[to]]<-append(stacks[[to]],stacks[[from]][1])
        #if(length(stacks[[from]]==1)) {stacks[from]<-list(NULL)}
        if (length(stacks[[from]][-1])==0) {stacks[from]<-list(NULL)}
        else {stacks[[from]]<-stacks[[from]][-1]}
        #print(valid.moves);print(select.move);
      #print(stacks)
      } 
    return(stacks)
  }

  
  
  N=3;M=3;T=16
  CoM<-c()
  for (times in 1:50){
      
      stacks<-hanoi(N,M)
      stacks.final<-hanoi.all.moves(stacks,T,N,M)
      sum.d<-do.call(sum,stacks.final)
      disk.pos<-lapply(1:M,function(l) {stacks.final[[l]]*(l-1)})
      sum.dp<-do.call(sum,disk.pos)
      CoM[times]<-sum.dp/sum.d
    }
  #length(unique(CoM))
  CoM.mean<-mean(unique(CoM))
  CoM.sd<-sd(unique(CoM))
  CoM.mean
  CoM.sd
  
  
  
  

  
