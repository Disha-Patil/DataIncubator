#tower of hanoi

#initialize
hanoi<-function(n,m)
{
  stacks<-vector("list",m)
  stacks[[1]]<-c(n:1)
  return(stacks)
}

hanoi.move<-function(stacks,n,m)
{
  #for(t in 1:moves){
    #print(t)
      #first elements only in each stack
        first<-sapply(stacks,"[[", 1)
        valid<-vector("list");loop=1
                  for(i in 1:m){
                    print(i);print(first)
                            if(i==1){
                              if(is.null(first[[i]])){break}
                              
                              if(is.null(first[[i+1]])){
                                  valid[[loop]]<-c(i,i+1); loop=loop+1}
                              else if(first[[i]]<first[[i+1]]){
                                  valid[[loop]]<-c(i,i+1); loop=loop+1}
                            }
                            else if(i==m){
                                    if(is.null(first[[i]])){break}
                              
                                    if(is.null(first[[i-1]])){
                                        valid[[loop]]<-c(i,i-1); loop=loop+1}
                                    else if(first[[i]]<first[[i-1]]){
                                        valid[[loop]]<-c(i,i-1); loop=loop+1}
                            }
                            else {
                                  if(is.null(first[[i]])){break}
                                  
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
        valid.moves<-hanoi.move(stacks,n,m)
        select.move<-sample(1:length(valid.moves),1)
        #print(select.move);print(valid.moves)
        from<-valid.moves[[select.move]][1]
        to<-valid.moves[[select.move]][2]
        stacks[[to]]<-stacks[[from]][1]
        stacks[[from]]<-stacks[[from]][-1]
        print(stacks)
      } 
  }

  
  stacks<-hanoi(3,3)
  hanoi.all.moves(stacks,6,3,3)
  
