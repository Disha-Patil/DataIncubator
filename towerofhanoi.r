#tower of hanoi
#N disks and M stacks problem for T moves

#initialize the start state by having all disks in first stack
hanoi<-function(n,m)
{
  stacks<-vector("list",m)
  stacks[[1]]<-c(n:1)
  return(stacks)
}

#identify the valid move at each stage
hanoi.move<-function(stacks,n,m)
{
        #first elements only in each stack
        first<-sapply(stacks,"[[", 1)
        valid<-vector("list");loop=1
       # print(first)
                  
                  #as many stacks as many first elements. check for valid moves for each disk on top
                  for(i in 1:m){
                   # print(i);
                            #for all indices check if the stack is empty i.e. is.null==T
                            
                            #for first stack first element check only i+1 index
                            if(i==1){
                              if(is.null(first[[i]])){next}
                              
                              if(is.null(first[[i+1]])){
                                  valid[[loop]]<-c(i,i+1); loop=loop+1}
                              else if(first[[i]]<first[[i+1]]){
                                  valid[[loop]]<-c(i,i+1); loop=loop+1}
                            }
                            #for last stack first element check only i-1 index
                            else if(i==m){
                                    if(is.null(first[[i]])){next}
                              
                                    if(is.null(first[[i-1]])){
                                        valid[[loop]]<-c(i,i-1); loop=loop+1}
                                    else if(first[[i]]<first[[i-1]]){
                                        valid[[loop]]<-c(i,i-1); loop=loop+1}
                            }
                            #for all others check i+1 and i-1 indices  
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
#valid is a list of valid moves that can be made. each entry in list has subelements in following sequence: c(from , to)
#e.g. valid[[1]][1] is the stack from which we can move the disk and valid[[1]][2] is the stack to which we can move the disk
    
   return(valid)
}
 
  
  #repeat the task of moving the disk for given number of moves and at every move update the stacks
  hanoi.all.moves<-function(stacks,moves,n,m)
  {
    for(t in 1:moves)
      {
      #print(t)
      #list of valid moves
        valid.moves<-hanoi.move(stacks,n,m)
      #randomly select one of the valid moves
        select.move<-sample(1:length(valid.moves),1)
        #print(select.move);print(valid.moves)
      
      #extract the from and to stacks information
        from<-valid.moves[[select.move]][1]
        to<-valid.moves[[select.move]][2]
        #to.top<-length(stack[[to]]) + 1
      
      #append the to stack
        stacks[[to]]<-append(stacks[[to]],stacks[[from]][1])
        #if(length(stacks[[from]]==1)) {stacks[from]<-list(NULL)}
      
      #remove the moved disk from the previous stack
        if (length(stacks[[from]][-1])==0) {stacks[from]<-list(NULL)}
        else {stacks[[from]]<-stacks[[from]][-1]}
   
        #print(valid.moves);print(select.move);
      #print(stacks)
      } 
    
    #return updated stacks
    return(stacks)
  }

  
  #set conditions
  N=3;M=3;T=16
  #center of mass vector
  CoM<-c()
  
  #repeat number of times to see what are the unque set of possibilities
  for (times in 1:50){
      
      stacks<-hanoi(N,M)
      stacks.final<-hanoi.all.moves(stacks,T,N,M)
    
    #denominator from center of mass formula
      sum.d<-do.call(sum,stacks.final)
      disk.pos<-lapply(1:M,function(l) {stacks.final[[l]]*(l-1)})
    #numerator from center of mass formula
      sum.dp<-do.call(sum,disk.pos)
    #calculate center of mass
      CoM[times]<-sum.dp/sum.d
    }
  #length(unique(CoM))
  #take mean and standard deviation of the center of mass, only the unique posiibilities
  CoM.mean<-mean(unique(CoM))
  CoM.sd<-sd(unique(CoM))
  
  #print mean and standard deviation
  print(CoM.mean)
  print(CoM.sd)
  
  
  
  

  
