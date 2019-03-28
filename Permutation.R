#Create vector of unique products
items=unique(df$Item)
val=data.frame("Products"=c(),"P-value"=c())

#loop trough all possible item combos to find which two sell well together using permutation()
for(i in 1:(NROW(items)-1)){
  
  for(j in (i+1):NROW(items)) {
    
    item1=as.character(items[i])
    item2=as.character(items[j])
    m1=mean(df[df$Item==item1,]$Value)
    m2=mean(df[df$Item==item2,]$Value)
    if(m1 < m2){
      val=rbind(val,
        data.frame("Products"=paste(item1,"-",item2,sep = ""),
                   "P-value"=PermutationTestSecond::Permutation(df, "Item", "Value",1000,item1,item2))
      )
    }else{
      val=rbind(val,
        data.frame("Products"=paste(item1,"-",item2,sep = ""),
                   "P-value"=PermutationTestSecond::Permutation(df, "Item", "Value",1000,item2,item1))
        )
      
    }
    
  }
  
}

#val is a dataframe displaying all combos and their respective p-value scores
val=val[order(val$"P.value",decreasing = F),]

#Filter for values where we can reject our NULL hypothesis
answer=val[val$P.value<=0.05,]
answer
