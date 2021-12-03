
#example layer stack
source("C:/Users/HughesJo/Documents/gitprojects/LSTD-Connectivity-Paper/scripts/x_Appendix.R")


view[view$H==0]=NA # only look at areas of agreement/disagreement for high quality habitat

view = dropLayer(view,"H")
plot(view)

quants = seq(0,1,length.out=101)#seq(0.25,1,length.out=4)

qTab = quantile(view,probs=quants)

cc = view
for(j in 1:nrow(qTab)){
  cc[[j]] <- cut(view[[j]], breaks=qTab[j,])
  
}
names(cc)=names(view)
plot(cc)

var = calc(cc, sd)
var[var>10]=NA

plot(var)

if(0){
  option="percentile"
  
  for(i in 1:length(quants)){
    #i=1
    outStack=view
    cLevels = qTab[,i]
    for(j in 1:length(cLevels)){
      #j=1
      
      
      outStack[[j]][!is.na(view[[j]])]=1
      if(option=="highest"){
        outStack[[j]][view[[j]]< cLevels[j]]=0
        cName=paste(option, (1-quants[i])*100,"percent")
      }
      if(option=="lowest"){
        outStack[[j]][view[[j]]>= cLevels[j]]=0
        cName=paste(option, quants[i]*100,"percent")
      }
      if(option=="percentile"){
        outStack[[j]][view[[j]]>= cLevels[j]]=0
        if(i>1){
          outStack[[j]][view[[j]]< oLevels[j]]=0
        }
        cName=paste(quants[i]*100,"percentile")
      }
    }
    oLevels=cLevels
    
    rs1 <- calc(outStack, sum)
    names(rs1)=cName
    
    if(i==1){
      res = stack(rs1)
    }else{
      res = addLayer(res,rs1)
    }
  }
  
  plot(res)
}