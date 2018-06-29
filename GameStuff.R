Decide=function(connections, S ,t, N, c0, c1, c2){
  
  maxIterate=N
  response=rep(0,maxIterate)
  k=1
  
n=matrix(NA, nrow = maxIterate+1, N) ####
Cupnk=c()
###1 indicates in and 0 not
  while (k<=N){
    Cupnk=list(which(n==1,arr.ind=TRUE)[,'col'])
    EvenPicks= Cupnk[which(Cupnk %% 2 = 0)]
    OddPicks=Cupnk[which(Cupnk %% 2 = 1)]
    
    if(k%%2==1){
      for (i in setdiff(1:N, Cupnk)){
        NotEvenPicks=setdiff(Neigh[i],EvenPicks)
        if (c0>(c1(1-S[i,t])*sum((S[,t]-c2*s[i,t])[NotEven])*sum(1-S[,t])[NotEven])){
          n[c(k,N+1),i]=1
        }}
      ####If Conditions holds, we pick option 1.
    }else{###k is even
      for (i in setdiff(1:N, Cupnk)){
        OddNeighbors=cup(Neigh[i], OddPicks)
        if(c0<(c1(1-S[i,t])*sum((S[,t]-c2*S[i,t])[OddNeighbors])*sum(1-S[,t])[OddNeighbors])){
          n[c(k,N+1),i]=0}
     #####returns 0 if condition holds
 }
    }
    
    if (n[k,]==rep(NA,N)){
      k=N+1
    }else{
      k=k+1
    }
  }

n[N+1,][which(S[,t]==1 & is.na(n[N+1,]))]=0
n[N+1,][which(S[,t]==0 & is.na(n[N+1,]))]=1

####Undecided Infected Individuals Isolate
####Undecided Suscpectible Individuals Socialize

reposnse=n[N+1,]
###### n[N+1,] denotes final choices of indviduals
return(response)
}
