################ Deciding Actions ################

Disguise=function(x){
  if (length(x)==0){
    x=0
  }else{
    for (i in 1:length(x)){
      #####Susceptible, Infected Individuals are perceived as such
      if (x[i]==-1){
        x[i]=0  ######Recovered indiviudals are seen as Susceptible
      }
    }}
  
  return(x)}

Decide=function(A, S ,t, N, c0, c1, c2){
  
  maxIterate=length(N)
  response=rep(NA,maxIterate)
  
  
  n=matrix(NA, nrow = maxIterate+1, length(N)) ####
  k=1 ##### Starts Iterate Count.
  while (k< maxIterate){
    
    EvenPicks= unique((which(n==1 ,arr.ind=TRUE)[,'col'])) ######Those who decided on even iterate
    EvenPicks=EvenPicks[which(EvenPicks%%2==0& !is.na(EvenPicks))]
    OddPicks=unique((which(n==1 ,arr.ind=TRUE)[,'col'])) ######Those who decided on odd iterate
    OddPicks= OddPicks[which(OddPicks%%2==1 & !is.na(OddPicks))]
    Cupnk=c(EvenPicks, OddPicks)
    
    if((k%%2)==1){ ####for odd k
      
      for (i in setdiff(N, Cupnk)){ ####for all undecided individuals
        NotEven=setdiff(which(A[i,]==1),EvenPicks)
        if (c0>( (c1*(1-Disguise(S[i,t]))*sum(Disguise(S[NotEven,t]))+(c2*Disguise(S[i,t])*sum(1-Disguise(S[NotEven,t])))))){
          n[c(k,length(N)+1),i]=1 ##### if condition holds we pick option 1
        }}
      
    }else{###for even k
      for (i in setdiff(N, Cupnk)){
        OddNeighbors=intersect(which(A[i,]==1), OddPicks) #####Which
        if(c0<((c1*(1-Disguise(S[i,t]))*sum(Disguise(S[OddNeighbors,t]))+(c2*Disguise(S[i,t])*sum(1-Disguise(S[OddNeighbors,t])))))){
          n[c(k,length(N)+1),i]=0} ##### if condition holds we pick option 1
      }
    }
    
    if (length(n[k,is.na(n[k,])])==length(N)){ ######If no one comes to a decision, end loop
      k=length(N)+1
    }else{
      k=k+1
    }
    
  }
  response=n[maxIterate+1,]
  
  if (c1>c2){
    response[which(S[,t]==1 & is.na(n[maxIterate+1,]))]=1
    response[which(S[,t]==0 & is.na(n[maxIterate+1,]))]=0
    response[which(S[,t]==-1)]=1
    
    ####Undecided Infected Individuals Socilize
    ####Undecided Suscpectible Individuals Isolate
    #####Recovered individuals always socialize
  }else{
    response[which(S[,t]==1 & is.na(n[maxIterate+1,]))]=0
    response[which(S[,t]==0 & is.na(n[maxIterate+1,]))]=1
    response[which(S[,t]==-1)]=1
    ####Undecided Infected Individuals Isolate
    ####Undecided Suscpectible Individuals Socialize
    #####Recovered individuals always socialize
    
  }
  return(response)
}

###############NETWORK GROWTH######################

Growth=function(n, NewNodes, N, MaxTime, A, S, K, a){
  n=n+NewNodes
  N=1:n
  NewA=matrix(0, nrow = n, ncol=n)
  NewA[1:nrow(A), 1:ncol(A)] = A
  A=NewA
  K=c(K, rep(0,NewNodes))
  p=rep(NA, (n-NewNodes))
  
  for (j in (n-NewNodes+1):n){ #####For Every New Node
    for (i in 1:(n-NewNodes)){  ###### Calculate Probility of Connection
      p[i]=K[i]/sum(K)
    }
    NewConnect=sample(1:(n-NewNodes), size = m, replace = FALSE, prob = p) ###Picks m nodes based on probability vector.
    A[j, NewConnect]=1  ######Draws New connects, taking advantage of symmetry
    A[NewConnect,j]=1 
    K[j]=K[j]+m ####j is guranteed m new connections
    K[NewConnect]=K[NewConnect]+1 #### Each NewConnect gets a new connections
  }
  NewS=matrix(NA, nrow = n, ncol=MaxTime)
  NewS[1:nrow(S), 1:ncol(S)] = S
  S=NewS
  S[(n-NewNodes):n,t+1]=0
  
  Newa=matrix(NA, nrow=n, ncol=MaxTime)
  Newa[1:nrow(a), 1:ncol(a)] = a
  a=Newa
  
  NewNet=list(n, N, A, S, K, a)
  
  return(NewNet)
}


###################### Parameters ##################
AvgEnd=numeric()
AvgErad=numeric()
AvgValue=numeric()
omega=seq(0, .15, .05)
for (l in 1:length(omega)){
  Realizations=50
  EradCount=0
  End=numeric()
  Value=numeric()
for (R in 1: Realizations){
  Counter=1
  n=100 ### Number of Individuals
  N=1:n
  m0=3 ###Number of initially connected nodes
  m=2  ###New connections for each node
  MaxTime=1000 ####Number of time steps
  delta=0.2 #### Recovery
  beta=0.3 ##### Beta
  GT=0 ####Toggles Game Theoretic Element 
  GROWTH=1 #### Toggles Growth
  allsick=0 ##########Starts with all individuals sick if 1, starts with 1 sick if 0.
  numsick=15 ########## If allsick=0, gives initial num infected
  
  c0= 1
  c1=0.3 ##### Risk Averseness
  c2= 0.3 ##### Empathy
  vac=0.0 #####fraction vacinated
  NewNodes=1
  
  
  ####### Generating Network ################
  A=matrix(0, nrow=n, ncol=n)
  p=rep(NA, n)
  K=rep(0, n)
  
  for (i in 1:(m0-1)){ ####Draws inital network as m0-gon
    A[i,i+1]=1
    A[i+1,i]=1 #Draws connects, taking advantage of symetry
    if (i==(m0-1)){
      A[i+1, 1]=1
      A[i+1, 1]=1
    }
  }
  for (i in 1:m0){
    K[i]=sum(A[i,])  ####Finds inital number of connections.
  }
  for (j in (m0+1):n){
    for (i in sample(N)){
      p[i]=K[i]/sum(K)
    }
    
    NewConnect=sample(N, size = m, replace = FALSE, prob = p) ###Picks m nodes based on probability vector.
    A[j, NewConnect]=1  ######Draws New connects, taking advantage of symmetry
    A[NewConnect,j]=1 
    K[j]=K[j]+m ####j is guranteed m new connections
    K[NewConnect]=K[NewConnect]+1 #### Each NewConnect gets a new connections
  }
  
  
  
  
  PI=rep(NA, n) 
  PR=rep(delta, n)
  
  S=matrix(NA,nrow= n, ncol=MaxTime) ####State Matrix######
  S[,1 ]=rep(0, n)
  if (allsick==1){S[,1]=rep(1, n)
  S[sample(N, size=vac*n)]=-1  #####Draws Vaccinated Individuals
  }else{
    S[sample(N, size=vac*n)]=-1 #####Draws Vaccinated Individuals
    S[sample(N,size =  numsick, prob=p),1]=1}
  
  a=matrix(NA, nrow=n, ncol=MaxTime)

  ################RUNNING THE GAME################## 
  InfecNum=numeric()
  t=1
  while (t < MaxTime){
    #####Insert Decision Making Here##############
    if (GT==1){
      a[,t]=Decide(A, S ,t, N, c0, c1, c2)
    }else{
      a[,t]=rep(1, length(N))
    }
    for (i in N){######### For each individual##############
      ################## Disease Dynamics ######################
      if (S[i,t]==0){ #### If susceptible, calculate probability of infection
        PI[i]=1-prod((1- beta*a[i,t]*a[which(A[,i]==1),t]*S[which(A[,i]==1),t]))
        if (runif(1)<PI[i]){ #####If less than prob, you are infected
          S[i, t+1]=1
        }else{S[i,t+1]=0}### else you are not
      }
      if (S[i,t]==1){ #### If infected, calculate prob for recovery
        if (runif(1)< delta){
          S[i,t+1]=-1
        }else{S[i,t+1]=1}
      }
      
      if (S[i,t]==-1){ ###Recovered stays Recovered
        if(runif(1)<omega[l]){
          S[i,t+1]=0}else{
            S[i,t+1]=-1
          }
      }
      
    }
    InfecNum[t]=sum(S[which(S[,t]>0),t])
    
    if (GROWTH==1){
      #####GROW NETWORK#######
      NewNet=Growth(n, NewNodes, N, MaxTime, A, S, K, a)
      n=NewNet[[1]]
      N=NewNet[[2]]
      A=NewNet[[3]]
      S=NewNet[[4]]
      K=NewNet[[5]]
      a=NewNet[[6]]
    }
    if ((InfecNum[t]==0)){ #####If Epidemic Dies Out End Simulation
      End[R]=t #####Keeps Track of Ending Time
      t=MaxTime
      EradCount=EradCount+1
    }else{####Else Simulation Continues
      t=t+1
      End[R]=MaxTime
    }
  }  
  Individuals=numeric()
  for (t in 1:(End[R])){
    Individuals[t]=length(S[which(!is.na(S[,t])), t])
  }
  
  Porportion=InfecNum/Individuals
  Value[R]=mean(Porportion[(End[R]-10):End[R]])
  print(R)
  print(mean(End))
}
  AvgEnd[l]=mean(End)
  AvgErad[l]=EradCount/Realizations
  AvgValue[l]= mean(Value)
  print(l)
  print(AvgEnd[l])
  
}

df=data.frame(omega, AvgValue, AvgEnd, AvgErad)
colnames(df) <- c("Waning Immunity", "Average Porportion Infected", "Average End Time", "Porportion of Epidemics Eradicated")
ggplot(df, aes(x=omega,y= AvgValue)) + geom_point()

plot(omega, AvgValue, xlab= "Waning Immunity", ylab="Average Porportion Infected")
plot(omega, AvgEnd, xlab= "Waning Immunity", ylab="Average End Time")
plot(omega, AvgErad, xlab= "Waning Immunity", ylab="Porportion of Epidemics Eradicated")
