Replicates=100
c1range=seq(0.1, 1, 0.1)
c2range=seq(0.1, 1, 0.1)
EradicationMat=matrix(0, nrow= length(c1range),ncol = length(c2range))


###################### Parameters ##################
n=100 ### Number of Individuals
N=1:n
m0=3 ###Number of initially connected nodes
m=2  ###New connections for each node
MaxTime=50 ####Number of time steps
delta=0.2 #### Recovery
beta=0.3 ##### Beta

c0= 1

GT=1 ####Toggles Game Theoretic Element 
allsick=0 ##########Starts with all individuals sick if 1, starts with 1 sick if 0.
numsick=5 ########## If allsick=0, gives initial num infected
JerkToggle=1 #####Turns on whether people
if(JerkToggle==1){
JerkFrac=0.30 ####Fraction who go out no matter what...
}else{
  JerkFrac=0
}
Fw=1
for (c1index in 1:length(c1range)){
for (c2index in 1:length(c2range)){
  
print(Fw)
  Fw=Fw+1
  c1=c1range[c1index] ##### Risk Averseness
  c2= c2range[c2index] ##### Empathy
  
for (R in 1:Replicates){
  
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
  if (allsick==1){S[,1]=rep(1, n)}else{
    S[sample(N,size =  numsick, prob=p),1]=1}
  
  Jerks= sample(N, size= JerkFrac*n) #####Who is a jerk 
  
  a=matrix(NA, nrow=n, ncol=MaxTime)
  ################ Deciding Actions ################
  Decide=function(A, S ,t, N, c0, c1, c2){
    
    maxIterate=length(N)
    response=rep(NA,maxIterate)
    
    
    n=matrix(NA, nrow = maxIterate+1, length(N)) ####
    k=1 ##### Starts Iterate Count.
    while (k< maxIterate){
      
      EvenPicks= unique((which(n==1 ,arr.ind=TRUE)[,'col'])) ######Those who decided on even iterate
      EvenPicks=EvenPicks[which(EvenPicks%%2==0)]
      OddPicks=unique((which(n==1 ,arr.ind=TRUE)[,'col'])) ######Those who decided on odd iterate
      OddPicks= OddPicks[which(OddPicks%%2==1)]
      Cupnk=c(EvenPicks, OddPicks)
      if((k%%2)==1){ ####for odd k
        
        for (i in setdiff(N, Cupnk)){ ####for all undecided individuals
          NotEven=setdiff(which(A[i,]==1),EvenPicks)
          if (c0>((c1*(1-S[i,t])*sum((S[NotEven,t])))+(c2*S[i,t]*sum((1-S[NotEven,t]))))){
            n[c(k,length(N)+1),i]=1 ##### if condition holds we pick option 1
          }}
        
        
      }else{###for even k
        for (i in setdiff(N, Cupnk)){
          OddNeighbors=intersect(which(A[i,]==1), OddPicks)
          if(c0<((c1*(1-S[i,t])*sum((S[OddNeighbors,t])))+(c2*S[i,t]*sum(1-S[OddNeighbors,t])) )){
            n[c(k,length(N)+1),i]=0
          } ##### if condition holds we pick option 1
          
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
      
      ####Undecided Infected Individuals Socilize
      ####Undecided Suscpectible Individuals Isolate
    }else{
      response[which(S[,t]==1 & is.na(n[maxIterate+1,]))]=0
      response[which(S[,t]==0 & is.na(n[maxIterate+1,]))]=1
      ####Undecided Infected Individuals Isolate
      ####Undecided Suscpectible Individuals Socialize
    }
    response[Jerks]=1 ######Jerks Go out
    
    return(response)
  }
  
  
  ################RUNNING THE GAME################## 
  for (t in 1:(MaxTime-1)){
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
          S[i,t+1]=0
        }else{S[i,t+1]=1}
      }}
  } 
 
  
  if (sum(S[which(S[,MaxTime]>0),t])==0){
    EradicationMat[c1index,c2index]=  EradicationMat[c1index,c2index]+1
  }
  
}
  
}
}

FractionEradicated=EradicationMat/Replicates