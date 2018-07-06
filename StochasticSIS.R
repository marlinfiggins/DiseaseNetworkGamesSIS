library(sna)
library(network)
library(ggplot2)
library(ggnet)
###################### Parameters ##################
n=100 ### Number of Individuals
N=1:n
m0=5 ###Number of initially connected nodes
m=3  ###New connections for each node
MaxTime=50 ####Number of time steps
delta=0.2 #### Recovery
beta=0.3 ##### Beta


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
    for (i in N){
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
S[sample(1:n, 1),1]=1

a=rep(1, n) #####action assuming no one isolates

################RUNNING THE GAME################## 

for (t in 1:(MaxTime-1)){
  
  for (i in N){######### For each individual##############
    
    #####Insert Decision Making Here##############
    
    ################## Disease Dynamics ######################
    if (S[i,t]==0){ #### If susceptible, calculate probability of infection
  PI[i]=1-prod((1- beta*a[i]*a[which(A[i,]==1)]*S[which(A[i,]==1),t]))
  if (runif(1)<PI[i]){ #####If less than prob, you are infected
    S[i, t+1]=1
  }else{S[i,t+1]=0}### else you are not
    }
    
  if (S[i,t]==1) #### If infected, calculate prob for recovery
     if (runif(1)< delta){
       S[i,t+1]=0
     }else{S[i,t+1]=1}
  }
} 


############################## Graph @ t= ##################
t=1
net=A
net=network(A, directed=FALSE) 
net %v% "state" = ifelse(S[,t] %in% c(1), "Infected", "Susceptible")
ggnet2(net, color='state',palette = c("Infected" = "red", "Susceptible" = "blue"), size='degree', mode = "circle")

t=10
net=A
net=network(A, directed=FALSE) 
net %v% "state" = ifelse(S[,t] %in% c(1), "Infected", "Susceptible")
ggnet2(net, color='state',palette = c("Infected" = "red", "Susceptible" = "blue"), size='degree', mode = "circle")

t=20

net=A
net=network(A, directed=FALSE) 
net %v% "state" = ifelse(S[,t] %in% c(1), "Infected", "Susceptible")
ggnet2(net, color='state',palette = c("Infected" = "red", "Susceptible" = "blue"), size='degree', mode = "circle")

t=30

net=A
net=network(A, directed=FALSE) 
net %v% "state" = ifelse(S[,t] %in% c(1), "Infected", "Susceptible")
ggnet2(net, color='state',palette = c("Infected" = "red", "Susceptible" = "blue"), size='degree', mode = "circle")

t=40
net=A
net=network(A, directed=FALSE) 
net %v% "state" = ifelse(S[,t] %in% c(1), "Infected", "Susceptible")
ggnet2(net, color='state',palette = c("Infected" = "red", "Susceptible" = "blue"), size='degree', mode = "circle")

