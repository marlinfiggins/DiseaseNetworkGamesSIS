library(sna)
library(network)
library(ggplot2)
library(ggnet)
n=10
N=1:n
A=matrix(0, nrow=n, ncol=n)
p=rep(NA, n)
K=rep(0, n)
m0=5 ###Number of initially connected nodes
m=3  ###New connections for each node
for (i in 1:(m0-1)){ ####Draws inital network as straight line from i to i+1
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
  
}

net=A
net=network(A, directed=FALSE)
#network.vertex.names(net)=N
ggnet2(net, size='degree')