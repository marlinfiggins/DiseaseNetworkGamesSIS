
###############NETWORK GROWTH######################

Growth=function(n, NewNodes, N, MaxTime, A, S, K, a){
  n=n+NewNodes
  N=1:n
  NewA <- matrix(0, nrow = n, ncol=n)
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



##### Put Inside Loop GROW NETWORK#######
NewNet=Growth(n, NewNodes, N, MaxTime, A, S, K, a)
n=NewNet[[1]]
N=NewNet[[2]]
A=NewNet[[3]]
S=NewNet[[4]]
K=NewNet[[5]]
a=NewNet[[6]]