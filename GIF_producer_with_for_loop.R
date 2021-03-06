library(sna)
library(network)
library(ggplot2)
library(ggnet)
library(animation)
library(magick)
library(gganimate)
###################### Parameters ##################
n=100 ### Number of Individuals
N=1:n
m0=5 ###Number of initially connected nodes
m=5  ###New connections for each node
MaxTime=50 ####Number of time steps
delta=0.2 #### Recovery
beta=0.3 ##### Beta
c0= 1
c1=0.5 #####Risk Averseness
c2= 0.6 #####Empathy Constant
vac=0.05 #####fraction vacinated
JerkFrac=0.3 ####Fraction who go out no matter what...

GT=1 ####Toggles Game Theoretic Element 

allsick=0 ##########Starts with all individuals sick if 1, starts with 1 sick if 0.
numsick=5 ########## If allsick=0, gives initial num infected

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
    p[i]=K[i]/sum(K) ########Probability node i gets new connection
  }
  
  NewConnect=sample(N, size = m, replace = FALSE, prob = p) ###Picks m nodes based on probability vector.
  A[j, NewConnect]=1  ######Draws New connects, taking advantage of symmetry
  A[NewConnect,j]=1 
  K[j]=K[j]+m ####j is guranteed m new connections
  K[NewConnect]=K[NewConnect]+1 #### Each NewConnect gets a new connections
}
##########################################SIR###############################
############################################################################

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

PI=rep(NA, n) 
PR=rep(delta, n)

S=matrix(NA,nrow= n, ncol=MaxTime) ####State Matrix######
S[,1 ]=rep(0, n)
if (allsick==1){S[,1]=rep(1, n)
S[sample(N, size=vac*n)]=-1  #####Draws Vaccinated Individuals

}else{
  S[sample(N, size=vac*n)]=-1 #####Draws Vaccinated Individuals
  S[sample(N,size =  numsick, prob=p),1]=1}

#a=rep(1, n) #####action assuming no one isolates
a=matrix(NA, nrow=n, ncol=MaxTime)
################ Deciding Actions ################
Decide=function(A, S ,t, N, c0, c1, c2){
  
  maxIterate=length(N)
  response=rep(NA,maxIterate)
  
  
  n=matrix(NA, nrow = maxIterate+1, length(N)) ####
  k=1 ##### Starts Iterate Count.
  while (k<=maxIterate){
    
    EvenPicks= unique((which(n==1 ,arr.ind=TRUE)[,'col'])) ######Those who decided on even iterate
    EvenPicks=EvenPicks[which(EvenPicks%%2==0)]
    OddPicks=unique((which(n==1 ,arr.ind=TRUE)[,'col'])) ######Those who decided on odd iterate
    OddPicks= OddPicks[which(OddPicks%%2==1)]
    Cupnk=c(EvenPicks, OddPicks) ###### Individuals who have decided
    
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
        S[i,t+1]=-1
      }else{S[i,t+1]=1}
    }
    if (S[i,t]==-1){ ###Recovered stays Recovered
      S[i,t+1]=-1
    }
    
  }
} 

print(sum(S[which(S[,t]>0),t]))



############################## Graph @ t= ##################
net=A
net=network(A, directed=FALSE) 


for(i in 1:20) {
  #i=1
  t=i
  net %v% "state" = ifelse(S[,t]>0, "Infected", ifelse(S[,t]<0 , "Recovered", "Susceptible"))
  state=factor(net %v% "state")
  assign(paste('SIR', i),ggnet2(net, color=state,palette = c("Infected" = "red", "Susceptible" = "blue", "Recovered"="yellow"), size='degree', label=N, label.size=2, label.color = 'white', mode = "circle") + guides( size=FALSE))
  ggsave(paste('SIR',i,'.png')) 
  }


#############convert to gif#########################################

png.files <- paste('SIR',1:20,'.png') #Mention the number of files to be read
GIF.convert <- function(x, output = "SIRanimation.gif")#Create a function to read, animate and convert the files to gif
{
  image_read(x) %>%
    image_animate(fps = 1) %>%
    image_write(output)
}

GIF.convert(png.files)

file.remove(list.files(pattern=".png"))

##########################################SIS##########################
########################################################################
PI=rep(NA, n) 
PR=rep(delta, n)

S=matrix(NA,nrow= n, ncol=MaxTime) ####State Matrix######
S[,1 ]=rep(0, n)
if (allsick==1){S[,1]=rep(1, n)}else{
  S[sample(N,size =  numsick, prob=p),1]=1}

#a=rep(1, n) #####action assuming no one isolates
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

print(sum(S[,MaxTime]))

############################## Graph @ t= ##################

net=A
net=network(A, directed=FALSE) 

for(i in 1:20) {
#i=1
  t=i
  net %v% "state" = ifelse(S[,t] %in% c(1), "Infected", "Susceptible")
  state=factor(net %v% "state")
  assign(paste('SIS', i),ggnet2(net, color=state, palette = c("Infected" = "red", "Susceptible" = "blue"), size='degree', label=N, label.size=2, label.color = 'white', mode = "circle") + guides( size=FALSE))
  ggsave(paste('SIS',i,'.png'))
}

#############convert to gif#########################################

png.files <- paste('SIS',1:20,'.png') #Mention the number of files to be read
GIF.convert <- function(x, output = "SISanimation.gif")#Create a function to read, animate and convert the files to gif
{
  image_read(x) %>%
    image_animate(fps = 1) %>%
    image_write(output)
}

GIF.convert(png.files)

file.remove(list.files(pattern=".png"))

##########################################Jerk##########################
##########################################################################

PI=rep(NA, n) 
PR=rep(delta, n)

S=matrix(NA,nrow= n, ncol=MaxTime) ####State Matrix######
S[,1 ]=rep(0, n)
if (allsick==1){S[,1]=rep(1, n)}else{
  S[sample(N,size =  numsick, prob=p),1]=1}

Jerks= sample(N, size= JerkFrac*n) #####Who is a Jerk 

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
InfecNum=numeric()
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
  InfecNum[t]=sum(S[,t])
} 

print(sum(S[,MaxTime]))

############################## Graph @ t= ##################

JerkLab=rep('', n)
JerkLab[Jerks]='+'
net=A
net=network(A, directed=FALSE) 


for(i in 1:20) {
  #i=1
  t=i
  net %v% "state" = ifelse(S[,t]>0, "Infected", ifelse(S[,t]<0 , "Recovered", "Susceptible"))
  state=factor(net %v% "state")
  assign(paste('JERK', i),ggnet2(net, color=state, palette = c("Infected" = "red", "Susceptible" = "blue"), size='degree',  label=JerkLab, label.size=5, label.color = 'white',fontface = "bold", mode = "circle") + guides( size=FALSE))
  ggsave(paste('JERK',i,'.png')) 
}


#############convert to gif#########################################

png.files <- paste('JERK',1:20,'.png') #Mention the number of files to be read
GIF.convert <- function(x, output = "JERKanimation.gif")#Create a function to read, animate and convert the files to gif
{
  image_read(x) %>%
    image_animate(fps = 1) %>%
    image_write(output)
}

GIF.convert(png.files)

file.remove(list.files(pattern=".png"))
