library("lattice")
library(gridExtra)


## Example data
data=matrix(FractionEradicated[rev(1:length(c2range)),] , length(c1range) , length(c2range))
rownames(data)=paste( rep("",length(c1range)) , rev(c1range) , sep=" ")
colnames(data)= paste( rep("",length(c2range)), c2range , sep=" ")

## Try it out

## Try it out

p1<-levelplot(t(data[c(nrow(data):1) , ]), xlab="Empathy constant (c2)",ylab="Risk aversion constant (c1)",main="Beta=0.1")


p2<-levelplot(t(data[c(nrow(data):1) , ]), xlab="Empathy constant (c2)",ylab="Risk aversion constant (c1)",main="Beta=0.2")


p3<-levelplot(t(data[c(nrow(data):1) , ]), xlab="Empathy constant (c2)",ylab="Risk aversion constant (c1)",main="Beta=0.3, No Jerks")

grid.arrange(p3, nrow = 1)