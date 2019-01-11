#1
xi=c(50:54)
yi=c(40,46,44,55,49)
m<-length(xi)
a0=(sum(xi^2)*sum(yi)-(sum(xi*yi)*sum(xi)))/(m*(sum(xi^2))-sum(xi)^2)
a1=(m*sum(xi*yi)-sum(xi)*sum(yi))/(m*(sum(xi^2))-sum(xi)^2)
a0
a1

#2
f<-function(x){
  return(-93.6+2.7*x)
}
#3
xi=c(0:4)
yi=c(1,1.25,3.75,4.25,5.65)
poly.calc(xi,yi)

#4
f1<-function(x){
  return(1 - 0.07916667*x + 2.19375*x^2 - 0.9958333*x^3 + 0.13125*x^4)
}
f1(2.75)

#5
plot(xi,yi)
curve(f1,add=TRUE)

#11
f<-function(x){
  return(x^2-6)
  
}
trapzfun(f,0,1)
#12
f<-function(x){
  return(x^3+(4*x^2)-10)
  
}
trapzfun(f,1,2)

#13-15
h=0.1
x=seq(0,1,by=h)
f<-function(x){
  return(x^2)
}
f0=f(x[1])
x[2:10]
fi=sapply(x[2:10],f)
fn=f(x[length(x)])
trap<-function(f0,fi,fn,h){
  L=h*(f0+2*sum(fi)+fn)/2
  return(L)
}
trap(f0,fi,fn,h)