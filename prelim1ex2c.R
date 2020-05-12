library(deSolve)

#i = initial values of state variables 
i<-c(x=100,z=100)

#times = time sequence for output
time<-seq(0,10,by=1)

#function that computes values of derivatives in the system
togglefunc<-function(time,state,pars){
  x<-state[1]
  z<-state[2]
  
  dzdt=1/(1+(x/xz)^nxz)-z
  dxdt=(ax+bx*S)/(1+S+(z/zx)^nzx)-x
  
  return(list(c(dzdt,dxdt)))
}

#set parameters
pars<-c(  ax<-1.5,
          bx<-5,
          zx<-0.4,
          xz<-1.5,
          nzx<-2.7,
          nxz<-2.7,
          S<-1000)

#run solver
out<-ode(i,time,togglefunc)

#plot
plot(x=out[0:4],y=out[12:15],main='X when S=1000')

