library(deSolve)

#i = initial values of state variables 
i<-c(x=0,y=0,z=0)

#times = time sequence for output
time<-seq(0,10,by=1)

#function that computes values of derivatives in the system
togglefunc<-function(time,state,pars){
 x<-state[1]
 y<-state[2]
 z<-state[3]
 #x<-(ax+bx*S)/(1+S+(z/zx)^nzx)
 #y<-(ay+by*S)/(1+S+(x/xy)^nxy)/dely
 #z<-1/(1+(x/xz)^nxz+(y/yz)^nyz)/delz
  
  dxdt=(ax+bx*S)/(1+S+(z/zx)^nzx)-x
  dydt=(ay+by*S)/(1+S+(x/xy)^nxy)-dely*y
  dzdt=1/(1+(x/xz)^nxz+(y/yz)^nyz)-delz*z
  
  return(list(c(dxdt,dydt,dzdt)))
  }

#set parameters
pars<-c(  S<-10^5,
          ax<-3.9*10^-2,
          ay<-4.3*10^-3,
          bx<-6.1,
          by<-5.7,
          dely<-1.05,
          delz<-1.04,
          zx<-1.3*10^-5,
          yz<-11*10^-3,
          xz<-12*10^-2,
          xy<-7.9*10^-4,
          nzx<-2.32,
          nxy<-2,
          nxz<-2,
          nyz<-2)

#run solver
out<-ode(i,time,togglefunc)

#plot
#plot(x=out[1:11],y=out[12:22],xlab='time',ylab='X when S=0.02')
#plot(x=out[1:11],y=out[23:33],xlab='time',ylab='X when S=10')
plot(x=out[1:11],y=out[34:44],xlab='time',ylab='X when S=10^5')

