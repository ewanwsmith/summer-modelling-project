library(deSolve)
v1h3seir <- function(times, inits, params){
  
  Sh1 <- inits[1] #susceptible host 1
  Eh1 <- inits[2] #exposed host 1
  Ih1 <- inits[3] #infected host 1
  Rh1 <- inits[4] #recovered host 1
  Sh2 <- inits[5] #susceptible host 2
  Eh2 <- inits[6] #exposed host 2
  Ih2 <- inits[7] #infected host 2
  Rh2 <- inits[8] #recovered host 2
  Sh3 <- inits[1] #susceptible host 3
  Eh3 <- inits[2] #exposed host 3
  Ih3 <- inits[3] #infected host 3
  Rh3 <- inits[4] #recovered host 3
  Sv <- inits[9] #susceptible vector
  Iv <- inits[10] #infected vector
  
  Nh1 <- Sh1 + Eh1 + Ih1 + Rh1 #total host 1 popn
  Nh2 <- Sh2 + Eh2 + Ih2 + Rh2 #total host 2 popn
  Nh3 <- Sh3 + Eh3 + Ih3 + Rh3 #total host 3 popn 
  Nv <- Sv + Iv #total vector popn
  
  
  with(as.list(c(params)), {
    
    dSh1 <- bh1 * Nh1 - fh1 / Nh1 * Th1v * Iv * Sh1 - muh1 * Sh1 
    dEh1 <- fh1 / Nh1 * Th1v * Iv * Sh1 - muh1 * Eh1 - epsilon1 * Eh1
    dIh1 <- epsilon1 * Eh1 - muh1 * Ih1 - gammah1 * Ih1
    dRh1 <- gammah1 * Ih1 - muh1 * Rh1

    dSh2 <- bh2 * Nh2 - fh2 / Nh2 * Th2v * Iv * Sh2 - muh2 * Sh2
    dEh2 <- fh2 / Nh2 * Th2v * Iv * Sh2 - muh2 * Eh2 - epsilon2 * Eh2
    dIh2 <- epsilon2 * Eh2 - muh2 * Ih2 - gammah2 * Ih2
    dRh2 <- gammah2 * Ih2 - muh2 * Rh2 

    dSh3 <- bh3 * Nh3 - fh3 / Nh3 * Th3v * Iv * Sh3 - muh3 * Sh3
    dEh3 <- fh3 / Nh3 * Th3v * Iv * Sh3 - muh3 * Eh3 - epsilon3 * Eh3
    dIh3 <- epsilon3 * Eh3 - muh3 * Ih3 - gammah3 * Ih3
    dRh3 <- gammah3 * Ih3 - muh3 * Rh3 

    dSv <- bv * Nv - fh1 / Nh1 * Tvh1 * Sv * Ih1 - fh2 / Nh2 * Tvh2 * Sv * Ih2 - muv * Sv
    dIv <- fh1 / Nh1 * Tvh1 * Sv * Ih1 - fh2 / Nh2 * Tvh2 * Sv * Ih2 - muv * Iv
    
    return(list(c(dSh1, dEh1, dIh1, dRh1, dSh2, dEh2, dIh2, dRh2, dSh3, dEh3, dIh3, dRh3, dSv, dIv)))
  })
  
}

inits <- c(45,5,0,0,50,0,0,0,50,0,0,0,2700,300)
params <- c(bh1 = 0.01, muh1 = 0.01, fh1 = 0.25, epsilon1 = 0.2, gammah1 = 0.1, Th1v = 0.1, Tvh1 = 0.2,
bh2 = 0.01, muh2 = 0.01, fh2 = 0.125, epsilon2 = 0.2, gammah2 = 0.1, Th2v = 0.5, Tvh2 = 0.5,
bh3 = 0.01, muh3 = 0.01, fh3 = 0.125, epsilon3 = 0.2, gammah3 = 0.1, Th3v = 0.5, Tvh3 = 0.5,
bv = 0.01, muv = 0.01)
# b = birth rate, m = death rate, f =  vector biting rate, gamma = recovery rate
# Thv = Pr(host infected after bite from infected vector), Thv = Pr(vector infected after bite of infected host)
# epsilon = rate at which exposed hosts become infectious
times <- seq(0,100,1)
out <- as.data.frame(ode(inits, times, v1h3seir, params ))
par(mfrow=c(1,4))


plot(out$`1` ~out$time, type='l', col='dark green',
    xlab= 'time', ylab='N', ylim = c(0,60))
lines(out$`2`~out$time, col='orange')
lines(out$`3`~out$time, col='red')
lines(out$`4`~out$time, col='blue')

plot(out$`5`~out$time, type = 'l', col= 'dark green',
    xlab= 'time', ylab='N', ylim = c(0,60))
lines(out$`6`~out$time, col= 'orange')
lines(out$`7`~out$time, col='red')
lines(out$`8`~out$time, col='blue')

plot(out$`9`~out$time, type = 'l', col= 'dark green',
    xlab= 'time', ylab='N', ylim = c(0,60))
lines(out$`10`~out$time, col= 'orange')
lines(out$`11`~out$time, col='red')
lines(out$`12`~out$time, col='blue')

plot(out$`13`~out$time, type = 'l', col= 'dark green',
    xlab= 'time', ylab='N', ylim = c(0,4000))
    lines(out$`14`~out$time, col= 'red')
