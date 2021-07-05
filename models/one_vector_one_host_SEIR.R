library(deSolve)
v1h1seir <- function(times, inits, params){
  
  Sh <- inits[1] #susceptible host
  Eh <- inits[2] #exposed host
  Ih <- inits[3] #infected host
  Rh <- inits[4] #recovered host
  Sv <- inits[5] #susceptible vector
  Iv <- inits[6] #infected vector
  
  Nh <- Sh + Eh + Ih + Rh #total host popn
  Nv <- Sv + Iv #total vector popn
  
  
  
  with(as.list(c(params)), {
    
    dSh <- bh * Nh - f / Nh * Thv * Iv * Sh - muh * Sh
    dEh <- f / Nh * Thv * Iv * Sh - epsilon * Eh - muh * Eh
    dIh <- epsilon * Eh - muh * Ih - gammah * Ih
    dRh <- gammah * Ih - muh * Rh
    dSv <- bv * Nv - f / Nh * Tvh * Sv * Ih - muv * Sv
    dIv <- f / Nh * Tvh * Sv * Ih - muv * Iv
    
    return(list(c(dSh, dEh, dIh, dRh, dSv, dIv)))
  })
  
}

inits <- c(45,0,5,0,2700,300)
params <- c(bh = 0.05, muh = 0.05, bv = 0.05, muv = 0.05, f = 0.125, gammah = 0.1, Thv = 0.5, Tvh = 0.5, epsilon = 0.1)
# b = birth rate, m = death rate, f =  vector biting rate, gamma = recovery rate
# Thv = Pr(host infected after bite from infected vector), Thv = Pr(vector infected after bite of infected host)
# epsilon = rate at which exposed hosts become infectious
times <- seq(0,100,1)
out <- as.data.frame(ode(inits, times, v1h1seir, params ))
par(mfrow=c(1,2))


plot(out$`1` ~out$time, type='l', col='dark green',
    xlab= 'time', ylab='N', ylim = c(0,60))
lines(out$`2`~out$time, col='orange')
lines(out$`3`~out$time, col='red')
lines(out$`4`~out$time, col='blue')

plot(out$`5`~out$time, type = 'l', col= 'dark green',
    xlab= 'time', ylab='N', ylim = c(0,4000))
lines(out$`6`~out$time, col= 'red')
