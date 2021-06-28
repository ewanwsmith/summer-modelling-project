library(deSolve)
v1h1sir <- function(times, inits, params){
  
  Sh <- inits[1] #suspectibe host
  Ih <- inits[2] #infected host
  Rh <- inits[3] #recovered host
  Sv <- inits[4] #susceptible vector
  Iv <- inits[5] #infected vector
  
  Nh <- Sh + Ih + Rh #total host popn
  Nv <- Sv + Iv #total vector popn
  
  
  
  with(as.list(c(params)), {
    
    dSh <- bh * Nh - f / Nh * Thv * Iv * Sh - muh * Sh 
    dIh <- f / Nh * Thv * Iv * Sh - muh - gammah * Ih
    dRh <- gammah * Ih - muh * Rh
    dSv <- bv * Nv - f / Nh * Tvh * Sv * Ih - muv * Sv
    dIv <- f / Nh * Tvh * Sv * Ih - muv * Iv
    
    return(list(c(dSh, dIh, dRh, dSv, dIv)))
  })
  
}

inits <- c(45,5,0,2700,300)
params <- c(bh = 0.01, muh = 0.01, bv = 0.01, muv = 0.01, f = 0.25, gammah = 0.1, Thv = 0.5, Tvh = 0.5)
# b = birth rate, mu = death rate, f = vector bite rate, gamma = recovery rate
# Thv = Pr(v -> h transmittion upon biting), Tvh = Pr(h -> v transmittion upon biting)
times <- seq(0,1000,1)

out <- as.data.frame(ode(inits, times, v1h1sir, params ))
head(out)
tail(out)

par(mfrow=c(1,2))
plot(out$`1` ~out$time, type='l', col='dark green', 
    xlab= 'time', ylab='N', ylim = c(0,60))
lines(out$`2`~out$time, col='orange')
lines(out$`3`~out$time, col='red')

plot(out$`4`~out$time, type = 'l', col= 'dark green',
    xlab= 'time', ylab='N', ylim = c(0,4000))
lines(out$`5`~out$time, col= 'orange')