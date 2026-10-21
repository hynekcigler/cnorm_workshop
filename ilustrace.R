###------------------------------------------------------------------------------------###
###                                                                                    ###
### (c) 2026 | Hynek Cígler                                                            ###
### contact: cigler@fss.muni.cz                                                        ###
### Psychology Research Institute                                                      ###
### Faculty of Social Studies, Masaryk University                                      ###
###                                                                                    ###
### Workshop: Tvorba kontinuálních norem při vývoji psychodiagnostických metod         ###
### Konference psychologická diagnostika Brno 2026 | 22. říjen 2026                    ###
### Ilustrativní příklady použité v přednášce                                          ###
###                                                                                    ###
###------ Sat Oct 17 15:30:05 2026 ----------------------------------------------------###

library(psych)
library(yarrr)

# Výběrová chyba ----------------------------------------------------------


n <- 25:1000
SE_M <- 1/sqrt(n)
SE_SD <- 1/sqrt(2*(n-1))

SE <- function(s=1, r, round. = NULL) {
  result <- s*sqrt(1-r)
  if(is.numeric(round.)) {
    return(round(result, round.)) 
  } else {
    return(result)
  }
}

SE_q <- function(s=1, q=0, n, round. = NULL) {
  result <- s*sqrt(1/n + (q**2)/(2*(n-1)))
  if(is.numeric(round.)) {
    return(round(result, round.)) 
  } else {
    return(result)
  }
}


SE_comb <- function(s=1, q=0, n, r, round. = NULL) {
  result <- s*sqrt(1 - r + 1/n + (q**2)/(2*(n-1)))
  if(is.numeric(round.)) {
    return(round(result, round.)) 
  } else {
    return(result)
  }
}


## Chyba parametrů
plot(n, SE_M, type = "l", 
     lwd = 3, col = piratepal("basel")[1], 
     xlim = c(0, max(n)), ylim = c(0, max(SE_M)), 
     xlab = "Velikost vzorku (min = 25)", 
     ylab = "Velikost chyby")
lines(n, SE_SD, 
     lwd = 3, col = piratepal("basel")[2])
abline(v=seq(50,1000, by = 50), col = "gray")
legend("topright", 
       c("M", "SD"), lwd = 3, col = piratepal("basel"), inset = .02)


## Dopad na skóre
plot(n, SE_M, type = "l", 
     lwd = 3, col = piratepal("basel")[1], 
     xlim = c(0, max(n)), ylim = c(0,  max(SE_q(q = 3, n = n))), 
     xlab = "Velikost vzorku (min = 25)", 
     ylab = "Velikost chyby")
lines(n, SE_SD, 
      lwd = 3, col = piratepal("basel")[2])
lines(n, SE_q(q = 1, n = n), col = piratepal("basel")[3], lwd = 3, lty = 2)
lines(n, SE_q(q = 2, n = n), col = piratepal("basel")[4], lwd = 3, lty = 2)
lines(n, SE_q(q = 3, n = n), col = piratepal("basel")[5], lwd = 3, lty = 2)
abline(v=seq(50,1000, by = 50), col = "gray")
legend("topright", 
       c("M", "SD", "z = 1", "z = 2", "z = 3"), lwd = 3, 
       lty = c(1,1,2,2,2), 
       col = piratepal("basel"), inset = .02)


SE_q(q = 0, n = 50, round. = 2)
SE_q(q = 0, n = 250, round. = 2)

SE_q(q = 1, n = 50, round. = 2)
SE_q(q = 1, n = 250, round. = 2)

SE_q(q = 2, n = 50, round. = 2)
SE_q(q = 2, n = 250, round. = 2)

SE_q(q = 3, n = 50, round. = 2)
SE_q(q = 3, n = 250, round. = 2)


result <- NULL
for (r in c(.7, .9)) {
  for (x in 0:3) {
    temp <- c(SE(r = r, round. = 2), 
              SE_comb(q=x, r=r, n = c(50, 250, 1000), round. = 2))
    result <- rbind(result, temp)
  }
}
rownames(result) <- c("r=.7, x=0", 
                      "r=.7, x=1", 
                      "r=.7, x=2", 
                      "r=.7, x=3", 
                      "r=.9, x=0",
                      "r=.9, x=1",
                      "r=.9, x=2",
                      "r=.9, x=3")
colnames(result) <- c("SE", "n=50", "n=250", "n=1000")
result

result2 <- NULL
for (x in 0:3) {
  temp <- c(SE_comb(q=x, r=1, n = c(50, 250, 1000), round. = 2))
  result2 <- rbind(result2, temp)
}
rownames(result2) <- c("x=0","x=1","x=2","x=3"); colnames(result2) <- c("n=50", "n=250", "n=1000")
result2

write.table(result,file = "clipboard", sep="\t", dec=",")
write.table(result2,file = "clipboard", sep="\t", dec=",")


windows(10,6)
plot(NA, xlim = c(0,1000), ylim = c(0,.75), 
     las = 1, 
     xlab = "Velikost vzorku (min = 25)", 
     ylab = "Kombinovaná chyba měření a norem (SD = 1)")
i <- 1
for (r in c(.7, .8, .9)) {
  j <- 1
  for (q in 0:3) {
    lines(n, SE_comb(q = q, n = n, r = r), 
          lwd = 3, lty = i, col = piratepal("basel")[j])
    j <- j + 1
  }
  i <- i + 1
}
abline(h = SE(r = c(.7, .8, .9)), lty = 1:3, col = "black")
legend("bottom", c(paste0("r = .7, ", paste0("z = ", 0:3)), 
                   paste0("r = .8, ", paste0("z = ", 0:3)), 
                   paste0("r = .9, ", paste0("z = ", 0:3))), 
       ncol = 3, lwd = 3, cex = .8, inset = .02, 
       lty = rep(1:3, each = 4), 
       col = piratepal("basel")[1:4])




# Výběrová chyba percentilu -----------------------------------------------

sim.perc <- function(n, quants = c(1:99)/100) {
  x <- rnorm(n = n)
  xdist <- ecdf(x)
  result_sys <- (quantile(xdist, quants) - qnorm(quants))
  result_rand <- (quantile(xdist, quants) - qnorm(quants))**2 ## squared-error
  list(sys = result_sys, 
       rand = result_rand)
}
sim.perc(n = 100)

samples <- 1000
n <- c(25, 50, 100, 250, 1000)

result_sys <- result_ran <- NULL
for (ns in 1:length(n)) {
  cat("Sample size", n[ns], "\n")
  result1_ran <- result1_sys <- NULL
  for (s in 1:samples) {
    temp <- sim.perc(n = n[ns])
    result1_sys <- rbind(result1_sys, temp$sys)
    result1_ran <- rbind(result1_ran, temp$rand)
  }
  result_sys <- cbind(result_sys, colMeans(result1_sys))
  result_ran <- cbind(result_ran, colMeans(result1_ran)) ## MSE
}
colnames(result_sys) <- colnames(result_ran) <- n


windows(12,6); layout(t(1:2))
plot(1:99, result_sys[,1], type = "l", lwd = 3, 
     main = "Systematická zkreslení odhadu percentilu", 
     xlab = "percentil", ylab = "rozdíl (z-skór)", 
     col = piratepal("basel")[1])
for (i in 2:5) {
  lines(1:99, result_sys[,i], lwd = 3, col = piratepal("basel")[i])
}
legend("bottomleft", as.character(n), col = piratepal("basel"), 
       lwd = 3, inset = .01, title = "N", cex = .8, ncol = 2)

plot(1:99, sqrt(result_ran[,1]), type = "l", lwd = 3, 
     main = "Náhodná chyba odhadu percentilu", 
     xlab = "percentil", ylab = "RMSE (z-skór)", 
     col = piratepal("basel")[1], 
     ylim = c(0, max(sqrt(result_ran))), 
     sub = "Přerušovaná čára je chyba lineární transformace", cex.sub = .8)
lines(1:99, SE_q(n = n[1], q = qnorm((1:99)/100)), col = piratepal("basel")[1], lty = 2)
for (i in 2:5) {
  lines(1:99, sqrt(result_ran[,i]), lwd = 3, col = piratepal("basel")[i])
  lines(1:99, SE_q(n = n[i], q = qnorm((1:99)/100)), col = piratepal("basel")[i], lty = 2)
  
}
legend("top", as.character(n), col = piratepal("basel"), 
       lwd = 3, inset = .01, title = "N", cex = .8, ncol = 2)
