###------------------------------------------------------------------------------------###
###                                                                                    ###
### (c) 2026 | Hynek Cígler                                                            ###
### contact: cigler@fss.muni.cz                                                        ###
### Psychology Research Institute                                                      ###
### Faculty of Social Studies, Masaryk University                                      ###
###                                                                                    ###
### Workshop: Tvorba kontinuálních norem při vývoji psychodiagnostických metod         ###
### Konference psychologická diagnostika Brno 2026 | 22. říjen 2026                    ###
###                                                                                    ###
###------ Sat Oct 17 15:44:46 2026 ----------------------------------------------------###



library(cNORM)
library(psych)
library(yarrr)
library(DescTools)
library(gamlss)

dat <- read.csv2("data.csv")
## vek_roky   - věk respondenta v letech
## group      - four age groups
## vek_kategorie - more age groups
## T01_M      - rychlost práce (M-skór) v testu Grafomotorické tempo
## T01_IQ     - standardní IQ skór pro T01
## M06_W      - matematické usuzování (Slovní úlohy) - W-skór
## M06_IQ     - std IQ skór pro M06_W
## C07_M      - rychlost práce v subtestů Diktát (M-skór)
## C07_IQ_cas - příslušný std IQ skór
## C07_W      - správnost práce v subtestu Diktát (W-skór)
## C07_IQ_spravnost - příslušný std IQ skór


dat$vek_kategorie <- factor(dat$vek_kategorie, levels = 
                              c("MŠ", "MŠ0", "ZŠ1", "ZŠ2", "ZŠ3", "ZŠ4", "ZŠ5", "ZŠ6", "ZŠ7", "ZŠ8", "ZŠ9", 
                                 "15-17", "18-19", "20-22", "23-25", "55-59", "60-69", "70-79"))

head(dat)
describe(dat)


## oříznema data jen na juniorský vzorek

dat <- dat[dat$vek_roky < 40, ]


# T01: Grafomotorické tempo -----------------------------------------------

## Kontrola standardních skórů
hist(dat$T01_IQ, breaks = 20)
qqnorm(dat$T01_IQ); qqline(dat$T01_IQ)
pirateplot(T01_IQ ~ vek_kategorie, dat)

## Deskriptivy
hist(dat$T01_M)
pirateplot(T01_M ~ vek_kategorie, dat)
plot(dat$vek_roky, dat$T01_M)



## Vyřadit chybějící data v T01

dat2 <- dat[!is.na(dat$T01_M), ]



# Klasické normy ----------------------------------------------------------

vek_zaokrouhleny <- floor(dat2$vek_roky)
veky <- sort(unique(vek_zaokrouhleny))
table(vek_zaokrouhleny)

normy_diskretni <- NULL
for (i in 2:17) {
  normy_diskretni <- rbind(normy_diskretni, 
                           c(vek = veky[i],
                             M = mean(dat2$T01_M[vek_zaokrouhleny == veky[i]]),
                             SD = sd(dat2$T01_M[vek_zaokrouhleny == veky[i]])))
}
normy_diskretni <- data.frame(normy_diskretni)
rownames(normy_diskretni) <- normy_diskretni$vek


plot(dat2$vek_roky, dat2$T01_M, 
     xlab = "věk", ylab = "skóre", 
     main = "Klasické normy", col = "gray")
lines(normy_diskretni$vek, normy_diskretni$M, lwd = 3, type = "s")
lines(normy_diskretni$vek, normy_diskretni$M-normy_diskretni$SD, lwd = 1, 
      lty=2, type = "s")
lines(normy_diskretni$vek, normy_diskretni$M+normy_diskretni$SD, lwd = 1, 
      lty=2, type = "s")

# Regresní normy ----------------------------------------------------------


## Lineární predikce, konstantní reziduální SD -------------

m_reg <- lm(T01_M ~ vek_roky, data = dat2)
IQ_reg <- 100 + 15 * residuals(m_reg) / sigma(m_reg)

plot(dat2$vek_roky, dat2$T01_M, 
     xlab = "věk", ylab = "skóre", 
     main = "Lineární regresní normy")
lines.lm(m_reg, conf.level = .95, pred.level = 1-(pnorm(1, lower.tail = F)*2), 
         args.pband = list(col = SetAlpha("green", 0.5)))



## GAMLSS ------------------------------------------------------------------


m_gamlss <- gamlss(T01_M ~ pb(vek_roky), sigma.formula = ~ pb(vek_roky),
  family = NO(), data = dat2[c("T01_M", "vek_roky")], trace = FALSE)

p <- predictAll(m_gamlss, type = "response")
IQ_gamlss <- 100 + 15 * (dat2$T01_M - p$mu) / p$sigma


roky <- seq(7, 26, by = .01)
gamlss_predict <- predictAll(m_gamlss, newdata = data.frame(vek_roky = roky), type = "response")

lines(roky, gamlss_predict$mu, lwd = 3)
lines(roky, gamlss_predict$mu - gamlss_predict$sigma, lwd = 1, lty = 2)
lines(roky, gamlss_predict$mu + gamlss_predict$sigma, lwd = 1, lty = 2)



# cNORM: Kontinuální normy -------------------------------------------------------

group <- cNORM::getGroups(dat2$vek_roky, 8)
table(group)


m1 <- cnorm(raw = dat2$T01_M, group = group, scale = "IQ", 
            k = 4, t = 4)

## Co je vevnitř toho objektu? 
m1$data ## data!
m1$model ## regresní objekt! 
summary(m1$model)

print(m1)
printSubset(m1)
plotCnorm()
plot(m1, 1)
plot(m1, 2)
plot(m1, 3)
plot(m1, 4)
plot(m1, 5)
plotCnorm(m1, 6)
plotPercentiles(m1, percentiles = c(.001, .01, 0.025, 0.1, 0.25, 0.5, 0.75, .9, .975, .99, .999))

x <- plotPercentileSeries(m1,
                          end = 12,
                          percentiles = c(.001, .01, 0.025, 0.1, 0.25, 0.5, 0.75, .9, .975, .99, .999))
pdf("T01_percentily.pdf")
x
dev.off()


## cross-validace
cnorm.cv(m1$data, max = 7, repetitions = 10, pCutoff = .05)
# pdf("T01_crossvalidace.pdf")
# system.time(try(m1_cross <- cnorm.cv(m1$data, max = 7, repetitions = 10, pCutoff = .05)))
# dev.off()
# print(m1_cross, digits = 3)

## Model se čtyřmi parametry!
m2 <- cnorm(raw = dat2$T01_M, group = group, scale = "IQ", 
            k = 3, t = 4, 
            terms = 4)

cNORM::compare(m1, m2)

plotPercentiles(m2, percentiles = c(.001, .01, 0.025, 0.1, 0.25, 0.5, 0.75, .9, .975, .99, .999))



plotRaw(m2)
plotRaw(m2, group = T)
plotNorm(m2)
plotNorm(m2, group = T)

checkConsistency(m2, minNorm = 60, maxNorm = 140,
                 minAge = 4, maxAge = 25)
plotDerivative(m2, minAge = 5, maxAge = 23, minNorm = 60, maxNorm = 140)

predictRaw(norm=100, age = 10, coefficients = m2$model$coefficients)
predictNorm(raw=0, A = 10, model = m2, minNorm = 40, maxNorm = 160)


## Nakreslí vývoj s SD
getNormCurve(100, m2)
plot(getNormCurve(100, m2)[,2:3], type = "l", lwd = 3, 
     xlab = "věk", ylab = "M-skór", las = 1, 
     ylim = c(-.6, 1), 
     main = "T01: Vztah průměrného výkonu a věku")
lines(getNormCurve(115, m2)[,2:3], type = "l", lwd = 1, lty = 2)
lines(getNormCurve(85, m2)[,2:3], type = "l", lwd = 1, lty = 2)

abline(v=10, h=0)



## zkontroluj oproti originálním normám použitým v BACH

m2_IQ <- predictNorm(dat$T01_M, A = dat$vek_roky, model = m2, minNorm = 60, maxNorm = 140)
hist(m2_IQ)
ICC(cbind(dat$T01_IQ, m2_IQ))
cor.test(dat$T01_IQ, m2_IQ)
plot(dat$T01_IQ, m2_IQ, 
     xlab = "originální BACH", ylab = "workshop")
## Rozdíl je zčásti způsoben odstraněním seniorů, kteří výpočty komplikují


## Export normativní tabulky s přesností na měsíc

T01_norm <- normTable(A = seq(5, 23, by=1/12), model = m2,
                      minNorm = 60, maxNorm = 140, 
                      step = .1, pretty = F, monotonuous = T)

### Tento postup vytvoří jednoduchou tabulku pro další použití!
x <- data.frame(norm=T01_norm[[1]]$norm, as.data.frame(lapply(T01_norm, '[[', "raw")))
names(x)[-1] <- round(as.numeric(gsub("X", "", names(x)[-1]))*12)
write.csv2(x, "T01_norm.csv", row.names = F)
head(x)





# Srovnání oproti typů norem -------------------------------------


## cNORM
plot(getNormCurve(100, m2)[,2:3], type = "l", lwd = 3, 
     xlab = "věk", ylab = "M-skór", las = 1, 
     ylim = c(-.6, 1), 
     col = piratepal("basel")[1], 
     main = "T01: Vztah průměrného výkonu a věku")
lines(getNormCurve(115, m2)[,2:3], type = "l", lwd = 1, lty = 2, 
      col = piratepal("basel")[1])
lines(getNormCurve(85, m2)[,2:3], type = "l", lwd = 1, lty = 2, 
      col = piratepal("basel")[1])
abline(v=10, h=0)

## Lineární normy
lines.lm(m_reg, conf.level = NA, pred.level = 1-(pnorm(1, lower.tail = F)*2), 
         col = piratepal("basel")[2], 
         args.pband = list(col = NA, border = piratepal("basel")[2]))

## GAMLSS
lines(roky, gamlss_predict$mu, lwd = 3, col = piratepal("basel")[3])
lines(roky, gamlss_predict$mu - gamlss_predict$sigma, lwd = 1, lty = 2,
      col = piratepal("basel")[3])
lines(roky, gamlss_predict$mu + gamlss_predict$sigma, lwd = 1, lty = 2, 
      col = piratepal("basel")[3])

## Klasické normy

lines(normy_diskretni$vek-.5, normy_diskretni$M, lwd = 3, 
      type = "s", col = piratepal("basel")[4])
lines(normy_diskretni$vek-.5, normy_diskretni$M-normy_diskretni$SD, lwd = 1, lty=2, 
      type = "s", col = piratepal("basel")[4])
lines(normy_diskretni$vek-.5, normy_diskretni$M+normy_diskretni$SD, lwd = 1, lty=2,
      type = "s", col = piratepal("basel")[4])

legend("bottomright", c("cNORM", "LM", "GAMLSS", "diskrétní"), 
       col = piratepal("basel"), lwd = 3, inset = .01, cex = .8)








# Další parametrické postupy ----------------------------------------------


m3 <- cnorm.shash(age= dat2$vek_roky, score = dat2$T01_M, scale = "IQ")
# m3 <- cnorm.shash(age= dat$vek_roky, score = dat$T01_M, 
#                   mu_degree = 3, 
#                   sigma_degree = 1, 
#                   epsilon_degree = 1, 
#                   delta_degree = 1, scale = "IQ")
summary(m3)


cNORM::compare(m2, m3)



### Dokreslení do předchozího grafu -----------------------------------------

getNormCurveShash <- function(norm, model, minAge, maxAge, step = 0.1) {
  age <- seq(minAge, maxAge, by = step)
  pars <- cNORM:::predictCoefficients_shash(model, age)
  
  z <- (norm - attr(model$result, "scaleMean")) /
    attr(model$result, "scaleSD")
  
  raw <- with(pars, cNORM::qshash(
    pnorm(z), mu = mu, sigma = sigma,
    epsilon = epsilon, delta = delta
  ))
  
  data.frame(norm = norm, age = age, raw = raw)
}

lines(getNormCurveShash(100, m3, 8, 23)$age, getNormCurveShash(100, m3, 8, 23)$raw, 
      lwd = 3, col = piratepal("basel")[5])
lines(getNormCurveShash(115, m3, 8, 23)$age, getNormCurveShash(115, m3, 8, 23)$raw, 
      lwd = 2, lty=2, col = piratepal("basel")[5])
lines(getNormCurveShash(85, m3, 8, 23)$age, getNormCurveShash(85, m3, 8, 23)$raw, 
      lwd = 2, lty=2, col = piratepal("basel")[5])




# GRAFICKÝ INTERFACE! -----------------------------------------------------

cNORM.GUI(launch.browser = TRUE)
cNORM.GUI2(launch.browser = TRUE)





