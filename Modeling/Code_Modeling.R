##### 1) Inform the directory:
setwd("D:/SaLLy - ST/Projeto 1/SR2024/Public_Files") # In our case
getwd()

##### 2) Charge the packages (after installation, if necessary):
library(CARBayesST)
require(ggplot2)

##### 3) Reading files
### Write the name of the biome:
# 'Amazonia', 'Caatinga', 'Cerrado', 'Mata_Atlantica', 'Pampas', or 'Pantanal'.
biome <- 'Pampas'

Dataset <- read.table(file = paste("Data_", biome, ".txt", sep = ""), 
                      header = TRUE, sep = "\t", dec = ".")
M_W <- as.matrix(read.csv2(file = paste("W_", biome, ".csv", sep = ""), 
                           sep = ",", header = TRUE))
# For the univariate models the response and each covariate should be vectors of
# length (NT)×1, where N is the number of spatial units and T is the number of 
# time periods. Each vector should be ordered so that the first N data points 
# are the set of all N spatial locations at time 1, the next N are the set of 
# spatial locations for time 2 and so on.
T_value <- 120 # in this study
N_value <- nrow(M_W)

##### 4) Run the model (it can be very slow for some biomes)
f_bayes <- vec_FS ~ vec_LUT + vec_Humid + vec_Temp + offset(log(vec_Area)) 
Mod <- ST.CARlinear(formula = f_bayes, data = Dataset,
                    family = "poisson", W = M_W,
                    burnin = 15000, n.sample = 30000, thin = 15, MALA = FALSE)

##### 5) Outputs for some parts of Tables 3, 4, and 5:
Mod$formula
Mod$model
Mod$summary.results
Mod$modelfit
Mod$accept
# Specifically for trace plots of chains, try, e.g.:
plot(Mod$samples$beta[,2])

##### 6) Relative risks (Table 5)
### Variable: LUT
xi <- sd(Dataset[,"vec_LUT"])
rr <- exp(xi * quantile(x = Mod$samples$beta[,2], c(0.025, 0.5, 0.975)))
print(xi)
print(rr)

### Variable: HUMID
xi <- sd(Dataset[,"vec_Humid"])
rr <- exp(xi * quantile(x = Mod$samples$beta[,3], c(0.025, 0.5, 0.975)))
print(xi)
print(rr)

### Variable: TEMP
xi <- sd(Dataset[,"vec_Temp"])
rr <- exp(xi * quantile(x = Mod$samples$beta[,4], c(0.025, 0.5, 0.975)))
print(xi)
print(rr)

##### 7) Estimation of the average temporal trend (used in Figure 6)
Date <- rep(x = seq(from = as.Date("2012-01-01"),
                    to = as.Date("2021-12-31"), by = "1 month"),
            each = N_value) # in this study
time <- seq(from = as.Date("2012-01-01"),
            to = as.Date("2021-12-31"),
            by = "1 month") # in this study
lci <- tapply(X = Mod$fitted.values, INDEX = Date, FUN = quantile, p = .025)
mean <- tapply(X = Mod$fitted.values, INDEX = Date, FUN = mean)
uci <- tapply(X = Mod$fitted.values, INDEX = Date, FUN = quantile, p = .975)
obs <- tapply(X = Dataset[,"vec_FS"], INDEX = Date, FUN = mean)

fig <- data.frame(time, obs, lci, mean, uci)

ggplot() + 
  geom_line(data = fig, aes(x = time, y = mean), color = "blue") +
  geom_line(data = fig, aes(x = time, y = lci), 
            color = "red", linetype = "dotted") +
  geom_line(data = fig, aes(x = time, y = uci), 
            color = "red", linetype = "dotted") +
  geom_point(data = fig, aes(x = time, y = obs), size = 1) + 
  xlab('Years') +
  ylab('Fire Spots')

##### 8) delta (used in Figure 7) and spatial residuals (used in Figure 8)
### Spatial residuals (mean_r1: raw residuals, and mean_r2: Pearson residuals)
mean_r1 <- tapply(X = Mod$residuals[,1], 
                  INDEX = Dataset[,"vec_Munic"], FUN = mean)
mean_r2 <- tapply(X = Mod$residuals[,2], 
                  INDEX = Dataset[,"vec_Munic"], FUN = mean)

### delta (classification in the object deltaClass)
deltaLCI <- apply(X = Mod$samples$delta, MARGIN = 2, FUN = quantile, p = 0.025)
deltaUCI <- apply(X = Mod$samples$delta, MARGIN = 2, FUN = quantile, p = 0.975)
deltaClass <- numeric(length = length(mean_r1))
for(k in 1:length(mean_r1)) {
  deltaClass[k] <- ifelse(test = deltaLCI[k] < 0 & deltaUCI[k] < 0, 
                          yes = -1,
                          no = ifelse(test = deltaLCI[k] > 0 & deltaUCI[k] > 0,
                                      yes = 1,
                                      no = 0))
}

##### 9) Posterior predictive checks (used in Figure 9)
num_it <- nrow(Mod$samples$delta)
t_bar <- (T_value + 1)/2
aux_times <- (Dataset[,"vec_Month"] - t_bar) / T_value

### PPC (it can be slow for some biomes)
mu_rep <- matrix(data = 0, ncol = num_it, nrow = N_value*T_value) # (Eq. 9)
Y_rep <- matrix(data = 0, ncol = num_it, nrow = N_value*T_value) # (Eq. 10)
for(it in 1:num_it) {
  aux1 <- rep(x = Mod$samples$phi[it,], times = T_value)
  aux2 <- rep(x = Mod$sample$alpha[it], times = N_value*T_value)
  aux3 <- rep(x = Mod$samples$delta[it,], times = T_value)
  psi <- aux1 + (aux2 + aux3)*aux_times
  aux4 <- rep(x = Mod$samples$beta[it, 1], times = N_value*T_value)
  aux5 <- Mod$samples$beta[it, 2]*Dataset[,"vec_LUT"]
  aux6 <- Mod$samples$beta[it, 3]*Dataset[,"vec_Humid"]
  aux7 <- Mod$samples$beta[it, 4]*Dataset[,"vec_Temp"]
  regr <- aux4 + aux5 + aux6 + aux7
  mu <- exp(psi + regr + log(Dataset[,"vec_Area"])) 
  mu_rep[,it] <- mu
  Y_rep[,it] <- outer(1, mu, Vectorize(rpois)) |> apply(2, as.vector)
  print(it) # if you want to see the loop progress.
}

### Plot (it also can be slow)
vec_FS_cat <- factor(x = ifelse(test = Dataset[,"vec_FS"] <= 10, 
                                Dataset[,"vec_FS"], 11),
                     levels = 0:11,
                     labels = c("0", "1", "2", "3", "4", "5",
                                "6", "7", "8", "9", "10", ">10"))
Y_rep_cat <- factor(x = ifelse(test = Y_rep <= 10, Y_rep, 11),
                    levels = 0:11,
                    labels = c("0", "1", "2", "3", "4", "5",
                               "6", "7", "8", "9", "10", ">10"))

freq_abs_obs <- table(vec_FS_cat)
freq_rel_obs <- prop.table(freq_abs_obs)*100
freq_abs_rep <- table(Y_rep_cat)
freq_rel_rep <- prop.table(freq_abs_rep)*100

barplot(rbind(freq_rel_obs, freq_rel_rep), beside = TRUE,
        ylab = "%", col = c("darkblue","red"))
legend("top",
       c("Observed", "Simulated"),
       cex = 0.8,
       fill = c("darkblue","red"))
