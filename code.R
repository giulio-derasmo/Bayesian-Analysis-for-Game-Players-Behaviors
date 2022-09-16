# ------- LIBRARIES ------------------------------
library(bayesvl)
library(Hmisc)
library(ggplot2)
library(kableExtra)
library(R2jags)
library(mcmcplots)
library(bayesplot)
library(ggmcmc)
library(caret)
library(nnet)
library(Hmisc)
library(TeachingDemos)

# ------- IMPORT THE DATA AND FIRST ANALYSIS ----------------

# import the dataset
dat <- read.csv("data_640_validated.csv", header = TRUE)

# rename and keep this three
dat$Anti_Anthro <- dat$C12
dat$TakeWood    <- dat$E16
dat$CutTree     <- dat$E17

dat <- dat[, c("Anti_Anthro","TakeWood","CutTree")]

# summary of the data
tab <- as.data.frame(sapply(dat, summary))
t(tab) %>%
  kbl(format = "latex", booktabs = T)  %>%
  kable_styling(bootstrap_options = "striped",latex_options = "HOLD_position")

# plotting of the histogram 
par(mfrow = c(1,3))
plot(prop.table(table(dat$Anti_Anthro)), col = "black", type = "h",
     xlab = "Level of Disagreement", ylab = "Density", main = "Empirical distro of Anti Anthro")
plot(prop.table(table(dat$TakeWood)), col = "black", type = "h",
     xlab = "Level of Frequencies in taking wood", ylab = "Density", main = "Empirical distro of TakeWood")
plot(prop.table(table(dat$CutTree)), col = "black", type = "h",
     xlab = "Level of Frequencies in cutting tree", ylab = "Density", main = "Empirical distro of CutTree")
par(mfrow = c(1,1))

# correlation matrix 
corr_matrix <- rcorr(as.matrix(dat))$r
corr_matrix %>%
  kbl(format = "latex", booktabs = T)


# ------- OLD MODEL ----------------------------------

# define the model for JAGS
model.string = "
  model
	{
		for( i in 1:N ) {
			Anti_Anthro[i] ~ dnorm(mu[i], precision)
			mu[i] <- a_Anti_Anthro + b_TakeWood * TakeWood[i] +
			              b_CutTree * CutTree[i]
		} 
		# PREDICTION
    Ypred1 ~ dnorm(mu1, precision)  # random variable
    mu1 <- a_Anti_Anthro + b_TakeWood * 1 +
			              b_CutTree * 4

    # PRIOR
		a_Anti_Anthro ~ dnorm(0, 0.01)
		b_TakeWood ~ dnorm(0.0, 0.1)
		b_CutTree ~ dnorm(0.0, 0.1) 
		precision ~ dgamma(0.001, 0.001)
	}
	"

# write the model to a text file
writeLines(model.string, con = "model_jags.txt")

# Building the data
data <- as.list(dat)
data$N <- nrow(dat)

# list of parameters name
parameters <- c("a_Anti_Anthro","b_TakeWood",
                "b_CutTree", "Ypred1")
# initial value 
inits <- list(a_Anti_Anthro = 3, b_TakeWood = 0, b_CutTree = 0)
initial.values <- list(inits)

# MCMC with jags
set.seed(123) # set seed for reproducibility
model1 <- jags(data = data,
               inits = initial.values,
               parameters.to.save = parameters,
               model.file = "model_jags.txt",
               n.burnin = 2000, 
               n.chains = 1, n.thin = 1, 
               n.iter = 5000)

# look at summary
tab <- model1$BUGSoutput$summary[parameters, ]
tab %>%
  kbl(format = "latex", booktabs = T)  %>%
  kable_styling(bootstrap_options = "striped", latex_options=c("scale_down","HOLD_position"))


# look at the trace-plot
mcmcplots::traplot(model1, parms = c("a_Anti_Anthro",
                                     "b_TakeWood",
                                     "b_CutTree"),
                   plot.title = "Trace-Plot of the main parameters")



# look at the posterior predictive density
Ypred1 = model1$BUGSoutput$sims.array[,,"Ypred1"]

polygon_ax <- function(Ypred, min, max){
  
  region = emp.hpd(Ypred, conf=0.95)
  lower = region[1]
  upper = region[2]
  
  densy = density(Ypred) 
  xx <- densy$x
  yy <- densy$y
  
  # Lower and higher indices on the X-axis
  l <- min(which(xx >= lower))
  u <- max(which(xx <  upper))
  
  x1 <- c(xx[c(l, l:u, u)])
  y1 <- c(0, yy[l:u], 0)
  
  return(list(x1 = x1, y1 = y1))
}


axs = polygon_ax(Ypred1, -2,8)
plot(density(Ypred1), lwd = 2, col = "purple",
     main="Predicition of new answer given TakeWood = 1, CutTree = 4",
     ylim=c(0,.5), xlab = "y")
polygon(axs$x1, axs$y1,
        col = yarrr::transparent('orange', trans.val = .7))
legend("topright", legend = 'Empirical HPD 95%',
       col = yarrr::transparent('orange', trans.val = .7),
       pch = 15, bty = 'n')


# -------- MULTINOMIAL LOGISTIC MODEL --------------------

data <- as.list(dat)   # variable
data$N <- nrow(dat)    # n-row
data$J <- length(as.numeric(levels(as.factor(dat$Anti_Anthro))))  # n-categories

# define the model for JAGS
model.string = "
  model
	{
	  # ------- Multinomial Logit Regression ------ #
    for(i in 1:N){
        
        # The observation of 1,2,3,4 or 5
        # with baseline 5
        Anti_Anthro[i] ~ dcat(p[i, 1:J])
        
        for (j in 1:J){
          log(q[i,j]) <-  intercept[j] + 
                          b_TakeWood[j] * TakeWood[i] + 
                          b_CutTree[j] * CutTree[i]
        
          p[i,j] <- q[i,j]/sum(q[i,1:J])  
        } 
      }   
    
    # We need to fix the effects corresponding 
    # to the >>last<< observation category to 0:
    intercept[J] <- 0
    b_TakeWood[J] <- 0
    b_CutTree[J] <- 0
    
    # ------  PRIOR --------- #
    for(j in 1:(J-1)){
        intercept[j] ~ dnorm(0, 0.01)
        b_TakeWood[j] ~ dnorm(0, 0.1)
        b_CutTree[j] ~ dnorm(0, 0.001)
    }
    
    # ------- PREDICTION  -------- #
    Anti_Anthro_new ~ dcat(pnew[1:J])
    
    for (j in 1:J){
      log(qnew[j]) <-  intercept[j] + 
                      b_TakeWood[j] * 1 + 
                      b_CutTree[j] * 4
    
      pnew[j] <- qnew[j]/sum(qnew[1:J])  
      } 
  }
"


## write the model to a text file
writeLines(model.string, con = "model_multicategory.txt")


# List of parameters name
parameters <- c("intercept", "b_TakeWood", "b_CutTree", "Anti_Anthro_new")

# Starting value initialization
inits1 <- list( intercept = c(0,0,0,0,NA),
                b_TakeWood = c(0,0,0,0,NA),
                b_CutTree = c(0,0,0,0,NA))

inits2 <- list( intercept = c(0.5,0.5,0.5,0.5,NA),
                b_TakeWood = c(-0.5,-0.5,-0.5,-0.5,NA),
                b_CutTree = c(0.5,0.5,0.5,0.5,NA))

initial.values <- list(inits1 = inits1, inits2 = inits2)

# MCMC with jags
set.seed(123)
MNL <- jags(data = data,
            inits = initial.values,
            parameters.to.save = parameters,
            model.file = "model_multicategory.txt",
            n.chains = 2, n.thin = 10,  
            n.burnin = 7000,
            n.iter = 40000)


# look at the summary
MNL_summary <- MNL$BUGSoutput$summary[c(paste("b_TakeWood[", seq(1,4), "]", sep = ""),
                                        paste("b_CutTree[", seq(1,4), "]", sep = ""),
                                        paste("intercept[", seq(1,4), "]", sep = "")), ]
MNL_summary %>%
  kbl(format = "latex", booktabs = T)  %>%
  kable_styling(bootstrap_options = "striped", latex_options=c("scale_down","HOLD_position"))


# MCMC Posterior parameters 
MNL.mcmc = as.mcmc(MNL)
b_TakeWood <- MNL.mcmc[, c(paste("b_TakeWood[", seq(1,4), "]", sep = ""))]
b_CutTree  <- MNL.mcmc[, c(paste("b_CutTree[", seq(1,4), "]", sep = ""))]
intercept  <- MNL.mcmc[, c(paste("intercept[", seq(1,4), "]", sep = ""))]

# ------ Posterior densities -------- #
pdf("dens_b_TakeWood.pdf",  width = 4, height = 3) 
mcmc_areas(b_TakeWood, prob = .95, point_est = "mean", border_size = .7)+ geom_vline(xintercept=0)
dev.off()

pdf("dens_b_CutTree.pdf",  width = 4, height = 3) 
mcmc_areas(b_CutTree, prob = .95, point_est = "mean", border_size = .7)+ geom_vline(xintercept=0)
dev.off()

pdf("dens_intercept.pdf",  width = 4, height = 3) 
mcmc_areas(intercept, prob = .95, point_est = "mean", border_size = .7)+ geom_vline(xintercept=0)
dev.off()

# --------  Trace Plot  --------- #
pdf("traceplotTakeWood.pdf",  width = 7, height = 5) 
mcmcplots::traplot(b_TakeWood, plot.title = "Trace-Plot of TakeWood")
dev.off()

pdf("traceplotCutTree.pdf",  width = 7, height = 5) 
mcmcplots::traplot(b_CutTree, plot.title = "Trace-Plot of CutTree")
dev.off()

pdf("traceplotIntercept.pdf",  width = 7, height = 5) 
mcmcplots::traplot(intercept, plot.title = "Trace-Plot of Intercept")
dev.off()


# --------   Moving Average --------  # 
comulative_average = function(row) cumsum(row)/seq_along(row)
c_mean <- function(A) apply(A, 2, comulative_average)

b_TakeWood_cmean <- lapply(b_TakeWood, c_mean)
b_CutTree_cmean <- lapply(b_CutTree, c_mean)
intercept_cmean <- lapply(intercept, c_mean)

pdf("MovingAvgTakeWood.pdf", width = 7, height = 5) 
mcmcplots::traplot(b_TakeWood_cmean,
                   plot.title = "Comulative average of Takewood")
dev.off()

pdf("MovingAvgCutTree.pdf",  width = 7, height = 5) 
mcmcplots::traplot(b_CutTree_cmean, 
                   plot.title = "Comulative average of CutTree")
dev.off()

pdf("MovingAvgIntercept.pdf",  width = 7, height = 5) 
mcmcplots::traplot(intercept_cmean, 
                   plot.title = "Comulative average of intercept")
dev.off()


# -------- Interval ---------- #
pdf("intervalsTakeWood.pdf",  width = 7, height = 4) 
mcmc_intervals(b_TakeWood)
dev.off()

pdf("intervalsCutTree.pdf",  width = 7, height = 4) 
mcmc_intervals(b_CutTree)
dev.off()

pdf("intervalsIntercept.pdf",  width = 7, height = 4) 
mcmc_intervals(intercept)
dev.off()


# ---------- ACF -------------- # 
pdf("ACFTakeWood.pdf",  width = 7, height = 5.3) 
mcmc_acf(b_TakeWood)
dev.off()

pdf("ACFCutTree.pdf",  width = 7, height = 5.3) 
mcmc_acf(b_CutTree)
dev.off()

pdf("ACFIntercept.pdf",  width = 7, height = 5.3) 
mcmc_acf(intercept)
dev.off()

# --------- GELMAN PLOT ----------- # 
pdf("gelmanTakeWood.pdf",  width = 7, height = 5.3) 
gelman.plot(b_TakeWood)
dev.off()

pdf("gelmanCutTree.pdf",  width = 7, height = 5.3) 
gelman.plot(b_CutTree)
dev.off()

pdf("gelmanIntercept.pdf",  width = 7, height = 5.3) 
gelman.plot(intercept)
dev.off()


# ---------  The Geweke diagnostic ------- #
pdf("GewekeTakeWood.pdf", width = 7, height = 5.3) 
geweke.plot(b_TakeWood)
dev.off()

pdf("GewekeCutTree.pdf", width = 7, height = 5.3) 
geweke.plot(b_CutTree)
dev.off()

pdf("GewekeIntercept.pdf", width = 7, height = 5.3) 
geweke.plot(intercept)
dev.off()


# -------- Heidelberger & Welch ------- # 
print("TakeWood coefficients")
heidel.diag(b_TakeWood)

print("CutTree coefficients")
heidel.diag(b_CutTree)

print("Intercept coefficients")
heidel.diag(intercept)


# correlation matrix
corr_matrix <- rcorr(cbind(b_TakeWood[[1]], b_CutTree[[1]]))$r
corr_matrix %>%
  kbl(format = "latex", booktabs = T)  %>%
  kable_styling(bootstrap_options = "striped", latex_options=c("scale_down","HOLD_position"))


# log odds table
mean_b_Takewood <- t(MNL_summary[c(paste("b_TakeWood[", seq(1,4), "]", sep = "")),  "mean"])
rownames(mean_b_Takewood) <- "b_Takewood"
colnames(mean_b_Takewood) <- seq(1,4)

mean_b_CutTree <- t(MNL_summary[c(paste("b_CutTree[", seq(1,4), "]", sep = "")),  "mean"])
rownames(mean_b_CutTree) <- "b_CutTree"
colnames(mean_b_CutTree) <- seq(1,4)

mean_tab <- rbind(mean_b_Takewood,mean_b_CutTree)

df <- as.data.frame(mcmc_intervals_data(MNL.mcmc, prob = 0.90, point_est = "mean"))
rownames(df) <- df$parameter
df <- df[c(paste("b_TakeWood[", seq(1,4), "]", sep = ""),
           paste("b_CutTree[", seq(1,4), "]", sep = "")),
         c("ll","m" ,"hh")]
colnames(df) <- c("lower", "mean", "upper")
df %>%
  kbl(format = "latex", booktabs = T)  %>%
  kable_styling(bootstrap_options = "striped", latex_options="HOLD_position")


# risk
exp_mean_tab <- exp(mean_tab)
exp_mean_tab %>%
  kbl(format = "latex", booktabs = T)  %>%
  kable_styling(bootstrap_options = "striped", latex_options="HOLD_position")


# ------- Possible improvement and analysis -----------------

# plotting the regression coefficient 
par(mfrow = c(1,2))
plot(seq(1,4), mean_b_CutTree, pch = 20, col = 2, cex = 2, 
     xlab = "Category", ylab = "Average Beta CutTree")
lines(spline(1:4, mean_b_CutTree, n=1000), lwd=2, col = "blue")

plot(seq(1,4), mean_b_Takewood, pch = 20, col = 2, cex = 2, 
     xlab = "Category", ylab = "Average Beta TakeWood")
lines(spline(1:4, mean_b_Takewood, n=1000), lwd=2, col = "blue")

par(mfrow = c(1,1))

## --------- ADDING A SQUARE TERM IN TakeWood VARIABLE ---------- ##
# define the model for JAGS 
model.string = "
  model
	{
		for( i in 1:N ) {
			Anti_Anthro[i] ~ dnorm(mu[i], precision)
			mu[i] <- a_Anti_Anthro + b_TakeWood * TakeWood[i] + 
			              gamma_TakeWood * TakeWood[i]^2 +
			              b_CutTree * CutTree[i]
		} 
		
    # PRIOR
		a_Anti_Anthro ~ dnorm(0, 0.01)
		b_TakeWood ~ dnorm(0.0, 0.1)
		gamma_TakeWood ~ dnorm(0.0, 0.1)
		b_CutTree ~ dnorm(0.0, 0.1) 
		precision ~ dgamma(0.001, 0.001)
	}
	"

## write the model to a text file
writeLines(model.string, con = "model_jags_quadratic1.txt")

# list of parameters name
parameters <- c("a_Anti_Anthro",
                "b_TakeWood", "gamma_TakeWood",
                "b_CutTree")
# initial value 
inits <- list(a_Anti_Anthro = 3, b_TakeWood = 0,
              gamma_TakeWood = 0, b_CutTree = 0)
initial.values <- list(inits)

# MCMC with jags
set.seed(123) # set seed for reproducibility
model2 <- jags(data = data,
               inits = initial.values,
               parameters.to.save = parameters,
               model.file = "model_jags_quadratic1.txt",
               n.burnin = 2000, 
               n.chains = 1, n.thin = 1, 
               n.iter = 5000)

# look at the summary
modelSummary2 = model2$BUGSoutput$summary[c("a_Anti_Anthro", "b_CutTree",   
                                            "b_TakeWood", "gamma_TakeWood"), ]
modelSummary2 %>%
  kbl(format = "latex", booktabs = T)  %>%
  kable_styling(bootstrap_options = "striped", latex_options=c("scale_down","HOLD_position"))



## --------- ADDING A SQUARE TERM IN CutTree VARIABLE ---------- ##
# define the model for JAGS 
model.string = "
  model
	{
		for( i in 1:N ) {
			Anti_Anthro[i] ~ dnorm(mu[i], precision)
			mu[i] <- a_Anti_Anthro + b_TakeWood * TakeWood[i] + 
			              b_CutTree * CutTree[i] +
			              gamma_CutTree * CutTree[i]^2 
		} 
		
    # PRIOR
		a_Anti_Anthro ~ dnorm(0, 0.01)
		b_TakeWood ~ dnorm(0.0, 0.1)
		b_CutTree ~ dnorm(0.0, 0.1) 
		gamma_CutTree ~ dnorm(0.0, 0.1)
		precision ~ dgamma(0.001, 0.001)
	}
	"


## write the model to a text file
writeLines(model.string, con = "model_jags_quadratic2.txt")

# list of parameters name
parameters <- c("a_Anti_Anthro","b_TakeWood",
                "b_CutTree", "gamma_CutTree")
# initial value 
inits <- list(a_Anti_Anthro = 3, b_TakeWood = 0,
              gamma_CutTree = 0, b_CutTree = 0)
initial.values <- list(inits)


# MCMC with jags
set.seed(123) # set seed for reproducibility
model3 <- jags(data = data,
               inits = initial.values,
               parameters.to.save = parameters,
               model.file = "model_jags_quadratic2.txt",
               n.burnin = 2000, 
               n.chains = 1, n.thin = 1, 
               n.iter = 5000)

# look at the summary
modelSummary3 = model3$BUGSoutput$summary[c("a_Anti_Anthro", "b_CutTree",   
                                            "b_TakeWood", "gamma_CutTree"), ]

modelSummary3 %>%
  kbl(format = "latex", booktabs = T)  %>%
  kable_styling(bootstrap_options = "striped", latex_options=c("scale_down","HOLD_position"))


# compare the DIC score
# DIC SCORE
DIC_vec <- cbind(model1$BUGSoutput$DIC, model3$BUGSoutput$DIC, model2$BUGSoutput$DIC)
colnames(DIC_vec) <- c("Linear model","QuadraticTakeWood" ,"QuadraticCutTree")

DIC_vec %>%
  kbl(format = "latex", booktabs = T)



# ----------  PREDICTION  -------------------------------------------- 
par(mfrow = c(1,2))
plot(prop.table(table(MNL.mcmc[,"Anti_Anthro_new"][[1]])), type = "h", ylab = "Density", xlab = "Level of Disagreement", main = "First chain")
plot(prop.table(table(MNL.mcmc[,"Anti_Anthro_new"][[2]])), type = "h", ylab = "Density", xlab = "Level of Disagreement", main = "Second chain")
par(mfrow = c(1,1))


# --------- FREQUENTIST APPROCH --------------------------------------
# take a copy of the data
data <- dat

# factorize the response for the model 
data$Anti_Anthro <- as.factor(data$Anti_Anthro)
# impose as reference level the category 5
data$out <- relevel(data$Anti_Anthro,ref = "5")

# Train-Test split
set.seed(1234) # set seed for reproducibility
idx.tr = createDataPartition(y = data$out, 
                             p = .70, list = FALSE)

dat.tr = data[ idx.tr, ]
dat.te = data[-idx.tr, ]


# look at the class balance
table(data$Anti_Anthro)
table(dat.tr$Anti_Anthro)
table(dat.te$Anti_Anthro)


# Tuning parameter
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

set.seed(1234) # For reproducibility 
multinom_fit <- train(out ~ TakeWood + CutTree,
                      data = dat.tr, method = "multinom",
                      trControl  = trctrl,
                      tuneLength = 15
)

# Prediction 
test_pred <- predict(multinom_fit, newdata = dat.te[, -1])


# How Accurately our model is working?
CM_object <- confusionMatrix(test_pred, dat.te$out)
accuracy <- CM_object$overall[1]
CMatrix <- CM_object$table

print(accuracy)
print(CMatrix)

# prediction on new test point
TakeWood <- 1
CutTree <- 4
new.data <- data.frame(TakeWood, CutTree)

test_pred <- predict(multinom_fit, newdata = new.data, type="prob")

print(test_pred)  #2

# look at the regression coefficient
coeff <- coef(multinom_fit$finalModel)
print("Log-odd coeff")
t(coeff)

print("Exp coeff risk")
t(exp(coeff))
