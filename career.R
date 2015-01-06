
setwd("/Users/Qi/Documents/Programming/R/Employment")

dat <- read.table("career.txt", as.is = T, header = T)

names(dat) <- c("emp", "sex", "ed")

# keep only individuals with completed information on sex and education
dat <- dat[!is.na(dat$ed),]

# recode sex (male = 0; female = 1)
dat$sex <- dat$sex-1

# generate dummy variables for education
ed <- factor(dat$ed)

dummies <- model.matrix(~ed)

dat <- cbind(dat, as.data.frame(dummies)[,-1])

# education as categorical variable
fit <- glm(emp ~ sex + as.factor(ed), data = dat, family = binomial)
summary(fit)

loglikelihood <- function(parm){

	b0 <- parm[1]
	b1 <- parm[2]
	b2 <- parm[3]
	b3 <- parm[4]
	b4 <- parm[5]
	b5 <- parm[6]
	b6 <- parm[7]
	b7 <- parm[8]

	f <- b0 + dat$sex*b1 + dat$ed2*b2 + dat$ed3*b3 + dat$ed4*b4 +
	dat$ed5*b5 + dat$ed6*b6 + dat$ed7*b7

	h <- 1/(1+exp(-f))
	l <- as.numeric(dat$emp * log(h) + (1 - dat$emp) * log(1 - h))


	-sum(l)
}


start <- c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5)
llfit <- optim(start, loglikelihood, method = "BFGS", hessian = T)
llfit
se <- sqrt(diag(solve(llfit$hessian)))
se


# education as continuous variable
fit <- glm(emp ~ sex + ed, data = dat, family = binomial)
summary(fit)

loglikelihood <- function(parm){

	b0 <- parm[1]
	b1 <- parm[2]
	b2 <- parm[3]
	
	

	f <- b0 + dat$sex*b1 + dat$ed*b2


	h <- 1/(1+exp(-f))
	l <- as.numeric(dat$emp * log(h) + (1 - dat$emp) * log(1 - h))


	-sum(l)
}


start <- c(0.5, 0.5, 0.5)
llfit <- optim(start, loglikelihood, hessian = T)
llfit
se <- sqrt(diag(solve(llfit$hessian)))
se



