#Additive Causality Model

library(rstan)

real <- seq(from = -.2,to=.6,by=.1)
obs <- matrix(nrow = 100,ncol = 11)
for(i in 1:100){
  obs[i,10] <- 0
  for(j in 1:9){
    obs[i,j] <- floor(runif(1,0,1.99999999))
    obs[i,10] <- round(obs[i,10] + obs[i,j]*real[j],3)
  }
  if(obs[i,10] >= 1){
    obs[i,11] <- 1
  }else{
    obs[i,11] <- obs[i,10]
  }
}

d <- as.data.frame(obs)

N_cens <- sum(d$V11 == 1)
N_obs <- 100 - N_cens

d1 <- d[d$V11 < 1,]
d2 <- d[d$V11 >= 1,]

dstan <- list(N_obs = as.integer(N_obs),
              N_cens = N_cens,
              U = 1.0,
              I = 9,
              X1 = rbind(d1$V1,
              d1$V2,
              d1$V3,
              d1$V4,
              d1$V5,
              d1$V6,
              d1$V7,
              d1$V8,
              d1$V9),
              X2 = rbind(d2$V1,
                         d2$V2,
                         d2$V3,
                         d2$V4,
                         d2$V5,
                         d2$V6,
                         d2$V7,
                         d2$V8,
                         d2$V9))

#colnames(dstan) <- c("N","V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11")


stanmodelcode <- "
data{
  int I;
  int N_obs;
  int N_cens;
  matrix[I,N_obs] X1;
  matrix[I,N_cens] X2;
  real U;
}
parameters{
  vector[I] beta;
  real<upper=U> y_no[N_obs];
  real<lower=U> y_cens[N_cens];
  real<lower=0> sigma;
}
model{
  beta ~ normal(0.3,0.3);
  y_no ~ normal(X1' * beta, sigma);
  y_cens ~ normal(X2' * beta, sigma);
}
"
fit <- stan(model_code = stanmodelcode, model_name = "example", 
            data = dstan, iter = 2012, chains = 1,
            verbose = TRUE) 
plot(fit)

real <- seq(from = -.2,to=.6,by=.1)
obs <- matrix(nrow = 500,ncol = 11)
for(i in 1:500){
  obs[i,10] <- 0
  for(j in 1:9){
    obs[i,j] <- floor(runif(1,0,1.99999999))
    obs[i,10] <- round(obs[i,10] + obs[i,j]*real[j],3)
  }
  if(obs[i,10] >= 1){
    obs[i,11] <- 1
  }else{
    obs[i,11] <- obs[i,10]
  }
}

d <- as.data.frame(obs)

N_yes <- sum(d$V11 == 1)
N_no <- 500 - N_yes

d1 <- d[d$V11 < 1,]
d2 <- d[d$V11 >= 1,]

# model minus predictors 1 and 7

# if working correctly, this should find a latent protective factor and a latent risk factor

dstan2 <- list(N_no = as.integer(N_no), 
              N_yes = N_yes,
              U = 1.0,
              free_param = 2,
              I = 7,
              X1 = rbind(d1$V2,
                         d1$V3,
                         d1$V4,
                         d1$V5,
                         d1$V6,
                         d1$V8,
                         d1$V9),
              X2 = rbind(d2$V2,
                         d2$V3,
                         d2$V4,
                         d2$V5,
                         d2$V6,
                         d2$V8,
                         d2$V9))

stanmodelcode <- "
functions{
  real decision_function_lpdf(real x){
    return(log(-12 * (0.01 - 1) * x^2 + 12 * (0.01 - 1) * x + (3 - 2 * 0.01)));
  }
  real decision_function_lcdf(real x){
    return(log(-4 * (0.01 - 1)*x^3 + 6 * (0.01 - 1)*x^2 + (3-2*0.01)*x));
  }
}
data{
  int I;
  int N_no;
  int N_yes;
  int free_param;
  matrix[I,N_no] X1;
  matrix[I,N_yes] X2;
  real U;
}
parameters{
  vector[I] beta;
  vector[free_param] beta2;
  matrix <lower = 0, upper = 1>[free_param,N_no] X1_u;
  matrix <lower = 0, upper = 1>[free_param,N_yes] X2_u;
  real<upper=U> y_no[N_no];
  real<lower=U> y_yes[N_yes];
  real<lower=0> sigma;
}
model{
  beta ~ normal(0.3,0.3);
  beta2 ~ normal(0.3,0.3);
  for (i in 1:free_param){
    for (j in 1:N_no){
      X1_u[i,j] ~ decision_function();
    }
  }
  for (i in 1:free_param){
    for (j in 1:N_yes){
      X2_u[i,j] ~ decision_function();
    }
  }
  y_no ~ normal(X1' * beta + X1_u' * beta2, sigma);
  y_yes ~ normal(X2' * beta + X2_u' * beta2, sigma);
}
generated quantities{
  real mean1y;
  real mean1n;
  real mean2y;
  real mean2n;
  mean1n = mean(X1_u[1,]);
  mean1y = mean(X1_u[2,]);
  mean2n = mean(X2_u[1,]);
  mean2y = mean(X2_u[2,]);
}
"
fit2 <- stan(model_code = stanmodelcode, model_name = "example2", 
            data = dstan2, iter = 2012, chains = 1,
            verbose = TRUE) 
print(fit2)

plot(fit2)



