source("R/import and convert.R")
source("R/functions.R")
library(tidyverse)

mod <- wave2_data %>%
  select(D_EmergencyConSL_w2,
         D_EmergencyConPre_w2,
         weight2,
         D_Age5Cat_w2) %>%
  pivot_longer(1:2, names_to = "Cat", values_to = "Y_N") %>%
  mutate(Y_N = as.numeric(Y_N ==  "Yes")) %>%
  return_ORs(Y_N ~ Cat + D_Age5Cat_w2, weights = weight2)
glm(
  Y_N ~ Cat + D_Age5Cat_w2,
  data = .,
  family = "binomial",
  weights = weight2
)

mod %>%
  cbind(exp(coef(.)), exp(confint(.)))


wave2_data %>%
  select(D_EmergencyConSL_w2, D_EmergencyConPre_w2) %>%
  pivot_longer(1:2, names_to = "Cat", values_to = "Y_N") %>%
  mutate(Y_N = as.numeric(Y_N ==  "Yes")) %>%
  xtabs(data = .) %>%
  vcd::oddsratio(log = FALSE)

ec_dat <- wave2_data %>%
  select(D_EmergencyConSL_w2,
         D_EmergencyConPre_w2,
         weight2,
         D_Age5Cat_w2) %>%
  filter(!is.na(D_EmergencyConSL_w2), !is.na(D_EmergencyConPre_w2)) %>% 
  mutate(id = 1:n()) %>% 
  pivot_longer(1:2, names_to = "Cat", values_to = "Y_N") %>%
  mutate(Y_N = as.numeric(Y_N ==  "Yes"))

# bootstrapping etc. ------------------------------------------------------

mfxboot <- function(modform,dist,df, weights = NULL, boot=1000,digits=3){
  require(rlang)
  x <- eval_tidy(quo(glm(
    modform,
    data = df,
    family = binomial(dist),
    weights = !!substitute(weights)
  )), data = df)
  
  # x <- glm(modform, family=binomial(link=dist),data)
  # get marginal effects
  pdf <- ifelse(dist=="probit",
                mean(dnorm(predict(x, type = "link"))),
                mean(dlogis(predict(x, type = "link"))))
  marginal.effects <- pdf*coef(x)
  # start bootstrap
  bootvals <- matrix(rep(NA,boot*length(coef(x))), nrow=boot)
  set.seed(1111)
  for(i in 1:boot){
    samp1 <- df[sample(1:dim(df)[1],replace=T,dim(df)[1]),]
    x1 <- glm(modform, family=binomial(link=dist),samp1)
    pdf1 <- ifelse(dist=="probit",
                   mean(dnorm(predict(x1, type = "link"))),
                   mean(dlogis(predict(x1, type = "link"))))
    bootvals[i,] <- pdf1*coef(x1)
  }
  res <- cbind(marginal.effects,apply(bootvals,2,sd),marginal.effects/apply(bootvals,2,sd))
  if(names(x$coefficients[1])=="(Intercept)"){
    res1 <- res[2:nrow(res),]
    res2 <- matrix(as.numeric(sprintf(paste("%.",paste(digits,"f",sep=""),sep=""),res1)),nrow=dim(res1)[1])     
    rownames(res2) <- rownames(res1)
  } else {
    res2 <- matrix(as.numeric(sprintf(paste("%.",paste(digits,"f",sep=""),sep="")),nrow=dim(res)[1]))
    rownames(res2) <- rownames(res)
  }
  colnames(res2) <- c("marginal.effect","standard.error","z.ratio")  
  return(res2)
}




attempt_1 <- ec_dat %>% 
mfxboot(Y_N ~ Cat + D_Age5Cat_w2, "logit", df = ., weight2)

exp(attempt_1)


mod <- glm(
  Y_N ~ Cat,
  data = ec_dat,
  family = binomial(link = logit),
  weights = weight2)

exp(coef(mod))

pdf <- mean(dnorm(predict(mod, type = "link")))

marginal.effects <- pdf*coef(mod)

library(rsample)

plan(multicore)



mods <- ec_dat %>% 
  bootstraps(10000, strata = Cat) %>% 
  # rowwise() %>% 
  mutate(model = future_map(splits, function(x) {
    as.data.frame(x) %>% 
      glm(
        Y_N ~ Cat + D_Age5Cat_w2,
        data = .,
        family = binomial(link = logit),
        weights = weight2)
  })) 

mods %>% 
  select(-splits) %>% 
  rowwise() %>% 
  mutate(lnor = model$coefficients[2]) %>% 
  ggplot(aes(lnor)) +
  geom_histogram()

mods %>% 
  select(-splits) %>% 
  rowwise() %>% 
  mutate(lnor = model$coefficients[2]) %>% 
  ungroup() %>% pull(lnor) %>% quantile(c(0.025, 0.5, 0.975)) %>% exp
  
  
boot3 <- mods %>% 
  select(-splits) %>% 
  rowwise() %>% 
  mutate(lnor = model$coefficients[2]) %>% 
  ungroup() 


boot1 %>% 
  summarise(mean_or = median(lnor),
            sd_or = sd(lnor)) %>% 
  mutate(or = exp(mean_or),
         ll = exp(qnorm(0.025, mean_or, sd_or)),
         ul = exp(qnorm(0.975, mean_or, sd_or)))

boot1 %>% pull(lnor) %>% quantile(c(0.025, 0.5, 0.975)) %>% exp
boot2 %>% pull(lnor) %>% quantile(c(0.025, 0.5, 0.975)) %>% exp
boot3 %>% pull(lnor) %>% quantile(c(0.025, 0.5, 0.975)) %>% exp


# conditional logit -------------------------------------------------------
library(cond)
library(survival)

mod <- clogit(
  Y_N ~ Cat + strata(id),
  data = ec_dat,
  # weights = weight2,
  method = "approximate")

cbind(exp(coef(mod)), exp(confint(mod))) %>% 
  round(3)
     


Epi::clogistic(
  Y_N ~ Cat,
  strata = id,
  data = ec_dat) %T>% {coef(.) %>% exp %>% print} %>% confint(.) %>% exp


ec_dat2 <- wave2_data %>%
  select(D_EmergencyConSL_w2,
         D_EmergencyConPre_w2,
         weight2,
         D_Age5Cat_w2) %>%
  filter(!is.na(D_EmergencyConSL_w2), !is.na(D_EmergencyConPre_w2),
         D_EmergencyConSL_w2 != D_EmergencyConPre_w2) %>% 
  mutate(id = 1:n()) %>% 
  pivot_longer(1:2, names_to = "Cat", values_to = "Y_N") %>%
  mutate(Y_N = as.numeric(Y_N ==  "Yes"))


mod <- gee::gee(Y_N ~ Cat, data = ec_dat, family = binomial, id = id)

exp(mod$coefficients)
summary(mod)
confint(mod)
class(mod)
