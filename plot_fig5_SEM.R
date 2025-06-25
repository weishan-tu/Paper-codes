# load package
library(piecewiseSEM)
library(tidyverse)
library(lmerTest)       
library(bestNormalize)
library(inspectdf)
library(performance)
#load data
network_dat <- read_csv("network_data.csv")

#normalizing transformations
predictors_std <- network_dat
predictors_std$area <- scale2(log10(predictors_std$area))
predictors_std$isolation <- log10(predictors_std$isolation)
predictors_std$residence_time <- bestNormalize(predictors_std$residence_time)$x.t
predictors_std$bullfrog_abundance <- log(predictors_std$bullfrog_abundance+1)
predictors_std$native_abundance <- log(predictors_std$native_abundance  +1)
predictors_std$crayfish_abundance <- log(predictors_std$crayfish_abundance+1)
predictors_std$water_area <- bestNormalize(predictors_std$water_area)$x.t
predictors_std$human_activity <- bestNormalize(predictors_std$human_activity)$x.t
predictors_std$vegetation_coverage <- bestNormalize(predictors_std$vegetation_coverage)$x.t
predictors_std$times <- as.factor(predictors_std$times)
predictors_std$island <- as.factor(predictors_std$island)

##SEM
options(na.action = "na.omit")
mod1 <- psem(
  lmer(Connectance ~ residence_time + bullfrog_abundance + native_abundance + area + isolation + water_area + vegetation_coverage + human_activity + crayfish_abundance + (1|island) + (1|times),
       data = predictors_std, REML=TRUE,
       control=lmerControl(optimizer="optimx", optCtrl=list(method='nlminb'))),
  lmer(bullfrog_abundance ~ residence_time + (1|island) + (1|times),
       data = predictors_std, REML=TRUE,
       control=lmerControl(optimizer="optimx", optCtrl=list(method='nlminb'))))
summary(mod1, .progressBar = T) 
fisherC(mod1) # 
dSep(mod1)
plot(mod1)

sem_dt1 <- coefs(mod1)%>%
  select(-9)%>%
  filter(
    str_detect(Response, "~~", negate = TRUE) 
  )%>%
  dplyr::relocate(
    from = Predictor,
    to = Response,
    weight = Std.Estimate,
    p = P.Value
  )
write_excel_csv(sem_dt1,"out/sem_dt1.csv")

mod2 <- psem(
  lmer(Modularity  ~ residence_time + bullfrog_abundance + native_abundance + area + isolation + water_area + vegetation_coverage + human_activity + crayfish_abundance + (1|island) + (1|times),
       data = predictors_std, REML=TRUE,
       control=lmerControl(optimizer="optimx", optCtrl=list(method='nlminb'))),
  lmer(bullfrog_abundance ~ residence_time + (1|island) + (1|times),
       data = predictors_std, REML=TRUE,
       control=lmerControl(optimizer="optimx", optCtrl=list(method='nlminb'))))

fisherC(mod2) # 
dSep(mod2)
plot(mod2)

sem_dt2 <- coefs(mod2) |>
  select(-9)%>%
  filter(
    str_detect(Response, "~~", negate = TRUE) 
  )%>%
  dplyr::relocate(
    from = Predictor,
    to = Response,
    weight = Std.Estimate,
    p = P.Value
  )
write_excel_csv(sem_dt2,"out/sem_dt2.csv")

mod3 <- psem(
  lmer(Nstedness   ~ residence_time + bullfrog_abundance + native_abundance + area + isolation + water_area + vegetation_coverage + human_activity + crayfish_abundance + (1|island) + (1|times),
       data = predictors_std, REML=TRUE,
       control=lmerControl(optimizer="optimx", optCtrl=list(method='nlminb'))),
  lmer(bullfrog_abundance ~ residence_time + (1|island) + (1|times),
       data = predictors_std, REML=TRUE,
       control=lmerControl(optimizer="optimx", optCtrl=list(method='nlminb'))))

summary(mod3, .progressBar = T) 
fisherC(mod3) # 
dSep(mod3)
plot(mod3)

sem_dt3 <- coefs(mod3)%>%
  select(-9)%>%
  filter(
    str_detect(Response, "~~", negate = TRUE) 
  )%>%
  dplyr::relocate(
    from = Predictor,
    to = Response,
    weight = Std.Estimate,
    p = P.Value
  )
write_excel_csv(sem_dt3,"out/sem_dt3.csv")
# 

####
####SEM visualization using Adobe illustrator