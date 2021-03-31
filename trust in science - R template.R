
# Loading packages and data (includes saved location-scale results) -------------------------------------------

#When working with new version of R, need to follow instructions for installation... 
# https://github.com/stan-dev/rstan/wiki/Installing-RStan-from-source-on-a-Mac  for instructions on install Rstan... 

remove.packages("rstan")
if (file.exists(".RData")) file.remove(".RData")

install.packages("Rcpp", repos = "https://rcppcore.github.io/drat")

library(effects)
library(tidyverse)
library(haven)
library(readxl)
library(httr)
library(stargazer)
library(modelr)
library(lme4)
library(rstan)
library(brms)

load("~/Dropbox/Academic Articles/Vaccine coverage - WGP/Trust in science - residuals/Country_residuals_trust.RData")
setwd("~/Dropbox/Academic Articles/Vaccine coverage - WGP/Trust in science")
wellcome_trust <- read_dta("Wellcome_trust_science.dta")


# Estimating location-scale model (Table 1 - models 1-3)-----------------------------------------

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#Table 1: model 1
m0_IRTlong <- brm(bf(trust_science2 ~ 1  + (1 |s| countrynew), sigma ~ 1 + (1 |s| countrynew)),
                 data = wellcome_trust,
                 family = gaussian(),
                 chains = 4, 
                 iter = 20000)
summary(m0_IRTlong)

#Adding predictors to the location and scale - GDP, Gini and HLO

##WBI data (subset of all variables)
WBI<- read_dta("Country datasets/WBI_v1.dta")

Gini <- read_excel("Country datasets/Excel_data/inequality_index_gini.xlsx")%>%
  pivot_longer(c(`1967`:`2018`), names_to = "year", values_to = "gini")%>%
  group_by(country)%>%
  fill(gini)%>%
  ungroup()%>%
  filter(year==2018)

Gini$country[Gini$country=="Congo, Dem. Rep."] <- "Republic of Congo"
Gini$country[Gini$country=="Cote d'Ivoire"] <- "Ivory Coast"
Gini$country[Gini$country=="Egypt, Arab Rep."] <- "Egypt"
Gini$country[Gini$country=="Kyrgyz Republic"] <- "Kyrgyzstan"
Gini$country[Gini$country=="Lao"] <- "Laos"
Gini$country[Gini$country=="Slovak Republic"] <- "Slovakia"
Gini$country[Gini$country=="United Kingdom"] <- "UK"
Gini$country[Gini$country=="United States"] <- "USA"

HLO <- read_dta("Country datasets/HLO.dta")
HLO$countrynew[HLO$countrynew=="Congo, Dem. Rep."] <- "Republic of Congo"
HLO$countrynew[HLO$countrynew=="Cote d'Ivoire"] <- "Ivory Coast"
HLO$countrynew[HLO$countrynew=="Egypt, Arab Rep."] <- "Egypt"
HLO$countrynew[HLO$countrynew=="Eswatini"] <- "Swaziland"
HLO$countrynew[HLO$countrynew=="Gambia, The"] <- "Gambia"
HLO$countrynew[HLO$countrynew=="Iran, Islamic Rep."] <- "Iran"
HLO$countrynew[HLO$countrynew=="Korea, Rep."] <- "South Korea"
HLO$countrynew[HLO$countrynew=="Kyrgyz Republic"] <- "Kyrgyzstan"
HLO$countrynew[HLO$countrynew=="Lao PDR"] <- "Laos"
HLO$countrynew[HLO$countrynew=="Macedonia, FYR"] <- "Macedonia"
HLO$countrynew[HLO$countrynew=="Russian Federation"] <- "Russia"
HLO$countrynew[HLO$countrynew=="Slovak Republic"] <- "Slovakia"
HLO$countrynew[HLO$countrynew=="United Kingdom"] <- "UK"
HLO$countrynew[HLO$countrynew=="United States"] <- "USA"
HLO$countrynew[HLO$countrynew=="Yemen, Rep."] <- "Yemen"

wellcome.country <-merge(wellcome_trust, WBI, by.x = "countrynew", by.y = "countrynew", all.x=TRUE, all.y=TRUE)
wellcome.country <-merge(wellcome.country, HLO, by.x = "countrynew", by.y = "countrynew", all.x=TRUE, all.y=TRUE)
wellcome.country <-merge(wellcome.country, Gini, by.x = "countrynew", by.y = "country", all.x=TRUE, all.y=TRUE)

#Checking the variables
m.1 <- lmer(trust_science2 ~ 1 + (1 | countrynew), data = wellcome.country)
m.2 <- lmer(trust_science2 ~ HLO + (1 | countrynew), data = wellcome.country)
m.3 <- lmer(trust_science2 ~ gdppercapita_2017 + (1 | countrynew), data = wellcome.country)
m.4 <- lmer(trust_science2 ~ gini + (1 | countrynew), data = wellcome.country)
m.5 <- lmer(trust_science2 ~ HLO + gdppercapita_2017 + gini + (1 | countrynew), data = wellcome.country)
stargazer(m.1, m.2, m.3, m.4, m.5, type="text")

summary(wellcome.country$gdppercapita_2017)
summary(wellcome.country$gini)
summary(wellcome.country$HLO)

m1_IRTlong <- brm(bf(trust_science2 ~ 1  + scale(gdppercapita_2017) + scale(HLO) + (1 |s| countrynew), sigma ~ 1 + scale(gdppercapita_2017) + scale(HLO) + (1 |s| countrynew)),
                  data = wellcome.country,
                  family = gaussian(),
                  chains = 4, 
                  iter = 20000)
summary(m1_IRTlong)

#Table 1: Model 2
m2_IRTlong <- brm(bf(trust_science2 ~ 1  + scale(gdppercapita_2017) + scale(HLO) + scale(gini) + (1 |s| countrynew), sigma ~ 1 + scale(gdppercapita_2017) + scale(HLO) + scale(gini) + (1 |s| countrynew)),
                  data = wellcome.country,
                  family = gaussian(),
                  chains = 4, 
                  iter = 20000)
summary(m2_IRTlong)

#Table 1: Model 3
m3_IRTlong <- brm(bf(trust_science2 ~ 1  + scale(gdppercapita_2017) + scale(HLO) + scale(gini) + male + I(age_c/10) + ed_mid + ed_high + scale(lg_inc2, center=TRUE) + (1 |s| countrynew), sigma ~ 1 + scale(gdppercapita_2017) + scale(HLO) + scale(gini) + male + I(age_c/10) + ed_mid + ed_high + scale(lg_inc2, center=TRUE) + (1 |s| countrynew)),
                  data = wellcome.country,
                  family = gaussian(),
                  chains = 4, 
                  iter = 20000)
summary(m3_IRTlong)

m4_IRTlong <- brm(bf(trust_science2 ~ 1  + scale(gdppercapita_2017) + scale(HLO) + scale(gini) + male + I(age_c/10) + scale(lg_inc2, center=TRUE) + (1 |s| countrynew), sigma ~ 1 + scale(gdppercapita_2017) + scale(HLO) + scale(gini) + male + I(age_c/10) + scale(lg_inc2, center=TRUE) + (1 |s| countrynew)),
                  data = wellcome.country,
                  family = gaussian(),
                  chains = 4, 
                  iter = 20000)
summary(m4_IRTlong)


# Extracting the location-scale residuals ---------------------------------------

#Residuals on a meaninfgul scale - mean and SD
effects0<-coef(m0_IRTlong)
effects0<-as.data.frame(effects0)
effects0$Country <- rownames(effects0)
effects0$trust_mean <- effects0$countrynew.Estimate.Intercept
effects0$vac_m2.5 <- effects0$countrynew.Q2.5.Intercept
effects0$vac_m97.5 <- effects0$countrynew.Q97.5.Intercept
effects0$trust_sd <- exp(effects0$countrynew.Estimate.sigma_Intercept)
effects0$vac_sd2.5 <- exp(effects0$countrynew.Q2.5.sigma_Intercept)
effects0$vac_sd97.5 <- exp(effects0$countrynew.Q97.5.sigma_Intercept)

#Residuals on standardized scale - used for current analysis
effects1<-ranef(m0_IRTlong)
effects1<-as.data.frame(effects1)
effects1$Country <- rownames(effects1)
effects1$trust_u0 <- effects1$countrynew.Estimate.Intercept
effects1$vac_m2.5 <- effects1$countrynew.Q2.5.Intercept
effects1$vac_m97.5 <- effects1$countrynew.Q97.5.Intercept
effects1$trust_u1 <- effects1$countrynew.Estimate.sigma_Intercept
effects1$vac_sd2.5 <- effects1$countrynew.Q2.5.sigma_Intercept
effects1$vac_sd97.5 <- effects1$countrynew.Q97.5.sigma_Intercept

#To just keep residuals
effects0 <- effects0 %>%
  select(Country,trust_mean, trust_sd)

effects1 <- effects1 %>%
  select(Country, trust_u0, trust_u1)
  

# APPENDIX FIGURE 1 and 2: Plotting the residuals - raw and SD scale (empty model) -------------------------------

#Appendix figure 2: Meaningful scale - mean and SD
effects0 <- effects0 %>% 
  mutate(ranking_m = rank(trust_mean, ties.method = 'first'),
         ranking_sd = rank(trust_sd, ties.method = 'first'), 
         mean_sd = mean(trust_sd))

plot(effects0$ranking_m, effects0$trust_mean, type = "n", xlab = "Country (ranked)", ylab = "conditional modes of r.e. for mean trust in science")
segments(effects0$ranking_m, effects0$vac_m2.5, effects0$ranking_m, effects0$vac_m97.5)
points(effects0$ranking_m, effects0$trust_mean, col = "blue")
abline(h = 0, col = "red")

#Figure 2
plot(effects0$ranking_sd, effects0$trust_sd, type = "n", xlab = "Country (ranked)", ylab = "conditional modes of r.e. for SD trust in science")
segments(effects0$ranking_sd, effects0$vac_sd2.5, effects0$ranking_sd, effects0$vac_sd97.5)
points(effects0$ranking_sd, effects0$trust_sd, col = "blue")
abline(h = effects0$mean_sd, col = "red")

ggplot(data = effects0, mapping = aes(x = trust_mean, y = trust_sd)) + 
  geom_point(na.rm = TRUE)

#Residual scale
effects1 <- effects1 %>% 
  mutate(ranking_m = rank(trust_u0, ties.method = 'first'),
         ranking_sd = rank(trust_u1, ties.method = 'first'))

plot(effects1$ranking_m, effects1$trust_u0, type = "n", xlab = "Country (ranked)", ylab = "conditional modes of r.e. for mean trust in science")
segments(effects1$ranking_m, effects1$vac_m2.5, effects1$ranking_m, effects1$vac_m97.5)
points(effects1$ranking_m, effects1$trust_u0, col = "blue")
abline(h = 0, col = "red")

plot(effects1$ranking_sd, effects1$trust_u1, type = "n", xlab = "Country (ranked)", ylab = "conditional modes of r.e. for SD trust in science")
segments(effects1$ranking_sd, effects1$vac_sd2.5, effects1$ranking_sd, effects1$vac_sd97.5)
points(effects1$ranking_sd, effects1$trust_u1, col = "blue")
abline(h = 0, col = "red")

ggplot(data = effects1, mapping = aes(x = trust_u0, y = trust_u1)) + 
  geom_point(na.rm = TRUE)

#Mapping the residuals
WorldData <- map_data('world')
WorldData %>% filter(region != "Antarctica") -> WorldData
WorldData <- fortify(WorldData)

#Appendix Figure 1
p <- ggplot()
p <- p + geom_map(data=WorldData, map=WorldData,
                  aes(x=long, y=lat, group=group, map_id=region),
                  fill="gray80", colour="#7f7f7f", size=0.2) 
p <- p +  geom_map(data=effects1, map=WorldData,
                   aes(fill=trust_u0, map_id=Country),
                   colour="#7f7f7f", size=0.2) #Adds in the data with features of countries [fill is the characteristics
p <- p + scale_y_continuous(breaks=c()) #removes lattitute values
p <- p + scale_x_continuous(breaks=c()) #removes longitude values
p <- p + labs(fill="u0", title="Trust in science (mean)", x="", y="") #removes x and y titles, adds title for legend and overall title
p <- p +   coord_quickmap()
p

p <- ggplot()
p <- p + geom_map(data=WorldData, map=WorldData,
                  aes(x=long, y=lat, group=group, map_id=region),
                  fill="gray80", colour="#7f7f7f", size=0.2) 
p <- p +  geom_map(data=effects1, map=WorldData,
                   aes(fill=trust_u1, map_id=Country),
                   colour="#7f7f7f", size=0.2) #Adds in the data with features of countries [fill is the characteristics
p <- p + scale_y_continuous(breaks=c()) #removes lattitute values
p <- p + scale_x_continuous(breaks=c()) #removes longitude values
p <- p + labs(fill="u1", title="Trust in science (consensus)", x="", y="") #removes x and y titles, adds title for legend and overall title
p <- p +   coord_quickmap()
p



# FINAL EMPIRICAL MODELS (VACCINE SUPPORT): Extracted country residuals with controls for HLO and Gini (table 1, model 2) --------------------------------------------

effects2<-ranef(m2_IRTlong)
effects2<-as.data.frame(effects2)
effects2$Country <- rownames(effects2)
effects2$trust_u0 <- effects2$countrynew.Estimate.Intercept
effects2$vac_m2.5 <- effects2$countrynew.Q2.5.Intercept
effects2$vac_m97.5 <- effects2$countrynew.Q97.5.Intercept
effects2$trust_u1 <- effects2$countrynew.Estimate.sigma_Intercept
effects2$vac_sd2.5 <- effects2$countrynew.Q2.5.sigma_Intercept
effects2$vac_sd97.5 <- effects2$countrynew.Q97.5.sigma_Intercept
effects2 <- effects2 %>%
  select(Country, trust_u0, trust_u1)


ggplot(data = effects2, mapping = aes(x = trust_u0)) + 
  geom_histogram(bins = 30, color = "black", fill= "white", na.rm=TRUE)

ggplot(data = effects2, mapping = aes(x = trust_u1)) + 
  geom_histogram(bins = 30, color = "black", fill= "white", na.rm=TRUE)

effects2 <- effects2 %>%
  select(Country, trust_u0, trust_u1)

#Merging data with individual data
merged.wellcome <-merge(wellcome_trust, effects2, by.x = "countrynew", by.y = "Country", all.x=TRUE, all.y=TRUE)


# Figure 1 (and appendix table 1) -----------------------------------------

#Appendix table 1, model 1
lm.1 <- lmer(vaccine_IRT ~ scale(trust_u0)*scale(trust_u1) + (1 | countrynew), data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
summary(lm.1) 
lm.1a <- lmer(vaccine_IRT ~ trust_u0*trust_u1 + (1 | countrynew), data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
summary(lm.1a) 

#Figure 1 (top) Interaction plot - scale selected given the range of u1
eff.picked <- effect("trust_u0*trust_u1", lm.1a,
                         xlevels=list(trust_u1 = c(-.2, -.1, 0, .1, .2)),
                         se=TRUE, confidence.level=.95, typical=mean)
plot(eff.picked, xlab = "Trust Mean", ylab = "Marginal Effects on Vaccines support")

#Appendix table 1, model 2 
lm.2 <- lmer(vaccine_IRT ~ scale(trust_u0)*scale(trust_u1) + scale(trust_science2)*scale(trust_u1) + (1 | countrynew), data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
summary(lm.2) 
lm.2a <- lmer(vaccine_IRT ~ trust_science2 + trust_science2*trust_u1 + trust_u0*trust_u1 + (1 | countrynew), data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
summary(lm.2a) 

#Figure 1 (bottom) Interaction plot - scale selected given the range of u1
eff.picked2 <- effect("trust_science2*trust_u1", lm.2a,
                     xlevels=list(trust_u1 = c(-.2, -.1, 0, .1, .2)),
                     se=TRUE, confidence.level=.95, typical=mean)
plot(eff.picked2, xlab = "Individual trust", ylab = "Marginal Effects on Vaccines support")


# Appendix table 2 (and figure 4) -----------------------------------------

#Vaccines important for children
bin.0 <- glmer(vacCHILD_bin ~ scale(trust_u0) + scale(trust_u1) + (1 | countrynew), family = binomial("logit"), nAGQ = 15, data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
summary(bin.0) 
bin.1 <- glmer(vacCHILD_bin ~ scale(trust_u0)*scale(trust_u1) + (1 | countrynew), family = binomial("logit"), nAGQ = 15, data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
summary(bin.1) 

#Appendix Table 2, model 1
bin.1a <- glmer(vacCHILD_bin ~ trust_u0*trust_u1 + (1 | countrynew), family = binomial("logit"), nAGQ = 15, data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
summary(bin.1a) 

#Figure 4 (top)
eff.bin <- effect("trust_u0*trust_u1", bin.1a, 
                     xlevels=list(trust_u1 = c(-.2, -.1, 0, .1, .2)),
                     se=TRUE, confidence.level=.95, typical=mean)
plot(eff.bin, xlab = "Trust Mean", ylab = "Marginal Effects on Vaccines important for children")


#Vaccines are safe
bin.0 <- glmer(vacSAFE_bin ~ scale(trust_u0) + scale(trust_u1) + (1 | countrynew), family = binomial("logit"), nAGQ = 15, data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
summary(bin.0) 
bin.1 <- glmer(vacSAFE_bin ~ scale(trust_u0)*scale(trust_u1) + (1 | countrynew), family = binomial("logit"), nAGQ = 15, data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
summary(bin.1) 

#Appendix Table 2, model 2
bin.1a <- glmer(vacSAFE_bin ~ trust_u0*trust_u1 + (1 | countrynew), family = binomial("logit"), nAGQ = 15, data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
summary(bin.1a) 

#Figure 4 (middle)
eff.bin <- effect("trust_u0*trust_u1", bin.1a,
                  xlevels=list(trust_u1 = c(-.2, -.1, 0, .1, .2)),
                  se=TRUE, confidence.level=.95, typical=mean)
plot(eff.bin, xlab = "Trust Mean", ylab = "Marginal Effects on Vaccines are safe")


#Vaccines are effective
bin.0 <- glmer(vacEFFECT_bin ~ scale(trust_u0) + scale(trust_u1) + (1 | countrynew), family = binomial("logit"), nAGQ = 15, data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
summary(bin.0) 
bin.1 <- glmer(vacEFFECT_bin ~ scale(trust_u0)*scale(trust_u1) + (1 | countrynew), family = binomial("logit"), nAGQ = 15, data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
summary(bin.1) 

#Appendix Table 2, model 3
bin.1a <- glmer(vacEFFECT_bin ~ trust_u0*trust_u1 + (1 | countrynew), family = binomial("logit"), nAGQ = 15, data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
summary(bin.1a) 

#Figure 4 (bottom)
eff.bin <- effect("trust_u0*trust_u1", bin.1a,
                  xlevels=list(trust_u1 = c(-.2, -.1, 0, .1, .2)),
                  se=TRUE, confidence.level=.95, typical=mean)
plot(eff.bin, xlab = "Trust Mean", ylab = "Marginal Effects on Vaccines are effective")


# Appendix table 3 (and figure 5) -----------------------------------------

merged.wellcome2 <-merge(wellcome_trust, effects1, by.x = "countrynew", by.y = "Country", all.x=TRUE, all.y=TRUE)

#Appendix Table 3, model 1
bin.1a <- glmer(vacCHILD_bin ~ trust_u0*trust_u1 + (1 | countrynew), family = binomial("logit"), nAGQ = 15, data = merged.wellcome2) #SD has no obvious 0... [So might be better on residual scale]
summary(bin.1a) 

#Figure 5 (top)
eff.bin <- effect("trust_u0*trust_u1", bin.1a, 
                  xlevels=list(trust_u1 = c(-.2, -.1, 0, .1, .2)),
                  se=TRUE, confidence.level=.95, typical=mean)
plot(eff.bin, xlab = "Trust Mean", ylab = "Marginal Effects on Vaccines important for children")


#Appendix Table 3, model 2
bin.1a <- glmer(vacSAFE_bin ~ trust_u0*trust_u1 + (1 | countrynew), family = binomial("logit"), nAGQ = 15, data = merged.wellcome2) #SD has no obvious 0... [So might be better on residual scale]
summary(bin.1a) 

#Figure 5 (middle)
eff.bin <- effect("trust_u0*trust_u1", bin.1a,
                  xlevels=list(trust_u1 = c(-.2, -.1, 0, .1, .2)),
                  se=TRUE, confidence.level=.95, typical=mean)
plot(eff.bin, xlab = "Trust Mean", ylab = "Marginal Effects on Vaccines are safe")

#Appendix Table 3, model 3
bin.1a <- glmer(vacEFFECT_bin ~ trust_u0*trust_u1 + (1 | countrynew), family = binomial("logit"), nAGQ = 15, data = merged.wellcome2) #SD has no obvious 0... [So might be better on residual scale]
summary(bin.1a) 

#Figure 5 (bottom)
eff.bin <- effect("trust_u0*trust_u1", bin.1a,
                  xlevels=list(trust_u1 = c(-.2, -.1, 0, .1, .2)),
                  se=TRUE, confidence.level=.95, typical=mean)
plot(eff.bin, xlab = "Trust Mean", ylab = "Marginal Effects on Vaccines are effective")



# Exporting data for analysis in Stata ------------------------------------

write_dta(merged.wellcome, "wellcome_merge.dta")




# Revised models - for PNAS v3 --------------------------------------------
#Load in data (wellcome trust) and country info (WBI, HLO and Gini)

effects1<-ranef(m0_IRTlong)
effects1<-as.data.frame(effects1)
effects1$Country <- rownames(effects1)
effects1$trust_u0 <- effects1$countrynew.Estimate.Intercept
effects1$vac_m2.5 <- effects1$countrynew.Q2.5.Intercept
effects1$vac_m97.5 <- effects1$countrynew.Q97.5.Intercept
effects1$trust_u1 <- effects1$countrynew.Estimate.sigma_Intercept
effects1$vac_sd2.5 <- effects1$countrynew.Q2.5.sigma_Intercept
effects1$vac_sd97.5 <- effects1$countrynew.Q97.5.sigma_Intercept
effects1 <- effects1 %>%
  select(Country, trust_u0, trust_u1)


effects2<-ranef(m2_IRTlong)
effects2<-as.data.frame(effects2)
effects2$Country <- rownames(effects2)
effects2$trust_u0 <- effects2$countrynew.Estimate.Intercept
effects2$vac_m2.5 <- effects2$countrynew.Q2.5.Intercept
effects2$vac_m97.5 <- effects2$countrynew.Q97.5.Intercept
effects2$trust_u1 <- effects2$countrynew.Estimate.sigma_Intercept
effects2$vac_sd2.5 <- effects2$countrynew.Q2.5.sigma_Intercept
effects2$vac_sd97.5 <- effects2$countrynew.Q97.5.sigma_Intercept
effects2 <- effects2 %>%
  select(Country, trust_u0, trust_u1)

effects3<-coef(m3_IRTlong)
effects3<-as.data.frame(effects3)
effects3$Country <- rownames(effects3)
effects3$trust_mean <- effects3$countrynew.Estimate.Intercept
effects3$vac_m2.5 <- effects3$countrynew.Q2.5.Intercept
effects3$vac_m97.5 <- effects3$countrynew.Q97.5.Intercept
effects3$trust_sd <- exp(effects3$countrynew.Estimate.sigma_Intercept)
effects3$vac_sd2.5 <- exp(effects3$countrynew.Q2.5.sigma_Intercept)
effects3$vac_sd97.5 <- exp(effects3$countrynew.Q97.5.sigma_Intercept)
effects3 <- effects3 %>%
  select(Country, trust_mean, trust_sd)


effects4<-ranef(m3_IRTlong)
effects4<-as.data.frame(effects4)
effects4$Country <- rownames(effects4)
effects4$trust_u0 <- effects4$countrynew.Estimate.Intercept
effects4$vac_m2.5 <- effects4$countrynew.Q2.5.Intercept
effects4$vac_m97.5 <- effects4$countrynew.Q97.5.Intercept
effects4$trust_u1 <- effects4$countrynew.Estimate.sigma_Intercept
effects4$vac_sd2.5 <- effects4$countrynew.Q2.5.sigma_Intercept
effects4$vac_sd97.5 <- effects4$countrynew.Q97.5.sigma_Intercept
effects4 <- effects4 %>%
  select(Country, trust_u0, trust_u1)


effects5<-ranef(m3_IRTimp)
effects5<-as.data.frame(effects5)
effects5$Country <- rownames(effects5)
effects5$trust_u0 <- effects5$countrynew.Estimate.Intercept
effects5$vac_m2.5 <- effects5$countrynew.Q2.5.Intercept
effects5$vac_m97.5 <- effects5$countrynew.Q97.5.Intercept
effects5$trust_u1 <- effects5$countrynew.Estimate.sigma_Intercept
effects5$vac_sd2.5 <- effects5$countrynew.Q2.5.sigma_Intercept
effects5$vac_sd97.5 <- effects5$countrynew.Q97.5.sigma_Intercept
effects5 <- effects5 %>%
  select(Country, trust_u0, trust_u1)


effects6<-ranef(m3_IRTmiss)
effects6<-as.data.frame(effects6)
effects6$Country <- rownames(effects6)
effects6$trust_u0 <- effects6$countrynew.Estimate.Intercept
effects6$vac_m2.5 <- effects6$countrynew.Q2.5.Intercept
effects6$vac_m97.5 <- effects6$countrynew.Q97.5.Intercept
effects6$trust_u1 <- effects6$countrynew.Estimate.sigma_Intercept
effects6$vac_sd2.5 <- effects6$countrynew.Q2.5.sigma_Intercept
effects6$vac_sd97.5 <- effects6$countrynew.Q97.5.sigma_Intercept
effects6 <- effects6 %>%
  select(Country, trust_u0, trust_u1)

effects7<-ranef(m3_IRTlong.gm)
effects7<-as.data.frame(effects7)
effects7$Country <- rownames(effects7)
effects7$trust_u0 <- effects7$countrynew.Estimate.Intercept
effects7$vac_m2.5 <- effects7$countrynew.Q2.5.Intercept
effects7$vac_m97.5 <- effects7$countrynew.Q97.5.Intercept
effects7$trust_u1 <- effects7$countrynew.Estimate.sigma_Intercept
effects7$vac_sd2.5 <- effects7$countrynew.Q2.5.sigma_Intercept
effects7$vac_sd97.5 <- effects7$countrynew.Q97.5.sigma_Intercept
effects7 <- effects7 %>%
  select(Country, trust_u0, trust_u1)

effects7$trust_u1_5 <-ntile(effects7$trust_u1, 5)

ggplot(data = effects4, mapping = aes(x = trust_u0)) + 
  geom_histogram(bins = 30, color = "black", fill= "white", na.rm=TRUE)

ggplot(data = effects4, mapping = aes(x = trust_u1)) + 
  geom_histogram(bins = 30, color = "black", fill= "white", na.rm=TRUE)

ggplot(data = effects7, mapping = aes(x = trust_u1)) + 
  geom_histogram(bins = 30, color = "black", fill= "white", na.rm=TRUE)

effects7 %>%
  group_by(trust_u1_5) %>%
  summarise(
    trust_var = mean(trust_u1, na.rm=TRUE)
    )

effects8<-coef(m3_IRTlong.gm)
effects8<-as.data.frame(effects8)
effects8$Country <- rownames(effects8)
effects8$trust_mean <- effects8$countrynew.Estimate.Intercept
effects8$vac_m2.5 <- effects8$countrynew.Q2.5.Intercept
effects8$vac_m97.5 <- effects8$countrynew.Q97.5.Intercept
effects8$trust_sd <- exp(effects8$countrynew.Estimate.sigma_Intercept)
effects8$vac_sd2.5 <- exp(effects8$countrynew.Q2.5.sigma_Intercept)
effects8$vac_sd97.5 <- exp(effects8$countrynew.Q97.5.sigma_Intercept)
effects8 <- effects8 %>%
  select(Country, trust_mean, trust_sd, vac_sd2.5, vac_sd97.5)

#Appendix figure 2: Meaningful scale - mean and SD
effects8 <- effects8 %>% 
  mutate(ranking_m = rank(trust_mean, ties.method = 'first'),
         ranking_sd = rank(trust_sd, ties.method = 'first'), 
         mean_sd = mean(trust_sd))

plot(effects8$ranking_sd, effects8$trust_sd, type = "n", xlab = "Countries (ranked)", ylab = "Strength of social norm of trust in science")
segments(effects8$ranking_sd, effects8$vac_sd2.5, effects8$ranking_sd, effects8$vac_sd97.5)
points(effects8$ranking_sd, effects8$trust_sd, col = "blue")
abline(h = effects8$mean_sd, col = "red")

wellcome.country.red <- filter(wellcome.country, countrynew != "Czech Republic") 
lm.1b <- lmer(vaccine_IRT ~ trust_science2 + trust_u0*trust_u1 + trust_science2*trust_u1 + (1 | countrynew), data = wellcome.country.red) #SD has no obvious 0... [So might be better on residual scale]
summary(lm.1b) 




#Merging data with individual data - using origina version of residuals
wellcome.country <-merge(wellcome_trust, WBI, by.x = "countrynew", by.y = "countrynew", all.x=TRUE, all.y=TRUE)
wellcome.country <-merge(wellcome.country, HLO, by.x = "countrynew", by.y = "countrynew", all.x=TRUE, all.y=TRUE)
wellcome.country <-merge(wellcome.country, Gini, by.x = "countrynew", by.y = "country", all.x=TRUE, all.y=TRUE)
wellcome.country <-merge(wellcome.country, effects2, by.x = "countrynew", by.y = "Country", all.x=TRUE, all.y=TRUE)


lm.0 <- lmer(vaccine_IRT ~ scale(trust_u0) + scale(trust_science2) + scale(gdppercapita_2017) + scale(HLO) + scale(gini) + male + I(age_c/10) + ed_mid + ed_high + scale(lg_inc2, center=TRUE) + (1 | countrynew), data = wellcome.country) 
summary(lm.0)
lm.0a <- lmer(vaccine_IRT ~ trust_u0 + trust_science2 + scale(gdppercapita_2017) + scale(HLO) + scale(gini) + male + I(age_c/10) + ed_mid + ed_high + scale(lg_inc2, center=TRUE) + (1 | countrynew), data = wellcome.country) 
summary(lm.0a)
lm.1 <- lmer(vaccine_IRT ~ scale(trust_science2) + scale(trust_u0)*scale(trust_u1) + scale(trust_science2)*scale(trust_u1) + scale(gdppercapita_2017) + scale(HLO) + scale(gini) + male + I(age_c/10) + ed_mid + ed_high + scale(lg_inc2, center=TRUE) + (1 | countrynew), data = wellcome.country) #SD has no obvious 0... [So might be better on residual scale]
summary(lm.1) 
lm.1a <- lmer(vaccine_IRT ~ trust_science2 + trust_u0*trust_u1 + trust_science2*trust_u1 + scale(gdppercapita_2017) + scale(HLO) + scale(gini) + male + I(age_c/10) + ed_mid + ed_high + scale(lg_inc2, center=TRUE) + (1 | countrynew), data = wellcome.country) #SD has no obvious 0... [So might be better on residual scale]
summary(lm.1a) 

eff.picked <- effect("trust_u0*trust_u1", lm.1a,
                     xlevels=list(trust_u1 = c(-.2, -.1, 0, .1, .2)),
                     se=TRUE, confidence.level=.95, typical=mean)
plot(eff.picked, xlab = "Trust Mean", ylab = "Marginal Effects on Vaccines support")

eff.picked2 <- effect("trust_science2*trust_u1", lm.1a,
                      xlevels=list(trust_u1 = c(-.2, -.1, 0, .1, .2)),
                      se=TRUE, confidence.level=.95, typical=mean)
plot(eff.picked2, xlab = "Individual trust", ylab = "Marginal Effects on Vaccines support")


#Merging data with individual data - using updated version of residuals
wellcome.country <-merge(wellcome_trust, WBI, by.x = "countrynew", by.y = "countrynew", all.x=TRUE, all.y=TRUE)
wellcome.country <-merge(wellcome.country, HLO, by.x = "countrynew", by.y = "countrynew", all.x=TRUE, all.y=TRUE)
wellcome.country <-merge(wellcome.country, Gini, by.x = "countrynew", by.y = "country", all.x=TRUE, all.y=TRUE)
wellcome.country <-merge(wellcome.country, effects4, by.x = "countrynew", by.y = "Country", all.x=TRUE, all.y=TRUE)

lm.0 <- lmer(vaccine_IRT ~ scale(trust_u0) + scale(trust_science2) + scale(gdppercapita_2017) + scale(HLO) + scale(gini) + male + I(age_c/10) + ed_mid + ed_high + scale(lg_inc2, center=TRUE) + (1 | countrynew), data = wellcome.country) 
summary(lm.0)
lm.0a <- lmer(vaccine_IRT ~ trust_u0 + trust_science2 + scale(gdppercapita_2017) + scale(HLO) + scale(gini) + male + I(age_c/10) + ed_mid + ed_high + scale(lg_inc2, center=TRUE) + (1 | countrynew), data = wellcome.country) 
summary(lm.0a)
lm.1 <- lmer(vaccine_IRT ~ scale(trust_science2) + scale(trust_u0)*scale(trust_u1) + scale(trust_science2)*scale(trust_u1) + scale(gdppercapita_2017) + scale(HLO) + scale(gini) + male + I(age_c/10) + ed_mid + ed_high + scale(lg_inc2, center=TRUE) + (1 | countrynew), data = wellcome.country) #SD has no obvious 0... [So might be better on residual scale]
summary(lm.1) 
lm.1a <- lmer(vaccine_IRT ~ trust_science2 + trust_u0*trust_u1 + trust_science2*trust_u1 + scale(gdppercapita_2017) + scale(HLO) + scale(gini) + male + I(age_c/10) + ed_mid + ed_high + scale(lg_inc2, center=TRUE) + (1 | countrynew), data = wellcome.country) #SD has no obvious 0... [So might be better on residual scale]
summary(lm.1a) 

eff.picked <- effect("trust_u0*trust_u1", lm.1a,
                     xlevels=list(trust_u1 = c(-.2, -.1, 0, .1, .2)),
                     se=TRUE, confidence.level=.95, typical=mean)
plot(eff.picked, xlab = "Trust Mean", ylab = "Marginal Effects on Vaccines support")

eff.picked2 <- effect("trust_science2*trust_u1", lm.1a,
                      xlevels=list(trust_u1 = c(-.2, -.1, 0, .1, .2)),
                      se=TRUE, confidence.level=.95, typical=mean)
plot(eff.picked2, xlab = "Individual trust", ylab = "Marginal Effects on Vaccines support")

wellcome.country %>%
  select(trust_science2, trust_u0, trust_u1, gdppercapita_2017, gini, HLO) %>%
  cor(use = "pairwise.complete.obs")

#Merging data with individual data - using origina version of residuals
wellcome.country <-merge(wellcome_trust, WBI, by.x = "countrynew", by.y = "countrynew", all.x=TRUE, all.y=TRUE)
wellcome.country <-merge(wellcome.country, HLO, by.x = "countrynew", by.y = "countrynew", all.x=TRUE, all.y=TRUE)
wellcome.country <-merge(wellcome.country, Gini, by.x = "countrynew", by.y = "country", all.x=TRUE, all.y=TRUE)
wellcome.country <-merge(wellcome.country, effects1, by.x = "countrynew", by.y = "Country", all.x=TRUE, all.y=TRUE)

lm.1a <- lmer(vaccine_IRT ~ trust_science2 + trust_u0*trust_u1 + trust_science2*trust_u1 + scale(gdppercapita_2017) + scale(HLO) + scale(gini) + male + I(age_c/10) + ed_mid + ed_high + scale(lg_inc2, center=TRUE) + (1 | countrynew), data = wellcome.country) #SD has no obvious 0... [So might be better on residual scale]
summary(lm.1a) 

#Merging data with individual data - using updated version of residuals
wellcome.country <-merge(wellcome_trust, WBI, by.x = "countrynew", by.y = "countrynew", all.x=TRUE, all.y=TRUE)
wellcome.country <-merge(wellcome.country, HLO, by.x = "countrynew", by.y = "countrynew", all.x=TRUE, all.y=TRUE)
wellcome.country <-merge(wellcome.country, Gini, by.x = "countrynew", by.y = "country", all.x=TRUE, all.y=TRUE)
wellcome.country <-merge(wellcome.country, effects5, by.x = "countrynew", by.y = "Country", all.x=TRUE, all.y=TRUE)

lm.1a <- lmer(vaccine_IRT ~ trust_science2 + trust_u0*trust_u1 + trust_science2*trust_u1 + scale(gdppercapita_2017) + scale(HLO) + scale(gini) + male + I(age_c/10) + ed_mid + ed_high + scale(lg_inc2, center=TRUE) + (1 | countrynew), data = wellcome.country) #SD has no obvious 0... [So might be better on residual scale]
summary(lm.1a) 

#Merging data with individual data - using updated version of residuals
wellcome.country <-merge(wellcome_trust, WBI, by.x = "countrynew", by.y = "countrynew", all.x=TRUE, all.y=TRUE)
wellcome.country <-merge(wellcome.country, HLO, by.x = "countrynew", by.y = "countrynew", all.x=TRUE, all.y=TRUE)
wellcome.country <-merge(wellcome.country, Gini, by.x = "countrynew", by.y = "country", all.x=TRUE, all.y=TRUE)
wellcome.country <-merge(wellcome.country, effects6, by.x = "countrynew", by.y = "Country", all.x=TRUE, all.y=TRUE)

lm.1a <- lmer(vaccine_IRT3 ~ trust_science2 + trust_u0*trust_u1 + trust_science2*trust_u1 + scale(gdppercapita_2017) + scale(HLO) + scale(gini) + male + I(age_c/10) + ed_mid + ed_high + scale(lg_inc2, center=TRUE) + (1 | countrynew), data = wellcome.country) #SD has no obvious 0... [So might be better on residual scale]
summary(lm.1a) 


wellcome.country <-merge(wellcome_trust, WBI, by.x = "countrynew", by.y = "countrynew", all.x=TRUE, all.y=TRUE)
wellcome.country <-merge(wellcome.country, HLO, by.x = "countrynew", by.y = "countrynew", all.x=TRUE, all.y=TRUE)
wellcome.country <-merge(wellcome.country, Gini, by.x = "countrynew", by.y = "country", all.x=TRUE, all.y=TRUE)
wellcome.country <-merge(wellcome.country, effects7, by.x = "countrynew", by.y = "Country", all.x=TRUE, all.y=TRUE)

lm.1a <- lmer(vaccine_IRT ~ trust_science2 + trust_u0*trust_u1 + trust_science2*trust_u1 + scale(gdppercapita_2017) + scale(HLO) + scale(gini) + male + I(age_c/10) + ed_mid + ed_high + scale(lg_inc2, center=TRUE) + (1 | countrynew), data = wellcome.country) #SD has no obvious 0... [So might be better on residual scale]
summary(lm.1a) 

lm.1 <- lmer(vaccine_IRT ~ trust_science2 + trust_u0 + (1 | countrynew), data = wellcome.country) #SD has no obvious 0... [So might be better on residual scale]
summary(lm.1) 
lm.2 <- lmer(vaccine_IRT ~ scale(trust_science2) + scale(trust_u0) + (1 | countrynew), data = wellcome.country) #SD has no obvious 0... [So might be better on residual scale]
summary(lm.2) 
lm.3 <- lmer(vaccine_IRT ~ trust_science2 + trust_u0*trust_u1 + trust_science2*trust_u1 + (1 | countrynew), data = wellcome.country) #SD has no obvious 0... [So might be better on residual scale]
summary(lm.3) 
lm.4 <- lmer(vaccine_IRT ~ scale(trust_science2) + scale(trust_u0)*scale(trust_u1) + scale(trust_science2)*scale(trust_u1) + (1 | countrynew), data = wellcome.country) #SD has no obvious 0... [So might be better on residual scale]
summary(lm.4) 

eff.country <- effect("trust_u0*trust_u1", lm.3,
                     xlevels=list(trust_u1 = c(-.12, -.05, 0, .05, .12)),
                     se=TRUE, confidence.level=.95, typical=mean)
plot(eff.country, xlab = "Trust Mean", ylab = "Marginal Effects on Vaccines support", lattice = list(layout=c(5,1)))

trust.labs <- c(`-0.12` = "Strong norm of trust (Q1)", `-0.05` = "Q2", `0` = "Moderate norm of trust (Q3)", `0.05` = "Q4", `0.12` = "Weak norm of trust (Q5)")

data.country <- as.data.frame(eff.country)
ggplot(data=data.country, aes(x=trust_u0, y = fit)) + 
         geom_smooth(method=lm) + 
  geom_line(aes(y = lower,
                group=factor(trust_u1)), linetype =3) +
  geom_line(aes(y = upper,
                group=factor(trust_u1)), linetype =3) +
  geom_ribbon(aes(ymin = lower, ymax=upper), alpha=.08) +
  xlab("Average trust in science") +
  ylab("Marginal effects on vaccine confidence") +
  theme_light(base_size = 16) +  
  scale_y_continuous(breaks = round(seq(min(data.country$fit), max(data.country$fit), by = 0.5),1)) +
  facet_wrap(~ trust_u1,labeller=labeller(trust_u1=trust.labs), nrow=1) 

eff.individual <- effect("trust_science2*trust_u1", lm.3,
                      xlevels=list(trust_u1 = c(-.12, -.05, 0, .05, .12)),
                      se=TRUE, confidence.level=.95, typical=mean)
plot(eff.individual, xlab = "Trust Mean", ylab = "Marginal Effects on Vaccines confidence", lattice = list(layout=c(5,1)))

data.individual <- as.data.frame(eff.individual)
ggplot(data=data.individual, aes(x=trust_science2, y = fit)) + 
  geom_smooth(method=lm) + 
  geom_line(aes(y = lower,
                group=factor(trust_u1)), linetype =3) +
  geom_line(aes(y = upper,
                group=factor(trust_u1)), linetype =3) +
  geom_ribbon(aes(ymin = lower, ymax=upper), alpha=.08) +
  xlab("Individual trust in science") +
  ylab("Marginal effects on vaccine confidence") +
  theme_light(base_size = 16) +  
  scale_y_continuous(breaks = round(seq(min(data.individual$fit), max(data.individual$fit), by = 0.5),1)) +
  facet_wrap(~ trust_u1,labeller=labeller(trust_u1=trust.labs), nrow=1) 




# Revised trust (location-scale) models - PNAS v3 -------------------------
wellcome.country <-merge(wellcome_trust, WBI, by.x = "countrynew", by.y = "countrynew", all.x=TRUE, all.y=TRUE)
wellcome.country <-merge(wellcome.country, HLO, by.x = "countrynew", by.y = "countrynew", all.x=TRUE, all.y=TRUE)
wellcome.country <-merge(wellcome.country, Gini, by.x = "countrynew", by.y = "country", all.x=TRUE, all.y=TRUE)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#Imputing trust for all those missing a score on all items [no better]
m3_IRTimp <- brm(bf(trust_science3 ~ 1  + scale(gdppercapita_2017) + scale(HLO) + scale(gini) + male + I(age_c/10) + ed_mid + ed_high + scale(lg_inc2, center=TRUE) + (1 |s| countrynew), sigma ~ 1 + scale(gdppercapita_2017) + scale(HLO) + scale(gini) + male + I(age_c/10) + ed_mid + ed_high + scale(lg_inc2, center=TRUE) + (1 |s| countrynew)),
                  data = wellcome.country,
                  family = gaussian(),
                  chains = 4, 
                  iter = 20000)
summary(m3_IRTimp)

#Drop observation if individual does not answer all questions [no better]
m3_IRTmiss <- brm(bf(trust_science4 ~ 1  + scale(gdppercapita_2017) + scale(HLO) + scale(gini) + male + I(age_c/10) + ed_mid + ed_high + scale(lg_inc2, center=TRUE) + (1 |s| countrynew), sigma ~ 1 + scale(gdppercapita_2017) + scale(HLO) + scale(gini) + male + I(age_c/10) + ed_mid + ed_high + scale(lg_inc2, center=TRUE) + (1 |s| countrynew)),
                  data = wellcome.country,
                  family = gaussian(),
                  chains = 4, 
                  iter = 20000)
summary(m3_IRTmiss)

#Table 1: Model 3
m3_IRTlong.gm <- brm(bf(trust_science2 ~ 1  + scale(gdppercapita_2017) + scale(HLO) + scale(gini) + scale(male, scale=FALSE) + I(age_c/10) + scale(ed_mid, scale=FALSE) + scale(ed_high, scale=FALSE) + scale(lg_inc2, center=TRUE) + (1 |s| countrynew), sigma ~ 1 + scale(gdppercapita_2017) + scale(HLO) + scale(gini) + scale(male, scale=FALSE) + I(age_c/10) + scale(ed_mid, scale=FALSE) + scale(ed_high, scale=FALSE) + scale(lg_inc2, center=TRUE) + (1 |s| countrynew)),
                  data = wellcome.country,
                  family = gaussian(),
                  chains = 4, 
                  iter = 20000)
summary(m3_IRTlong.gm)

print(summary(m3_IRTlong.gm), digits = 3)


#To try - in stages. 
#1. re-estimate interaction models using m3_IRTimp/miss. These approaches may work better - although careful with vaccine support version (should match) [No better]
#1a. Double check using appropriate versions of vaccine... 
#2. Check correlations of existing residuals with predictors [as expected, no high correlations]
#3. re-estimate m3_IRTlong with centred versions of gender, education [in progress]
#4. adjust all graphs to look better... 
#2. impute HLO etc and re-run standard IRTmodel (m3_IRTlong). (or versions above if they look better). Not full MI (too slow, but impute and mean)
#3. drop HLO and re-run standard IRT model (m3_IRTlong). (or versions above if they look better)

install.packages("mice")
library(mice)

wellcome.country.reduced <- wellcome.country
wellcome.country.reduced <- wellcome.country.reduced %>%
  select(countrynew, gdppercapita_2017, HLO, gini, male, age_c, ed_mid, ed_high, lg_inc2, trust_science2)
imp <- mice(wellcome.country.reduced, m = 5, print = FALSE)



# OLD MODELS: Location-scale models using various versions of trust in science -------------------------------------------------
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#Trust derived from factora analyis
m0_factor <- brm(bf(trust_science1 ~ 1  + (1 |s| countrynew), sigma ~ 1 + (1 |s| countrynew)),
                 data = wellcome_trust,
                 family = gaussian(),
                 chains = 4, 
                 iter = 10000)
summary(m0_factor) #Would probably need to be run for longer... warning about low ESS.

#Early version of final model using IRT (too few MCMC chains)
m0_IRT <- brm(bf(trust_science2 ~ 1  + (1 |s| countrynew), sigma ~ 1 + (1 |s| countrynew)),
              data = wellcome_trust,
              family = gaussian(),
              chains = 4, 
              iter = 10000)
summary(m0_IRT)

launch_shinystan(m0_IRT)

#Imputing missing values of trust in science with score of 0
m0_IRTimp <- brm(bf(trust_science3 ~ 1  + (1 |s| countrynew), sigma ~ 1 + (1 |s| countrynew)),
                 data = wellcome_trust,
                 family = gaussian(),
                 chains = 4, 
                 iter = 10000)
summary(m0_IRTimp)

#Removing all cases with missing values on any item. 
m0_IRTmiss <- brm(bf(trust_science4 ~ 1  + (1 |s| countrynew), sigma ~ 1 + (1 |s| countrynew)),
                  data = wellcome_trust,
                  family = gaussian(),
                  chains = 4, 
                  iter = 10000)
summary(m0_IRTmiss)


# OLD MODELS: Modelling the residuals (from empty location-scale models)-------------------------------------------------
merged.wellcome <-merge(wellcome_trust, effects0, by.x = "countrynew", by.y = "Country", all.x=TRUE, all.y=TRUE)
merged.wellcome <-merge(wellcome_trust, effects1, by.x = "countrynew", by.y = "Country", all.x=TRUE, all.y=TRUE)

ggplot(data = merged.wellcome, mapping = aes(x = trust_science2)) + 
  geom_histogram(binwidth = .2)


#Sig mean, but nothing else
by_country <- group_by(merged.wellcome, countrynew)
by_country <- summarise(by_country, vac_kids = mean(vac_kids, na.rm = TRUE))
ggplot(data = by_country, mapping = aes(x = vac_kids)) + 
  geom_histogram()

bin.1 <- glmer(vac_kids ~ trust_u0*trust_u1 + (1 | countrynew), family = binomial("logit"), nAGQ = 15, data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
summary(bin.1) 

#Sig mean and SD. Non-sig interaction, but in standard direction
lm.1 <- lmer(vaccine_IRT ~ trust_u0*trust_u1 + (1 | countrynew), data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
summary(lm.1) 
eff.lm1 <- effect("trust_u0*trust_u1", lm.1)
plot(eff.lm1, xlab = "Trust Mean", ylab = "Marginal Effects on Vaccines support")

#Sig mean and SD. Non-sig interaction, but in standard direction
bin.1 <- glmer(vacIRT_bin ~ trust_u0*trust_u1 + (1 | countrynew), family = binomial("logit"), nAGQ = 15, data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
summary(bin.1) 
eff.bin1 <- effect("trust_u0*trust_u1", bin.1, KR=T)
plot(eff.bin1, xlab = "Trust Mean", ylab = "Marginal Effects on Vaccines support")

#Sig mean, SD and interaction. In standard direction
bin.0 <- glmer(vacCHILD_bin ~ trust_u0 + trust_u1 + (1 | countrynew), family = binomial("logit"), nAGQ = 15, data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
summary(bin.0) 

bin.1 <- glmer(vacCHILD_bin ~ trust_u0*trust_u1 + (1 | countrynew), family = binomial("logit"), nAGQ = 15, data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
summary(bin.1) 

eff.bin1 <- effect("trust_u0*trust_u1", bin.1, KR=T)
plot(eff.bin1, xlab = "Trust Mean", ylab = "Marginal Effects on Vaccines important for children")

#Sig SD  In standard direction
bin.0 <- glmer(vacSAFE_bin ~ trust_u0 + trust_u1 + (1 | countrynew), family = binomial("logit"), nAGQ = 15, data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
summary(bin.0) 

bin.1 <- glmer(vacSAFE_bin ~ trust_u0*trust_u1 + (1 | countrynew), family = binomial("logit"), nAGQ = 15, data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
summary(bin.1) 
eff.bin1 <- effect("trust_u0*trust_u1", bin.1, KR=T)
plot(eff.bin1, xlab = "Trust Mean", ylab = "Marginal Effects on Vaccines are safe")

#Sig mean, SD and [nearly] interaction. In standard direction
bin.0 <- glmer(vacEFFECT_bin ~ trust_u0 + trust_u1 + (1 | countrynew), family = binomial("logit"), nAGQ = 15, data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
summary(bin.0) 

bin.1 <- glmer(vacEFFECT_bin ~ trust_u0*trust_u1 + (1 | countrynew), family = binomial("logit"), nAGQ = 15, data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
summary(bin.1) 
eff.bin1 <- effect("trust_u0*trust_u1", bin.1, KR=T)
plot(eff.bin1, xlab = "Trust Mean", ylab = "Marginal Effects on Vaccines are effective")

bin.1 <- glmer(vacCHILD_bin2 ~ trust_u0*trust_u1 + (1 | countrynew), family = binomial("logit"), nAGQ = 15, data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
summary(bin.1) 

bin.1 <- glmer(vacSAFE_bin2 ~ trust_u0*trust_u1 + (1 | countrynew), family = binomial("logit"), nAGQ = 15, data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
summary(bin.1) 

bin.1 <- glmer(vacEFFECT_bin2 ~ trust_u0*trust_u1 + (1 | countrynew), family = binomial("logit"), nAGQ = 15, data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
summary(bin.1) 

#Individual level analysis 
bin.1 <- glmer(vacCHILD_bin ~ trust_science2*trust_u0 + trust_science2*trust_u1 + (1 | countrynew), family = binomial("logit"), nAGQ = 15, data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
summary(bin.1) 
bin.1 <- glmer(vacCHILD_bin ~ scale(trust_science2)*scale(trust_u0) + scale(trust_science2)*scale(trust_u1) + (1 | countrynew), family = binomial("logit"), nAGQ = 15, data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
summary(bin.1) 

eff.bin1 <- effect("trust_science2*trust_u0", bin.1, KR=T)
plot(eff.bin1, xlab = "Trust individual", ylab = "Marginal Effects on vaccines are important for children")
eff.bin1 <- effect("trust_science2*trust_u1", bin.1, KR=T)
plot(eff.bin1, xlab = "Trust individual", ylab = "Marginal Effects on vaccines are important for children")

bin.1 <- glmer(vacSAFE_bin ~ trust_science2*trust_u0 + trust_science2*trust_u1 + (1 | countrynew), family = binomial("logit"), nAGQ = 15, data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
summary(bin.1) 
bin.1 <- glmer(vacSAFE_bin ~ scale(trust_science2)*scale(trust_u0) + scale(trust_science2)*scale(trust_u1) + (1 | countrynew), family = binomial("logit"), nAGQ = 15, data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
summary(bin.1) 


eff.bin1 <- effect("trust_science2*trust_u0", bin.1, KR=T)
plot(eff.bin1, xlab = "Trust individual", ylab = "Marginal Effects on Vaccines are safe")
eff.bin1 <- effect("trust_science2*trust_u1", bin.1, KR=T)
plot(eff.bin1, xlab = "Trust individual", ylab = "Marginal Effects on Vaccines are safe")

bin.1 <- glmer(vacEFFECT_bin ~ trust_science2*trust_u0 + trust_science2*trust_u1 + (1 | countrynew), family = binomial("logit"), nAGQ = 15, data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
summary(bin.1) 
bin.1 <- glmer(vacEFFECT_bin ~ scale(trust_science2)*scale(trust_u0) + scale(trust_science2)*scale(trust_u1) + (1 | countrynew), family = binomial("logit"), nAGQ = 15, data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
summary(bin.1) 

eff.bin1 <- effect("trust_science2*trust_u0", bin.1, KR=T)
plot(eff.bin1, xlab = "Trust individual", ylab = "Marginal Effects on Vaccines are effective")
eff.bin1 <- effect("trust_science2*trust_u1", bin.1, KR=T)
plot(eff.bin1, xlab = "Trust individual", ylab = "Marginal Effects on Vaccines are effective")

#Individual analysis
bin.1 <- glmer(vacCHILD_bin ~ scale(trust_science2)*scale(trust_u0) + scale(trust_science2)*scale(trust_u1) + (1 | countrynew), family = binomial("logit"), nAGQ = 15, data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
summary(bin.1) 

bin.1 <- glmer(vacSAFE_bin ~ scale(trust_science2)*scale(trust_u0) + scale(trust_science2)*scale(trust_u1) + (1 | countrynew), family = binomial("logit"), nAGQ = 15, data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
summary(bin.1) 

bin.1 <- glmer(vacEFFECT_bin ~ scale(trust_science2)*scale(trust_u0) + scale(trust_science2)*scale(trust_u1) + (1 | countrynew), family = binomial("logit"), nAGQ = 15, data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
summary(bin.1) 

bin.1 <- glmer(vacCHILD_bin ~ scale(trust_science2) + scale(trust_u0)*scale(trust_u1) + (1 | countrynew), family = binomial("logit"), nAGQ = 15, data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
summary(bin.1) 
eff.bin1 <- effect("scale(trust_u0)*scale(trust_u1)", bin.1, KR=T)
plot(eff.bin1, xlab = "Trust Mean", ylab = "Marginal Effects on Vaccines important for children")

bin.1 <- glmer(vacCHILD_bin ~ trust_science2 + trust_u0*trust_u1 + (1 | countrynew), family = binomial("logit"), nAGQ = 15, data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
summary(bin.1) 

merged.wellcome$trust_gm <- merged.wellcome$trust_science2 - merged.wellcome$trust_u0
bin.1 <- glmer(vacCHILD_bin ~ trust_gm + trust_u0*trust_u1 + (1 | countrynew), family = binomial("logit"), nAGQ = 15, data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
summary(bin.1) 
bin.1 <- glmer(vacCHILD_bin ~ scale(trust_gm) + scale(trust_u0)*scale(trust_u1) + (1 | countrynew), family = binomial("logit"), nAGQ = 15, data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
summary(bin.1) 


# EXPLORATORY WORK: Modelling consensus at country level -----------------------------------------------------

effects1 <- effects1 %>%
  select(Country, trust_u0, trust_u1)

##Gapminder data
child_mortality <- read_excel("Country datasets/Excel_data/child_mortality_0_5_year_olds_dying_per_1000_born.xlsx") %>%
  pivot_longer(c(`1800`:`2100`), names_to = "year", values_to = "child_mortality")%>%
  group_by(country)%>%
  fill(child_mortality)%>%
  ungroup()%>%
  filter(year==2018)

Gini <- read_excel("Country datasets/Excel_data/inequality_index_gini.xlsx")%>%
  pivot_longer(c(`1967`:`2018`), names_to = "year", values_to = "gini")%>%
  group_by(country)%>%
  fill(gini)%>%
  ungroup()%>%
  filter(year==2018)

TB <- read_excel("Country datasets/Excel_data/tb_with_hivplus_deaths_per_100000_estimated.xlsx")%>%
  pivot_longer(c(`1990`:`2007`), names_to = "year", values_to = "TB")%>%
  group_by(country)%>%
  fill(TB)%>%
  ungroup()%>%
  filter(year==2007)

democracy_score <- read_excel("Country datasets/Excel_data/democracy_score_use_as_color.xlsx")%>%
  pivot_longer(c(`1800`:`2011`), names_to = "year", values_to = "democracy")%>%
  group_by(country)%>%
  fill(democracy)%>%
  ungroup()%>%
  filter(year==2011)

HDI <- read_excel("Country datasets/Excel_data/hdi_human_development_index.xlsx")%>%
  pivot_longer(c(`1990`:`2015`), names_to = "year", values_to = "hdi")%>%
  group_by(country)%>%
  fill(hdi)%>%
  ungroup()%>%
  filter(year==2015)

Malaria <- read_excel("Country datasets/Excel_data/malaria_deaths_per_100000_reported.xlsx")%>%
  pivot_longer(c(`1990`:`2006`), names_to = "year", values_to = "malaria")%>%
  group_by(country)%>%
  fill(malaria)%>%
  ungroup()%>%
  filter(year==2006)

merged.gapminder <- left_join(Gini, democracy_score, by = "country")%>%
  left_join(., HDI, by = "country")%>%
  left_join(., Malaria, by = "country")%>%
  left_join(., TB, by = "country")%>%
  left_join(., child_mortality, by = "country")

#Renaming gapminder data to match effects
merged.gapminder$country[merged.gapminder$country=="Congo, Dem. Rep."] <- "Republic of Congo"
merged.gapminder$country[merged.gapminder$country=="Cote d'Ivoire"] <- "Ivory Coast"
merged.gapminder$country[merged.gapminder$country=="Egypt, Arab Rep."] <- "Egypt"
merged.gapminder$country[merged.gapminder$country=="Kyrgyz Republic"] <- "Kyrgyzstan"
merged.gapminder$country[merged.gapminder$country=="Lao"] <- "Laos"
merged.gapminder$country[merged.gapminder$country=="Slovak Republic"] <- "Slovakia"
merged.gapminder$country[merged.gapminder$country=="United Kingdom"] <- "UK"
merged.gapminder$country[merged.gapminder$country=="United States"] <- "USA"

##WBI data (subset of all variables)
WBI<- read_dta("Country datasets/WBI_v1.dta")

##Harmonised learning outcomes
HLO <- read_dta("Country datasets/HLO.dta")
HLO$countrynew[HLO$countrynew=="Congo, Dem. Rep."] <- "Republic of Congo"
HLO$countrynew[HLO$countrynew=="Cote d'Ivoire"] <- "Ivory Coast"
HLO$countrynew[HLO$countrynew=="Egypt, Arab Rep."] <- "Egypt"
HLO$countrynew[HLO$countrynew=="Eswatini"] <- "Swaziland"
HLO$countrynew[HLO$countrynew=="Gambia, The"] <- "Gambia"
HLO$countrynew[HLO$countrynew=="Iran, Islamic Rep."] <- "Iran"
HLO$countrynew[HLO$countrynew=="Korea, Rep."] <- "South Korea"
HLO$countrynew[HLO$countrynew=="Kyrgyz Republic"] <- "Kyrgyzstan"
HLO$countrynew[HLO$countrynew=="Lao PDR"] <- "Laos"
HLO$countrynew[HLO$countrynew=="Macedonia, FYR"] <- "Macedonia"
HLO$countrynew[HLO$countrynew=="Russian Federation"] <- "Russia"
HLO$countrynew[HLO$countrynew=="Slovak Republic"] <- "Slovakia"
HLO$countrynew[HLO$countrynew=="United Kingdom"] <- "UK"
HLO$countrynew[HLO$countrynew=="United States"] <- "USA"
HLO$countrynew[HLO$countrynew=="Yemen, Rep."] <- "Yemen"


##HCI - taken from WBI
HCI <- read_dta("Country datasets/HCI.dta")
HCI$countrynew[HCI$countrynew=="Congo, Dem. Rep."] <- "Republic of Congo"
HCI$countrynew[HCI$countrynew=="Cote d'Ivoire"] <- "Ivory Coast"
HCI$countrynew[HCI$countrynew=="Egypt, Arab Rep."] <- "Egypt"
HCI$countrynew[HCI$countrynew=="Eswatini"] <- "Swaziland"
HCI$countrynew[HCI$countrynew=="Gambia, The"] <- "Gambia"
HCI$countrynew[HCI$countrynew=="Iran, Islamic Rep."] <- "Iran"
HCI$countrynew[HCI$countrynew=="Korea, Rep."] <- "South Korea"
HCI$countrynew[HCI$countrynew=="Kyrgyz Republic"] <- "Kyrgyzstan"
HCI$countrynew[HCI$countrynew=="Lao PDR"] <- "Laos"
HCI$countrynew[HCI$countrynew=="Macedonia, FYR"] <- "Macedonia"
HCI$countrynew[HCI$countrynew=="Russian Federation"] <- "Russia"
HCI$countrynew[HCI$countrynew=="Slovak Republic"] <- "Slovakia"
HCI$countrynew[HCI$countrynew=="United Kingdom"] <- "UK"
HCI$countrynew[HCI$countrynew=="United States"] <- "USA"
HCI$countrynew[HCI$countrynew=="Yemen, Rep."] <- "Yemen"

Corruption_perception<- read_dta("Country datasets/Corruption_perception_WBI.dta")
corrupt_mean <- read_dta("Country datasets/corrupt_mean.dta")

merged.effects <-merge(effects1, merged.gapminder, by.x = "Country", by.y = "country", all.x=TRUE, all.y=TRUE)
merged.effects <-merge(merged.effects, WBI, by.x = "Country", by.y = "countrynew", all.x=TRUE, all.y=TRUE)
merged.effects <-merge(merged.effects, HCI, by.x = "Country", by.y = "countrynew", all.x=TRUE, all.y=TRUE)
merged.effects <-merge(merged.effects, Corruption_perception, by.x = "Country", by.y = "countrynew", all.x=TRUE, all.y=TRUE)
merged.effects <-merge(merged.effects, corrupt_mean, by.x = "Country", by.y = "countrynew", all.x=TRUE, all.y=TRUE)
merged.effects <-merge(merged.effects, HLO, by.x = "Country", by.y = "countrynew", all.x=TRUE, all.y=TRUE)


#Candidate variables
model.1<-lm(trust_u1 ~ I(gdppercapita_2017/10000), data=merged.effects)
model.2<-lm(trust_u1 ~ democracy, data=merged.effects)
model.3<-lm(trust_u1 ~ poverty190_mean, data=merged.effects)
model.4<-lm(trust_u1 ~ poverty320_mean, data=merged.effects)
model.5<-lm(trust_u1 ~ gini, data=merged.effects)
model.6<-lm(trust_u1 ~ log_pop17, data=merged.effects)
model.7<-lm(trust_u1 ~ ci_2018, data=merged.effects)
model.8<-lm(trust_u1 ~ corrupt_mean, data=merged.effects)
model.9<-lm(trust_u1 ~ HCI, data=merged.effects)
model.10<-lm(trust_u1 ~ I(HLO/1000), data=merged.effects)
model.11<-lm(trust_u1 ~ hdi, data=merged.effects)
model.12<-lm(trust_u1 ~ malaria, data=merged.effects)
model.13<-lm(trust_u1 ~ TB, data=merged.effects)
model.14<-lm(trust_u1 ~ child_mortality, data=merged.effects)

stargazer(model.1, model.2, model.3, model.4, model.5, model.6, model.7, type = "text")
stargazer(model.8, model.9, model.10, model.11, model.12, model.13, model.14, type = "text")

model.15<-lm(trust_u1 ~ I(gdppercapita_2017/10000) + gini + I(HLO/1000), data=merged.effects)
stargazer(model.15, type="text")

stargazer(model.15, type="html", out="model.doc")

ggplot(data = merged.effects, mapping = aes(x = HLO)) + 
  geom_histogram()

ggplot(data = merged.effects, mapping = aes(x = gini)) + 
  geom_histogram()

#Contrlling for country differences
merged.wellcome <-merge(wellcome_trust, merged.effects, by.x = "countrynew", by.y = "Country", all.x=TRUE, all.y=TRUE)
bin1.a <- glmer(vacCHILD_bin ~ trust_u0 + trust_u1 + (1 | countrynew), family = binomial("logit"), nAGQ = 15, data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
bin1.b <- glmer(vacCHILD_bin ~ trust_u0 + trust_u1 + gini + I(HLO/1000) + (1 | countrynew), family = binomial("logit"), nAGQ = 15, data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
bin1.c <- glmer(vacCHILD_bin ~ trust_u0*trust_u1 + gini + I(HLO/1000) + (1 | countrynew), family = binomial("logit"), nAGQ = 15, data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
stargazer(bin1.a, bin1.b, bin1.c, type="text")
stargazer(bin1.a, bin1.b, bin1.c, type="html", out="model2.doc")


bin2.a <- glmer(vacSAFE_bin ~ trust_u0 + trust_u1 + (1 | countrynew), family = binomial("logit"), nAGQ = 15, data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
bin2.b <- glmer(vacSAFE_bin ~ trust_u0 + trust_u1 + gini + I(HLO/1000) + (1 | countrynew), family = binomial("logit"), nAGQ = 15, data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
bin2.c <- glmer(vacSAFE_bin ~ trust_u0*trust_u1 + gini + I(HLO/1000) + (1 | countrynew), family = binomial("logit"), nAGQ = 15, data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
stargazer(bin2.a, bin2.b, bin2.c, type="text")
stargazer(bin2.a, bin2.b, bin2.c, type="html", out="model3.doc")

bin3.a <- glmer(vacEFFECT_bin ~ trust_u0 + trust_u1 + (1 | countrynew), family = binomial("logit"), nAGQ = 15, data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
bin3.b <- glmer(vacEFFECT_bin ~ trust_u0 + trust_u1 + gini + I(HLO/1000) + (1 | countrynew), family = binomial("logit"), nAGQ = 15, data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
bin3.c <- glmer(vacEFFECT_bin ~ trust_u0*trust_u1 + gini + I(HLO/1000) + (1 | countrynew), family = binomial("logit"), nAGQ = 15, data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
stargazer(bin3.a, bin3.b, bin3.c, type="text")
stargazer(bin3.a, bin3.b, bin3.c, type="html", out="model4.doc")

bin4.a<- glmer(vacCHILD_bin ~ trust_science2*trust_u0 + trust_science2*trust_u1 + gini + I(HLO/1000) + (1 | countrynew), family = binomial("logit"), nAGQ = 15, data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
bin4.b<- glmer(vacSAFE_bin ~ trust_science2*trust_u0 + trust_science2*trust_u1 + gini + I(HLO/1000) + (1 | countrynew), family = binomial("logit"), nAGQ = 15, data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
bin4.c<- glmer(vacEFFECT_bin ~ trust_science2*trust_u0 + trust_science2*trust_u1 + gini + I(HLO/1000) + (1 | countrynew), family = binomial("logit"), nAGQ = 15, data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
stargazer(bin4.a, bin4.b, bin4.c, type="text")
stargazer(bin4.a, bin4.b, bin4.c, type="html", out="model5.doc")




# Some multilevel plots - may need random slope ---------------------------

#Vaccine support across countries... varying levels... say 3.
#Show plot of individual relationships between trust and vaccine support for 3 levels of consensus. Split by consensus...? 

## FOR REFERENCE: First load the vaccine model - empty
#load("~/Dropbox/Academic Articles/Vaccine coverage - WGP/World data.RData")
#summary(m0_IRT)
#Rename as vaccine_LS and delete ALL other datasets
#vaccine_LS <- m0_IRT
#save.image("~/Dropbox/Academic Articles/Vaccine coverage - WGP/Trust in science/Country_residuals_vaccine.RData")

load("~/Dropbox/Academic Articles/Vaccine coverage - WGP/Trust in science - residuals/Country_residuals_vaccine.RData")
load("~/Dropbox/Academic Articles/Vaccine coverage - WGP/Trust in science - residuals/Country_residuals_trust.RData")

summary(vaccine_LS)
effects.vac<-ranef(vaccine_LS)
effects.vac<-as.data.frame(effects.vac)
effects.vac$Country <- rownames(effects.vac)
effects.vac$vac_u0 <- effects.vac$countrynew.Estimate.Intercept
effects.vac$vac_m2.5 <- effects.vac$countrynew.Q2.5.Intercept
effects.vac$vac_m97.5 <- effects.vac$countrynew.Q97.5.Intercept
effects.vac$vac_u1 <- effects.vac$countrynew.Estimate.sigma_Intercept
effects.vac$vac_sd2.5 <- effects.vac$countrynew.Q2.5.sigma_Intercept
effects.vac$vac_sd97.5 <- effects.vac$countrynew.Q97.5.sigma_Intercept

effects.vac <- effects.vac %>%
  select(Country, vac_u0, vac_u1)
effects.vac$vac_u1_3 <-ntile(effects.vac$vac_u1, 3)

#Location and scale results for trust (adjusted by HLO etc)
summary(m2_IRTlong)
effects.trust<-ranef(m2_IRTlong)
effects.trust<-as.data.frame(effects.trust)
effects.trust$Country <- rownames(effects.trust)
effects.trust$trust_u0 <- effects.trust$countrynew.Estimate.Intercept
effects.trust$vac_m2.5 <- effects.trust$countrynew.Q2.5.Intercept
effects.trust$vac_m97.5 <- effects.trust$countrynew.Q97.5.Intercept
effects.trust$trust_u1 <- effects.trust$countrynew.Estimate.sigma_Intercept
effects.trust$vac_sd2.5 <- effects.trust$countrynew.Q2.5.sigma_Intercept
effects.trust$vac_sd97.5 <- effects.trust$countrynew.Q97.5.sigma_Intercept

effects.trust <- effects.trust %>%
  select(Country, trust_u0, trust_u1)
effects.trust$trust_u1_3 <-ntile(effects.trust$trust_u1, 3)

merged.wellcome <-merge(wellcome_trust, effects.trust, by.x = "countrynew", by.y = "Country", all.x=TRUE, all.y=TRUE)
merged.wellcome <-merge(merged.wellcome, effects.vac, by.x = "countrynew", by.y = "Country", all.x=TRUE, all.y=TRUE)

ggplot(data = merged.wellcome, aes(x = trust_u1, y = vac_u1)) +
  geom_point() + 
  geom_smooth(method=lm)


# New facet label names
vac.labs <- c(`1` = "High consensus", `2` = "Mid consensus", `3` = "Low consensus")

merged.wellcome %>%
  drop_na(vac_u1_3) %>%
  ggplot(aes(x=trust_science2, y=vaccine_IRT, color=as.factor(countrynew))) + 
  geom_smooth(method=lm) + 
  theme(legend.position="none") +
  facet_wrap(~ vac_u1_3, labeller=labeller(vac_u1_3=vac.labs)) 



# Old material from first plots - JJ --------------------------------------
merged.wellcome <-merge(wellcome_trust, effects1, by.x = "countrynew", by.y = "Country", all.x=TRUE, all.y=TRUE)

lm.1 <- lmer(vaccine_IRT ~ trust_u0*trust_u1 + (1 | countrynew), data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
summary(lm.1) 


lm.1 <- lmer(vaccine_IRT ~ trust_science2 + trust_u1 + trust_u0*trust_u1 + (1 | countrynew), data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
summary(lm.1) 
lm.2 <- lmer(vaccine_IRT ~ trust_science2 + trust_u1 + trust_u0*trust_u1 + (1 + trust_science2 | countrynew), data = merged.wellcome) #SD has no obvious 0... [So might be better on residual scale]
summary(lm.2) 

-2*(logLik(lm.1) - logLik(lm.2))
eff.lm1 <- effect("trust_science2*trust_u1", lm.1)
plot(eff.lm1, xlab = "Individual trust", ylab = "Marginal Effects on Vaccines support")

# New facet label names
u1.labs <- c(`1` = "High consensus", `2` = "HM", `3` = "Mid consensus", `4` = "ML", `5` = "Low consensus")

merged.wellcome %>%
  drop_na(u1_5) %>%
  ggplot(aes(x=trust_science2, y=vaccine_IRT, color=as.factor(countrynew))) + 
  geom_smooth(method=lm) + 
  theme(legend.position="none") +
  facet_wrap(~ u1_5, labeller=labeller(u1_5=u1.labs)) 

lm.2 <- lmer(vaccine_IRT ~ trust_science2 + (1 + trust_science2 | countrynew), data = merged.wellcome, na.action=na.exclude) #SD has no obvious 0... [So might be better on residual scale]
summary(lm.2) 

merged.wellcome$random.coefficients.predictions <- predict(lm.2)
ggplot(aes(x = trust_science2, y = random.coefficients.predictions, color = as.factor(countrynew)), data = merged.wellcome) +
  geom_line(size=.3) +
  theme(legend.position="none")



