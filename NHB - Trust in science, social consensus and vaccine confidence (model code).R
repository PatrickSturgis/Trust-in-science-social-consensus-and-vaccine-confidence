#NATURE HUMAN BEHAVIOR - Trust in science, social consensus, and vaccine confidence 

# Loading packages and data  -------------------------------------------

library(effects)
library(tidyverse)
library(haven)
library(readxl)
library(ltm)
library(lme4)
library(rstan)
library(brms)
library(ggrepel)
library(RColorBrewer)
library(stargazer)
library(tidybayes)
library(mice)

# Reading in Raw data -----------------------------------------------------
setwd([insert file location here])

wgm.2018.original <- read_dta("Wellcome_Trust_February_originalfileSTATA.dta")

#WORLD BANK INDICATOR DATA
# https://databank.worldbank.org/home.aspx
WBI.raw <- read_csv("WBI_v1.csv") %>%
  drop_na(`Country Code`) %>%
  subset(select = (-`Series Code`)) %>%
  na_if("..") 
gini.raw <- read_excel("inequality_index_gini.xlsx")
# https://datacatalog.worldbank.org/dataset/human-capital-index
HCI.raw <- read_csv("HCIData.csv") %>%
  subset(select = (-`Indicator Code`)) 

# Restructuring all country-level data ------------------------------------

#Identifying appropriate WBI data
WBI.reduced <- subset(WBI.raw, `Series Name` == "Population, total" | 
                        `Series Name` == "Adjusted net national income per capita (current US$)" |
                        `Series Name` == "GDP per capita (current US$)" |
                        `Series Name` == "Intentional homicides (per 100,000 people)" |
                        `Series Name` == "Poverty headcount ratio at $1.90 a day (2011 PPP) (% of population)" |
                        `Series Name` == "Poverty headcount ratio at $3.20 a day (2011 PPP) (% of population)" |
                        `Series Name` =="CPIA transparency, accountability, and corruption in the public sector rating (1=low to 6=high)") %>%
  rename(`2009` = `2009 [YR2009]`,
         `2010` = `2010 [YR2010]`,
         `2011` = `2011 [YR2011]`,
         `2012` = `2012 [YR2012]`,
         `2013` = `2013 [YR2013]`,
         `2014` = `2014 [YR2014]`,
         `2015` = `2015 [YR2015]`,
         `2016` = `2016 [YR2016]`,
         `2017` = `2017 [YR2017]`,
         `2018` = `2018 [YR2018]`) %>%
  pivot_longer(c(`2009`:`2018`), names_to = "year", values_to = "total") %>%
  group_by(`Country Name`, `Series Name`)%>%
  fill(total) %>%
  ungroup()%>%
  filter(year==2018) %>%
  group_by(`Country Name`) %>% 
  pivot_wider(names_from = `Series Name`, values_from = `total`)

WBI.reduced$population.total <- as.numeric(WBI.reduced$`Population, total`)  
WBI.reduced$income.per.capita <- as.numeric(WBI.reduced$`Adjusted net national income per capita (current US$)`)  
WBI.reduced$gdp.per.capita <- as.numeric(WBI.reduced$`GDP per capita (current US$)`)  
WBI.reduced$homicides <- as.numeric(WBI.reduced$`Intentional homicides (per 100,000 people)`)  
WBI.reduced$poverty.190 <- as.numeric(WBI.reduced$`Poverty headcount ratio at $1.90 a day (2011 PPP) (% of population)`)  
WBI.reduced$poverty.320 <- as.numeric(WBI.reduced$`Poverty headcount ratio at $3.20 a day (2011 PPP) (% of population)`)  
WBI.reduced$CPIA <- as.numeric(WBI.reduced$`CPIA transparency, accountability, and corruption in the public sector rating (1=low to 6=high)`)  

WBI <- WBI.reduced %>%
  subset(select = c(`Country Name`, population.total, income.per.capita, gdp.per.capita, homicides, poverty.190, poverty.320, CPIA))

#Identifying appropriate HCI data
HCI.reduced <- subset(HCI.raw, `Indicator Name` == "Harmonized Test Scores" | 
                        `Indicator Name` == "Human Capital Index (HCI) (scale 0-1)") %>%
  group_by(`Country Name`) %>% 
  pivot_wider(names_from = `Indicator Name`, values_from = `2017`) %>%
  rename(`HLO` = `Harmonized Test Scores`,
         `HCI` = `Human Capital Index (HCI) (scale 0-1)`)

HLO <- HCI.reduced %>%
  subset(select = c(`Country Name`, HLO, HCI))

#Identifying appropriate gini data
gini <- gini.raw %>%
  pivot_longer(c(`1967`:`2018`), names_to = "year", values_to = "gini")%>%
  group_by(country)%>%
  fill(gini)%>%
  ungroup()%>%
  filter(year==2018) %>%
  subset(select = (-year))

rm(WBI.raw, WBI.reduced, HCI.raw, HCI.reduced, gini.raw)


# Data wrangling ----------------------------------------------------------

###Linking country data to WGM

wgm.2018.original$country <- as_factor(wgm.2018.original$countrynew)
wgm.2018.original$country <- as.character(wgm.2018.original$country)
table(wgm.2018.original$country)

#WGM - compare against map_data
country.code <- wgm.2018.original %>%
  group_by(country) %>%
  summarise(country = first(country)) %>%
  subset(select = (country))

WorldData <- map_data('world')
WorldData %>% filter(region != "Antarctica") -> WorldData
WorldData <- fortify(WorldData)
WorldData$region[WorldData$subregion=="Hong Kong"] <- "Hong Kong" #Adding in HK
WorldData$region[WorldData$subregion=="Macau"] <- "Macau" #Adding in Macau
data <- as.data.frame(WorldData$region)%>%
  group_by(`WorldData$region`) %>%
  summarise(`Country` = first(`WorldData$region`))

missing.countries <- country.code %>%
  anti_join(data, by = c("country" = "WorldData$region"))
table(WorldData$region)
table(missing.countries$country)

#WBI - compare against revised WGM
missing.countries <- country.code %>%
  anti_join(WBI, by = c("country" = "Country Name"))
table(missing.countries$country) #Taiwan not available
table(WBI$`Country Name`) #Taiwan not available

WBI$`Country Name`[WBI$`Country Name`=="Egypt, Arab Rep."] <- "Egypt"
WBI$`Country Name`[WBI$`Country Name`=="Gambia, The"] <- "Gambia"
WBI$`Country Name`[WBI$`Country Name`=="Iran, Islamic Rep."] <- "Iran"
WBI$`Country Name`[WBI$`Country Name`=="Cote d'Ivoire"] <- "Ivory Coast"
WBI$`Country Name`[WBI$`Country Name`=="Kyrgyz Republic"] <- "Kyrgyzstan"
WBI$`Country Name`[WBI$`Country Name`=="Lao PDR"] <- "Laos"
WBI$`Country Name`[WBI$`Country Name`=="Macedonia, FYR"] <- "Macedonia"
WBI$`Country Name`[WBI$`Country Name`=="West Bank and Gaza"] <- "Palestine"
WBI$`Country Name`[WBI$`Country Name`=="Russian Federation"] <- "Russia"
WBI$`Country Name`[WBI$`Country Name`=="Congo, Rep."] <- "Republic of Congo" 
WBI$`Country Name`[WBI$`Country Name`=="Slovak Republic"] <- "Slovakia"
WBI$`Country Name`[WBI$`Country Name`=="Korea, Rep."] <- "South Korea"
WBI$`Country Name`[WBI$`Country Name`=="Eswatini"] <- "Swaziland"
WBI$`Country Name`[WBI$`Country Name`=="United Kingdom"] <- "UK"
WBI$`Country Name`[WBI$`Country Name`=="United States"] <- "USA"
WBI$`Country Name`[WBI$`Country Name`=="Venezuela, RB"] <- "Venezuela"
WBI$`Country Name`[WBI$`Country Name`=="Yemen, Rep."] <- "Yemen"

#HLO - compare against revised WGM
missing.countries <- country.code %>%
  anti_join(HLO, by = c("country" = "Country Name"))
table(missing.countries$country) #Belarus, Bolivia, Libya, Taiwan, Turkmenistan, Uzbekistan, Venezuela not available
table(HLO$`Country Name`) 

HLO$`Country Name`[HLO$`Country Name`=="Egypt, Arab Rep."] <- "Egypt"
HLO$`Country Name`[HLO$`Country Name`=="Gambia, The"] <- "Gambia"
HLO$`Country Name`[HLO$`Country Name`=="Iran, Islamic Rep."] <- "Iran"
HLO$`Country Name`[HLO$`Country Name`=="Cote d'Ivoire"] <- "Ivory Coast"
HLO$`Country Name`[HLO$`Country Name`=="Kyrgyz Republic"] <- "Kyrgyzstan"
HLO$`Country Name`[HLO$`Country Name`=="Lao PDR"] <- "Laos"
HLO$`Country Name`[HLO$`Country Name`=="Macedonia, FYR"] <- "Macedonia"
HLO$`Country Name`[HLO$`Country Name`=="West Bank and Gaza"] <- "Palestine"
HLO$`Country Name`[HLO$`Country Name`=="Congo, Rep."] <- "Republic of Congo" 
HLO$`Country Name`[HLO$`Country Name`=="Russian Federation"] <- "Russia"
HLO$`Country Name`[HLO$`Country Name`=="Slovak Republic"] <- "Slovakia"
HLO$`Country Name`[HLO$`Country Name`=="Korea, Rep."] <- "South Korea"
HLO$`Country Name`[HLO$`Country Name`=="Eswatini"] <- "Swaziland"
HLO$`Country Name`[HLO$`Country Name`=="United Kingdom"] <- "UK"
HLO$`Country Name`[HLO$`Country Name`=="United States"] <- "USA"
HLO$`Country Name`[HLO$`Country Name`=="Yemen, Rep."] <- "Yemen"

#Gini - compare against revised WGM
missing.countries <- country.code %>%
  anti_join(gini, by = "country")
table(missing.countries$country) ##Afghanistan, Cambodia, Kosovo, Kuwait, Libya, NewZeland, Saudo Arabia, Singapore, Taiwan not available
table(gini$country) 

gini$country[gini$country=="Cote d'Ivoire"] <- "Ivory Coast"
gini$country[gini$country=="Kyrgyz Republic"] <- "Kyrgyzstan"
gini$country[gini$country=="Lao"] <- "Laos"
gini$country[gini$country=="North Macedonia"] <- "Macedonia" #Wrong in NHB paper??
gini$country[gini$country=="Congo, Rep."] <- "Republic of Congo" 
gini$country[gini$country=="Slovak Republic"] <- "Slovakia"
gini$country[gini$country=="United Kingdom"] <- "UK"
gini$country[gini$country=="United States"] <- "USA"


#Coding individual level data

#Gender
attributes(wgm.2018.original$WP1219)
table(wgm.2018.original$WP1219)
wgm.2018.original$male <- ifelse(wgm.2018.original$WP1219==1, 1, 0)
table(wgm.2018.original$male)

#Age
table(wgm.2018.original$WP1220)
wellcome.country$age_c <- wellcome.country$WP1220 - mean(wellcome.country$WP1220, na.rm = TRUE)

#Education
attributes(wgm.2018.original$WP3117)
table(wgm.2018.original$WP3117)
wgm.2018.original$ed_mid <- ifelse(wgm.2018.original$WP3117==2, 1, 0)
wgm.2018.original$ed_mid[wgm.2018.original$WP3117==4 | wgm.2018.original$WP3117==5] <- NA
wgm.2018.original$ed_high <- ifelse(wgm.2018.original$WP3117==3, 1, 0)
wgm.2018.original$ed_high[wgm.2018.original$WP3117==4 | wgm.2018.original$WP3117==5] <- NA
table(wgm.2018.original$ed_mid)
table(wgm.2018.original$ed_high)

#Income
wgm.2018.original$lg_inc2 <- log1p(wgm.2018.original$income_2)
summary(wgm.2018.original$lg_inc2)
wgm.2018.original$lg_inc2[wgm.2018.original$lg_inc2==0] <- NA

#Vaccine support
wgm.2018.original %>%
  subset(., select = c(WP20010, WP20011, WP20012, WP20013, WP20014, WP20015, WP20016, WP20017, WP20018)) %>%
  sapply(attr,"label")

table(wgm.2018.original$WP20010)
wgm.2018.original$vac_child_bin2 <- ifelse(wgm.2018.original$WP20010==1, 1, 0)
wgm.2018.original$vac_child_bin2[wgm.2018.original$WP20010==98 | wgm.2018.original$WP20010==99] <- NA
table(wgm.2018.original$vac_child_bin2)

table(wgm.2018.original$WP20013)
wgm.2018.original$vac_safe_bin2 <- ifelse(wgm.2018.original$WP20013==1, 1, 0)
wgm.2018.original$vac_safe_bin2[wgm.2018.original$WP20013==98 | wgm.2018.original$WP20013==99] <- NA
table(wgm.2018.original$vac_safe_bin2)

table(wgm.2018.original$WP20016)
wgm.2018.original$vac_effect_bin2 <- ifelse(wgm.2018.original$WP20016==1, 1, 0)
wgm.2018.original$vac_effect_bin2[wgm.2018.original$WP20016==98 | wgm.2018.original$WP20016==99] <- NA
table(wgm.2018.original$vac_effect_bin2)

wgm.2018.original$vaccine <- wgm.2018.original$vac_child_bin2 + wgm.2018.original$vac_safe_bin2 + wgm.2018.original$vac_effect_bin2 
table(wgm.2018.original$vaccine)
wgm.2018.original$vaccine_bin2 <- ifelse(wgm.2018.original$vaccine==3, 1, 0)
table(wgm.2018.original$vaccine_bin2)

# Measuring trust in science - IRT models ----------------------------------------------
wgm.2018.original %>%
  subset(., select = c(WP19991, WP19993, WP19996, WP19997, WP19998, WP19999, WP20000, WP20001, WP20007, WP20008)) %>%
  sapply(attr,"label")

#Full 7-item scale
wgm.2018.original %>%
  subset(., select = c(WP19991, WP19996, WP19997, WP19998, WP19999, WP20000, WP20001)) %>%
  sapply(attr,"label")

table(wgm.2018.original$WP19991)
table(wgm.2018.original$WP19996)
table(wgm.2018.original$WP19997)
table(wgm.2018.original$WP19998)
table(wgm.2018.original$WP19999)
table(wgm.2018.original$WP20000)
table(wgm.2018.original$WP20001)
wgm.2018.original$WP19991[wgm.2018.original$WP19991 ==98 | wgm.2018.original$WP19991==99] <- NA
wgm.2018.original$WP19996[wgm.2018.original$WP19996 ==98 | wgm.2018.original$WP19996==99] <- NA
wgm.2018.original$WP19997[wgm.2018.original$WP19997 ==98 | wgm.2018.original$WP19997==99] <- NA
wgm.2018.original$WP19998[wgm.2018.original$WP19998 ==98 | wgm.2018.original$WP19998==99] <- NA
wgm.2018.original$WP19999[wgm.2018.original$WP19999 ==98 | wgm.2018.original$WP19999==99] <- NA
wgm.2018.original$WP20000[wgm.2018.original$WP20000 ==98 | wgm.2018.original$WP20000==99] <- NA
wgm.2018.original$WP20001[wgm.2018.original$WP20001 ==98 | wgm.2018.original$WP20001==99] <- NA

wgm.2018.original <-wgm.2018.original %>% 
  mutate_at(vars(WP19991, WP19996, WP19997, WP19998, WP19999, WP20000, WP20001),
            list("2" = ~ 5 - .)) 

wgm.2018.irt <- subset(wgm.2018.original, select = c(WP19991_2, WP19996_2, WP19997_2, WP19998_2, WP19999_2, WP20000_2, WP20001_2))
irt.1 <- grm(wgm.2018.irt) #graded response model
coef(irt.1) #Invert values for reporting to match Stata
summary(irt.1)
science.trust <- factor.scores(irt.1, method = "EAP", resp.patterns = wgm.2018.irt)$score.dat  #These match stata , latent option

science.trust$trust_science_full <- science.trust$z1*(-1) #Invert scale so higher score means more trust
science.trust$trust_science_full[is.na(science.trust$WP19991_2) & is.na(science.trust$WP19996_2) & is.na(science.trust$WP19997_2) & is.na(science.trust$WP19998_2) & is.na(science.trust$WP19999_2) & is.na(science.trust$WP20000_2) & is.na(science.trust$WP20001_2)] <- NA

science.trust <- science.trust %>%
  rowid_to_column("ID") %>%
  subset(select = c(ID, trust_science_full))

wgm.2018.original <- wgm.2018.original %>%
  rowid_to_column("ID") %>%
  left_join(science.trust, by = "ID") 

#Medical professionals
wgm.2018.original %>%
  subset(., select = c(WP19993, WP20008)) %>%
  sapply(attr,"label")

table(wgm.2018.original$WP19993)
table(wgm.2018.original$WP20008)
wgm.2018.original$WP19993[wgm.2018.original$WP19993 ==98 | wgm.2018.original$WP19993==99] <- NA
wgm.2018.original$WP20008[wgm.2018.original$WP20008 ==98 | wgm.2018.original$WP20008==99] <- NA

wgm.2018.original <-wgm.2018.original %>% 
  mutate_at(vars(WP19993, WP20008),
            list("2" = ~ 5 - .)) 

wgm.2018.irt2 <- subset(wgm.2018.original, select = c(WP19993_2, WP20008_2))
irt.2 <- grm(wgm.2018.irt2) #graded response model
coef(irt.2) #Values matched to stata
summary(irt.2)
science.trust2 <- factor.scores(irt.2, method = "EAP", resp.patterns = wgm.2018.irt2)$score.dat  #These match stata , latent option
science.trust2$trust_med_advice <- science.trust2$z1 
science.trust2$trust_med_advice[is.na(science.trust2$WP19991_2) & is.na(science.trust2$WP19996_2)] <- NA

science.trust2 <- science.trust2 %>%
  rowid_to_column("ID") %>%
  subset(select = c(ID, trust_med_advice))

wgm.2018.original <- wgm.2018.original %>%
  left_join(science.trust2, by = "ID") 


# Merge all country data and tidy -----------------------------------------

wellcome.country <- wgm.2018.original %>%
  left_join(WBI, by = c("country" = "Country Name")) %>%
  left_join(HLO, by = c("country" = "Country Name")) %>%
  left_join(gini, by = "country")

rm(country.code, data, gini, HLO, missing.countries, WBI, WorldData, science.trust, science.trust2, wgm.2018.irt, wgm.2018.irt2)


# Evaluating the distribtion of trust in scientists - Using full trust scale [7-items - 11c, 12, 13, 14a, 14b, 15a, 15b] ------------------------
ggplot(data = wellcome.country, mapping = aes(x = trust_science_full)) + 
  geom_histogram(bins = 30, color = "black", fill= "white", na.rm=TRUE)

#Separately by country
ggplot(data = wellcome.country, mapping = aes(x = trust_science_full)) + 
  geom_histogram(bins = 30, color = "black", fill= "white", na.rm=TRUE) +
  facet_wrap(facets = wellcome.country$WP5)



# TABLE 1: Estimating location-scale model - Using full trust scale [7-items - 11c, 12, 13, 14a, 14b, 15a, 15b]-----------------------------------------
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#Full model
trust7_full.gm <- brm(bf(trust_science_full ~ 1  + scale(gdp.per.capita) + scale(HLO) + scale(gini) + scale(male, scale=FALSE) + I(age_c/10) + scale(ed_mid, scale=FALSE) + scale(ed_high, scale=FALSE) + scale(lg_inc2, center=TRUE) + (1 |s| countrynew), sigma ~ 1 + scale(gdp.per.capita) + scale(HLO) + scale(gini) + scale(male, scale=FALSE) + I(age_c/10) + scale(ed_mid, scale=FALSE) + scale(ed_high, scale=FALSE) + scale(lg_inc2, center=TRUE) + (1 |s| countrynew)),
                      data = wellcome.country,
                      family = gaussian(),
                      chains = 4, 
                      iter = 20000)
print(summary(trust7_full.gm), digits = 3)

mcmc_plot(trust7_full.gm, type = "trace")

#Extracting and saving location-scale residuals for subsequent analyses
effects.trust7_full.gm<-ranef(trust7_full.gm)
effects.trust7_full.gm<-as.data.frame(effects.trust7_full.gm)
effects.trust7_full.gm$Country <- rownames(effects.trust7_full.gm)
effects.trust7_full.gm$trust_u0 <- effects.trust7_full.gm$countrynew.Estimate.Intercept
effects.trust7_full.gm$vac_m2.5 <- effects.trust7_full.gm$countrynew.Q2.5.Intercept
effects.trust7_full.gm$vac_m97.5 <- effects.trust7_full.gm$countrynew.Q97.5.Intercept
effects.trust7_full.gm$trust_u1 <- effects.trust7_full.gm$countrynew.Estimate.sigma_Intercept
effects.trust7_full.gm$vac_sd2.5 <- effects.trust7_full.gm$countrynew.Q2.5.sigma_Intercept
effects.trust7_full.gm$vac_sd97.5 <- effects.trust7_full.gm$countrynew.Q97.5.sigma_Intercept
effects.trust7_full.gm <- effects.trust7_full.gm %>%
  subset(select = c(Country, trust_u0, trust_u1)) 

wellcome.effects <- merge(wellcome.country, effects.trust7_full.gm, by.x = "country", by.y = "Country", all.x=TRUE, all.y=TRUE)

#Plotting the residuals - u0 and u1 from location-scale model - to check assumptions
ggplot(data = effects.trust7_full.gm, mapping = aes(x = trust_u0)) + 
  geom_histogram(bins = 30, color = "black", fill= "white", na.rm=TRUE)

ggplot(data = effects.trust7_full.gm, mapping = aes(x = trust_u1)) + 
  geom_histogram(bins = 30, color = "black", fill= "white", na.rm=TRUE)

#Assessing the correlation of the location and scale residuals
ggplot(data=effects.trust7_full.gm, mapping = aes(x = trust_u0, y = trust_u1)) + 
  geom_point()
cor.test(effects.trust7_full.gm$trust_u0, effects.trust7_full.gm$trust_u1)

#Comparison against raw mean and SD
wellcome.country.mean <- wellcome.country %>%
  group_by(countrynew) %>%
  summarise(
    trust_mean = mean(trust_science_full, na.rm=TRUE),
    trust_sd = sd(trust_science_full, na.rm=TRUE)
  )

ggplot(data=wellcome.country.mean, mapping = aes(x = trust_mean, y = trust_sd)) + 
  geom_point()
cor.test(wellcome.country.mean$trust_mean, wellcome.country.mean$trust_sd)


#FIGURE 1: Plotted using full trust scale [7-items - 11c, 12, 13, 14a, 14b, 15a, 15b]-----------------------------------------
#Define plot
palette <- brewer.pal("Greys", n=9)
color.background = palette[1]
color.grid.major = palette[5]
color.axis.text = palette[6]
color.axis.title = palette[7]
color.title = palette[9]

#Extract estimated mean and SD for each country
effectsSD.trust_full.gm<-coef(trust7_full.gm)
effectsSD.trust_full.gm<-as.data.frame(effectsSD.trust_full.gm)
effectsSD.trust_full.gm$Country <- rownames(effectsSD.trust_full.gm)
effectsSD.trust_full.gm$trust_mean <- effectsSD.trust_full.gm$countrynew.Estimate.Intercept
effectsSD.trust_full.gm$vac_m2.5 <- effectsSD.trust_full.gm$countrynew.Q2.5.Intercept
effectsSD.trust_full.gm$vac_m97.5 <- effectsSD.trust_full.gm$countrynew.Q97.5.Intercept
effectsSD.trust_full.gm$trust_sd <- exp(effectsSD.trust_full.gm$countrynew.Estimate.sigma_Intercept)
effectsSD.trust_full.gm$vac_sd2.5 <- exp(effectsSD.trust_full.gm$countrynew.Q2.5.sigma_Intercept)
effectsSD.trust_full.gm$vac_sd97.5 <- exp(effectsSD.trust_full.gm$countrynew.Q97.5.sigma_Intercept)
effectsSD.trust_full.gm <- effectsSD.trust_full.gm %>%
  subset(select= c(Country, trust_mean, trust_sd, vac_sd2.5, vac_sd97.5))

effectsSD.trust_full.gm <- effectsSD.trust_full.gm %>% 
  mutate(ranking_m = rank(trust_mean, ties.method = 'first'),
         ranking_sd = rank(trust_sd, ties.method = 'first'), 
         mean_sd = mean(trust_sd))

ggplot()+
  geom_pointrange(data=effectsSD.trust_full.gm,mapping=aes(x=ranking_sd, y=trust_sd, ymin=vac_sd2.5,ymax=vac_sd97.5), position="identity", width=0.5, size=.1, color="black")+
  theme_bw() +
  # Set the entire chart region to a light gray color
  theme(panel.background=element_rect(fill=color.background, color=color.background)) +
  theme(plot.background=element_rect(fill=color.background, color=color.background)) +
  theme(axis.ticks=element_blank()) +
  # Set title and axis labels, and format these and tick marks
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=20))+
  xlab("Countries (ranked)") +
  ylab("Estimated SD for trust in science") +
  # Big bold line at y=0
  geom_hline(yintercept=effectsSD.trust_full.gm$mean_sd, size=.5, alpha=0.7,colour="#EF3B2C", linetype="twodash") + 
  geom_text_repel(aes(x=effectsSD.trust_full.gm$ranking_sd, y=effectsSD.trust_full.gm$trust_sd, label=ifelse(effectsSD.trust_full.gm$ranking_sd<10,as.character(effectsSD.trust_full.gm$Country),'')),                   
                  segment.alpha = .1, nudge_y = -4, size=2) +
  geom_text_repel(aes(x=effectsSD.trust_full.gm$ranking_sd, y=effectsSD.trust_full.gm$trust_sd, label=ifelse(effectsSD.trust_full.gm$ranking_sd>115,as.character(effectsSD.trust_full.gm$Country),'')),                   
                  segment.alpha = .1, nudge_y = 4, size=2) 


# TABLE 2: Estimating multilevel models - Using full trust scale [7-items - 11c, 12, 13, 14a, 14b, 15a, 15b] ----------------------------------------
wellcome.effects <- merge(wellcome.country, effects.trust7_full.gm, by.x = "country", by.y = "Country", all.x=TRUE, all.y=TRUE)

#Binary model (coding 4/5 as positive)

###Model 1
bin.1 <- glmer(vaccine_bin2 ~ trust_science_full + trust_u0 + (1 | countrynew), family = binomial("logit"), nAGQ=15, data = wellcome.effects) 
summary(bin.1) 

###Model 2
bin.2 <- glmer(vaccine_bin2 ~ trust_science_full + trust_u0*trust_u1 + trust_science_full*trust_u1 + (1 | countrynew), family = binomial("logit"), nAGQ=15, data = wellcome.effects) 
summary(bin.2) 

##Vaccine sub-scales
bin.1.child <- glmer(vac_child_bin2 ~ trust_science_full + trust_u0 + (1 | countrynew), family = binomial("logit"), nAGQ=15, data = wellcome.effects) 
bin.2.child <- glmer(vac_child_bin2 ~ trust_science_full + trust_u0*trust_u1 + trust_science_full*trust_u1 + (1 | countrynew), family = binomial("logit"), nAGQ=15, data = wellcome.effects) 
bin.1.safe <- glmer(vac_safe_bin2 ~ trust_science_full + trust_u0 + (1 | countrynew), family = binomial("logit"), nAGQ=15, data = wellcome.effects) 
bin.2.safe <- glmer(vac_safe_bin2 ~ trust_science_full + trust_u0*trust_u1 + trust_science_full*trust_u1 + (1 | countrynew), family = binomial("logit"), nAGQ=15, data = wellcome.effects) 
bin.1.effect <- glmer(vac_effect_bin2 ~ trust_science_full + trust_u0 + (1 | countrynew), family = binomial("logit"), nAGQ=15, data = wellcome.effects) 
bin.2.effect <- glmer(vac_effect_bin2 ~ trust_science_full + trust_u0*trust_u1 + trust_science_full*trust_u1 + (1 | countrynew), family = binomial("logit"), nAGQ=15, data = wellcome.effects) 

stargazer(bin.1.child, bin.2.child, bin.1.safe, bin.2.safe, bin.1.effect, bin.2.effect, type="text")
summary(bin.1.child)
summary(bin.2.child)
summary(bin.1.safe)
summary(bin.2.safe)
summary(bin.1.effect)
summary(bin.2.effect)


# FIGURE 2: Plotting interaction effects from model 2-------------------------------------------------------

#Panel 1: Country-level trust in science and vaccine confidence
#Identifying cut points
effects.trust7_full.gm$trust_u1_5 <-ntile(effects.trust7_full.gm$trust_u1, 5)
effects.trust7_full.gm %>%
  group_by(trust_u1_5) %>%
  summarise(
    trust_var = mean(trust_u1, na.rm=TRUE)
  )

eff7.country <- effect("trust_u0*trust_u1", bin.2, kr=TRUE,
                       xlevels=list(trust_u1 = c(-.15, -.06, 0, .06, .16)),
                       se=TRUE, confidence.level=.95, typical=mean)

trust.labs <- c(`-0.15` = "High trust consensus (Q1)", `-0.06` = "Q2", `0` = "Moderate trust consensus (Q3)", `0.06` = "Q4", `0.16` = "Weak trust consensus (Q5)")
data.country <- as.data.frame(eff7.country)
ggplot(data=data.country, aes(x=trust_u0, y = fit)) + 
  geom_smooth(method = "glm") + 
  geom_line(aes(y = lower,
                group=factor(trust_u1)), linetype =3) +
  geom_line(aes(y = upper,
                group=factor(trust_u1)), linetype =3) +
  geom_ribbon(aes(ymin = lower, ymax=upper), alpha=.08) +
  xlab("Average trust in science") +
  ylab("Marginal effects on vaccine confidence") +
  theme_light(base_size = 16) +  
  scale_y_continuous(trans = "probit") +
  facet_wrap(~ trust_u1,labeller=labeller(trust_u1=trust.labs), nrow=1) 

#Panel 2: Individual-level trust in science and vaccine support
eff.individual <- effect("trust_science_full*trust_u1", bin.2, kr=TRUE,
                         xlevels=list(trust_u1 = c(-.15, -.06, 0, .06, .16)),
                         se=TRUE, confidence.level=.95, typical=mean)

data.individual <- as.data.frame(eff.individual)
ggplot(data=data.individual, aes(x=trust_science_full, y = fit)) + 
  geom_smooth(method="glm") + 
  geom_line(aes(y = lower,
                group=factor(trust_u1)), linetype =3) +
  geom_line(aes(y = upper,
                group=factor(trust_u1)), linetype =3) +
  geom_ribbon(aes(ymin = lower, ymax=upper), alpha=.08) +
  xlab("Individual trust in science") +
  ylab("Marginal effects on vaccine confidence") +
  theme_light(base_size = 16) +  
  scale_y_continuous(trans = "probit") +
  facet_wrap(~ trust_u1,labeller=labeller(trust_u1=trust.labs), nrow=1) 


# SUPLEMENTARY MATERIALS ------------------------------------------

#SUPPLEMENTARY TABLES
#TABLE S1: IRT model results (invert to match Stata)
coef(irt.1) #Invert values for reporting to match Stata
summary(irt.1)

#TABLE S2: Location-scale model parameter estimates for trust in science across countries (imputed)
wellcome.country.imp <- wellcome.country %>%
  subset(select = c(countrynew, male, trust_science_full, age_c, ed_mid, ed_high, lg_inc2, gdp.per.capita, gini, HLO))
imputed_Data <- mice(wellcome.country.imp, m=5, maxit = 50, method = 'pmm')

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
trust_imp.gm <- brm_multiple(bf(trust_science_full ~ 1  + scale(gdp.per.capita) + scale(HLO) + scale(gini) + scale(male, scale=FALSE) + I(age_c/10) + scale(ed_mid, scale=FALSE) + scale(ed_high, scale=FALSE) + scale(lg_inc2, center=TRUE) + (1 |s| countrynew), sigma ~ 1 + scale(gdp.per.capita) + scale(HLO) + scale(gini) + scale(male, scale=FALSE) + I(age_c/10) + scale(ed_mid, scale=FALSE) + scale(ed_high, scale=FALSE) + scale(lg_inc2, center=TRUE) + (1 |s| countrynew)),
                             data = imputed_Data,
                             family = gaussian(),
                             chains = 4, 
                             iter = 20000)
print(summary(trust_imp.gm), digits = 3)


#TABLE S3: Interaction model for social norms, trust in science and vaccine confidence (imputed)
effects.imputed<-ranef(trust_imp.gm)
effects.imputed<-as.data.frame(effects.imputed)
effects.imputed$Country <- rownames(effects.imputed)
effects.imputed$trust_u0 <- effects.imputed$countrynew.Estimate.Intercept
effects.imputed$vac_m2.5 <- effects.imputed$countrynew.Q2.5.Intercept
effects.imputed$vac_m97.5 <- effects.imputed$countrynew.Q97.5.Intercept
effects.imputed$trust_u1 <- effects.imputed$countrynew.Estimate.sigma_Intercept
effects.imputed$vac_sd2.5 <- effects.imputed$countrynew.Q2.5.sigma_Intercept
effects.imputed$vac_sd97.5 <- effects.imputed$countrynew.Q97.5.sigma_Intercept
effects.imputed <- effects.imputed %>%
  subset(select = c(Country, trust_u0, trust_u1))

wellcome.effects.imputed <-merge(wellcome.country, effects.imputed, by.x = "country", by.y = "Country", all.x=TRUE, all.y=TRUE)
bin.1.imp <- glmer(vaccine_bin2 ~ trust_science_full + trust_u0 + (1 | countrynew), family = binomial("logit"), nAGQ=15, data = wellcome.effects.imputed) 
summary(bin.1.imp) 
bin.2.imp <- glmer(vaccine_bin2 ~ trust_science_full + trust_u0*trust_u1 + trust_science_full*trust_u1 + (1 | countrynew), family = binomial("logit"), nAGQ=15, data = wellcome.effects.imputed) 
summary(bin.2.imp) 

bin.1.child.imp <- glmer(vac_child_bin2 ~ trust_science_full + trust_u0 + (1 | countrynew), family = binomial("logit"), nAGQ=15, data = wellcome.effects.imputed) 
bin.2.child.imp <- glmer(vac_child_bin2 ~ trust_science_full + trust_u0*trust_u1 + trust_science_full*trust_u1 + (1 | countrynew), family = binomial("logit"), nAGQ=15, data = wellcome.effects.imputed) 
bin.1.safe.imp <- glmer(vac_safe_bin2 ~ trust_science_full + trust_u0 + (1 | countrynew), family = binomial("logit"), nAGQ=15, data = wellcome.effects.imputed) 
bin.2.safe.imp <- glmer(vac_safe_bin2 ~ trust_science_full + trust_u0*trust_u1 + trust_science_full*trust_u1 + (1 | countrynew), family = binomial("logit"), nAGQ=15, data = wellcome.effects.imputed) 
bin.1.effect.imp <- glmer(vac_effect_bin2 ~ trust_science_full + trust_u0 + (1 | countrynew), family = binomial("logit"), nAGQ=15, data = wellcome.effects.imputed) 
bin.2.effect.imp <- glmer(vac_effect_bin2 ~ trust_science_full + trust_u0*trust_u1 + trust_science_full*trust_u1 + (1 | countrynew), family = binomial("logit"), nAGQ=15, data = wellcome.effects.imputed) 

summary(bin.1.child.imp)
summary(bin.2.child.imp)
summary(bin.1.safe.imp)
summary(bin.2.safe.imp)
summary(bin.1.effect.imp)
summary(bin.2.effect.imp)


#TABLE S4: Location-scale model parameter estimates for trust in science (medical professionals) - 11e, 22
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
trustmed_full.gm <- brm(bf(trust_med_advice ~ 1  + scale(gdp.per.capita) + scale(HLO) + scale(gini) + scale(male, scale=FALSE) + I(age_c/10) + scale(ed_mid, scale=FALSE) + scale(ed_high, scale=FALSE) + scale(lg_inc2, center=TRUE) + (1 |s| countrynew), sigma ~ 1 + scale(gdp.per.capita) + scale(HLO) + scale(gini) + scale(male, scale=FALSE) + I(age_c/10) + scale(ed_mid, scale=FALSE) + scale(ed_high, scale=FALSE) + scale(lg_inc2, center=TRUE) + (1 |s| countrynew)),
                        data = wellcome.country,
                        family = gaussian(),
                        chains = 4, 
                        iter = 20000)
print(summary(trustmed_full.gm), digits = 3)


#TABLE S5: Interaction model for social norms, trust in science (medical professionals) and vaccine confidence 
effects.trustmed_full.gm<-ranef(trustmed_full.gm)
effects.trustmed_full.gm<-as.data.frame(effects.trustmed_full.gm)
effects.trustmed_full.gm$Country <- rownames(effects.trustmed_full.gm)
effects.trustmed_full.gm$trust_u0 <- effects.trustmed_full.gm$countrynew.Estimate.Intercept
effects.trustmed_full.gm$vac_m2.5 <- effects.trustmed_full.gm$countrynew.Q2.5.Intercept
effects.trustmed_full.gm$vac_m97.5 <- effects.trustmed_full.gm$countrynew.Q97.5.Intercept
effects.trustmed_full.gm$trust_u1 <- effects.trustmed_full.gm$countrynew.Estimate.sigma_Intercept
effects.trustmed_full.gm$vac_sd2.5 <- effects.trustmed_full.gm$countrynew.Q2.5.sigma_Intercept
effects.trustmed_full.gm$vac_sd97.5 <- effects.trustmed_full.gm$countrynew.Q97.5.sigma_Intercept
effects.trustmed_full.gm <- effects.trustmed_full.gm %>%
  subset(select = c(Country, trust_u0, trust_u1))

wellcome.effects.med <-merge(wellcome.country, effects.trustmed_full.gm, by.x = "country", by.y = "Country", all.x=TRUE, all.y=TRUE)
bin.1.med <- glmer(vaccine_bin2 ~ trust_science_full + trust_u0 + (1 | countrynew), family = binomial("logit"), nAGQ=15, data = wellcome.effects.med) 
summary(bin.1.med) 
bin.2.med <- glmer(vaccine_bin2 ~ trust_science_full + trust_u0*trust_u1 + trust_science_full*trust_u1 + (1 | countrynew), family = binomial("logit"), nAGQ=15, data = wellcome.effects.med) 
summary(bin.2.med) 

bin.1.child.med <- glmer(vac_child_bin2 ~ trust_science_full + trust_u0 + (1 | countrynew), family = binomial("logit"), nAGQ=15, data = wellcome.effects.med) 
bin.2.child.med <- glmer(vac_child_bin2 ~ trust_science_full + trust_u0*trust_u1 + trust_science_full*trust_u1 + (1 | countrynew), family = binomial("logit"), nAGQ=15, data = wellcome.effects.med) 
bin.1.safe.med <- glmer(vac_safe_bin2 ~ trust_science_full + trust_u0 + (1 | countrynew), family = binomial("logit"), nAGQ=15, data = wellcome.effects.med) 
bin.2.safe.med <- glmer(vac_safe_bin2 ~ trust_science_full + trust_u0*trust_u1 + trust_science_full*trust_u1 + (1 | countrynew), family = binomial("logit"), nAGQ=15, data = wellcome.effects.med) 
bin.1.effect.med <- glmer(vac_effect_bin2 ~ trust_science_full + trust_u0 + (1 | countrynew), family = binomial("logit"), nAGQ=15, data = wellcome.effects.med) 
bin.2.effect.med <- glmer(vac_effect_bin2 ~ trust_science_full + trust_u0*trust_u1 + trust_science_full*trust_u1 + (1 | countrynew), family = binomial("logit"), nAGQ=15, data = wellcome.effects.med) 

summary(bin.1.child.med)
summary(bin.2.child.med)
summary(bin.1.safe.med)
summary(bin.2.safe.med)
summary(bin.1.effect.med)
summary(bin.2.effect.med)


