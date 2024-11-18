library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(tidyverse)
library(haven)
library(countrycode)

dfT <- read.csv('2PLtransparency.csv') %>% select(-X)
df <- read.csv('export-country-compare.csv', skip = 17) %>% 
  select(ISO.Code, Country) %>% 
  rename(iso = ISO.Code) %>% 
  left_join(dfT) %>% 
  drop_na %>% 
  mutate(ccodealp = countrycode::countrycode(iso,'iso2c','iso3c'))

qog <- read_dta('~/Downloads/qog_std_cs_jan24_stata14.dta') %>% 
  select(ccodealp, vdem_gcrrpt, wvs_confpar, ibp_obi, wbgi_gee, egov_egov) %>%  
  left_join(df %>% select(ccodealp, LegislativeTransparency2))

dfT2 <- read.csv('Tindex.csv') %>% 
  rename(ccodealp = isocode,
         dj_totalscore = dj_total.score) %>%
  select(-Countryname)

#dfT3 <- read.csv('RTI.csv') %>% 
#  select(Country, Total) %>% 
#  mutate(Country = countrycode::countryname(Country, destination = 'iso3c')) %>% 
#  rename(RTI = Total,
#         ccodealp = Country)
  

qog <- qog %>% left_join(dfT2) #%>% 
#  left_join(dfT3)

cor.test(qog$vdem_gcrrpt,qog$LegislativeTransparency2, use = "complete.obs")
cor.test(qog$wvs_confpar,qog$LegislativeTransparency2, use = "complete.obs")
cor.test(qog$wbgi_gee,qog$LegislativeTransparency2, use = "complete.obs")

cor.test(qog$egov_egov,qog$LegislativeTransparency2, use = "complete.obs")
cor.test(qog$ibp_obi,qog$LegislativeTransparency2, use = "complete.obs")
cor.test(qog$df_totalscore,qog$LegislativeTransparency2, use = "complete.obs")
cor.test(qog$dj_totalscore,qog$LegislativeTransparency2, use = "complete.obs")

# cor.test(qog$RTI,qog$LegislativeTransparency2, use = "complete.obs")


