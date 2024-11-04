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
  select(ccodealp, ipu_l_s, wdi_pop, vdem_libdem, ibp_obi) %>% 
  mutate(proximity = ipu_l_s/wdi_pop^(1/3),
         ipu_l_s = log(ipu_l_s),
         wdi_pop = log(wdi_pop)) %>% 
  left_join(df %>% select(ccodealp, LegislativeTransparency2))

summary(lm(LegislativeTransparency2 ~ proximity, data = qog))
summary(lm(LegislativeTransparency2 ~ ipu_l_s + wdi_pop, data = qog))
summary(lm(LegislativeTransparency2 ~ vdem_libdem, data = qog))
summary(lm(LegislativeTransparency2 ~ ibp_obi, data = qog))

dfLijphart <- read.csv('~/Downloads/L.csv') %>% 
  rename(ccodealp = country) %>% 
  select(ccodealp, exec_parties_1981_2010) %>% 
  left_join(df)

summary(lm(LegislativeTransparency2 ~ exec_parties_1981_2010, data = dfLijphart))

qog <- read_dta('~/Downloads/qog_std_cs_jan24_stata14.dta') %>% 
  select(ccodealp, wdi_gdpcappppcon2017, ibp_obi) %>% 
  left_join(df %>% select(ccodealp, LegislativeTransparency2))

gyt <- read.csv("~/Downloads/ReplicationData-PowerSharing-BormannEtAl.csv", header=T, sep=",") %>% 
  filter(year == 2009) %>% 
  select(country, inclusive) %>% 
  mutate(ccodealp = countrycode::countrycode(country, 'country.name', 'iso3c')) %>% 
  left_join(qog)

summary(lm(LegislativeTransparency2 ~ inclusive, data = gyt))
summary(lm(LegislativeTransparency2 ~ inclusive, data = gyt %>% filter(wdi_gdpcappppcon2017>1000)))
summary(lm(LegislativeTransparency2 ~ inclusive, data = gyt %>% filter(wdi_gdpcappppcon2017<10000)))

summary(lm(ibp_obi ~ inclusive, data = gyt))
summary(lm(ibp_obi ~ inclusive, data = gyt %>% filter(wdi_gdpcappppcon2017>1000)))
summary(lm(ibp_obi ~ inclusive, data = gyt %>% filter(wdi_gdpcappppcon2017<10000)))
