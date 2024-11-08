library(tidyverse)

temp <- read.csv('export-country-compare.csv', skip =17)
df1 <- temp[,c(1,2,5)]
colnames(df1) <- c('iso', 'country', 'openCommittee')
df1 <- df1 %>% mutate(openCommittee = case_when(openCommittee == 'Yes' ~ 1,
                                                openCommittee == 'No' ~ 0,
                                                .default = NA))

temp <- read.csv('export-country-compare-2.csv', skip =17)
df2 <- temp[,c(1,2,5)]
colnames(df2) <- c('iso', 'country', 'publicDrafts')
df2 <- df2 %>% mutate(publicDrafts = case_when(publicDrafts == 'All' ~ 1,
                                                publicDrafts == 'Some' ~ 0,
                                                publicDrafts == 'None' ~ 0,
                                                .default = NA),
                      publicDrafts = as.integer(publicDrafts))

temp <- read.csv('export-country-compare-3.csv', skip =16)
df3 <- temp[,c(1,2,5)]
colnames(df3) <- c('iso', 'country', 'FOIlaw')
df3 <- df3 %>% mutate(FOIlaw = case_when(FOIlaw == 'Yes' ~ 1,
                                                FOIlaw == 'No' ~ 0,
                                                .default = NA))

temp <- read.csv('export-country-compare-4.csv', skip =16)
df4 <- temp[,c(1,2,5)]
colnames(df4) <- c('iso', 'country', 'annualReport')
df4 <- df4 %>% mutate(annualReport = case_when(annualReport == 'Yes' ~ 1,
                                         annualReport == 'No' ~ 0,
                                         .default = NA))

temp <- read.csv('export-country-compare-5.csv', skip = 17)
df5 <- temp[,c(1,2,5)]
colnames(df5) <- c('iso', 'country', 'budget')
df5 <- df5 %>% mutate(budget = case_when(budget == 'Yes' ~ 1,
                                               budget == 'No' ~ 0,
                                               .default = NA))

temp <- read.csv('export-country-compare-6.csv', skip = 16)
df6 <- temp[,c(1,2,5)]
colnames(df6) <- c('iso', 'country', 'openPlenaries')
df6 <- df6 %>% mutate(openPlenaries = case_when(openPlenaries == 'Yes' ~ 1,
                                         openPlenaries == 'No' ~ 0,
                                         .default = NA))

temp <- read.csv('export-country-compare-7.csv', skip = 17)
df7 <- temp[,c(1,2,5)]
colnames(df7) <- c('iso', 'country', 'rollcall')
df7 <- df7 %>% mutate(rollcall = case_when(rollcall == 'All' ~ 1,
                                                rollcall == 'Some' ~ 0,
                                           rollcall == 'None' ~ 0,
                                           .default = NA),
                      rollcall = as.integer(rollcall))

temp <- read.csv('export-country-compare-8.csv', skip = 17)
df8 <- temp[,c(1,2,5)]
colnames(df8) <- c('iso', 'country', 'agendasCommittes')
df8 <- df8 %>% mutate(agendasCommittes = case_when(agendasCommittes == 'All' ~ 1,
                                                 agendasCommittes == 'Some' ~ 0,
                                               agendasCommittes == 'None' ~ 0,
                                               .default = NA),
                      agendasCommittes = as.integer(agendasCommittes))

temp <- read.csv('export-country-compare-9.csv', skip = 17)
df9 <- temp[,c(1,2,5)]
colnames(df9) <- c('iso', 'country', 'agendasPlenary')
df9 <- df9 %>% mutate(agendasPlenary = case_when(agendasPlenary == 'All' ~ 1,
                                                 agendasPlenary == 'Some' ~ 0,
                                                 agendasPlenary == 'None' ~ 0,
                                                 .default = NA),
                      agendasPlenary = as.integer(agendasPlenary))

temp <- read.csv('export-country-compare-10.csv', skip = 17)
df10 <- temp[,c(1,2,5)]
colnames(df10) <- c('iso', 'country', 'channel')
df10 <- df10 %>% mutate(channel = case_when(channel == 'Yes' ~ 1,
                                            channel == 'No' ~ 0,
                                           .default = NA))

df <- df2 %>% 
  #left_join(df1) %>% 
  left_join(df3) %>% 
  left_join(df4) %>% 
  left_join(df5) %>% 
  #left_join(df6) %>% 
  left_join(df7) %>% 
  left_join(df8) %>% 
  left_join(df9) %>% 
  left_join(df10) %>% 
  drop_na %>% 
  select(-iso) %>% 
  pivot_longer(
    cols = -country,  # Exclude the 'iso' column from pivoting
    names_to = "indicator",  # Name of the new column for indicator names
    values_to = "value"  # Name of the new column for indicator values
  )
  
# df <- df %>%
#  mutate(across(c(publicDrafts, rollcall, agendasOnline1, agendasOnline2), as.ordered))

library(brms)

fit <- brm(
  bf(value ~ 1 + (1 | country) + (1 | indicator), family = bernoulli()), 
  data = df, 
  chains = 4, 
  cores = 4, 
  iter = 5000
)

summary(fit)
coef(fit)
x <- coef(fit)
x <- x$country[,1,1]
transparency <- data.frame(Country = names(x), LegislativeTransparency = x)
rownames(transparency) <- NULL

x <- (df2[,3] + df3[,3] + df4[,3] + df5[,3] + df7[,3] + df8[,3] + df9[,3] + df10[,3])/8
x <- data.frame(Country = df2[,2], Naive = x) %>% drop_na


dfTest <- transparency %>% left_join(x)
cor(dfTest$LegislativeTransparency, dfTest$Naive)

write.csv(dfTest,'1PLtransparency.csv')

formula_va_2pl <- bf(value ~ exp(loggamma) * theta + xi,
                     loggamma ~ 1 + (1 | i | indicator),
                     theta ~ 1 + (1 | country),
                     xi ~ (1 | i | indicator),
                     nl = TRUE)

irt_priors <- 
  prior(normal(0, 1), class = 'b', nlpar = 'loggamma') +
  prior(normal(0, 2), class = 'b', nlpar = 'theta') +
  prior(normal(0, 2), class = 'b', nlpar = 'xi')

fit <- brm(
  formula = formula_va_2pl, 
  prior = irt_priors,
  data = df, 
  family = bernoulli(link = "probit"),
  chains = 4, 
  cores = 4, 
  iter = 10000,
  seed = 123,
  control = list(adapt_delta = 0.99, max_treedepth = 15)
)

summary(fit)
#plot(fit)
#coef(fit)
x <- coef(fit)
a <- x$indicator
x <- x$country[,1,1]
transparency <- data.frame(Country = names(x), LegislativeTransparency2 = x)
rownames(transparency) <- NULL

aDiff <- data.frame(item = rownames(a[,,1]),a[,,1])
rownames(aDiff) = NULL

ggplot(aDiff, aes(x = item, y = Estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Q2.5, ymax = Q97.5), width = 0.2) +
  coord_flip() +  # Flip coordinates for easier reading
  labs(
    title = "Discrimination",
    x = "Item",
    y = "Estimate"
  ) +
  theme_minimal()

a[,1,1]
exp(a[,1,1])
# Roll call and public drafts discriminate more, while FOI law and annual reports inform less.
a[,1,2]
# FOI laws and reports are easy to comply with, similarly a channel is also easy

aDiff <- data.frame(item = rownames(a[,,2]),a[,,2])
rownames(aDiff) = NULL

ggplot(aDiff, aes(x = item, y = Estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Q2.5, ymax = Q97.5), width = 0.2) +
  coord_flip() +  # Flip coordinates for easier reading
  labs(
    title = "Easiness",
    x = "Item",
    y = "Estimate"
  ) +
  theme_minimal()

dfTest <- dfTest %>% left_join(transparency)
cor(dfTest$LegislativeTransparency2, dfTest$Naive)
cor(dfTest$LegislativeTransparency2, dfTest$LegislativeTransparency)
cor(dfTest$LegislativeTransparency, dfTest$Naive)

write.csv(dfTest, '2PLtransparency.csv')
