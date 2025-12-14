library(tidyverse)
library(magrittr)
library(lme4)
library(lmerTest)
library(emmeans)
library(outliers)

data <- read.table('LL_LP_SAA_cytokine_data_0925.csv', sep = ",", header = TRUE, quote = "")
data %<>% mutate_at(c("ID", "group"), factor)
head(data)
str(data)
#add separator for new controls vs older data points
data$time <- ifelse(data$ID == 'Vesunna'|
                      data$ID == 'Rita'|
                      data$ID == 'Myla',
                    'new', 'old') %>% factor()

# plot all saa data
plot1 <- ggplot(data, aes(hrs, saa))
plot1 + geom_jitter(aes(color = group)) +
  geom_smooth(aes(color = group), se = FALSE) +
  geom_line(aes(group = ID))

# plot saa without horse AB as outlier
plot2 <- data %>% filter(ID != 'AB' & ID != 'Myla') %>% ggplot(aes(hrs, saa))
plot2 + geom_jitter(aes(color = group)) +
  geom_smooth(aes(color = group), se = FALSE) +
  geom_line(aes(group = ID))

# check time zero for initial outliers
saa_0 <- data %>% filter (hrs == 0) %>% .$saa
dixon.test(saa_0, two.sided=F)
  #yes, value of 1716 at time 0 is an outlier in horse AB
  #Check eotaxin, gcsf and IL8
eotaxin_0 <- data %>% filter (hrs == 0) %>% .$Eotaxin
dixon.test(eotaxin_0, two.sided = F)
gcsf_0 <- data %>% filter (hrs == 0) %>% .$G.CSF
dixon.test(gcsf_0, two.sided = F)
IL8_0 <- data %>% filter (hrs == 0) %>% .$IL.8
dixon.test(IL8_0, two.sided = F)
  #all yes, Myla is an outlier

# remove the outliers for analysis
data2 <- data %>% filter(ID != 'AB' & ID != 'Myla')

detach(package:plyr)
data_summary <- data2 %>% 
  group_by(hrs, group) %>%
  summarise(mean = mean(saa), sd = sd(saa), min = min(saa), max = max(saa))
#knitr::kable(data_summary)
kbl(data_summary, longtable = T, booktabs = T, align = "c") %>%
  column_spec(1, bold=T) %>%
  collapse_rows(columns = 1:2, latex_hline = "major", valign = "middle")

# create model
model_small <- lmer(saa ~ group*hrs + (1|ID), data = data2)
coef(summary(as(model_small, "lmerModLmerTest"), ddf="Kenward-Roger"))
# note the only real difference is over time, no groups are different in this analysis
# interestingly, the slope for LB40 is different over time while the others are not

# compare all 3 groups at each time point
emm_small2 <- emmeans(model_small, ~ group|hrs, at = list(hrs=c(0,0.5, 1, 2, 4, 8, 24, 48, 72)))
emm_small2
pairs(emm_small2) # there are small differences in 20 vs 40 at 48 & 72 hrs
                  # with 40 being lower than 20
contrast(emm_small2, "trt.vs.ctrl") # no differences now

# compare all horses over time without regard for group for saa data
model_small2 <- lmer(saa ~ factor(hrs) + (1|ID), data = data2)
summary(model_small2)
emm_small3 <- emmeans(model_small2, ~ hrs, at = list(hrs=c(0,0.5, 1, 2, 4, 8, 24, 48, 72)))
contrast(emm_small3, "trt.vs.ctrl")

# Eotaxin, G-CSF, IL1a, IL-5, IL-18, IL-6, IL-17A, IL-2, IL-4, IFNg, IL-8, IP-10, IL-10, TNFa
model_eotaxin <- lmer(Eotaxin ~ group*hrs + (1|ID), data = data2)
coef(summary(as(model_eotaxin, "lmerModLmerTest"), ddf="Kenward-Roger")) # Nothing there
emm_eotaxin <- emmeans(model_eotaxin, ~ group|hrs, at = list(hrs=c(0,0.5, 1, 2, 4, 8, 24, 48, 72)))
pairs(emm_eotaxin)
contrast(emm_eotaxin, "trt.vs.ctrl")
  #lots of differences - need to graph

model_gcsf <- lmer(G.CSF ~ group*hrs + (1|ID), data = data2)
coef(summary(as(model_gcsf, "lmerModLmerTest"), ddf="Kenward-Roger")) # Nothing there
emm_gcsf <- emmeans(model_gcsf, ~ group|hrs, at = list(hrs=c(0,0.5, 1, 2, 4, 8, 24, 48, 72)))
pairs(emm_gcsf)
contrast(emm_gcsf, "trt.vs.ctrl")
  #differences vs control at early time points - graph

model_IL5 <- lmer(IL.5 ~ group*hrs + (1|ID), data = data2)
coef(summary(as(model_IL5, "lmerModLmerTest"), ddf="Kenward-Roger")) # Nothing there
emm_IL5 <- emmeans(model_IL5, ~ group|hrs, at = list(hrs=c(0,0.5, 1, 2, 4, 8, 24, 48, 72)))
pairs(emm_IL5)
contrast(emm_IL5, "trt.vs.ctrl")
  #nothing
model_IL18 <- lmer(IL.18 ~ group*hrs + (1|ID), data = data2)
coef(summary(as(model_IL18, "lmerModLmerTest"), ddf="Kenward-Roger")) # Nothing there
emm_IL18 <- emmeans(model_IL18, ~ group|hrs, at = list(hrs=c(0,0.5, 1, 2, 4, 8, 24, 48, 72)))
pairs(emm_IL18)
contrast(emm_IL18, "trt.vs.ctrl")
  #20 vs 40 at 72 hrs??? - likely nothing

model_IL1a <- lmer(IL1a ~ group*hrs + (1|ID), data = data2)
coef(summary(as(model_IL1a, "lmerModLmerTest"), ddf="Kenward-Roger")) # Nothing there - comes close at 48 & 72
emm_IL1a <- emmeans(model_IL1a, ~ group|hrs, at = list(hrs=c(0,0.5, 1, 2, 4, 8, 24, 48, 72)))
pairs(emm_IL1a)
contrast(emm_IL1a, "trt.vs.ctrl")
  #nothing
model_IL.6 <- lmer(IL.6 ~ group*hrs + (1|ID), data = data2)
coef(summary(as(model_IL.6, "lmerModLmerTest"), ddf="Kenward-Roger")) # Nothing there - comes close at 48 & 72
emm_IL.6 <- emmeans(model_IL.6, ~ group|hrs, at = list(hrs=c(0,0.5, 1, 2, 4, 8, 24, 48, 72)))
pairs(emm_IL.6)
contrast(emm_IL.6, "trt.vs.ctrl")
  #20 vs 40 at 72 hrs??? - likely nothing, except 2nd cytokine

model_IL.17A <- lmer(IL.17A ~ group*hrs + (1|ID), data = data2)
coef(summary(as(model_IL.17A, "lmerModLmerTest"), ddf="Kenward-Roger")) # Nothing there
emm_IL.17A <- emmeans(model_IL.17A, ~ group|hrs, at = list(hrs=c(0,0.5, 1, 2, 4, 8, 24, 48, 72)))
pairs(emm_IL.17A)
contrast(emm_IL.17A, "trt.vs.ctrl")
  #nothing
model_IL.2 <- lmer(IL.2 ~ group*hrs + (1|ID), data = data2)
coef(summary(as(model_IL.2, "lmerModLmerTest"), ddf="Kenward-Roger")) # Nothing there
emm_IL.2 <- emmeans(model_IL.2, ~ group|hrs, at = list(hrs=c(0,0.5, 1, 2, 4, 8, 24, 48, 72)))
pairs(emm_IL.2)
contrast(emm_IL.2, "trt.vs.ctrl")
#20 vs 40 at 72 hrs??? - likely nothing, except 3rd cytokine?

model_IL.4 <- lmer(IL.4 ~ group*hrs + (1|ID), data = data2)
coef(summary(as(model_IL.4, "lmerModLmerTest"), ddf="Kenward-Roger")) # Nothing there
emm_IL.4 <- emmeans(model_IL.4, ~ group|hrs, at = list(hrs=c(0,0.5, 1, 2, 4, 8, 24, 48, 72)))
pairs(emm_IL.4)
contrast(emm_IL.4, "trt.vs.ctrl")
  #nothing
#####
model_IFNg <- lmer(IFNg ~ group*hrs + (1|ID), data = data2)
coef(summary(as(model_IFNg, "lmerModLmerTest"), ddf="Kenward-Roger")) # 20 & 40 diff at 48 & 72
emm_IFNg <- emmeans(model_IFNg, ~ group|hrs, at = list(hrs=c(0,0.5, 1, 2, 4, 8, 24, 48, 72)))
pairs(emm_IFNg)
contrast(emm_IFNg, "trt.vs.ctrl")
  #nothing
model_IL.8 <- lmer(IL.8 ~ group*hrs + (1|ID), data = data2)
coef(summary(as(model_IL.8, "lmerModLmerTest"), ddf="Kenward-Roger")) # 20 & 40 diff early on
emm_IL.8 <- emmeans(model_IL.8, ~ group|hrs, at = list(hrs=c(0,0.5, 1, 2, 4, 8, 24, 48, 72)))
pairs(emm_IL.8)
contrast(emm_IL.8, "trt.vs.ctrl")
  #LB20 vs control at early time points - argues a drug reaction??? plus 20 vs 40 is significant

model_IP.10 <- lmer(IP.10 ~ group*hrs + (1|ID), data = data2)
coef(summary(as(model_IP.10, "lmerModLmerTest"), ddf="Kenward-Roger")) # Nothing there
emm_IP.10 <- emmeans(model_IP.10, ~ group|hrs, at = list(hrs=c(0,0.5, 1, 2, 4, 8, 24, 48, 72)))
pairs(emm_IP.10)
contrast(emm_IP.10, "trt.vs.ctrl")

model_IL.10 <- lmer(IL.10 ~ group*hrs + (1|ID), data = data2)
coef(summary(as(model_IL.10, "lmerModLmerTest"), ddf="Kenward-Roger")) # Nothing there
emm_IL.10 <- emmeans(model_IL.10, ~ group|hrs, at = list(hrs=c(0,0.5, 1, 2, 4, 8, 24, 48, 72)))
pairs(emm_IL.10)
contrast(emm_IL.10, "trt.vs.ctrl")

model_TNFa <- lmer(TNFa ~ group*hrs + (1|ID), data = data2)
coef(summary(as(model_TNFa, "lmerModLmerTest"), ddf="Kenward-Roger")) # Nothing there
emm_TNFa <- emmeans(model_TNFa, ~ group|hrs, at = list(hrs=c(0,0.5, 1, 2, 4, 8, 24, 48, 72)))
pairs(emm_TNFa)
contrast(emm_TNFa, "trt.vs.ctrl")

# Check all cytokines for changes from baseline without regard for group

model_IL1B_nogroup <- lmer(IL1B ~ factor(hrs) + (1|ID), data = data2)
emm_IL1B_nogroup <- emmeans(model_IL1B_nogroup, ~ hrs, at = list(hrs=c(0,0.5, 1, 2, 4, 8, 24, 48, 72)))
contrast(emm_IL1B_nogroup, "trt.vs.ctrl")

model_IFNg_nogroup <- lmer(IFNg ~ factor(hrs) + (1|ID), data = data2)
emm_IFNg_nogroup <- emmeans(model_IFNg_nogroup, ~ hrs, at = list(hrs=c(0,0.5, 1, 2, 4, 8, 24, 48, 72)))
contrast(emm_IFNg_nogroup, "trt.vs.ctrl")

model_IL.10_nogroup <- lmer(IL.10 ~ factor(hrs) + (1|ID), data = data2)
emm_IL.10_nogroup <- emmeans(model_IL.10_nogroup, ~ hrs, at = list(hrs=c(0,0.5, 1, 2, 4, 8, 24, 48, 72)))
contrast(emm_IL.10_nogroup, "trt.vs.ctrl")

model_TNFa_nogroup <- lmer(TNFa ~ factor(hrs) + (1|ID), data = data2)
emm_TNFa_nogroup <- emmeans(model_TNFa_nogroup, ~ hrs, at = list(hrs=c(0,0.5, 1, 2, 4, 8, 24, 48, 72)))
contrast(emm_TNFa_nogroup, "trt.vs.ctrl")
