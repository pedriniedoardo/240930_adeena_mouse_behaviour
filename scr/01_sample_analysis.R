# libraries ---------------------------------------------------------------
library(tidyverse)
library(finalfit)
library(GGally)
library(mixOmics)

# read in the data --------------------------------------------------------
df <- read_csv("../data/M83HindlimbScoringRawData.csv")


# wrangling ---------------------------------------------------------------
# recode the variables
# There are three treatment groups (1051, 1381, 1461) with n=24 per group (some animals died at some timepoints).
# Each behaviour was recorded at different timepoints (baseline, 30 days post inoculation, 60dpi, 90dpi...). 

# remove the treatments that are too scarce
df_filter <- df %>%
  # recode the timepoints in the tp column
  mutate(tp = str_extract_all(Stage,pattern = "\\d+d|Baseline") %>% unlist()) %>%
  mutate(tp = str_remove(tp,"d")) %>%
  # group_by(Stage,tp) %>% summarise()
  mutate(tp = case_when(tp == "Baseline" ~ "0",
                        T ~ tp)) %>%
  mutate(tp = str_pad(as.numeric(tp),side = "left",pad = 0,width = 3)) %>%
  mutate(tp_fct = paste0(tp,"d")) %>%
  mutate(tp_num = as.numeric(tp)) %>%
  dplyr::select(-c(tp,Stage)) %>%
  # recode the treatments
  mutate(Treat = str_remove_all(Treatment," Treatment Group")) %>%
  mutate(Treat = paste0("T",Treat)) %>%
  dplyr::select(-Treatment) %>%
  filter(!tp_fct %in% c("103d", "110d","180d")) %>%
  # recode the name of the variables
  rename(dur = `Duration (s)`,
         dist = `Distance (m)`,
         speed = `Mean speed (m/s)`,
         t_mob = `Time mobile (s)`,
         t_imm = `Time immobile (s)`,
         grip_s = `Grip Strength (s)`,
         kyp_s = `Kyphosis Score`,
         hind_ps = `Hind Limb Paralysis Score`,
         hind_cs = `Hind Limb Clasping Score`)


# wrangling ---------------------------------------------------------------
# explore the measured variables
ggpairs(df_filter %>% dplyr::select(-c("Test","Animal")))

# dur, hind_cs has variance 0 therefore can be removed
# also drop speed as it colinear with distance. keep only distance
# also t_mob e t_imm are colinear. keep only t_mob
# grip_s kyp_s hind_ps bring a very small contribution to the dataset keep them for the moment, but flag them. After some testign I decided to discard them. they hold to low information.
# t_mob and dist are correlated, there migth be problems with colinearity. for the moment keep it, but consider remove one of the two

# do not run any preprocesing on the data
df_filter_final <- df_filter %>%
  dplyr::select(-c(dur,hind_cs,speed,t_imm)) %>%
  dplyr::select(-c(grip_s,kyp_s,hind_ps))

# EDA ---------------------------------------------------------------------
# # there are not many measured variables, try to run the PCA anyway
# # remove the metadata
# df_filter_final_var <- df_filter_final %>% 
#   mutate(row_meta = paste(Test,Animal,tp_fct,Treat,sep = "|")) %>%
#   column_to_rownames("row_meta") %>%
#   dplyr::select(dist,t_mob)
#   # dplyr::select(dist,hind_ps)
#   # dplyr::select(dist:grip_s)
#   # dplyr::select(dist:t_mob)
#   # dplyr::select(dist:hind_ps)
# 
# # scree plot
# tune_pca <- tune.pca(df_filter_final_var, scale = TRUE)
# plot(tune_pca)
# 
# # run PCA pull all the components
# pca_all <- pca(df_filter_final_var, center = TRUE, scale = TRUE,ncomp = 2)
# 
# # plot sample PCA for component 1 and 2
# plotIndiv(pca_all,
#           comp = c(1:2),   # Specify components to plot
#           ind.names = F, # Show row names of samples
#           title = 'mouse behavioural')
# 
# test <- biplot(pca_all)
# 
# # see all the combinations for all the components
# pca_all$variates$X %>%
#   data.frame() %>%
#   # filter only the top 5 PC
#   # dplyr::select(PC1,PC2,PC3,PC4,PC5) %>%
#   ggpairs(upper = "blank")+
#   theme_bw() +
#   theme(strip.background = element_blank())
# 
# test <- biplot(pca_all)

# after filtering the variables, we ended up with only two variables which are colinear
df_filter_final %>%
  ggplot(aes(x=dist,y=t_mob))+geom_point()
# it is probably better to analyze the dataset considering only the dist variable.

# focus on the distance variable
# explor the dataset and relative metadata.
# adeena mentioned to focus on the timepoints and treat variables
df_filter_final %>%
  ggplot(aes(x=tp_fct,y=dist)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_jitter(width = 0.1),shape=1)
# there seems to be a clear effect of timepoints and distance regadless of the treatment

# try to plot the treatment variable regardelss of the time point
df_filter_final %>%
  ggplot(aes(x=Treat,y=dist)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_jitter(width = 0.1),shape=1)
# there seems to be a slight trend in one of the treatment condition

# try to plot both data
df_filter_final %>%
  ggplot(aes(x=Treat,y=dist,col=tp_fct)) +
  geom_boxplot(outlier.shape = NA,width=0.8) +
  geom_point(position = position_jitterdodge(jitter.width = 0.1),shape=1)

df_filter_final %>%
  ggplot(aes(x=tp_fct,y=dist,col=Treat)) +
  geom_boxplot(outlier.shape = NA,width=0.8) +
  geom_point(position = position_jitterdodge(jitter.width = 0.1),shape=1)

# there doesn't seems to be a strong interaction between the two variabels

# analysis ----------------------------------------------------------------
# use a simple linear model additive
fit_01 <- df_filter_final %>%
  mutate(Animal = factor(Animal)) %>%
  lm(dist ~ Treat + tp_fct, data = .)

summary(fit_01)

library(ggfortify)
autoplot(fit_01)

# # try the interaction
# fit_02 <- df_filter_final %>%
#   mutate(Animal = factor(Animal)) %>%
#   lm(dist ~ Treat * tp_fct, data = .)
# 
# summary(fit_02)
# 
# library(ggfortify)
# autoplot(fit_02)


# use finalfit
dependent <- "dist"
explanatory <- c("tp_fct", "Treat")
explanatory_multi <- c("tp_fct*Treat")

fit_03 <- df_filter_final %>% 
  finalfit(dependent,
           explanatory, 
           explanatory_multi, 
           keep_models = TRUE, metrics = TRUE)

fit_03

df_filter_final %>% 
  finalfit(dependent,
           explanatory, 
           keep_models = TRUE, metrics = TRUE)

df_filter_final %>% 
  finalfit(dependent = "dist",
           explanatory = c("tp_num","Treat"),
           metrics = TRUE)

df_filter_final %>% 
  finalfit(dependent = "dist",
           explanatory = c("tp_num","Treat"),explanatory_multi = c("tp_num*Treat"),
           keep_models = TRUE,metrics = TRUE)

df_filter_final %>% 
  finalfit(dependent = "dist",
           explanatory = c("tp_fct","Treat"),explanatory_multi = c("tp_fct*Treat"),
           keep_models = TRUE,metrics = TRUE)

df_filter_final %>% 
  as.data.frame() %>%
  ff_plot(dependent = dependent, explanatory = explanatory)

# misc --------------------------------------------------------------------
# try also to plot time as continous variable
df_filter_final %>%
  ggplot(aes(x=tp_num,y=dist)) +
  # geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_jitter(width = 5),shape=1)




