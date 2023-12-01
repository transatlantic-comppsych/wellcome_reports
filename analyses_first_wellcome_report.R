library(tidyverse)
library(lme4)
library(lmerTest)

df_surprises <-  read.csv("all_pilots_with_mini_spin.csv")


dim(df_surprises)

unique(df_surprises$pilot_nr)
# 
# df_surprises %>% 
# group_by(pilot_nr) %>% 
# summarise(length(unique(Random_ID)))

# check n per subject
df_surprises   |> summarise(length(unique(Random_ID)), .by = pilot_nr)



# get averaged correlations per subject for each pilot
cors_by_pilot <- df_surprises %>%
  # filter(pilot_nr ==  "Pilot 10") %>% 
  group_by(pilot_nr, Random_ID) %>%
  summarize(cor=cor(SubjPE, Mood))


pilots <- unique(df_surprises$pilot_nr)

correlations_df <- cors_by_pilot  %>% 
  group_by(pilot_nr) %>% 
  summarise(mean_cor = mean(cor))

correlations_df <- correlations_df %>% arrange(match(pilot_nr, pilots))
correlations_df

# now plot across all pilots the relationship between subjective PE and Mood
pdf("mood_subj_PE_plts6_to_10.pdf")



pe_mood_plots <- list()

my_splits <- (split(df_surprises, df_surprises$pilot_nr))
my_splits <- my_splits[pilots] # make sure to re-arrange the order to be from 6-10 

for(i in 1:length(my_splits)){
  
  plot <- my_splits[[i]] %>% 
    ggplot(aes(x = SubjPE, y = Mood)) +
    geom_smooth(method = "lm", colour = "red") +
    geom_point(alpha = 0.2) +
    facet_wrap(~Random_ID)+
    ggtitle(paste(pilots[i], "overall r = ", round(correlations_df[i,2], 2)))
  
  # Store the plot in the list
  pe_mood_plots[[i]] <- plot
}

# Print  plots
for (i in 1: length(my_splits)) {
  print(pe_mood_plots[[i]])
}
dev.off()




library(lme4)
library(parameters)



# now run lme models for random interecept only and rint + random slope and choose between them

# first lmes with random intercept models
rint_models <- list()
for(i in 1:length(my_splits)){
  rint_models[[i]] <- lmer(Mood ~ SubjPE + (1| Random_ID), data = 
                             df_surprises[df_surprises$pilot_nr==pilots[i],], 
                           REML = FALSE, 
                           control = lmerControl(optimizer = "bobyqa"))
}
rint_models

# now  lmes with random slopes
rslope_models <- list()
for(i in 1:length(my_splits)){
  rslope_models[[i]] <- lmer(Mood ~ SubjPE + (SubjPE| Random_ID), data = 
                               df_surprises[df_surprises$pilot_nr==pilots[i],], 
                             REML = FALSE, 
                             control = lmerControl(optimizer = "bobyqa"))
}
rslope_models

# now compare between them
p_vals <- 0
for(i in 1: length(my_splits)){
  p_vals[i] <-  (anova(rint_models[[i]], rslope_models[[i]]))$`Pr(>Chisq)`[2]
}

format(p_vals, scientific = F) # the p-values show that there is always a significant difference



mix_models_per_pilot <- list() # the lme objects for each pilot
mix_models_coefficients <- list() # the coefficients for each pilot
std_param_mix_models_per_pilot <- list() # the standardised coefficients for each lme object
dfs_RE_raw_pe_mood <- list() # the dataframes that contain raw values and coefficeints (may not need this)
for(i in 1: length(my_splits)){
  
  mix_models_per_pilot[[i]] <-  lmer(Mood ~ SubjPE + (SubjPE| Random_ID), data = 
                                       df_surprises[df_surprises$pilot_nr==pilots[i],], 
                                     REML = FALSE, 
                                     control = lmerControl(optimizer = "bobyqa"))
  std_param_mix_models_per_pilot[[i]] <- parameters:: standardise_parameters( mix_models_per_pilot[[i]])
  
  mix_models_coefficients[[i]] <-  coef(mix_models_per_pilot[[i]])
  mix_models_coefficients[[i]] <- data.frame(mix_models_coefficients[[i]]$Random_ID)
  mix_models_coefficients[[i]]$Random_ID <- rownames(mix_models_coefficients[[i]])
  colnames(mix_models_coefficients[[i]]) <-c( "intercept", "slope", "Random_ID")
  
  #now merge these datasets with the raw values 
  dfs_RE_raw_pe_mood[[i]] <- left_join(my_splits[[i]], mix_models_coefficients[[i]], by = "Random_ID" )
  
}
names(std_param_mix_models_per_pilot) <- pilots
std_param_mix_models_per_pilot$`Pilot 7`$Std_Coefficient[1] # to get intercept for example

# display coefficients
df_std_coefficients <- data.frame(do.call(rbind,std_param_mix_models_per_pilot))
df_std_coefficients <- df_std_coefficients %>% 
   filter(Parameter != "(Intercept)") %>% 
 # mutate(experiment = names(std_param_mix_models_per_pilot))
   mutate(experiment = paste0("experiment ", 1:5)) %>% 
   relocate(experiment, .before = Std_Coefficient) %>% 
  dplyr::select(!c(Parameter, CI)) %>% 
  remove_rownames()
  
knitr:: kable(df_std_coefficients)

# Put all datasets together now 
df_all_surprise_experiments <- do.call(rbind,dfs_RE_raw_pe_mood)


# test the ICC, i.e. variance explained by random effects.
# first for IDs

test_icc_id <- lmer(Mood ~  (1| Random_ID), data = 
                      df_all_surprise_experiments, 
                    REML = FALSE, 
                    control = lmerControl(optimizer = "bobyqa")) 
icc_results_id <- performance::icc(test_icc_id)

icc_results_id


#  nesting by pilot (i.e. number of experiment)


test_icc_pilot <- lmer(Mood ~  (1| pilot_nr) , data = 
                         df_all_surprise_experiments, 
                       REML = FALSE, 
                       control = lmerControl(optimizer = "bobyqa")) 
icc_results_pilot <- performance::icc(test_icc_pilot)
icc_results_pilot

# it seems that pilot number explains very little of the variance


# now test whether adding slope improves fit

big_model_1 <- lmer (Mood ~ SubjPE + (1| Random_ID) ,
                     data = df_all_surprise_experiments, 
                     REML = FALSE, 
                     control = lmerControl(optimizer = "bobyqa"))


big_model_2 <- lmer (Mood ~ SubjPE + (SubjPE| Random_ID) ,
                     data = df_all_surprise_experiments, 
                     REML = FALSE, 
                     control = lmerControl(optimizer = "bobyqa"))
summary(big_model_2)
standard_beta <- parameters:: standardise_parameters (big_model_2)
AIC(big_model_1, big_model_2)

big_model_3 <- lmer (Mood ~ SubjPE*Social_Anxiety + (SubjPE| Random_ID) ,
                     data = df_all_surprise_experiments, 
                     REML = FALSE, 
                     control = lmerControl(optimizer = "bobyqa"))
summary(big_model_3)
anova(big_model_2, big_model_3)


# example of how to get positive slopes per dataframe here. Will g --------


df_all_surprise_experiments_with_anxiety_status <-df_all_surprise_experiments %>% 
  group_by(Random_ID) %>% 
  filter(row_number()==1) %>% 
  mutate(positve_mood_slopes = case_when(slope>0~1, slope <=0 ~0)) %>% 
  mutate(high_social_anxiety = case_when(Social_Anxiety=="high"~1, Social_Anxiety=="low"~0))

glimpse(df_all_surprise_experiments_with_anxiety_status)

only_first_row_df_all_surprise_experiments_with_anxiety_status <- df_all_surprise_experiments_with_anxiety_status %>% 
  distinct(Random_ID, .keep_all = TRUE)

only_first_row_df_all_surprise_experiments_with_anxiety_status %>% 
  count(high_social_anxiety, positve_mood_slopes)

table(only_first_row_df_all_surprise_experiments_with_anxiety_status$Social_Anxiety)
table(table(only_first_row_df_all_surprise_experiments_with_anxiety_status$positve_mood_slopes, 
            only_first_row_df_all_surprise_experiments_with_anxiety_status$high_social_anxiety))
perc_pos_slop <- prop.table(table( 
  only_first_row_df_all_surprise_experiments_with_anxiety_status$high_social_anxiety,
  only_first_row_df_all_surprise_experiments_with_anxiety_status$positve_mood_slopes))

chisq.test(table(only_first_row_df_all_surprise_experiments_with_anxiety_status$positve_mood_slopes, 
                 only_first_row_df_all_surprise_experiments_with_anxiety_status$high_social_anxiety))


intercept <- mean(df_all_surprise_experiments$intercept)
slope <- mean(df_all_surprise_experiments$slope)

ggplot(df_all_surprise_experiments, aes(x=SubjPE, y=Mood)) +
  geom_smooth(method = "lm", size = 0.5, se = FALSE, aes(group=Random_ID, color=Social_Anxiety)) +
  scale_color_manual(values = c("low" = "lightblue", "high" = "pink")) +
  geom_abline(intercept = intercept, slope = slope, color="purple", linetype="dashed", size=1) +
  xlab("Subjective Prediction Error: feedback - prediction") + 
  ylab("Mood") +
  # theme_minimal() +
  # theme(axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 9)) +
  # annotate("text", x = Inf, y = Inf, label = paste("positive slope without social anxiety =", 100*round(perc_pos_slop[1,2],2), "%"), hjust = 1.1, vjust = 90, color = "black", size = 3.7) +
  # annotate("text", x = Inf, y = Inf, label = paste("positive slope with social anxiety =", 100*round(perc_pos_slop[2,2],2), "%"), hjust = 1.1, vjust = 92, color = "black", size = 3.7)+
  ggtitle("Relationship between Mood and Surprises",
          subtitle = paste("estimated slopes of the association in n = ", 
                           length(unique(df_all_surprise_experiments$Random_ID))))+
  theme(plot.title = element_text(size=14), plot.subtitle = element_text(size = 10))+
  theme(legend.title = element_text(size = 9), legend.text = element_text(size = 8))+
  theme(axis.title = element_text(size = 10)) +
  annotate("label", x = 0, y = 55, label = paste("beta = ", round(standard_beta$Std_Coefficient[2],2), ", 95%CI = ",
                                                 round(standard_beta$CI_low[2],2), "-", 
                                                 round(standard_beta$CI_high[2],2))) 


# now create one for each group
# 
# 
# intercept_high_social_anx <- mean(df_all_surprise_experiments
#                                   [df_all_surprise_experiments$Social_Anxiety == "high",]$intercept)
# slope_high_social_anx <- mean(df_all_surprise_experiments
#                               [df_all_surprise_experiments$Social_Anxiety == "high",]$slope)
# 
# intercept_low_social_anx <- mean(df_all_surprise_experiments
#                                  [df_all_surprise_experiments$Social_Anxiety == "low",]$intercept)
# slope_low_social_anx <- mean(df_all_surprise_experiments
#                              [df_all_surprise_experiments$Social_Anxiety == "low",]$slope)
# 
# ggplot(df_all_surprise_experiments, aes(x=SubjPE, y=Mood)) +
#   geom_smooth(method = "lm", size = 0.5, se = FALSE, aes(group=Random_ID, color=Social_Anxiety)) +
#   scale_color_manual(values = c("low" = "lightblue", "high" = "pink")) +
#   geom_abline(intercept = intercept_high_social_anx, slope_high_social_anx = slope, color="pink", linetype="dashed", size=2) +
#   geom_abline(intercept = intercept_low_social_anx, slope_low_social_anx = slope, color="lightblue", linetype="dashed", size=2) +
#   xlab("Subjective Prediction Error: feedback - prediction") + 
#   ylab("Mood") +
#   # theme_minimal() +
#   # theme(axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 9)) +
#   # annotate("text", x = Inf, y = Inf, label = paste("positive slope without social anxiety =", 100*round(perc_pos_slop[1,2],2), "%"), hjust = 1.1, vjust = 90, color = "black", size = 3.7) +
#   # annotate("text", x = Inf, y = Inf, label = paste("positive slope with social anxiety =", 100*round(perc_pos_slop[2,2],2), "%"), hjust = 1.1, vjust = 92, color = "black", size = 3.7)+
#   ggtitle("Relationship between Mood and Subjective Prediction Errors",
#           subtitle = paste("estimated slopes of the association in n = ", 
#                            length(unique(df_all_surprise_experiments$Random_ID))))+
#   theme(plot.title = element_text(size=22), plot.subtitle = element_text(size = 18))+
#   theme(legend.title = element_text(size = 16), legend.text = element_text(size = 14))+
#   theme(axis.title = element_text(size = 16)) +
#   annotate("label", x = 0, y = 55, label = paste("beta = ", round(standard_beta$Std_Coefficient[2],2), ", 95%CI = ",
#                                                  round(standard_beta$CI_low[2],2), "-", 
#                                                  round(standard_beta$CI_high[2],2))) 
# 
# 


