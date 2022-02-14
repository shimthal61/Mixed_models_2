library(visdat)
library(buildmer)
factorial_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/16_mixed_models_pt2/master/data/factorial_data.csv")

head(factorial_data)

factorial_data <- factorial_data %>% 
  mutate(Subject = factor(Subject),
         Item = factor(Item),
         Prime = factor(Prime),
         Target = factor(Target))

factorial_data %>% 
  group_by(Prime, Target) %>% 
  summarise(mean_time = mean(Time),
            sd_time = sd(Time))

factorial_data <- factorial_data %>% 
  filter(!is.na(Time))

vis_miss(factorial_data)

factorial_data %>% 
  ggplot(aes(x = Prime:Target, y = Time, colour = Prime:Target)) +
  geom_violin(width = 0.5) +
  geom_point(alpha = 0.2, position = position_jitter(width = 0.1, seed = 42)) +
  guides(colour = 'none') +
  theme_minimal() +
  stat_summary(fun.data = 'mean_cl_boot', colour = 'black') +
  labs(y = "Reaction Time",
       title = "Effect of Prime and Target Valence on Reaction Time") +
  coord_flip()

# We need to set our contrasts

contrasts(factorial_data$Prime) <- matrix(c(.5, -.5))
contrasts(factorial_data$Target) <- matrix(c(.5, -.5))

# This is with all our random effects
factorial_model <- lmer(Time ~ Prime * Target +
                          (1 + Prime * Target | Subject) +
                          (1 + Prime * Target | Item),
                        data = factorial_data)


factorial_model <- lmer(Time ~ Prime * Target +
                          (1 | Subject) +
                          (1 | Item),
                        data = factorial_data)

null_factorial_model <- lmer(Time ~ (1 | Subject) + (1 | Item),
                             data = factorial_data)

anova(factorial_model, null_factorial_model) 

# Our factorial model has lower AIC and deviance

summary(factorial_model)

check_model(factorial_model)

descdist(factorial_data$Time)

# Our data is near the gamma distribution, so we need to build the appropriate model

gamma_factorial_model <- glmer(Time ~ Prime * Target +
                                 (1 | Subject) +
                                 (1 + Prime | Item),
                               nAGQ = 0,
                               family = Gamma,
                               data = factorial_data)

summary(gamma_factorial_model)


gamma_factorial_model <- buildmer(Time ~ Prime * Target +
                                    (1 | Subject) +
                                    (1 | Item),
                                  lme4::factorial_data)

emmeans(gamma_factorial_model, pairwise ~ Prime * Target, adjust = "none")

# Both our interaction effects are significant.


#----------------------------------------------------------------------------

binomial_data_chall <- read_csv("https://raw.githubusercontent.com/ajstewartlang/16_mixed_models_pt2/master/data/accuracy_data.csv")

head(binomial_data_chall)

binomial_data_chall <- binomial_data_chall %>% 
  mutate(Subject = factor(Subject),
         Face = factor(Face),
         FaceExpression = factor(FaceExpression))

(binomial_desc <- binomial_data_chall %>% 
  group_by(FaceExpression) %>% 
  summarise(mean_acc = mean(Acc), sd_acc = sd(Acc)))

binomial_desc %>% 
  ggplot(aes(x = FaceExpression, y = mean_acc, colour = FaceExpression)) +
  geom_point(size = 3) +
  theme_minimal()

binomial_model_chall <- glmer(Acc ~ FaceExpression +
                                (1 | Subject),
                              data = binomial_data_chall,
                              family = binomial)  

summary(binomial_model_chall)

null_binomial_model_chall <- glmer(Acc ~ (1 | Subject),
                                   data = binomial_data_chall,
                                   family = binomial) 

anova(binomial_model_chall, null_binomial_model_chall)

binnedplot(fitted(binomial_model_chall), resid(binomial_model_chall, type = "response"))

model <- buildmer(Acc ~ FaceExpression +
                    (1 + FaceExpression | Face) +
                    (1 + FaceExpression | Subject),
                  data = binomial_data_chall,
                  family = binomial)

anova(model, binomial_model_chall)


#-------------------------------------------------------------------------------
  
challenge_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/Goodness_of_fit/master/eye_data/RPs.csv")

head(challenge_data)

challenge_data <- challenge_data %>% 
  mutate(P.s = factor(P.s),
         Item = factor(Item),
         Cond = factor(Cond),
         Fit = factor(Fit)) %>% 
  rename(Subject = P.s) 
  
challenge_data %>%
  filter(!is.na(Antecedent)) %>% 
  filter(!is.na(Consequent)) %>% 
  filter(!is.na(spillover)) %>% 
  group_by(Fit) %>% 
  summarise(mean_antecedent = mean(Antecedent),
            mean_Consequent = mean(Consequent),
            mean_spillover = mean(spillover))

challenge_data %>%
  ggplot(aes(x = Fit, y = Antecedent, colour = Fit)) +
  geom_violin(width = 0.3) +
  geom_point (alpha = 0.3, position = position_jitter(width = 0.1, seed = 42)) +
  theme_minimal() +
  stat_summary(fun.data = 'mean_cl_boot', colour = 'black', size = 1) +
  guides(colour = 'none') +
  labs(y = "Antecendent Reaction Time (ms.)",
       title = "Effect of Fit on Antecedent Reaction Time") +
  coord_flip()

challenge_data %>%
  ggplot(aes(x = Fit, y = Consequent, colour = Fit)) +
  geom_violin(width = 0.4) +
  geom_point (alpha = 0.3, position = position_jitter(width = 0.1, seed = 42)) +
  theme_minimal() +
  stat_summary(fun.data = 'mean_cl_boot', colour = 'black', size = 1) +
  guides(colour = 'none') +
  labs(y = "Consequence Reaction Time (ms.)",
       title = "Effect of Fit on Consequent Reaction Time") +
  coord_flip()

challenge_data %>%
  ggplot(aes(x = Fit, y = spillover, colour = Fit)) +
  geom_violin(width = 0.4) +
  geom_point (alpha = 0.3, position = position_jitter(width = 0.1, seed = 42)) +
  theme_minimal() +
  stat_summary(fun.data = 'mean_cl_boot', colour = 'black', size = 1) +
  guides(colour = 'none') +
  labs(y = "Spillover Reaction Time (ms.)",
       title = "Effect of Fit on Spillover Reaction Time") +
  coord_flip()

challenge_model <- lmer(Antecedent ~ Fit +
                          (1 + Fit | Cond),
                        data = challenge_data)

summary(challenge_model)
