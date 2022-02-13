binomial_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/16_mixed_models_pt2/master/data/regressions.csv")

head(binomial_data)
str(binomial_data)

(binomial_data <- binomial_data %>% 
  mutate(Subject = factor(Subject),
         Item = factor(Subject),
         Condition = factor(Condition),
         Region = factor(Region)))

binomial_data %>% 
  group_by(Condition) %>% 
  summarise(mean_DV = mean(DV), SD_DV = sd(DV))

binomial_model <- glmer(DV ~ Condition + 
                          (1 + Condition | Subject) +
                          (1 + Condition | Item),
                          data = binomial_data, family = binomial)

# We need to massively simplify our random effect structure to create a model with our data

binomial_model <- glmer(DV ~ Condition + 
                          (1 | Subject),
                        data = binomial_data,
                        family = binomial)

summary(binomial_model) 

# We can have a look to see how many trials we did our each condition

binomial_data %>% 
  group_by(Condition, DV) %>% 
  summarise(n())

# We have far more trials where have no regressions. Not a great dataset. 

# We don't need to check for residuals when using binomial data. We instead used a binned residual plot. 
# We expect to find 95% to fall between the jagged lines, which it does:

binnedplot(fitted(binomial_model), resid(binomial_model, type = "response"))