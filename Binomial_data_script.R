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

# We need to simplify our random effect structure to a huge extent to create a model with our data

binomial_model <- glmer(DV ~ Condition + 
                          (1 | Subject),
                        data = binomial_data,
                        family = binomial)

