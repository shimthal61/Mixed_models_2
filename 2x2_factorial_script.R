library(tidyverse)
library(visdat)
library(ggthemes)
library(lme4)
library(emmeans)
library(lmerTest)
library(performance)
library(fitdistrplus)

factorial_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/15_mixed_models_pt1/master/data/2x2.csv")

head(factorial_data)

factorial_data <- factorial_data %>% 
  mutate(Subject = factor(Subject),
         Item = factor(Item),
         Sentence = factor(Sentence),
         Context = factor(Context))

# This command has returned 4 lots of NA values
factorial_data %>% 
  group_by(Sentence, Context) %>% 
  summarise(mean_RT = mean(RT), sd_RT = sd(RT), na.rm = FALSE)


# We can use the {visdat} function to identify the missing data. It looks as though the missing points are in the RT column.
vis_dat(factorial_data)
vis_miss(factorial_data)

# We can also find missing data by filtering the data
factorial_data %>% 
  filter(is.na(RT)) %>% 
  count() #Here we have 12 missing data points

(factorial_summary <- factorial_data %>% 
  filter(!is.na(RT)) %>% #This line excludes any missing data
  group_by(Sentence, Context) %>% 
  summarise(mean_RT = mean(RT), sd_RT = sd(RT)))

factorial_data %>% 
  filter(!is.na(RT)) %>% 
  ggplot(aes(x = Context:Sentence, y = RT, colour = Sentence)) +
  geom_violin(width = 0.6) +
  geom_point(alpha = 0.2, position = position_jitter(width = 0.1, seed = 42)) +
  stat_summary(fun.data = 'mean_cl_boot', colour = 'black', size = 0.5) +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, 8000, by = 2000),
                     limits = c(0, 8000)) +
  scale_x_discrete(labels = c("Negative:Negative" = "Negative",
                              "Negative:Positive" = "Negative",
                              "Positive:Negative" = "Positive",
                              "Positive:Positive" = "Positive")) +
  labs(x = "Context",
       y = "Reaction Time (ms.)",
       title = "The effect of Context and Sentence on Reaction Time") +
  theme(text = element_text(size = 13)) +
  coord_flip()

# Next, let's build an interaction plot

(labels <- factorial_summary %>% 
  filter(Context == "Positive") %>% 
  mutate(label = case_when(Sentence == "Negative" ~ "Negative Sentence",
                           Sentence == "Positive" ~ "Positive Sentence")))

factorial_summary %>% 
  ggplot(aes(x = Context, y = mean_RT)) +
  geom_line(size = 1.2, aes(group = Sentence, colour = Sentence)) +
  geom_point(size = 2.6, aes(colour = Sentence)) +
  geom_text(size = 5, aes(label = label,
                          colour = Sentence),
            data = labels,
            nudge_x = 0.29,
            nudge_y = -10) +
  guides(colour = 'none') +
  scale_y_continuous(breaks = seq(1450, 1650, by = 50),
                     limits = c(1450, 1650)) +
  theme_minimal() +
  labs(x = "Context",
       y = "Mean Reaction Time (ms.)",
       title = "Interaction between Context and Sentence Valence on Reaction Time")

# Before we build out model, we need to set the contrast coding for our factors. 
# Let’s set the contrast coding of our factors using deviation coding as follows. 
# This will allow us to compare each of our conditions to the average of the other conditions.

contrasts(factorial_data$Context) <- matrix(c(.5, -.5))
contrasts(factorial_data$Sentence) <- matrix(c(.5, -.5))

factorial_model <- lmer(RT ~ Context * Sentence +
                          (1 + Context + Sentence | Subject) +
                          (1 + Context + Sentence | Item),
                        data = factorial_data)

check_model(factorial_model)

# We may have a problem with our residuals. Let's look at the model using the Cullen and Frey plot:

missing_data_removed <- factorial_data %>% 
  filter(!is.na(RT))

descdist(missing_data_removed$RT)

# In the above plot, we can see that our data is close to the gamma distribution.
# We can try to model our data using a generalised linear model assuming sampling from the Gamma distribution using the
# glmer() function (generalised linear model).  It works the same, except we have to specify the distributional family

gamma_factorial_model <- glmer(RT ~ Context * Sentence +
                                 (1 + Context + Sentence | Subject) +
                                 (1 + Sentence | Item),
                               family = Gamma, 
                               nAGQ = 0,
                               data = factorial_data)

# In order to fit this model, I had to simplify the random effects terms and set nAGQ to 0 (its default is 1). 
# This means our parameter estimates are a little less exact than if we had gone with the default 
# (but at least the model converges on a solution).

summary(factorial_model) # Here, we have an significant main effect of context and interaction

summary(gamma_factorial_model) #Now, we just have a main interaction effect, which means pairwise comparisons

emmeans(factorial_model, pairwise ~ Context * Sentence, adjust = "none")

# In our factorial model, We can see the interaction is being driven by reading times to 
# Negative sentences in Negative vs. Positive contexts. Let's have a look at our gamma model

emmeans(gamma_factorial_model, pairwise ~ Context * Sentence, adjust = "none")

# The interaction is being driven by reading times to Negative sentences in Negative vs. Positive contexts.