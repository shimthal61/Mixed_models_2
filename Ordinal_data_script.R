library(ordinal)

ordinal_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/16_mixed_models_pt2/master/data/ordinal_data.csv")

head(ordinal_data)

(ordinal_data <- ordinal_data %>% 
  mutate(Subject = factor(Subject),
         SportType = factor(SportType)) %>% 
  mutate(Order = order,
         Ratings = ratings) %>%
  mutate(VideoCondition = as.character(VideoCondition)) %>% 
  mutate(VideoCondition = factor(recode(VideoCondition,
                                        "2" = "Match",
                                        "3" = "Mismatch",
                                        "4" = "Neutral"))) %>% 
  dplyr::select(Subject, SportType, VideoCondition, Ratings)) #We need to tell r to use the select function from dplyr package

ordinal_data_tidied <- ordinal_data %>%
  mutate(Subject = factor(Subject), SportsType = factor(SportType)) %>%
  mutate(VideoCondition = as.character(VideoCondition)) %>%
  mutate(VideoCondition = factor(recode(VideoCondition, "2" = "Match", 
                                        "3" = "Mismatch", "4" = "Neutral"))) %>%
  dplyr::select(Subject, SportType, VideoCondition, Ratings)

# We need to do one more thing before our data set is ready for modelling. 
# We need to make sure that our DV is coded as an ordered factor.
# The name after the $ symbol refers to the column name in the data frame that preceded the $.

ordinal_data_tidied$Ratings <- as.ordered(ordinal_data$Ratings)

ordinal_data_tidied %>% 
  ggplot(aes(x = VideoCondition, y = Ratings, colour = VideoCondition)) +
  geom_point(alpha = 0.5, position = position_jitter(width = 0.1, seed = 42)) +
  guides(colour = 'none') +
  theme_minimal() +
  stat_summary(fun = "median", size = 1.5, alpha = .5, colour = 'black') +
  labs(x = "Video Condition",
       y = "Likert Rating",
       title = "Effect of Video type on Sport Ratings") +
  theme(text = element_text(size = 13))

# Let's build our model next

ordinal_model <- clmm(Ratings ~ VideoCondition + 
                        (1 + VideoCondition | Subject) +
                        (1 + VideoCondition | SportType), 
                      data = ordinal_data_tidied)   

# We can also drop our fixed effect to create a NULL model

null_ordinal_model <- clmm(Ratings ~ 1 + 
                             (1 + VideoCondition | Subject) +
                             (1 + VideoCondition | SportType), 
                           data = ordinal_data_tidied)

anova(ordinal_model, null_ordinal_model)

emmeans(ordinal_model, pairwise ~ VideoCondition)
