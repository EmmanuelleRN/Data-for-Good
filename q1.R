library(tidyverse)

df <- read_csv("data/globalcount_data.csv")
glimpse(df)

to_bool
# changin to True/False
df <- df %>%
  mutate_if(is.character, as.factor) %>%
  mutate_at(c(vars(gender_not_listed:white), 
              vars(matches("issue"))), as.factor)

# Question 1 - What are the most critical issues that survey respondents face 
## a) Globally

df_issues <- df %>%
  pivot_longer(cols = starts_with("issue_person"),
               names_to = "issues_person",
               values_to = "issues_faced")

df_issues %>%
  #group_by(issues_person) %>%
  #summarise(n = sum(issues_faced)) %>%
  ggplot() +
  aes(y = issues_person, x = issues_faced) +
  geom_col()

df %>%
  pivot_longer(cols = starts_with("issue_city"),
               names_to = "issue_city",
               values_to = "issues_faced")  %>%
  ggplot() +
  aes(y = issue_city, x = issues_faced) +
  geom_col()

## b) within each identity category (e.g. by country, gender identity, 
## racial identity and by age)

df %>%
  pivot_longer(cols = starts_with("issue"),
               names_to = "issue",
               values_to = "issues_faced") %>%
  pivot_longer(cols = gender_not_listed:woman,
               names_to = "gender",
               values_to = "gender_n") %>%
  # pivot_longer(cols = racial_identity_not_listed:white,
  #              names_to = "race",
  #              values_to = "race_n") %>%
  group_by(iso3166, gender, gender_n, age, issue, issues_faced) %>%
  summarise(n = n()) %>% 
  filter(gender_n != 0, issues_faced != 0) %>% 
  ggplot() +
  aes(x = age, col = issue) +
  geom_boxplot() +
  facet_wrap(iso3166 ~ gender)

               