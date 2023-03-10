library(tidyverse)
library(readxl)

scorecard <- readxl::read_xlsx("score_card.xlsx")

new.scorecard <-
  scorecard %>%
  mutate_at(vars(1:3), ~ str_to_title(str_replace_all(., "_", " "))) %>%
    mutate(
      test = case_when(
        test == "Mood" ~ subtest.name,
        TRUE ~ test),
      test = case_when(
        test == 'Annette Handedness' ~ test,
        test == "Dot Counting" ~ test,
        test == "Peg Board" ~ test,
        test == "Coin In Hand" ~ test,
        test == "Hayling" ~ test,
        TRUE ~ str_to_upper(test)
        ),
    test = recode(test,
                  "HADS ANXIETY" = "HADS Anxiety",
                  "HADS DEPRESSION" = "HADS Depression",
                  "WMS IV PROCESS" = "WMS IV Process"
                  )
    ) %>%
  select(-c(raw, form))

new.scorecard <-
  new.scorecard %>%
  mutate(
    subtest.name = str_replace_all(subtest.name,"Rds", "RDS"),
    subtest.name = str_replace_all(subtest.name,"Soip", "SOIP"),
    subtest.name = str_replace_all(subtest.name,"Ii", "II"),
    subtest.name = str_replace_all(subtest.name,"Hads", "HADS"),
    subtest.name = str_replace_all(subtest.name,"Gad 7", "GAD-7"),
    subtest.name = str_replace_all(subtest.name,"Phq 9", "PHQ-9"),
    subtest.name = str_replace_all(subtest.name,"Bai", "BAI"),
    subtest.name = str_replace_all(subtest.name,"Bdi", "BDI"),
    subtest.name = str_replace_all(subtest.name,"Gnt", "GNT"),
         ) #%>%
  unique() %>% print(n = 200)
