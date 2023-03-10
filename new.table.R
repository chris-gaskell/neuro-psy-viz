library(tidyverse)
library(gt)

composites <- conv_scores %>% drop_na(composite)
subtests   <- conv_scores %>% filter(is.na(composite))

composites <-
  composites %>%
  select(test, domain.name, rank, composite, `95ci`, z, descriptor) %>%
  mutate(type = "Composites") %>%
  rename(name = domain.name, score = composite) %>%
  mutate(score = as.numeric(score),
         rank = as.character(nombre::nom_ord(rank, cardinal = F)))



subtests <-
  subtests %>%
  select(test, subtest.name, domain.name, rank, scaled.score, z, descriptor, cum.perc, base.rate) %>%
  mutate(type = "Subtests",
         base.rate = as.character(base.rate)
                                  ) %>%
  rename(name = subtest.name, score = scaled.score) %>%
  mutate(test = recode(test,
                       "wms_iv_process" = "wms_iv",
                       "wais_iv_LDS" = "wais_iv"),
         rank = as.character(nombre::nom_ord(rank, cardinal = F)),
         rank = coalesce(rank, cum.perc, base.rate),
  )

tests <-
  full_join(composites, subtests) %>%
  relocate(test, type, name, score, `95ci`) %>%
    mutate(score = ifelse(is.na(`95ci`), paste(score),
                          paste(score," (", `95ci`, ")", sep = "")
                          )) %>%
  mutate(score = na_if(score, "NA")) %>%
  mutate(across(where(is_character), str_to_title)) %>%
  mutate(
    test = str_to_upper(str_replace_all(test, "_", " ")),
    name = str_replace_all(name, "_", " "),
    name = str_remove(name, "Trails|Stroop|Design "),
    # name = str_to_with_abbreviations(name, str_to = str_to_title,
    #                                          abbreviations = c("BAI", "BDI", "GAD 7", "HADS", "PHQ 9", "II")),
    name = str_replace_all(name, "Ii", "II"),
    name = str_to_title(str_replace_all(name, "_", " "))
  ) %>%
  # mutate(test = factor(test, levels = c("TOPF", "WAIS IV"))) %>%
  # mutate(type = factor(type, levels = c("Composites", "Subtests"))) %>%
  mutate(test = fct_inorder(test)) %>%
  mutate(type = fct_inorder(type)) %>%
  arrange(test, type) %>%
  group_by(test, type) %>%
  mutate(rank = ifelse(is.na(base.rate), rank,
                  paste("br = ", as.character(base.rate)
                  ))) %>%
  mutate(rank = str_replace(rank, "br", "BR"),
         name = str_replace(name, "Ii", "II"),
         name = str_replace(name, "Iq", "IQ")
  )

  tests %>% gt(rowname_col = "name") %>%
  cols_hide(columns = c(domain.name, `95ci`, cum.perc, base.rate)) %>%
  cols_label(
    #raw = "Raw",
    score = "Score",
    name = "Name",
    #domain.name = "Domain",
    rank = "%",
    descriptor = "Descriptor",
    z = "Z",
    #base.rate = "Base Rate",
    #cum.perc = "Cum. %",
    #form = "Form"
    ) %>%
  tab_options(
    #row_group.background.color = "grey",
    row_group.font.size = 12,
    row_group.font.weight = "bold",
    column_labels.font.weight = "bold",
    table.font.size = 10,
    data_row.padding = px(2),
    table.width = pct(100),
    row_group.padding = px(5)
  ) %>%
  sub_missing(columns = 1:9, missing_text = "-") %>%
  gt::opt_row_striping(row_striping = T) %>%
  opt_stylize(style = 6, color = "gray") %>%
  tab_source_note(
    source_note = md("**Scaled** = scaled score,
                     **Rank** = percentile rank,
                     **Cum. %** = cumulative precentage")) %>%
  gt::cols_align(align = "center", columns = 4:9)



### Composites Table ####

score_card$composite.table <-
  score_card$conv_scores %>%
  ungroup() %>%
  drop_na(composite) %>%
  janitor::remove_empty(which = "cols") %>%
  select(c(
    "test", "domain.name", "completed.tests", "sum.scaled", "standard", "rank",
    "descriptor", "composite",
    "95ci"
  )) %>%
  group_by(test) %>%
  mutate(sum.scaled = ifelse(test == "topf", NA, sum.scaled),
         rank = ifelse(test == "topf", NA, rank),
         `95ci` = ifelse(test == "topf", NA, `95ci`)

  ) %>%
  mutate(
    test = str_to_upper(str_replace_all(test, "_", " ")),
    domain.name = str_to_title(str_replace_all(domain.name, "_", " ")),
    domain.name = str_replace(domain.name, "Iq", "IQ")
  ) %>%
  gt(rowname_col = "domain.name") %>%
  cols_hide(columns = c(standard)) %>%
  cols_label(
    descriptor = "Descriptor",
    sum.scaled = "Sum of SS",
    completed.tests = "Completed subtests",
    # domain.name = "Domain",
    composite = "Composite",
    rank = "Rank",
    `95ci` = "95% CI",
    # base.rate = "Base Rate",
    # classification = "Pass / Fail",
  ) %>%
  tab_options(
    row_group.background.color = "grey",
    row_group.font.size = 12,
    row_group.font.weight = "bold",
    table.font.size = 12,
    data_row.padding = px(2),
    column_labels.font.weight = "bold",
    table.width = pct(90),
    row_group.padding = px(5),
  ) %>%
  sub_missing(columns = 1:9, missing_text = "-") %>%
  gt::opt_row_striping(row_striping = T) %>%
  opt_stylize(style = 6, color = "gray") %>%
  tab_source_note(
    source_note = md("**Sum of SS** = sum of scaled scores,
                     **Rank** = percentile rank,
                     **CI** = confidence interval"
    ))





full_join(
  full_join(
    rbind(
      contrast.scores_wais %>%
        select(-critical.value) %>%
        mutate(test = "wais"),
      contrast.scores_wms %>%
        select(-c(critical.value, contrast.score.abs)) %>%
        mutate(test = "wms")
      ) %>%
      mutate(type = "domain.contrasts") %>%
      drop_na(contrast.score) %>%
      rename(name = domains, score = contrast.score, sig = critical.exceeded),
    index.scatter %>%
      rename(name = domain.key, score = scatter, sig = significant.scatter) %>%
      select(-c(smallest, largest))  %>%
      mutate(test = "wais")
    ) %>% mutate(score = as.character(score)) %>%
                   mutate(type = "index.scatter"),
  subtest_strengths %>%
    select(subtest.name, strength_vs_weakness) %>%
    rename(name = subtest.name, score = strength_vs_weakness) %>%
    mutate(type = "subtest.strengths")
  ) %>% print(n = 100)




