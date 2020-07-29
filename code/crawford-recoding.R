# Clean out

## Delete explanation of variables

crawford_data <- crawford_data[-1:-3,]

## Delete unneeded variables

crawford_data %<>% select(-c(Duration..in.seconds., Status,
                          RecipientLastName, RecipientFirstName, RecipientEmail,
                          ExternalReference, DistributionChannel, UserLanguage))

## Duration
crawford_data$Q_TotalDuration <- as.numeric(crawford_data$Q_TotalDuration)

# Covariates

## Sociodemographics

crawford_data %<>% rename(consent = Q23)
crawford_data %<>% rename(age = Q9)
crawford_data %<>% rename(sex = Q1)
crawford_data %<>% rename(state = Q10_1)
crawford_data %<>% rename(education = Q4)

## Ideology

### Continuous
crawford_data %<>% rename(libcon = Q5)
crawford_data %<>% mutate(libcon = factor(libcon,
                                       ordered = TRUE,
                                       levels = c("Extremely liberal", 
                                                  "Liberal", 
                                                  "Slightly liberal", 
                                                  "Moderate; middle of the road",
                                                  "Slightly conservative", 
                                                  "Conservative", 
                                                  "Extremely conservative")))
crawford_data %<>% mutate(libcon_num = as.numeric(libcon))

### Categorical 
crawford_data %<>% mutate(libcon_cat = case_when(libcon == "Extremely liberal" | 
                                                libcon == "Liberal" ~ "Liberal",
                                              libcon == "Moderate; middle of the road" | 
                                                libcon == "Slightly liberal" | 
                                                libcon == "Slightly conservative" ~ "Moderate", 
                                              libcon == "Conservative" | 
                                                libcon == "Extremely conservative" ~ "Conservative"))
crawford_data %<>% mutate(libcon_cat = factor(libcon_cat,
                                         ordered = TRUE,
                                         levels = c("Liberal", 
                                                    "Moderate",
                                                    "Conservative")))

## Partisanship

crawford_data %<>% rename(party = Q6)
crawford_data %<>% mutate(party = factor(party))
crawford_data %<>% mutate(party_rec = case_when(party == "Democratic party" ~ "Democratic",
                                             party == "Republican party" ~ "Republican",
                                             party == "None or 'independent'" ~ "Independent"))

# Outcomes

## True

crawford_data %<>% rename(report_true = Q20_105)
crawford_data %<>% mutate(report_true = as.numeric(report_true))

## Convincing

crawford_data %<>% rename(report_convincing = Q27)
crawford_data %<>% mutate(report_convincing = 
                         factor(report_convincing,
                                ordered = TRUE,
                                levels = c("Completely unconvincing", 
                                           "Quite unconvincing", 
                                           "Somewhat unconvincing", 
                                           "Neither convincing nor unconvincing",
                                           "Somewhat convincing", 
                                           "Quite convincing", 
                                           "Completely convincing")))
crawford_data %<>% mutate(report_convincing_num = as.numeric(report_convincing))

# Treatments

crawford_data %<>% select(-c(Task2.Climatechange.àlaWashburn.Skitka2018._DO_Q26, 
                          Task2.Climatechange.àlaWashburn.Skitka2018._DO_Q16,
                          Task2.Climatechange.àlaWashburn.Skitka2018._DO_Q13, 
                          Task2.Climatechange.àlaWashburn.Skitka2018._DO_Q33,
                          Task3.Affirmativeaction.àlaCrawfordetal.2013._DO_Q22,
                          Task3.Affirmativeaction.àlaCrawfordetal.2013._DO_Q27,
                          Task3.Affirmativeaction.àlaCrawfordetal.2013._DO_Q34,
                          Task3.Affirmativeaction.àlaCrawfordetal.2013._DO_Q20))

## Valence

crawford_data %<>%
  mutate(report_valence =
           case_when(Task3.Affirmativeaction.àlaCrawfordetal.2013._DO_left.congruent.stron == "2" 
                     ~ "left strong",
                     Task3.Affirmativeaction.àlaCrawfordetal.2013._DO_right.congruent == "2" 
                     ~ "right",
                     Task3.Affirmativeaction.àlaCrawfordetal.2013._DO_left.congruent == "2" 
                     ~ "left"))

## Congruence

crawford_data %<>%
  mutate(report_congruence = 
           case_when(report_valence == "left strong"  ~ libcon_num * (-1) + 4,
                     report_valence == "left" ~ libcon_num * (-1) + 4,
                     report_valence == "right" ~ libcon_num - 4))

crawford_data %<>%
  mutate(report_congruence_cat = 
           case_when(report_valence == "left strong" & libcon_cat == "Liberal" ~ "Congruent",
                     report_valence == "left strong" & libcon_cat == "Moderate" ~ "Neutral",
                     report_valence == "left strong" & libcon_cat == "Conservative" ~ "Incongruent",
                     report_valence == "left" & libcon_cat == "Liberal" ~ "Congruent",
                     report_valence == "left" & libcon_cat == "Moderate" ~ "Neutral",
                     report_valence == "left" & libcon_cat == "Conservative" ~ "Incongruent",
                     report_valence == "right" & libcon_cat == "Liberal" ~ "Incongruent",
                     report_valence == "right" & libcon_cat == "Moderate" ~ "Neutral",
                     report_valence == "right" & libcon_cat == "Conservative" ~ "Congruent"))
