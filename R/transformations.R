library(here)
source(here("R", "analysis-utils.R"))

log_odds = function(p){
  return(log(p/(1-p)))
}

transform_rating = function(rating, gamma, p0){
  c = (log_odds(rating) - (1-gamma) * log_odds(p0)) / gamma
  p = exp(c)/(exp(c)-1)
  return(p)
}
cols = colnames(TABLES.all)
cols = cols[! cols %in% c("prolific_id", "id")]
df.tables = TABLES.all %>% group_by(prolific_id, id) %>%
  pivot_longer(cols=all_of(cols), names_to="key", values_to="prob") %>%
  filter(!startsWith(key, "theta"))

# check negative values
df = df.tables %>%
  mutate(p=transform_rating(prob, 0.6, 0.5),
         question=case_when(key=="AC" ~ "bg", 
                            key=="A-C" ~ "b",
                            key=="-AC" ~ "g",
                            key=="-A-C" ~ "none"))


df.merged = left_join(data.prior.orig %>% select(-RT), 
                      df %>% select(-prob, -key) %>% filter(!is.na(question)),
                      by=c("prolific_id", "id", "question")) %>% 
  mutate(p = case_when(is.na(p) ~ response,
                       TRUE ~ p))


data.joint.transformed = left_join(
  df.merged %>% select(-question) %>% rename(human_exp1=p),
  data.production %>% select(-RT, -custom_response) %>% rename(utterance=response),
  by=c("prolific_id", "id", "utterance"))
saveRDS(joint.human.orig, paste(result_dir, "human-orig-exp1-exp2.rds", sep=.Platform$file.sep))


data.joint.transformed %>% filter(prolific_id == "tanyamcshane") %>% 
  ggplot(aes(x=human_exp1, y=response)) +
  geom_point(aes(colour=utterance)) +
  facet_wrap(~id) +
  theme(legend.position="none") +
  labs(x="inferred true p", y="rating")





