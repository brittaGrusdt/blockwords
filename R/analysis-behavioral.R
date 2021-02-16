library(cowplot)
library(ggpubr)
library(here)
library(gridExtra)
source(here("R", "analysis-utils.R"))
source(here("R", "plot-functions.R"))

exp.name = "blockwords"
use_filtered = TRUE

# Data --------------------------------------------------------------------
DATA = load_exp_data(exp.name, use_filtered)

df = DATA$joint.smooth %>%
  dplyr::select(-human_exp2) %>% 
  group_by(prolific_id, id) %>% 
  pivot_wider(names_from="utterance", names_prefix="utt.", values_from="human_exp1") %>% 
  mutate(stimulus=id) %>% 
  separate("id", into=c("relation", "prior"), sep="_") %>%
  mutate(prior.blue= case_when(startsWith(prior, "u-L") ~ "u-L",
                               TRUE ~ substr(prior, 1, 1)),
  relation=case_when(str_detect(stimulus, "if1") ~ "if1",
                         str_detect(stimulus, "if2") ~ "if2", 
                         str_detect(stimulus, "independent") ~ "independent"))
df.long = df %>% group_by(stimulus, prolific_id) %>% 
  pivot_longer(cols=starts_with("utt."), names_to="utterance", names_prefix="utt.",
               values_to="p")

df.rel_dep = df.long %>%
  filter(utterance %in% c("if blue does not fall green falls",
                          "if blue falls green falls")) %>% 
  pivot_wider(names_from="utterance", values_from="p") %>% 
  mutate(val=`if blue falls green falls` - `if blue does not fall green falls`)

df.rel_dep %>%
  mutate(relation=as_factor(relation)) %>% 
  ggplot(aes(x=relation, y=val)) +
  geom_violin()  +
  geom_jitter(alpha=0.5, width=0.2, height=0) +
  labs(y="P(if blue, green) - P(if not blue, green)")

df.unc_cert = df.long %>%
  filter(utterance == "blue falls") %>%
  mutate(belief = case_when(prior.blue == "u" ~ "uncertain",
                            prior.blue == "u-L" ~ "uncertain-low",
                            TRUE ~ "certain"))

df.unc_cert %>% 
  mutate(belief=as_factor(belief)) %>% 
  ggplot(aes(x=belief, y=p)) +
  geom_violin()  +
  geom_jitter(alpha=0.5, width=0.2, height=0) +
  labs(y="P(blue falls)")





