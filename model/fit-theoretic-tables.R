library(rwebppl)
library(here)
library(tidyverse)
source("model/R/helper-functions.R")

tables = readRDS(here("data", "prolific", "results", "toy-blocks-pilot-2",
                      "tables-empiric-augmented.rds"))
tbls = tables %>% dplyr::select(row_id, empirical_id, bg, b, g, none) %>% 
  unite("id", "empirical_id", "row_id", sep="_") %>%
  group_by(id) %>% 
  mutate(vs=list(c("bg", "b", "g", "none")),
         ps=list(c(`bg`, `b`, `g`, `none`))) %>%
  dplyr::select(id, vs, ps)

tables = readRDS(here("model", "data", "tables-model-empirical.rds")) %>%
  filter(cn %in% c("A implies C", "A || C"))
tbls = tables %>% dplyr::select(table_id, empirical_id, AC, `A-C`, `-AC`, `-A-C`) %>% 
  unite("id", "empirical_id", "table_id", sep="_") %>%
  group_by(id) %>% 
  mutate(vs=list(c("AC", "A-C", "-AC", "-A-C")),
         ps=list(c(`AC`, `A-C`, `-AC`, `-A-C`))) %>%
  dplyr::select(id, vs, ps)

  
posteriorSamples = 
  webppl(program_file = here("model", "fit-theoretic-tables.wppl"),
         data = list(tables=tbls), data_var = "data")

indSamples = filter(posteriorSamples, Parameter == "sigma")
depSamples = filter(posteriorSamples, Parameter %in% c("alpha", "beta"))
cnSamples = filter(posteriorSamples, Parameter == "p_cn")

posterior_pred = bind_rows(
  depSamples %>% group_by(Parameter) %>% summarize(ev=mean(value)),
  indSamples %>% group_by(Parameter) %>% summarize(ev=mean(value)),
  cnSamples %>% group_by(Parameter) %>% summarize(ev=mean(value))
) %>% pivot_wider(names_from="Parameter", values_from="ev")

tables.ll = tables %>%
  likelihood(posterior_pred$sigma, posterior_pred$alpha, posterior_pred$beta) %>%
  dplyr::select(table_id, empirical_id, logL_ind, logL_if_ac, cn,
                AC, `A-C`, `-AC`, `-A-C`)

df = tables.ll %>% rowid_to_column() %>% group_by(rowid) %>% 
  pivot_longer(cols=c("logL_ind", "logL_if_ac"), names_to="ll.key", values_to="ll.val") %>%
  filter(ll.val == max(ll.val))

df %>% filter((ll.key=="logL_ind" & cn=="A || C") | 
              (ll.key=="logL_if_ac" & cn!="A || C")) %>% nrow()


