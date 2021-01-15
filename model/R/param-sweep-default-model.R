library(rwebppl)
library(tidyverse)
library(here)
source(here("model", "R", "helpers-webppl.R"))
source(here("model", "R", "helper-functions.R"))
source(here("R", "utils.R"))
source(here("R", "utils-exp2.R"))

used_tables = "tables_model"
# used_tables = "tables-dirichlet"
params <- configure(c("speaker_empirical_tables", used_tables))

# params <- configure(c("speaker_prior_samples", "tables-dirichlet"))

params$save = FALSE

# Setup -------------------------------------------------------------------
dir.create(params$target_dir, recursive = TRUE)
params$target_params <- file.path(params$target_dir,
                                  params$target_params, fsep=.Platform$file.sep)

## Generate/Retrieve tables
tables <- readRDS(params$tables_empiric)
print(paste("tables read from:", params$tables_empiric))
params$tables = tables %>% ungroup %>%
  dplyr::select(table_id, ps, vs, stimulus, "ll", "cn")
params$indep_sigma = tables$indep_sigma %>% unique

if("predictions_for" %in% names(params)) {
  if(params$predictions_for == "empirical-tables") {
    params$bn_ids = tables %>% filter(!is.na(empirical_id)) %>% pull(table_id)
  } else if(params$predictions_for == "prior-samples-stimuli"){
    # for each stimulus sample tables from fitted distributions, i.e. from 
    # set of input tables
    set.seed(params$seed_tables)
    bn_ids = group_map(tables %>% group_by(stimulus), function(t, stim){
      return(tibble(table_id = sample(x=t$table_id, size=100, replace=TRUE),
                    stimulus = stim))
    })
    params$bn_ids = bind_rows(bn_ids) %>% pull(table_id)
  }
}

## Generate/Retrieve utterances
generate_utts <- function(params){
  utterances <- run_webppl(here("model", "webppl-model", "default-model",
                                "utterances.wppl"), params)
  utts <- utterances %>% map(function(x){x %>% pull(value)}) %>% unlist()
  utts %>% save_data(params$utts_path)
  return(utts)
}
if(params$generate_utterances || !file.exists(params$utts_path)){
  utterances <- generate_utts(params)
} else {
  utterances <- readRDS(params$utts_path)
  print(paste("utterances read from:", params$utts_path))
}
params$utterances <- utterances

# Run Model ---------------------------------------------------------------
save_to = here("model", "results", "sweep-correlations.rds")
if(file.exists(save_to)) {
  correlations = readRDS(save_to)
} else {
  correlations = tibble()
}

alphas = c(1, 2, 3, 5, 8)
costs = c(0, 0.001, 0.01, 0.05, 0.1, 1)

for(alpha in alphas){
  for(c in costs){
    df = correlations %>% filter(alpha == (!! alpha) & cost_conditional == c)
    if(nrow(df) == 0) { 
      params$alpha = alpha
      params$cost_conditional = c
      posterior <- run_webppl(params$model_path, params)
      
      speaker <- posterior$distributions %>% structure_speaker_data(params)
        # group_by(stimulus)
      
      if(params$predictions_for == "empirical-tables") {
        res.behav_model = join_model_behavioral_data(speaker, params);
        sp = res.behav_model %>%
          dplyr::select(prolific_id, id, utterance, model.p) %>%
          rename(stimulus = id, probs=model.p)
        res.behav_model.avg = join_model_behavioral_avg_stimulus(
          sp, params, "_predictions-empirical-based")                                                       
      } 
      if(params$predictions_for == "prior-samples-stimuli" & 
         used_tables == "tables-dirichlet") {
        res.behav_model.avg = join_model_behavioral_avg_stimulus(
          speaker, params, "_predictions-stimulus-prior-based")
      }
      
      data.wide = res.behav_model.avg %>%
        pivot_wider(names_from="predictor", values_from="p")
      
      res = cor.test(data.wide$behavioral, data.wide$model)
      correlations = bind_rows(
        correlations, 
        tibble(cor=res$estimate, p=res$p.value,
               ci.low=res$conf.int[1], ci.up=res$conf.int[2],
               alpha=alpha, cost_conditional=c)
      ) %>% arrange(desc(cor))
    }
  }
}

save_data(list(
  predictions_for = params$predictions_for,
  indep_sigma = params$indep_sigma,
  tables = params$tables, 
  cor = correlations
), here("model", "results", "sweep-correlations.rds"))
