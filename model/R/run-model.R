library(rwebppl)
library(tidyverse)
library(here)
source(here("model", "R", "helpers-webppl.R"))
source(here("model", "R", "helper-functions.R"))
source(here("R", "utils.R"))
source(here("R", "utils-exp2.R"))

# used_tables = "tables_model_filtered"
used_tables = "tables_dirichlet_filtered"
# used_tables = "tables_dirichlet_filtered_augmented" # is not saved in own folder!


# this only makes a difference for the states given to the speaker, in case of
# speaker_empirical_tables, these are *only* tables that were actually 
# used by participants
params <- configure(c("speaker", used_tables))
params$used_tables = used_tables
# Setup -------------------------------------------------------------------
dir.create(params$target_dir, recursive = TRUE)
params$target_params <- file.path(params$target_dir, params$target_params, fsep=fs)

## Generate/Retrieve tables
tables <- readRDS(params$tables_empiric)
print(paste("tables read from:", params$tables_empiric))
# specify bayes nets for which speaker distributions will be computed
# for each stimulus sample tables from fitted distributions, i.e. from 
# set of input tables
params$tables = tables %>% ungroup %>%
  dplyr::select(table_id, ps, vs, stimulus, ll, cn)

# specify input states (subset of all states) for which speaker predictions are computed!
params$bns_sampled = tables %>% dplyr::select(table_id, stimulus) %>% 
  group_by(table_id, stimulus) %>% summarize(n=n(), .groups="drop_last")
params$bn_ids = params$bns_sampled %>% pull(table_id) %>% unique()

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
posterior <- run_webppl(params$model_path, params)

# restructure data and save
if(params$level_max == "speaker") {
  speaker <- posterior %>% structure_speaker_data(params) %>% group_by(stimulus)
  res.avg = average_predictions(speaker, params, "model-avg-predictions")
  
  # for all input states, get model prediction along with behavioral observations
  # for dirichlet now we only look at averages, if we want to match tables,
  # use add_augmented when sampling dirichlet tables (makeDirichletTables)
  if(str_detect(used_tables, "tables_model")){
    res.behav_model = join_model_behavioral_data(speaker, params);
  }
  
} else if(params$level_max %in% c("priorN")){
    data <- structure_bns(posterior, params)
} else if(params$level_max == "log_likelihood"){
  data <- tibble(id=posterior$id$value, cn=posterior$cn$value,
                 logL=posterior$logL$value)
} else {
  data <- posterior %>% structure_listener_data(params)
  data_voi <- voi_default(data, params)
}
