library(here)
library(tidyverse)
source("R/utils.R")
source("R/utils-exp1.R")
source("R/utils-exp2.R")
source("R/Dirichlet-fits.R")

# Setup -------------------------------------------------------------------
# debug = TRUE test run vs. experimental (prolific) run
debug = FALSE 
exp_name = "toy-blocks-pilot-2"

data_fn <- paste("results_50_", exp_name, "_BG.csv", sep="")
# data_fn <- paste("results_54_", exp_name, "-main_BG.csv", sep="")

N_participants = 30
N_trials = list(train=14+3+10, test=13*2+1, color_vision=6,
                slider_choice=10, attention_check=3);

data_dir = ifelse(debug,  here("data", "test-runs"), here("data", "prolific"));
result_dir <- paste(data_dir, exp_name, sep=.Platform$file.sep)
if(!dir.exists(result_dir)){
  dir.create(result_dir, recursive=TRUE);
}
filtered_dir = paste(result_dir, "filtered_data", sep=.Platform$file.sep)
if(!dir.exists(filtered_dir)){
  dir.create(filtered_dir)
}  


# Processing --------------------------------------------------------------
data <- process_data(data_dir, data_fn, result_dir, exp_name, debug, N_trials)

# Save data in different formats ------------------------------------------
formatPriorElicitationData = function(test.prior, smoothed=TRUE){
  df.prior_responses = test.prior %>%
    dplyr::select(-custom_response, -QUD, -trial_number, -trial_name) 
  if(smoothed){
    df.prior_responses = df.prior_responses %>% dplyr::select(-r_orig) %>% 
      pivot_wider(names_from = "question", values_from = "r_smooth")
  } else {
    df.prior_responses = df.prior_responses %>% dplyr::select(-r_smooth) %>%
      pivot_wider(names_from = "question", values_from = "r_orig")
  }
  df.prior_responses = df.prior_responses %>% add_probs()
  
  prior_responses = df.prior_responses %>%
    pivot_longer(cols=c("b", "g", "bg", "none", starts_with("p_")),
                 names_to="prob", values_to="val") %>%
    translate_probs_to_utts()
  
  exp1.human = prior_responses %>% dplyr::select(-group, -n) %>%
    rename(human_exp1=val, question=prob) %>% 
    mutate(question = case_when(!question %in% c("bg", "b", "g", "none") ~ NA_character_,
                                TRUE ~ question))
  return(exp1.human)
}
test.prior =  data$test %>% filter(str_detect(trial_name, "multiple_slider"))
exp1.human.orig = formatPriorElicitationData(test.prior, smoothed = FALSE) %>%
  dplyr::select(-response)
exp1.human.smooth = formatPriorElicitationData(test.prior, smoothed = TRUE) %>%
  dplyr::select(-response) 
save_data(exp1.human.orig %>% rename(response=human_exp1),
        paste(result_dir, "human-exp1-orig.rds", sep=fs))
save_data(exp1.human.smooth %>% rename(response=human_exp1),
        paste(result_dir, "human-exp1-smoothed.rds", sep=fs))

test.production = data$test %>% filter(str_detect(trial_name, "fridge_")) %>%
  standardize_sentences()
exp2.human = test.production %>%
  dplyr::select(prolific_id, id, response, RT, custom_response) %>%
  rename(utterance=response) %>% add_column(human_exp2=1)
save_data(exp2.human %>% rename(response=utterance),
        paste(result_dir, "human-exp2.rds", sep=fs))

# merge data from prior elicitation and production
joint.human.smooth = left_join(
  exp1.human.smooth %>% dplyr::select(-question, -RT),
  exp2.human %>% dplyr::select(-RT, -custom_response),
  by=c("prolific_id", "id", "utterance")
)
joint.human.orig = left_join(
  exp1.human.orig %>% dplyr::select(-question, -RT),
  exp2.human %>% dplyr::select(-RT, -custom_response),
  by=c("prolific_id", "id", "utterance")
)
save_data(joint.human.orig, paste(result_dir, "human-exp1-orig-exp2.rds", sep=fs))
save_data(joint.human.smooth, paste(result_dir, "human-exp1-smoothed-exp2.rds", sep=fs))

# Quality of data in slider ratings:
df = exp1.human.orig %>% rename(response=human_exp1) %>%
  dplyr::select(-utterance) %>% filter(!is.na(question))
prior.quality = distancesResponses(df)
save_data(prior.quality, paste(result_dir, "test-data-prior-quality.rds", sep=fs))

# generate theoretic model tables (as in paper) ---------------------------
# tables.model = makeModelTables(result_dir)

# fit single dirichlet distribution for each stimulus ---------------------
run_fit_dirichlet = function(result_dir, exp_name, fn_suffix=""){
  path_tables <- paste(result_dir, fs, exp_name, "_tables_smooth.csv", sep="")
  params.fit = fitDirichlets(path_tables, target_dir=result_dir)
  write_csv(params.fit, paste(result_dir, "dirichlet-fits-params.csv", sep=fs))

  tables.dirichlet = sample_dirichlet(params.fit)

  df.params.fit = params.fit %>% add_column(p_cn=1, cn="cn1")
  fn = paste("dirichlet", fn_suffix, sep="")
  formatted.dirichlet = format_and_save_fitted_tables(
    tables.dirichlet, df.params.fit, result_dir, fn
  )
  return(df.params.fit)
}

# fit latent mixture distributions for each stimulus ----------------------
run_fit_latent_mixture = function(result_dir, exp_name) {
  path_tables <- paste(result_dir, fs, exp_name, "_tables_smooth.csv", sep="")
  params.fit = fitLatentMixture(path_tables, target_dir=result_dir)
  cns = c("A implies C", "A implies -C", "C implies A", "C implies -A")
  params.dep =  map_dfr(cns, function(cn){
    df = params.fit %>% dplyr::select(c(starts_with(cn), id)) %>% 
      rename_at(.vars = vars(starts_with(cn)), 
                .funs = funs(str_replace(., paste(cn, ".", sep=""), ""))) %>% 
      add_column(cn=(!! cn))
    return(df)
  })
  # independent params
  params.ind = params.fit %>% dplyr::select(c(starts_with("ind"), id)) %>%
    rename_at(.vars = vars(starts_with("ind")), 
              .funs = funs(str_replace(., "ind.", ""))) %>% 
    add_column(cn="ind")
  params = list(ind=params.ind, dep=params.dep)
  write_csv(params$ind, paste(result_dir, "latent-mixture-fits-ind-params.csv", sep=fs))
  write_csv(params$dep, paste(result_dir, "latent-mixture-fits-dep-params.csv", sep=fs))
  # todo implement following funs
  tables.latent_mixture = sample_latent_mixture(params)
  formatted.lm = format_and_save_fitted_tables(tables.latent_mixture, result_dir, "latent-mixture")
  return(params)
}


# goodness fits -----------------------------------------------------------
# res.goodness = compute_goodness_dirichlets(df.params.fit, result_dir, N_participants)
# p = plot_goodness_dirichlets(res.goodness, df.params.fit, result_dir)

# todo
params = list(ind=read_csv(paste(result_dir, "latent-mixture-fits-ind-params.csv", sep=fs)),
              dep=read_csv(paste(result_dir, "latent-mixture-fits-dep-params.csv", sep=fs)))
# todo: implement funs
# res.goodness = compute_goodness_latent_mixture(params, result_dir, N_participants, 100)
# p = plot_goodness_latent_mixture(res.goodness, params, result_dir)





