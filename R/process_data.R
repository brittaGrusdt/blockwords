library(here)
library(tidyverse)
source("R/utils.R")
source("R/utils-exp1.R")
source("R/utils-exp2.R")
source("R/Dirichlet-fits.R")

# Setup -------------------------------------------------------------------

# debug_run = TRUE  # vs. experimental (prolific) run
debug_run = FALSE

data_fn <- "results_50_toy-blocks-pilot-2_BG.csv"
result_fn = "toy-blocks-pilot-2"
N_participants = 30

data_dir = ifelse(debug_run,  here("data", "test-runs"), here("data", "prolific"));
result_dir <- paste(data_dir, result_fn, sep=fs)
if(!dir.exists(result_dir)){
  dir.create(result_dir, recursive=TRUE);
}
# Processing --------------------------------------------------------------
N_trials = list(train=14+3+10, test=13*2+1, color_vision=6,
                slider_choice=10, attention_check=3);
data <- process_data(data_dir, data_fn, result_dir, result_fn, debug_run, N_trials)

# create dir for filtered data if filtered later
filtered_dir <- paste(result_dir, "filtered_data", sep=fs)
if(!dir.exists(filtered_dir)){dir.create(filtered_dir, recursive=TRUE);
}

# Save data in different formats ------------------------------------------
# Quality of data in slider ratings: squared distance to mean each table cell
# entry (considering all participants) summed for each participant across all
# 4 questions/joint events
test.prior = data$test %>% filter(str_detect(trial_name, "multiple_slider"))
prior.quality = test.prior %>%  dplyr::select(-response, -custom_response) %>% 
  responsesSquaredDiff2Mean() %>%
  mutate(stimulus_id=factor(stimulus_id))
save_data(prior.quality, paste(filtered_dir, "test-data-prior-quality.rds", sep=fs))

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

df = exp1.human.smooth %>% rename(r_smooth=human_exp1) %>%
  dplyr::select(-utterance) %>% filter(!is.na(question))
distances = distancesResponses(df)
save_data(distances, paste(result_dir, "distances-quality.rds", sep=fs))

# generate tables that are provided to model
# theoretic tables
tables.model = makeModelTables(result_dir)

# fit dirichlet distribution for each stimulus
path_tables <- paste(result_dir, fs, result_fn, "_tables_smooth.csv", sep="")
params.fit = fitDirichlets(path_tables, target_dir=result_dir)
# params.fit = read_csv(paste(result_dir, "results-dirichlet-fits.csv", sep=fs))
tables.dirichlet = makeDirichletTables(params.fit, result_dir)
res.goodness = compute_goodness_dirichlets(params.fit, result_dir, N_participants)
p = plot_goodness_dirichlets(res.goodness, params.fit, result_dir)


# functions ---------------------------------------------------------------
filter_data = function(target_dir, exp.name, by_quality=FALSE,
                       by_color_vision=FALSE, out.by_comments=NA){
  # load smoothed tables
  tables = readRDS(paste(target_dir, "empiric-all-tables-smooth.rds", sep=fs))
  data = readRDS(paste(target_dir, fs, exp.name, "_tidy.rds", sep=""))
  stimuli = data$test$id %>% unique()
  df = tibble()
  if(by_quality) {
    exp1.quality = readRDS(paste(paste(target_dir, "filtered_data", sep=fs),
                           "test-data-prior-quality.rds", sep=fs))
    dat.quality = exp1.quality %>%
      # mutate(quantiles=list(quantile(sum_sq_diff))) %>%
      mutate(iqr=IQR(sum_sq_diff), mean=mean(sum_sq_diff),
             outlier=sum_sq_diff < (mean-2*iqr) | (sum_sq_diff>mean + 2*iqr)) %>%
      filter(!outlier) %>% dplyr::select(prolific_id, stimulus_id)
    
    df = bind_rows(df, right_join(tables, dat.quality %>% rename(id=stimulus_id)))
  }
  
  if(by_color_vision){
    dat.color = data$color %>%
      mutate(correct = expected == response,  N=max(trial_number)) %>%
      filter(correct) %>% group_by(prolific_id) %>%
      mutate(n=n()) %>% filter(n == N) %>% dplyr::select(prolific_id) %>% unique() %>%
      add_column(id=list(stimuli)) %>%
      unnest(c(id))

    message(paste("#participants excluded as at least 1 wrong color question:",
            length(data$color$prolific_id %>% unique())-length(dat.color$prolific_id %>% unique)))
    df=right_join(df, dat.color)
  }
  
  if(!is.na(out.by_comments)){
    df = anti_join(df, out.by_comments)
  }
  save_to = paste(target_dir, "filtered_data",
                  "empiric-filtered-tables-smooth.rds", sep=fs)
  saveRDS(df, save_to)
  print(paste("saved filtered smooth tables to:", save_to))
  message(paste("remaining tables:", nrow(df), "(", nrow(df)/nrow(tables), ")"))
  return(df)
}

# df.filtered=filter_data(result_dir, result_fn, by_quality=TRUE, by_color_vision=TRUE)


