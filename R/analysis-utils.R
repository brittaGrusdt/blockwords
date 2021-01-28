library(here)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(here)
library(reticulate)
library(scales)
library(truncnorm)
use_condaenv("anaconda3/py36")
library(greta)
source(here("R", "utils.R"))
source(here("R", "utils-exp1.R"))
source(here("R", "utils-exp2.R"))
source(here("R", "Dirichlet-fits.R"))

# General Data ------------------------------------------------------------
IDS.dep=c("if1_uh", "if1_u-Lh", "if1_hh", "if1_lh",
          "if2_ul", "if2_u-Ll", "if2_hl", "if2_ll");
IDS.ind = c("independent_ll", "independent_hh", "independent_hl",
            "independent_ul", "independent_uh"); #, "ind2");
IDS = c(IDS.dep, IDS.ind)

CNS.dep = c("A implies C", "A implies -C", "C implies A", "C implies -A");
ID_CNS = rbind(expand.grid(id=IDS.ind, cn=c("A || C"), stringsAsFactors=FALSE),
               expand.grid(id=IDS.dep, cn=c("A implies C", "C implies A"),
                           stringsAsFactors=FALSE));
questions.train = c("ry", "r", "y", "none")
labels.train = c(ry="Red and Yellow", r="Red and ¬Yellow",
                 y="¬Red and Yellow", none="¬Red and ¬Yellow")

questions.test = c("bg", "b", "g", "none")
labels.test = c(bg="Blue and Green", b="Blue and ¬Green",
                 g="¬Blue and Green", none="¬Blue and ¬Green")

train.edges = tribble(
  ~id, ~block, ~condition, ~dir,
  "ssw0", "yellow", "uncertain", "vert",
  "ssw0", "red", "low", "horiz",
  "ssw1", "red", "uncertain", "vert",
  "ssw1", "yellow", "low", "horiz",
  "uncertain0", "red", "uncertain", "horiz",
  "uncertain0", "yellow", "uncertain", "vert",
  "uncertain1", "red", "uncertainH", "horiz",
  "uncertain1", "yellow", "low", "horiz",
  "uncertain2", "red", "high", "horiz",
  "uncertain2", "yellow", "uncertain", "vert",
  "uncertain3", "red", "low", "vert",
  "uncertain3", "yellow", "high", "vert",
  "ac0", "red", "high", "horiz",
  "ac1", "yellow", "high", "vert",
  "ac2", "red", "uncertainH", "horiz",
  "ac3", "yellow", "uncertain", "vert",
  "ind0", "yellow", "low", "vert",
  "ind1", "yellow", "uncertain", "vert",
  "ind2", "green", "high", "vert",
  "ssw0", "yellow", "uncertainH", "vert",
  "ssw0", "red", "low", "horiz",
  "ssw1", "yellow", "low", "horiz",
  "ssw1", "red", "uncertain", "vert"
);
train.ramp = tribble(
  ~id, ~block, ~condition, ~dir,
  "ac0", "yellow", "high", "horiz",
  "ac1", "red", "low", "vert",
  "ac2", "yellow", "uncertain", "vert",
  "ac3", "red", "high", "horiz",
  "ind0", "red", "uncertainH", "vert",
  "ind1", "red", "high", "vert",
  "ind2", "blue", "uncertain", "horiz"
)

prior.dir_ramp = tribble(~relation, ~condition, ~dir, ~event,
                         "if2", "hl", "vert", "p_blue",
                         "if2", "ll", "horiz", "p_blue",
                         "if2", "ul", "vert", "p_blue",
                         "if2", "u-Ll", "vert", "p_blue",
                         "independent", "hh", "vert", "p_blue",
                         "independent", "hl", "vert", "p_green",
                         "independent", "ll", "horiz", "p_green",
                         "independent", "uh", "vert", "p_green",
                         "independent", "ul", "horiz", "p_green",
                         "if1", "hh", "vert", "g_given_b",
                         "if1", "lh", "vert", "g_given_b",
                         "if1", "uh", "vert", "g_given_b",
                         "if1", "u-Lh", "vert", "g_given_b")

prior.dir_edges = tribble(~relation, ~condition, ~dir, ~event,
                          "independent", "hh", "vert", "p_green",
                          "independent", "hl", "horiz", "p_blue",
                          "independent", "ll", "vert", "p_blue",
                          "independent", "uh", "horiz", "p_blue",
                          "independent", "ul", "vert", "p_blue",
                          "if2", "u-Ll", "horiz", "g_given_nb",
                          "if2", "ul", "horiz", "g_given_nb",
                          "if2", "hl", "horiz", "g_given_nb",
                          "if2", "ll", "horiz", "g_given_nb"
)

# Other -------------------------------------------------------------------
# ordered by informativity
levels.responses = rev(c(
  standardized.sentences$bg, standardized.sentences$none,
  standardized.sentences$g, standardized.sentences$b,
  standardized.sentences$only_b, standardized.sentences$only_g,
  standardized.sentences$only_nb, standardized.sentences$only_ng,
  standardized.sentences$if_bg, standardized.sentences$if_gb, 
  standardized.sentences$if_nbng, standardized.sentences$if_ngnb,
  standardized.sentences$if_bng, standardized.sentences$if_gnb,
  standardized.sentences$if_nbg, standardized.sentences$if_ngb,
  standardized.sentences$might_b, standardized.sentences$might_g,
  standardized.sentences$might_nb, standardized.sentences$might_ng
))

standardized.conj = c(standardized.sentences$bg, standardized.sentences$b,
                       standardized.sentences$g, standardized.sentences$none)
standardized.lit = c(standardized.sentences$only_b, standardized.sentences$only_g,
                      standardized.sentences$only_nb, standardized.sentences$only_ng)
standardized.ifs = levels.responses[str_detect(levels.responses, "if")]

# Experimental Data -------------------------------------------------------
load_exp_data = function(exp.name, use_filtered){
  data = list()
  result_dir = here("data", "prolific", exp.name)
  if(use_filtered) {
    result_dir = paste(result_dir, "filtered_data", sep=fs)
  }
  m = ifelse(use_filtered, "filtered data loaded", "non-filtered data loaded")
  message(paste(m, "from", result_dir))
  data$plot_dir = paste(result_dir, "plots", sep=fs)
  if(!dir.exists(data$plot_dir)){dir.create(data$plot_dir, recursive=TRUE)}
  data$result_dir = result_dir
  # Experimental Data
  data$production = readRDS(paste(result_dir, "human-exp2.rds", sep=fs));
  data$prior.smooth = readRDS(paste(result_dir, "human-exp1-smoothed.rds", sep=fs))
  data$prior.orig = readRDS(paste(result_dir, "human-exp1-orig.rds", sep=fs))
  data$joint.smooth = readRDS(paste(result_dir, "human-exp1-smoothed-exp2.rds", sep=fs))
  data$joint.orig = readRDS(paste(result_dir, "human-exp1-orig-exp2.rds", sep=fs))
  data$info = readRDS(paste(result_dir, "participants-info.rds", sep=fs))
  return(data)
}

# Filter Data ---------------------------------------------------------------
exclude_data = function(exp.name, ids){
  data = load_exp_data(exp.name, use_filtered=FALSE)
  anti_by = c("prolific_id", "id")
  exp2 = anti_join(data$production, ids, by=anti_by)
  exp1_smoothed = anti_join(data$prior.smooth, ids, by=anti_by)
  exp1_orig = anti_join(data$prior.orig, ids, by=anti_by)
  exp1_smoothed_exp2 = anti_join(data$joint.smooth, ids, by=anti_by)
  exp1_orig_exp2 = anti_join(data$joint.orig, ids, by=anti_by)
  return(list(exp2=exp2, exp1_sm=exp1_smoothed, exp1_orig=exp1_orig,
              exp1_sm_exp2=exp1_smoothed_exp2, exp1_orig_exp2=exp1_orig_exp2))
}

# according to criteria filter out and save all filtered data separetely
# @arg out.by_comments: tibble with cols: 'prolific_id', 'id'
filter_data = function(data.dir, exp.name, out.by_comments=NA, out.by_quality=NA) {
  filtered_dir <- paste(data.dir, "filtered_data", sep=fs)
  if(!dir.exists(filtered_dir)){dir.create(filtered_dir, recursive=TRUE);
  }
  data <- readRDS(paste(data.dir, fs, exp.name, "_tidy.rds", sep=""));
  data.production = readRDS(paste(data.dir, "human-exp2.rds", sep=fs));
  data.joint.orig = readRDS(paste(data.dir, "human-exp1-orig-exp2.rds", sep=fs))
  data.train.sliders = data$train.slider_choice
  
  df.all = data.production %>% dplyr::select(prolific_id, id)
  # exclude all trials of a participant
  # 1. attention check questions in beginning concerning block icons
  df.att = data$train.attention %>% filter(response != expected) 
  df.out = tibble()
  if(nrow(df.att) != 0){
    participants.att = df.att$prolific_id %>% unique()
    df.att = df.all %>% filter(prolific_id %in% participants.att) %>%
      dplyr::select(prolific_id, id) %>% distinct()
    df.out = bind_rows(df.out, df.att)
    message(paste(length(participants.att),
            'participants completely excluded due to attention checks'))
  }
  # 2. color vision questions
  dat.col = data$color %>% filter(response != expected) %>%
    dplyr::select(prolific_id, id) %>%
    group_by(prolific_id) %>%
    mutate(n_wrong=n()) %>% filter(n_wrong >= 1)
  if(nrow(dat.col) != 0){
    participants.col = dat.col$prolific_id %>% unique()
    df.col = df.all %>% filter(prolific_id %in% participants.col) %>%
      dplyr::select(prolific_id, id) %>% distinct()
    df.out = bind_rows(df.out, df.col)
    message(paste(length(participants.col),
                  'participant(s) completely excluded due to >= 1 color questions wrong'))
  }
  # 3. slider-choice trials
  dat.sliders = data.train.sliders %>%
    mutate(correct = response == expected, .groups="drop_last") %>%
    group_by(prolific_id) %>%
    summarize(ratio_correct=sum(correct)/n(), .groups="drop_last") %>%
    filter(ratio_correct < 0.5)
  if(nrow(dat.sliders) != 0){
    participants.sc = dat.sliders$prolific_id %>% unique()
    df.sc = df.all %>% filter(prolific_id %in% participants.sc) %>%
      dplyr::select(prolific_id, id) %>% distinct()
    df.out = bind_rows(df.out, df.sc)
    message(paste(length(participants.sc),
                  'participant(s) completely excluded due to less than 50% of slider-choice trials correct.'))
  } 
  # single trials
  # 4. utterance task 2 is rated with 0 in task 1
  df.utt = data.joint.orig %>%
    filter(!is.na(human_exp2) & human_exp1 == 0) %>%
    dplyr::select(prolific_id, id) %>% distinct()
  message(paste(nrow(df.utt), 'trial(s) excluded due to 0 probability in task 1, but chosen in task 2'))
  df.out = bind_rows(df.out, df.utt)
  
  # 5. due to comments
  if(!is.na(out.by_comments)){
    out.comments = read_csv(paste(data.dir, out.by_comments, sep=fs)) %>%
      dplyr::select(prolific_id, id)
    df.out = bind_rows(df.out, out.comments)
  }
  # 6. participants who choose <= 3 different utterances AND whose total time
  # spent was less than 20 minutes
  df.production.means = data.production %>% filter(id != "ind2") %>%
    dplyr::select(response, prolific_id, id) %>% 
    group_by(prolific_id, response) %>% 
    mutate(n=n()) %>% group_by(prolific_id) %>% mutate(N=n(), ratio=n/N) %>%
    arrange(desc(ratio)) %>% distinct() %>%
    mutate(response=as.factor(response))
  # add time spent
  dat.info = readRDS(paste(data.dir, "participants-info.rds", sep=fs))
  df = left_join(df.production.means,
                 data$info %>% dplyr::select(prolific_id, timeSpent), 
                 by=c("prolific_id")) %>% 
    mutate(timeSpent=round(timeSpent, 2))
  df.sum = df %>%
    dplyr::select(response, prolific_id, timeSpent) %>% distinct() %>%
    group_by(prolific_id) %>% mutate(n.utt=n()) %>%
    dplyr::select(-response) %>% distinct()
  # remove all trials for these participants
  trials = df$id %>% unique() %>% as.character()
  df.freq_time = df.sum %>% filter(n.utt <= 3 & timeSpent < 20) %>%
    dplyr::select(prolific_id)
  out.freq_time = df.freq_time %>%
    add_column(id=rep(list(trials), nrow(df.freq_time))) %>%
    unnest(c(id)) %>% group_by(prolific_id)
  
  df.out = bind_rows(df.out, out.freq_time)
  message(paste(length(out.freq_time$prolific_id %>% unique),
                'participant(s) excluded due to <= 3 different utterance AND < 20 minutes.'))
  # 7. Quality
  if(!is.na(out.by_quality)) {
    out.qual = read_csv(paste(data.dir, "out_by_quality_time.csv", sep=fs)) %>%
      dplyr::select(prolific_id, id)
    message(paste(length(out.qual$prolific_id %>% unique),
                  'participant(s) excluded due to large quality diff AND total time spent (very short/long).'))
    df.out = bind_rows(df.out, out.qual)
  }

  ratio_ex = round(nrow(df.out)/nrow(df.all), 2) * 100
  message(paste(ratio_ex, '% of all trials excluded in total.', sep=""))
  write_csv(df.out, paste(filtered_dir, "ids_excluded.csv", sep=fs))
  
  # save filtered data
  df.filtered = exclude_data(exp.name, df.out)

  save_data(df.filtered$exp2,
            paste(filtered_dir, "human-exp2.rds", sep=fs));
  save_data(df.filtered$exp1_sm,
            paste(filtered_dir, "human-exp1-smoothed.rds", sep=fs))
  save_data(df.filtered$exp1_orig,
            paste(filtered_dir, "human-exp1-orig.rds", sep=fs))
  save_data(df.filtered$exp1_sm_exp2,
            paste(filtered_dir, "human-exp1-smoothed-exp2.rds", sep=fs))
  save_data(df.filtered$exp1_orig_exp2,
            paste(filtered_dir, "human-exp1-orig-exp2.rds", sep=fs))
  
  df.info = anti_join(data$info, df.out %>% dplyr::select(prolific_id), by=c("prolific_id"))
  save_data(df.info, paste(filtered_dir, "participants-info.rds", sep=fs))
  
  # also save with all data (and with empiric-ids)
  df1 = data$test %>% filter(str_detect(trial_name, "multiple_slider"))
  df1 = anti_join(df1, df.out, by=c("prolific_id", "id")) %>%
    dplyr::select(-response) %>% rename(response=r_orig)
  df1 = add_smoothed_exp1(df1);
  df1 = standardize_color_groups_exp1(df1)
  save_prob_tables(df1, filtered_dir, exp.name);
  
  # fit dirichlet distributions to filtered data
  df.params.fit = run_fit_dirichlet(filtered_dir, exp.name, "dirichlet-filtered")
  N_participants = df1$prolific_id %>% unique() %>% length()
  res.goodness = compute_goodness_dirichlets(df.params.fit, filtered_dir, N_participants)
  p = plot_goodness_dirichlets(res.goodness, df.params.fit, filtered_dir)
  
  return(df.filtered)
}
