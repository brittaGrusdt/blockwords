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

# General Data ------------------------------------------------------------
exp.name = "toy-blocks-pilot-2"

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

RESULT.dir = here("data", "prolific", exp.name)
PLOT.dir = here("data", "prolific", exp.name, "plots")
if(!dir.exists(PLOT.dir)){dir.create(PLOT.dir, recursive=TRUE)}

# Experimental Data -------------------------------------------------------
data <- readRDS(paste(RESULT.dir, fs, exp.name, "_tidy.rds", sep=""));
data.info = data$info
data.comments = data$comments
data.color = data$color 
data.attention = data$train.attention

data.production = readRDS(paste(RESULT.dir, "human-exp2.rds", sep=fs));
data.prior.smooth = readRDS(paste(RESULT.dir, "human-exp1-smoothed.rds", sep=fs))
data.prior.orig = readRDS(paste(RESULT.dir, "human-exp1-orig.rds", sep=fs))
data.joint.smooth = readRDS(paste(RESULT.dir, "human-exp1-smoothed-exp2.rds", sep=fs))
data.joint.orig = readRDS(paste(RESULT.dir, "human-exp1-orig-exp2.rds", sep=fs))

# data quality
data.quality = readRDS(paste(RESULT.dir, fs, "test-data-prior-quality.rds", sep=""))
quality.means = data.quality %>% arrange(desc(mean.comparator)) %>%
  distinct_at(vars(c(comparator)), .keep_all = TRUE)
worst_quality.ids = quality.means[1: round(0.1 * nrow(quality.means)),] %>% pull(comparator)

# training data
data.train.smooth = data$train.smooth
data.train.orig = data$train.orig
# for each participant only the last 50% of all train trials
data.train.smooth.half = data.train.smooth %>% 
  separate(id, into=c("trial.relation", "trial.idx"), sep=-1, remove=FALSE) %>%
  group_by(prolific_id, trial.relation) %>% arrange(desc(trial_number)) %>%
  top_frac(0.5, trial_number) %>% 
  dplyr::select(-expected) %>% 
  pivot_wider(names_from=question, values_from=response) %>%
  ungroup() %>% 
  dplyr::select(-trial.relation, -trial.idx)

data.train.sliders = data$train.slider_choice

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


# Filter Data ---------------------------------------------------------------
exclude_data = function(ids){
  anti_by = c("prolific_id", "id")
  exp2 = anti_join(data.production, ids, by=anti_by)
  exp1_smoothed = anti_join(data.prior.smooth, ids, by=anti_by)
  exp1_orig = anti_join(data.prior.orig, ids, by=anti_by)
  exp1_smoothed_exp2 = anti_join(data.joint.smooth, ids, by=anti_by)
  return(list(exp2=exp2, exp1_sm=exp1_smoothed, exp1_orig=exp1_orig,
              exp1_sm_exp2=exp1_smoothed_exp2))
}

# according to criteria filter out and save all filtered data separetely
# @arg out.by_comments: tibble with cols: 'prolific_id', 'id'
filter_data = function(out.by_comments=NA){
  df.all = data.production %>% dplyr::select(prolific_id, id)
  # exclude all trials of a participant
  # 1. attention check questions in beginning concerning block icons
  df.att = data.attention %>% filter(response != expected) 
  df.out = tibble()
  if(nrow(df.att) != 0){
    participants.att = df.att$prolific_id %>% unique()
    df.att = df.all %>% filter(prolific_id %in% participants.att) %>%
      dplyr::select(prolific_id, id) %>% distinct()
    df.out = bind_rows(df.out, df.att)
    message(paste(length(participants.att), 'participants completely excluded due
                  to attention checks'))
  }
  # 2. color vision questions
  dat.col = data.color %>% filter(response != expected) %>%
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
  message(paste(nrow(df.utt), 'trial(s) excluded due to 0-probability in task 1'))
  df.out = bind_rows(df.out, df.utt)
  
  # 5. due to comments
  if(!is.na(out.by_comments)){
    df.out = bind_rows(df.out, out.by_comments)
  }
  ratio_ex = round(nrow(df.out)/nrow(df.all), 2) * 100
  message(paste(ratio_ex, '% of all trials excluded in total.', sep=""))
  
  # save filtered data
  df.filtered = exclude_data(df.out)
  # create dir for filtered data if filtered later
  filtered_dir <- paste(RESULT.dir, "filtered_data", sep=fs)
  if(!dir.exists(filtered_dir)){dir.create(filtered_dir, recursive=TRUE);
  }
  save_data(df.filtered$exp2,
    paste(filtered_dir, "human-exp2.rds", sep=fs));
  save_data(df.filtered$exp1_sm,
    paste(filtered_dir, "human-exp1-smoothed.rds", sep=fs))
  save_data(df.filtered$exp1_orig,
    paste(filtered_dir, "human-exp1-orig.rds", sep=fs))
  save_data(df.filtered$exp1_sm_exp2,
    paste(filtered_dir, "human-exp1-smoothed-exp2.rds", sep=fs))
  
  # also save with all data (and with empiric-ids)
  data = readRDS(paste(RESULT.dir, fs, exp.name, "_tidy.rds", sep=""));
  df1 = data$test %>% filter(str_detect(trial_name, "multiple_slider"))
  df1 = anti_join(df1, df.out, by=c("prolific_id", "id")) %>%
    dplyr::select(-response) %>% rename(response=r_orig)
  df1 = add_smoothed_exp1(df1);
  df1 = standardize_color_groups_exp1(df1)
  save_prob_tables(df1, filtered_dir, exp.name);
  
  return(df.filtered)
}

