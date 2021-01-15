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

data.production = readRDS(paste(RESULT.dir, "human-exp2.rds", sep=fs));
data.prior.smooth = readRDS(paste(RESULT.dir, "human-exp1-smoothed.rds", sep=fs))
data.prior.orig = readRDS(paste(RESULT.dir, "human-exp1-orig.rds", sep=fs))
data.joint.smooth = readRDS(paste(RESULT.dir, "human-exp1-smoothed-exp2.rds", sep=fs))
data.joint.orig = readRDS(paste(RESULT.dir, "human-exp1-orig-exp2.rds", sep=fs))

data.quality = readRDS(paste(RESULT.dir, fs, "filtered_data", fs,
                             "test-data-prior-quality.rds", sep=""))
data.distances = readRDS(paste(RESULT.dir, "distances-quality.rds", sep=fs))

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



# Filter Data ---------------------------------------------------------------
# according to criteria filter out and save all filtered data separetely
filter_data = function(){
  ids = data.joint.orig %>%
    filter(!is.na(human_exp2) & human_exp1 == 0) %>%
    dplyr::select(prolific_id, id) %>% distinct()
  
  # save filtered data
  # create dir for filtered data if filtered later
  filtered_dir <- paste(RESULT.dir, "filtered_data", sep=fs)
  if(!dir.exists(filtered_dir)){dir.create(filtered_dir, recursive=TRUE);
  }
  save_data(anti_join(data.production, ids),
    paste(filtered_dir, "human-exp2.rds", sep=fs));
  save_data(anti_join(data.prior.smooth, ids),
    paste(filtered_dir, "human-exp1-smoothed.rds", sep=fs))
  save_data(anti_join(data.prior.orig, ids),
    paste(filtered_dir, "human-exp1-orig.rds", sep=fs))
  save_data(anti_join(data.joint.smooth, ids),
    paste(filtered_dir, "human-exp1-smoothed-exp2.rds", sep=fs))
}
  
  
  
  # todo: tidy up following!
  
  # # load smoothed tables
  # tables = readRDS(paste(target_dir, "empiric-all-tables-smooth.rds", sep=fs))
  # data = readRDS(paste(target_dir, fs, exp.name, "_tidy.rds", sep=""))
  # stimuli = data$test$id %>% unique()
  # df = tibble()
  # if(by_quality) {
  #   exp1.quality = readRDS(paste(paste(target_dir, "filtered_data", sep=fs),
  #                                "test-data-prior-quality.rds", sep=fs))
  #   dat.quality = exp1.quality %>%
  #     # mutate(quantiles=list(quantile(sum_sq_diff))) %>%
  #     mutate(iqr=IQR(sum_sq_diff), mean=mean(sum_sq_diff),
  #            outlier=sum_sq_diff < (mean-2*iqr) | (sum_sq_diff>mean + 2*iqr)) %>%
  #     filter(!outlier) %>% dplyr::select(prolific_id, stimulus_id)
  #   
  #   df = bind_rows(df, right_join(tables, dat.quality %>% rename(id=stimulus_id)))
  # }
  # 
  # if(by_color_vision){
  #   dat.color = data$color %>%
  #     mutate(correct = expected == response,  N=max(trial_number)) %>%
  #     filter(correct) %>% group_by(prolific_id) %>%
  #     mutate(n=n()) %>% filter(n == N) %>% dplyr::select(prolific_id) %>% unique() %>%
  #     add_column(id=list(stimuli)) %>%
  #     unnest(c(id))
  #   
  #   message(paste("#participants excluded as at least 1 wrong color question:",
  #                 length(data$color$prolific_id %>% unique())-length(dat.color$prolific_id %>% unique)))
  #   df=right_join(df, dat.color)
  # }
  # 
  # if(!is.na(out.by_comments)){
  #   df = anti_join(df, out.by_comments)
  # }
  # save_to = paste(target_dir, "filtered_data",
  #                 "empiric-filtered-tables-smooth.rds", sep=fs)
  # saveRDS(df, save_to)
  # print(paste("saved filtered smooth tables to:", save_to))
  # message(paste("remaining tables:", nrow(df), "(", nrow(df)/nrow(tables), ")"))
  # return(df)



