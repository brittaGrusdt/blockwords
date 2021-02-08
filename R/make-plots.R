library(cowplot)
library(ggpubr)
library(here)
source(here("R", "analysis-utils.R"))

# WHICH DATA TO USE
# exp.name = "toy-blocks-pilot-2"
exp.name = "blockwords"
use_filtered = TRUE
# use_filtered = FALSE
plot_single_participants = FALSE

DATA = load_exp_data(exp.name, use_filtered)
################################################

# summary slider-ratings ----------------------------------------------------
tables.smooth = DATA$joint.smooth %>%
  dplyr::select(-human_exp2) %>% distinct() %>% 
  mutate(utterance = case_when(utterance==standardized.sentences$bg ~ "bg",
                              utterance==standardized.sentences$b ~ "b",
                              utterance==standardized.sentences$g ~ "g",
                              utterance==standardized.sentences$none ~ "none", 
                              TRUE ~ utterance)) %>%
  filter((utterance %in% questions.test)) %>%
  pivot_wider(names_from="utterance", values_from="human_exp1") %>%
  group_by(prolific_id, id) %>%
  rowid_to_column()

tables.long = tables.smooth %>% 
  pivot_longer(cols=c(bg, b, g, none), names_to="question", values_to="response") 
save_to = paste(DATA$plot_dir, "slider-ratings-densities", sep=fs)
if(!dir.exists(save_to)) {dir.create(save_to)}
tables.long %>%
  plotSliderDensities(questions.test, labels.test, target_dir=save_to)

save_to = paste(DATA$plot_dir, "slider-ratings-boxplots", sep=fs)
if(!dir.exists(save_to)) {dir.create(save_to)}
tables.long %>%
  plotSliderRatings(questions.test, labels.test, cluster_by="bg",
                    relation=FALSE, target_dir=save_to)

# Literal meaning probabilities -------------------------------------------
df = left_join(DATA$prior.smooth %>% dplyr::select(-question, -RT) %>%
                 rename(human_exp1=response),
               DATA$production %>% dplyr::select(-RT) %>% rename(utterance=response),
               by=c("id", "prolific_id", "utterance")) %>%
  filter(!is.na(human_exp2)) %>% group_by(id) %>% 
  mutate(human_exp1=case_when(str_detect(utterance, "might") & human_exp1>0.1 ~ 1,
                              TRUE ~ human_exp1))
p <- df %>%
  ggplot(aes(y=utterance, x=human_exp1)) +
  geom_point(aes(color=id)) +
  geom_vline(aes(xintercept=0.7), color="black", linetype='dashed') +
  theme_bw(base_size = 20) +
  theme(text = element_text(size=20), legend.position="bottom") +
  labs(x="rated probability (prior elicitation)", y="response")

ggsave(paste(DATA$plot_dir, "prior-vs-utt.png", sep=fs), p, width=14, height=7)

#Same plot, here results plotted separately for each participant. Stimuli are color coded.
p <- df %>%
  ggplot(aes(y=utterance, x=human_exp1)) +
  geom_point(aes(color=id)) +
  geom_vline(aes(xintercept=0.7), color="black", linetype='dashed') +
  facet_wrap(~prolific_id) +
  theme_bw(base_size = 14) +
  theme(legend.position="none") +
  labs(x="rated probability (prior elicitation)", y="response")
ggsave(paste(DATA$plot_dir, "prior-vs-utt-per-proband.png", sep=fs), p, width=18, height=18)



# likelihoods 
average_likelihoods = function(){
  target_dir = here("model", "results", tables_fn)
  plot_dir = paste(target_dir, "plots", sep=fs)
  if(!dir.exists(plot_dir)) {
    dir.create(plot_dir)
  }
  model.avg = readRDS(paste(target_dir, "model-avg-predictions.rds", sep=fs)) %>%
    filter(stimulus != "ind2") %>% dplyr::select(utterance, stimulus, p) %>%
    mutate(utterance=factor(utterance, levels=levels.responses)) %>% 
    pivot_wider(names_from="utterance", values_from="p", names_prefix="utt.")
  
  behavioral = readRDS(paste(dir_empiric, "behavioral-avg-task2.rds", sep=fs)) %>%
    filter(id != "ind2") %>% mutate(utterance=factor(utterance, levels=levels.responses))
  behav.avg = behavioral %>% dplyr::select(id, utterance, ratio) %>% 
    pivot_wider(names_from="utterance", values_from="ratio", names_prefix="utt.")
  behav.counts = behavioral %>% dplyr::select(id, utterance, count) %>%
    mutate(utterance=factor(utterance, levels=levels.responses)) %>% 
    pivot_wider(names_from="utterance", values_from="count", names_prefix="utt.")
  
  map_dfr(seq(1, nrow(behav.counts)), function(i){
    id = behav.counts[i,]$id
    data = behav.counts[i,] %>% dplyr::select(-id) %>% as.matrix()
    probs = model.avg %>% filter(stimulus == (!! id)) %>%
      ungroup() %>% dplyr::select(-stimulus) %>% as.matrix()
    p = dmultinom(data, prob=probs)
    return(tibble(id=id, ll=p))
  })
    
}

# Plots only relevant for non-filtered data -----------------------------------
if(!use_filtered && plot_single_participants) {
  pids = DATA$joint.orig %>% pull(prolific_id) %>% unique()
  stimuli =  DATA$prior.orig %>% pull(id) %>% unique()
  
  df.orig = DATA$joint.orig %>% 
    mutate(question = case_when(utterance==standardized.sentences$bg ~ "bg",
                                utterance==standardized.sentences$b ~ "b",
                                utterance==standardized.sentences$g ~ "g",
                                utterance==standardized.sentences$none ~ "none", 
                                TRUE ~ utterance))
  df.smooth = DATA$joint.smooth %>%
    mutate(question = case_when(utterance==standardized.sentences$bg ~ "bg",
                                utterance==standardized.sentences$b ~ "b",
                                utterance==standardized.sentences$g ~ "g",
                                utterance==standardized.sentences$none ~ "none", 
                                TRUE ~ utterance))
  # Slider Ratings Tables for each participant plotted seperately
  # (takes a while, and not so relevant)
  # 1.slider ratings with utterance and normalized ratings, 
  save_to = paste(DATA$plot_dir, "slider-ratings-tables", sep=fs)
  if(!dir.exists(save_to)) {dir.create(save_to)}

  for(pid in pids) {
    df.pid.orig = df.orig %>% filter(prolific_id == (!! pid))
    df.pid.smooth = df.smooth %>% filter(prolific_id == (!! pid))

    target_folder = paste(save_to, pid, sep=fs)
    if(!dir.exists(target_folder)){ dir.create(target_folder)
    }
    for(stimulus in stimuli) {
      df.stim.orig = df.pid.orig %>%
        filter(id==stimulus & question %in% questions.test) %>%
        rename(means=human_exp1)
      if(df.stim.orig %>% nrow() != 0){
        df.stim.smooth = df.pid.smooth %>% filter(id==stimulus) %>% rename(normalized=human_exp1)
        uttered = df.stim.smooth %>% filter(!is.na(human_exp2)) %>% pull(utterance)

        stim_utt = paste(stimulus, uttered, sep=": ")
        df.stim = left_join(df.stim.orig %>% dplyr::select(-human_exp2),
                            df.stim.smooth %>% filter(question %in% questions.test),
                            by=c("prolific_id", "id", "question"))
        p = df.stim %>% PlotMeans(stimulus, sd_error_bars = FALSE) +
          labs(y="unnormalized slider rating", x="",title=stim_utt) +
          geom_point(aes(y=normalized), size=2, color='orange') +
          theme(axis.text.x=element_blank(), legend.position="none")
        ggsave(paste(save_to, fs, pid, fs, stimulus, ".png", sep=""), p, width=12)
      }
    }
  }
  
  # model predictions with theoretic/dirichlet-fitted tables (takes a while)
  plotModelAndBehavioral("tables-model-filtered", c("independent_uh"))
  plotModelAndBehavioral("tables-dirichlet-filtered-augmented", c("independent_uh"))
  
  # normalized slider Ratings with produced utterance
  fn = paste(DATA$plot_dir, "by-participants-normalized-ratings-with-production", sep=fs)
  if(!dir.exists(fn)){dir.create(fn, recursive = TRUE)}
  plotSliderRatingsAndUtts(DATA$joint.smooth, fn)
}
