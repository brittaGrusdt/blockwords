library(cowplot)
library(ggpubr)
library(here)
source(here("R", "analysis-utils.R"))

# WHICH DATA TO USE
exp.name = "toy-blocks-pilot-2"
use_filtered = FALSE
DATA = load_exp_data(exp.name, use_filtered)
base_dir = here("data", "prolific", exp.name)
################################################

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


# Behavioral Data ---------------------------------------------------------
dat = DATA$joint.smooth %>% group_by(id) %>%
  filter(id %in% c("if1_hh", "if1_lh","if1_uh", "if1_u-Lh",
                   "if2_hl", "if2_ll", "if2_ul", "if2_u-Ll") & 
         human_exp2==1) %>%
  dplyr::select(prolific_id, id, human_exp1, utterance) %>%
  mutate(group=case_when(human_exp1 >= 0.4 & human_exp1 <= 0.6 ~ "uncertain", 
                         TRUE ~ "certain"))

df = dat %>% group_by(id) %>%
  mutate(conj = utterance %in% standardized.conj,
         lit = utterance %in% standardized.lit, 
         conditional = utterance %in% standardized.ifs) %>%
  pivot_longer(cols=c(conj, lit, conditional),
               names_to="utt.type", values_to="selected") %>%
  group_by(id, utt.type) %>% 
  summarize(ratio=sum(selected)/n(), prior.mean=mean(human_exp1), .groups="drop_last") %>% 
  separate("id", into=c("stimulus", "condition"), sep="_", remove = FALSE) %>%
  mutate(stimulus=as.factor(stimulus),
         utt.type=as.factor(utt.type))

# 1. more conditionals in uncertain conditions than certain
df %>%
  ggplot(aes(x=id)) +
  geom_bar(aes(y=ratio, fill=utt.type), stat="identity") + #, position=position_dodge()) +
  geom_point(aes(y=prior.mean))

# 2.causality plays a role for utterance when no more informative utt applicable
dat = DATA$joint.smooth %>% group_by(id) %>%
  filter(id %in% c("independent_ul","if1_uh", "if1_u-Lh",
                   "independent_uh", "if2_ul", "if2_u-Ll") & 
           human_exp2==1) %>%
  dplyr::select(prolific_id, id, human_exp1, utterance) %>%
  mutate(group=case_when(human_exp1 >= 0.4 & human_exp1 <= 0.6 ~ "uncertain", 
                         TRUE ~ "certain"))

df = dat %>% group_by(id) %>%
  mutate(conj = utterance %in% standardized.conj,
         lit = utterance %in% standardized.lit, 
         might = str_detect(utterance, "might"),
         conditional = utterance %in% standardized.ifs) %>%
  pivot_longer(cols=c(conj, lit, conditional, might),
               names_to="utt.type", values_to="selected") %>%
  group_by(id, utt.type) %>% 
  summarize(ratio=sum(selected)/n(), prior.mean=mean(human_exp1), .groups="drop_last") %>% 
  separate("id", into=c("stimulus", "condition"), sep="_", remove = FALSE) %>%
  mutate(stimulus=as.factor(stimulus),
         utt.type=as.factor(utt.type))

df %>%
  ggplot(aes(x=id)) +
  geom_bar(aes(y=ratio, fill=utt.type), stat="identity") + #, position=position_dodge()) +
  geom_point(aes(y=prior.mean))

# Model-vs-human summaries ---------------------------------------------------
# @arg tables_fn: "tables-dirichlet", "tables-dirichlet-filtered", "tables-model"
# @arg base_predictions: "table-based-predictions", "prior-samples-based-predictions"
plotAveragePredictions = function(tables_fn, base_predictions){
  target_dir = here("model", "results", tables_fn, base_predictions)
  plot_dir = paste(target_dir, "plots", sep=fs)
  if(!dir.exists(plot_dir)) {
    dir.create(plot_dir)
  }
  data.joint = readRDS(paste(target_dir, "model-behavioral-avg-stimuli.rds", sep=fs))
  data.joint = data.joint %>%
    mutate(utterance=factor(utterance, levels = levels.responses),
           predictor=as.factor(predictor))

  p.bars = data.joint %>% filter(p > 0) %>% arrange(desc(p)) %>% 
    ggplot(aes(x=p, y=utterance, fill=predictor)) +
    geom_bar(stat="identity", position=position_dodge()) +
    theme_bw(base_size=20) +
    theme(legend.position="bottom") +
    labs(x="ratio participants/average model prediction") +
    facet_wrap(~stimulus)
  
  ggsave(paste(plot_dir, "avg_comparison_bars.png", sep=fs), p.bars,
         height=20, width=16)

  data.wide = data.joint %>% pivot_wider(names_from="predictor", values_from="p")
  p.scatter = 
    ggscatter(data.wide, y = "behavioral", x = "model", add = "reg.line",
              conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson",
              ylab = "Empirical observations", xlab = "Model predictions") +
    geom_point(data=data.wide, aes(y=behavioral, x=model, color=utterance)) +
    theme_bw(base_size=20) + theme(legend.position = "top") 
  ggsave(paste(plot_dir, "avg_comparison_scattered.png", sep=fs),
         p.scatter, height=12, width=20)
  
  p.scatter.stim = p.scatter + facet_wrap(~stimulus)
  ggsave(paste(plot_dir, "avg_comparison_scattered-stim.png", sep=fs),
         p.scatter.stim, height=14, width=20)
  return(data.wide)
}
df = plotAveragePredictions("tables-dirichlet", "prior-samples-based-predictions")
df = plotAveragePredictions("tables-dirichlet", "table-based-predictions")

df = plotAveragePredictions("tables-dirichlet-filtered", "prior-samples-based-predictions")
df = plotAveragePredictions("tables-dirichlet-filtered", "table-based-predictions")

df = plotAveragePredictions("tables-model", "table-based-predictions")


# single participants
plotModelAndBehavioral = function(fn_tables){
  save_to = paste(DATA$plot_dir, "comparison-exp1-exp2-model", fn_tables, sep=fs)
  if(!dir.exists(save_to)) {dir.create(save_to, recursive = TRUE)}
  mapping = readRDS(
    here("model", "data", paste("mapping-", fn_tables, "-ids.rds", sep=""))
  )
  joint_data = readRDS(
    here("model", "results", fn_tables, "model-behavioral-each-table-id.rds")
  )
  dat = left_join(
    joint_data, 
    mapping %>% dplyr::select(empirical_id, p_id) %>% unnest(c("p_id")) %>% distinct(),
    by=c("empirical_id")
  )
  dat = dat %>% 
    separate(p_id, into=c("pid", "stimulus", "prior"), sep="_") %>%
    unite("stimulus", c("stimulus", "prior"), sep="_") %>%
    filter(id==stimulus & prolific_id == pid) %>%
    dplyr::select(-pid, -stimulus) %>% group_by(table_id) %>% 
    mutate(utterance=factor(utterance, levels=levels.responses))
  
  df.ids = dat %>% ungroup() %>% dplyr::select(prolific_id, id, empirical_id) %>%
    distinct() %>% rowid_to_column("rowid")
  
  for(i in seq(1, nrow(df.ids))) {
    if(i%%10==0) print(i)
    pid = df.ids[i,]$prolific_id
    stimulus = df.ids[i,]$id
    df.row = dat %>% filter(prolific_id == (!! pid) & id == (!! stimulus)) %>%
      mutate(table_id=as.factor(table_id))
    target_folder = paste(save_to, pid, sep=fs) 
    if(!dir.exists(target_folder)) dir.create(target_folder);
    
    behavioral = df.row %>% ungroup() %>%
      dplyr::select(-table_id, -model.p, -model.table, -orig.table) %>% distinct()
    behavioral.uttered = behavioral %>% filter(human_exp2==1) 
    table_ids = df.row$table_id %>% unique()
    
    p.speaker = behavioral %>%
      ggplot(aes(y=utterance)) +
      geom_bar(data=behavioral, aes(x=human_exp1), stat="identity", color='grey') +
      geom_point(data=df.row %>%
                   filter(model.p > 0), aes(x=model.p, color=table_id),
                 size=6) +
      geom_point(data=behavioral.uttered, aes(x=human_exp2, shape=utterance),
                 color='orange', size=8) +
      theme_classic(base_size=20) +
      theme(legend.position="bottom") +
      geom_vline(aes(xintercept=0.8), color="gray", linetype="solid", size=1,
                 show.legend=FALSE)
    tbl.orig = df.row %>% filter(orig.table)
    if(tbl.orig %>% nrow() != 0){
      p.speaker = p.speaker +
        geom_point(data=tbl.orig, aes(x=model.p, color=table_id, shape=orig.table),
                   size=6) +
        scale_shape_manual(
          name="", values = c(8, rep(18, length(levels.responses))),
          breaks=c(TRUE, levels.responses),
          labels=c("prediction for exact empirical table", rep("utterance participant",
                                                               length(levels.responses)))
        )
    } else {
      p.speaker = p.speaker +
        scale_shape_manual(
          name="", values = rep(18, length(levels.responses)),
          breaks=levels.responses,
          labels=rep("utterance participant", length(levels.responses))
        )
    }
    ggsave(paste(save_to, fs, pid, fs, stimulus, ".png", sep=""), p.speaker,
           height=12, width=20)
  }
}


# Plots only relevant for non-filtered data -----------------------------------
if(!use_filtered) {
  # Slider Ratings Tables for each participant plotted seperately
  # (takes a while, and not so relevant)
  # 1.slider ratings with utterance and normalized ratings, 
  # save_to = paste(DATA$plot_dir, "slider-ratings-tables", sep=fs)
  # if(!dir.exists(save_to)) {dir.create(save_to)}
  # 
  # for(pid in pids) {
  #   df.pid.orig = df.orig %>% filter(prolific_id == (!! pid))
  #   df.pid.smooth = df.smooth %>% filter(prolific_id == (!! pid))
  #   
  #   target_folder = paste(save_to, pid, sep=fs) 
  #   if(!dir.exists(target_folder)){ dir.create(target_folder)
  #   }
  #   for(stimulus in stimuli) {
  #     df.stim.orig = df.pid.orig %>%
  #       filter(id==stimulus & question %in% questions.test) %>%
  #       rename(means=human_exp1)
  #     if(df.stim.orig %>% nrow() != 0){
  #       df.stim.smooth = df.pid.smooth %>% filter(id==stimulus) %>% rename(normalized=human_exp1)
  #       uttered = df.stim.smooth %>% filter(!is.na(human_exp2)) %>% pull(utterance)
  #       
  #       stim_utt = paste(stimulus, uttered, sep=": ") 
  #       df.stim = left_join(df.stim.orig %>% dplyr::select(-human_exp2),
  #                           df.stim.smooth %>% filter(question %in% questions.test),
  #                           by=c("prolific_id", "id", "question"))
  #       p = df.stim %>% PlotMeans(stimulus, sd_error_bars = FALSE) + 
  #         labs(y="unnormalized slider rating", x="",title=stim_utt) +
  #         geom_point(aes(y=normalized), size=2, color='orange') +
  #         theme(axis.text.x=element_blank(), legend.position="none")
  #       ggsave(paste(save_to, fs, pid, fs, stimulus, ".png", sep=""), p, width=12)
  #     }
  #   }
  # }
  
  # model predictions with theoretic/dirichlet-fitted tables (takes a while)
  plotModelAndBehavioral("tables-model")
  plotModelAndBehavioral("tables-dirichlet")
  
  # normalized slider Ratings with produced utterance
  fn = paste(DATA$plot_dir, "by-participants-normalized-ratings-with-production", sep=fs)
  if(!dir.exists(fn)){dir.create(fn, recursive = TRUE)}
  plotSliderRatingsAndUtts(DATA$joint.smooth, fn)
  
  # Data Quality ------------------------------------------------------------
  dat.quality = readRDS(paste(here("data", "prolific", exp.name), fs,
                              "test-data-prior-quality.rds", sep=""))
  quality.means = dat.quality %>% arrange(desc(mean.comparator)) %>%
    distinct_at(vars(c(comparator)), .keep_all = TRUE)
  worst_quality.ids = quality.means[1: round(0.1 * nrow(quality.means)),] %>% pull(comparator)
  
  quality = dat.quality %>%
    mutate(in_worst=case_when(comparator %in% worst_quality.ids ~ comparator,
                              TRUE ~ "other"))
  p = quality  %>%
    ggplot(aes(x=id,  y=sum_sq_diffs)) +
    geom_boxplot(outlier.shape=NA) +
    geom_jitter(aes(colour=in_worst, shape=in_worst), height=0, width=0.05, alpha=0.4, size=3) +
    theme_bw(base_size = 20) +
    theme(axis.text.x=element_text(angle=90, vjust=0.5), legend.position="top")
  ggsave(paste(DATA$plot_dir, "quality-sum-sq-diff-to-mean.png", sep=fs), p, width=15, height=10)
  
  
  quality.means %>% 
    ggplot(aes(x=mean.comparator)) +
    geom_density() +
    geom_jitter(aes(y=0, color=comparator), width=0, height=0.2) +
    ggtitle("avg data quality each proband across stimuli")
  
  dat.quality %>% distinct_at(vars(c(id)), .keep_all = TRUE) %>% 
    ggplot(aes(x=mean.id)) +
    geom_density() +
    geom_jitter(aes(y=0, color=id), width=0, height=0.2) +
    theme(legend.position="bottom") +
    ggtitle("avg data quality values across stimuili")
  
  # Training Data -----------------------------------------------------------
  dat.tidy = readRDS(paste(base_dir, fs, exp.name, "_tidy.rds", sep=""))
  data.train.sliders = dat.tidy$train.slider_choice
  data.train.smooth = dat.tidy$train.smooth
  data.train.orig = data.tidy$train.orig
  # for each participant only the last 50% of all train trials
  data.train.smooth.half = data.train.smooth %>%
    separate(id, into=c("trial.relation", "trial.idx"), sep=-1, remove=FALSE) %>%
    group_by(prolific_id, trial.relation) %>% arrange(desc(trial_number)) %>%
    top_frac(0.5, trial_number) %>%
    dplyr::select(-expected) %>%
    pivot_wider(names_from=question, values_from=response) %>%
    ungroup() %>%
    dplyr::select(-trial.relation, -trial.idx)
  
  
  dat.sliders = data.train.sliders %>%
    mutate(correct = response == expected, .groups="drop_last")
  
  # ratio correct per participant
  dat.sliders %>% group_by(prolific_id) %>%
    summarize(ratio_correct=sum(correct)/n(), .groups="drop_last") %>% 
    ggplot(aes(x=0, y=ratio_correct)) +
    geom_boxplot(outlier.shape=NA) +
    geom_jitter(aes(color=prolific_id), shape=16, width=0.1, height=0) +
    theme(legend.position = "none")
    
  # ratio correct per id (difficulty slider-choice trials)
  df.sliders = dat.sliders %>% group_by(id) %>% 
    summarize(ratio_correct = sum(correct)/n(), .groups="drop_last") %>%
    arrange(desc(ratio_correct)) %>%
    mutate(id=as_factor(id))
  
  df.sliders %>%
    ggplot(aes(y=id, x=ratio_correct)) +
    geom_point() +
    labs(x="ratio participants who gave correct response")
  
  
  # Data Training Trials
  df.train.edges = data.train.smooth.half %>% filter(id %in% train.edges$id) %>%
    group_by(prolific_id, id) %>% 
    mutate(red=ry + r, yellow=ry + y) %>% 
    left_join(train.edges, join_by = c("id")) %>%
    pivot_longer(c(red, yellow, ry, none, r, y), names_to="event", values_to="response")  %>%
    filter(event == block)
  
  df.train.ramp = data.train.smooth.half %>% filter(id %in% train.ramp$id) %>%
    group_by(prolific_id, id) %>% 
    mutate(red=ry + r, yellow=ry + y) %>% 
    left_join(train.ramp, join_by = c("id")) %>%
    pivot_longer(c(red, yellow, ry, none, r, y), names_to="event", values_to="response")  %>%
    filter(event == block)
  
  df.train.edges %>%
    mutate(relation=case_when(id %in% c("ssw0", "ssw1") ~ "if2",
                              id %in% c("ac0", "ac1", "ac2", "ac3") ~ "if1",
                              TRUE ~ "independent"),
           condition=factor(condition, levels = c("low", "uncertain", "uncertainH", "high"))) %>% 
    plot_ratings_across_conditions("training-trials block on edge")
  
  df.train.ramp %>% 
    mutate(relation=case_when(id %in% c("ssw0", "ssw1") ~ "if2",
                              id %in% c("ac0", "ac1", "ac2", "ac3") ~ "if1",
                              TRUE ~ "independent"),
           condition=factor(condition, levels = c("low", "uncertain", "uncertainH", "high"))) %>% 
    plot_ratings_across_conditions("training trials ramp with ball")
  
  ## Training data: ratings per stimulus, block on edge (low/uncertain/high prior)
  p1 = df.train.edges %>% filter(condition=="high") %>% 
    mutate(condition=as.factor(condition)) %>% 
    ggplot(aes(x=condition, y=response)) + 
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(aes(color=dir), shape=16, width=0.2, alpha=0.5) +
    facet_wrap(~id) +
    theme_classic(base_size = 20) +
    theme(axis.text.x = element_text(size=15)) +
    ggtitle("block on edge") +
    coord_flip()
  ggsave(paste(DATA$plot_dir, "train-edges-high.png", sep=.Platform$file.sep), p1, width=15, height=10)
  p1
  
  p2 = df.train.edges %>% filter(startsWith(condition, "uncertain")) %>% 
    mutate(condition=as.factor(condition)) %>% 
    ggplot(aes(x=condition, y=response)) + 
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(aes(color=dir), shape=16, width=0.2, alpha=0.5) +
    facet_wrap(~id) +
    theme_classic(base_size = 20) +
    theme(axis.text.x = element_text(size=15)) +
    ggtitle("block on edge") +
    coord_flip()
  ggsave(paste(DATA$plot_dir, "train-edges-uncertain.png", sep=.Platform$file.sep), p2, width=15, height=10)
  p2   
  
  p3 = df.train.edges %>% filter(condition == "low") %>% 
    mutate(condition=as.factor(condition)) %>% 
    ggplot(aes(x=condition, y=response)) + 
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(aes(color=dir), shape=16, width=0.2, alpha=0.5) +
    facet_wrap(~id)+
    theme(axis.text.x = element_text(size=15)) +
    ggtitle("block on edge") +
    coord_flip()
  # theme(axis.text.x=element_text(angle=90))
  ggsave(paste(DATA$plot_dir, "train-edges-low.png", sep=.Platform$file.sep), p3, width=15, height=10)
  p3 
}
