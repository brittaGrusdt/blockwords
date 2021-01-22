library(cowplot)
library(ggpubr)
library(here)
source(here("R", "analysis-utils.R"))

pids = data.joint.orig %>% pull(prolific_id) %>% unique()
stimuli =  data.prior.orig %>% pull(id) %>% unique()

df.orig = data.joint.orig %>% 
  mutate(question = case_when(utterance==standardized.sentences$bg ~ "bg",
                               utterance==standardized.sentences$b ~ "b",
                               utterance==standardized.sentences$g ~ "g",
                               utterance==standardized.sentences$none ~ "none", 
                               TRUE ~ utterance))
df.smooth = data.joint.smooth %>%
  mutate(question = case_when(utterance==standardized.sentences$bg ~ "bg",
                               utterance==standardized.sentences$b ~ "b",
                               utterance==standardized.sentences$g ~ "g",
                               utterance==standardized.sentences$none ~ "none", 
                               TRUE ~ utterance))
  
# summary slider-ratings ----------------------------------------------------
tables.smooth = data.joint.smooth %>%
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
save_to = paste(PLOT.dir, "slider-ratings-densities", sep=fs)
if(!dir.exists(save_to)) {dir.create(save_to)}
tables.long %>%
  plotSliderDensities(questions.test, labels.test, target_dir=save_to)

save_to = paste(PLOT.dir, "slider-ratings-boxplots", sep=fs)
if(!dir.exists(save_to)) {dir.create(save_to)}
tables.long %>%
  plotSliderRatings(questions.test, labels.test, cluster_by="bg",
                    relation=FALSE, target_dir=save_to)

# Slider Ratings ----------------------------------------------------------
# 1.slider ratings with utterance and normalized ratings
save_to = paste(PLOT.dir, "slider-ratings-tables", sep=fs)
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
# Literal meaning probabilities -------------------------------------------
df = left_join(data.prior.smooth %>% dplyr::select(-question, -RT) %>%
                 rename(human_exp1=response),
               data.production %>% dplyr::select(-RT) %>% rename(utterance=response),
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

ggsave(paste(PLOT.dir, "prior-vs-utt.png", sep=fs), p, width=14, height=7)

#Same plot, here results plotted separately for each participant. Stimuli are color coded.
p <- df %>%
  ggplot(aes(y=utterance, x=human_exp1)) +
  geom_point(aes(color=id)) +
  geom_vline(aes(xintercept=0.7), color="black", linetype='dashed') +
  facet_wrap(~prolific_id) +
  theme_bw(base_size = 14) +
  theme(legend.position="none") +
  labs(x="rated probability (prior elicitation)", y="response")
ggsave(paste(PLOT.dir, "prior-vs-utt-per-proband.png", sep=fs), p, width=18, height=18)

# Slider Ratings
# normalized ratings with produced utterance
fn = paste(PLOT.dir, "by-participants-normalized-ratings-with-production", sep=fs)
if(!dir.exists(fn)){dir.create(fn, recursive = TRUE)}
plotSliderRatingsAndUtts(data.joint.smooth, fn)
# todo here

# Data Quality ------------------------------------------------------------
quality.means = data.quality %>% arrange(desc(mean.comparator)) %>%
  distinct_at(vars(c(comparator)), .keep_all = TRUE)
worst.ids = quality.means[1: round(0.1 * nrow(quality.means)),] %>% pull(comparator)

quality = data.quality %>%
  mutate(in_worst=case_when(comparator %in% worst.ids ~ comparator,
                            TRUE ~ "other"))
p = quality  %>%
  ggplot(aes(x=id,  y=sum_sq_diffs)) +
  geom_boxplot(outlier.shape=NA) +
  geom_jitter(aes(colour=in_worst, shape=in_worst), height=0, width=0.05, alpha=0.4, size=3) +
  theme_bw(base_size = 20) +
  theme(axis.text.x=element_text(angle=90, vjust=0.5), legend.position="top")
ggsave(paste(PLOT.dir, "quality-sum-sq-diff-to-mean.png", sep=fs), p, width=15, height=10)



# Model-vs-human ----------------------------------------------------------
plotModelAndBehavioral = function(use_dirichlet_tables){
  if(use_dirichlet_tables){
    fn_tables ="tables-dirichlet"
  } else{fn_tables = "tables-model"
  }
  save_to = paste(PLOT.dir, "comparison-exp1-exp2-model", fn_tables, sep=fs)
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

# model predictions with theoretic/dirichlet-fitted tables
plotModelAndBehavioral(use_dirichlet_tables = FALSE)
plotModelAndBehavioral(use_dirichlet_tables = TRUE)

plotAveragePredictions = function(tables_fn, across_empirical){
  if(across_empirical){
    fn = "predictions-empirical-based"
    data = readRDS(here("model", "results", tables_fn,
                        paste("model-behavioral-avg-stimuli_", fn, ".rds", sep="")))
  } else {
    fn = "predictions-stimulus-prior-based"
    data = readRDS(here("model", "results", tables_fn,
                        paste("model-behavioral-avg-stimuli_", fn, ".rds", sep="")))
  }
  data = data %>% mutate(utterance=factor(utterance, levels = levels.responses),
                         predictor=as.factor(predictor))

  p.bars = data %>% filter(p > 0) %>% arrange(desc(p)) %>% 
    ggplot(aes(x=p, y=utterance, fill=predictor)) +
    geom_bar(stat="identity", position=position_dodge()) +
    theme_bw(base_size=20) +
    theme(legend.position="bottom") +
    labs(x="ratio participants/average model prediction") +
    facet_wrap(~stimulus)
  target_dir = paste(PLOT.dir, "comparison-exp1-exp2-model", tables_fn, sep=fs)
  if(!dir.exists(target_dir)){dir.create(target_dir)}
  
  ggsave(paste(target_dir, paste(fn, "bars.png", sep="_"), sep=fs), p.bars,
         height=20, width=16)

  data.wide = data %>% pivot_wider(names_from="predictor", values_from="p")
  p.scatter = 
    ggscatter(data.wide, y = "behavioral", x = "model", add = "reg.line",
              conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson",
              ylab = "Empirical observations", xlab = "Model predictions") +
    geom_point(data=data.wide, aes(y=behavioral, x=model, color=utterance)) +
    theme_bw(base_size=20) + theme(legend.position = "top") 
  ggsave(paste(target_dir, paste(fn, "scattered.png", sep="_"), sep=fs),
         p.scatter, height=12, width=20)
  
  p.scatter.stim = p.scatter + facet_wrap(~stimulus)
  ggsave(paste(target_dir, paste(fn, "scattered-stim.png", sep="_"), sep=fs),
         p.scatter.stim, height=14, width=20)
  return(data.wide)
}
df = plotAveragePredictions("tables-dirichlet", across_empirical=TRUE)
df = plotAveragePredictions("tables-dirichlet", across_empirical=FALSE)
df = plotAveragePredictions("tables-model", across_empirical=TRUE)


# Training Data -----------------------------------------------------------

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
ggsave(paste(PLOT.dir, "train-edges-high.png", sep=.Platform$file.sep), p1, width=15, height=10)
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
ggsave(paste(PLOT.dir, "train-edges-uncertain.png", sep=.Platform$file.sep), p2, width=15, height=10)
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
ggsave(paste(PLOT.dir, "train-edges-low.png", sep=.Platform$file.sep), p3, width=15, height=10)
p3 

