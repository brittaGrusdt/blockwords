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

# plot particular conditions
pids =  dat$prolific_id %>% unique
dat <- tables.long %>% ungroup() %>%
  filter(id %in% c("if1_u-Lh", "independent_ul")) %>%
  mutate(order=case_when(question=="bg" ~ 1,
                         question=="b" ~ 2,
                         question=="g" ~ 3,
                         question=="none" ~ 4),
          question=as.factor(question),         
          question=fct_recode(question,
                              "both" = "bg", 
                              "only blue" = "b",
                              "only green" = "g"),
         question=fct_reorder(question, order))
df = dat %>% filter(prolific_id %in% pids[1:6]) %>% 
  group_by(question, prolific_id)
p <- dat %>% 
  ggplot(aes(x=question, y=response)) +
  geom_violin() +
  geom_jitter(data=df, aes(shape=prolific_id), width=0.1, height = 0) +
  geom_line(data=df, aes(group=prolific_id), alpha=0.5) +
  theme_classic() +
  theme(legend.position="none") +
  labs(y="observed slider rating") +
  facet_wrap(~id)
ggsave(paste(DATA$plot_dir, "example.png", sep=fs), p)


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
p1 = df %>%
  ggplot(aes(x=id)) +
  geom_bar(aes(y=ratio, fill=utt.type), stat="identity") + #, position=position_dodge()) +
  geom_point(aes(y=prior.mean))
p1

# 2.causality plays a role for utterance when no more informative utt applicable
dat = DATA$joint.smooth %>% group_by(id) %>%
  filter(id %in% c("independent_ul","if1_uh", "if1_u-Lh",
                   "independent_uh", "if2_ul", "if2_u-Ll")) %>%
  filter(human_exp2==1) %>% dplyr::select(prolific_id, id, utterance)

df.unc = dat %>% group_by(id) %>%
  mutate(utt.type = case_when(utterance %in% standardized.conj ~ "conj",
                              utterance %in% standardized.lit ~ "lit",
                              utterance %in% standardized.ifs ~ "conditional",
                              str_detect(utterance, "might") ~ "might")) %>%  
  group_by(id, utt.type) %>% dplyr::select(-utterance) %>% 
  summarize(count=n(), .groups="drop_last") %>% 
  mutate(N=sum(count)) %>% group_by(id, utt.type) %>% 
  mutate(ratio=count/N) %>% dplyr::select(id, utt.type, ratio) %>%
  mutate(stimulus=as.factor(id),
         utt.type=as.factor(utt.type))
  
p2 = df.unc %>%
  ggplot(aes(x=id)) +
  geom_bar(aes(y=ratio, fill=utt.type), stat="identity") +
  theme(legend.position="top")
p2
ggsave(paste(DATA$plot_dir, "behavioral-ifs.png", sep=fs), p2)

df.unc %>% mutate(id=as.character(id), utt.type=as.character(utt.type)) %>%
  filter(utt.type=="conditional") %>% arrange(ratio)

# Model-vs-human summaries ---------------------------------------------------
# @arg tables_fn: "tables-dirichlet", "tables-dirichlet-filtered", "tables-model"
# @arg base_predictions: "table-based-predictions", "prior-samples-based-predictions"
plotAveragePredictions = function(tables_fn, dir_empiric){
  target_dir = here("model", "results", tables_fn)
  plot_dir = paste(target_dir, "plots", sep=fs)
  if(!dir.exists(plot_dir)) {
    dir.create(plot_dir)
  }
  model.avg = readRDS(paste(target_dir, "model-avg-predictions.rds", sep=fs)) %>%
    filter(stimulus != "ind2") %>% rename(model=p)
  behav.avg = readRDS(paste(dir_empiric, "behavioral-avg-task2.rds", sep=fs)) %>%
    rename(stimulus = id) %>% filter(stimulus != "ind2") %>% rename(behavioral=ratio)
  
  data.joint = left_join(behav.avg %>% dplyr::select(-N, -count),
                         model.avg %>% dplyr::select(-predictor, -best.utt), 
                         by=c("stimulus", "utterance"))
  data.joint.long = data.joint %>%
    pivot_longer(cols=c(behavioral, model), names_to="predictor", values_to="p") %>%
    mutate(utterance=factor(utterance, levels = levels.responses),
           predictor=as.factor(predictor))

  p.bars = data.joint.long %>% filter(p > 0) %>% arrange(desc(p)) %>% 
    ggplot(aes(x=p, y=utterance, fill=predictor)) +
    geom_bar(stat="identity", position=position_dodge()) +
    theme_bw(base_size=20) +
    theme(legend.position="bottom") +
    labs(x="ratio participants/average model prediction") +
    facet_wrap(~stimulus)
  ggsave(paste(plot_dir, "avg_comparison_bars.png", sep=fs), p.bars,
         height=20, width=16)

  df.joint = data.joint %>% chunk_utterances()
  p.scatter = 
    ggscatter(df.joint, y = "behavioral", x = "model", add = "reg.line",
              conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson",
              ylab = "Empirical observations", xlab = "Model predictions") +
    geom_point(data=df.joint, aes(y=behavioral, x=model, color=utterance)) +
    theme_bw(base_size=20) + theme(legend.position = "top") 
  ggsave(paste(plot_dir, "avg_comparison_scattered.png", sep=fs),
         p.scatter, height=12, width=20)
  
  p.scatter.stim = p.scatter + facet_wrap(~stimulus)
  ggsave(paste(plot_dir, "avg_comparison_scattered-stim.png", sep=fs),
         p.scatter.stim, height=14, width=20)
  return(df.joint)
}

df = plotAveragePredictions("tables-dirichlet-filtered", DATA$result_dir)
df = plotAveragePredictions("tables-model", DATA$result_dir)

# single participants
plotModelAndBehavioral = function(fn_tables){
  save_to = paste(DATA$plot_dir, "by-stimulus", sep=fs)
  if(!dir.exists(save_to)) {dir.create(save_to, recursive = TRUE)}
  mapping = readRDS(
    here("model", "data", paste("mapping-", fn_tables, "-ids.rds", sep=""))
  )
  dat = readRDS(
    here("model", "results", fn_tables, "model-behavioral-predictions.rds")
  ) %>% mutate(utterance=factor(utterance, levels=levels.responses)) %>%
    dplyr::select(-stimulus)
  
  df.ids = dat %>% ungroup() %>% dplyr::select(prolific_id, id) %>%
    distinct() 
  
  for(i in seq(1, nrow(df.ids))) {
    if(i%%10==0) print(i)
    pid = df.ids[i,]$prolific_id
    id = df.ids[i,]$id
    df.row = dat %>% filter(prolific_id == pid & id == (!! id)) %>%
      mutate(table_id=as.factor(table_id))
    target_folder = paste(save_to, id, sep=fs) 
    if(!dir.exists(target_folder)) dir.create(target_folder);
    
    behavioral = df.row %>% ungroup() %>%
      dplyr::select(-table_id, -model.p, -orig.table) %>% distinct()
    behavioral.uttered = behavioral %>% filter(human_exp2==1) 
    emp_id = behavioral.uttered$empirical_id %>% unique()
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
      geom_vline(aes(xintercept=0.7), color="gray", linetype="solid", size=1,
                 show.legend=FALSE) +
      ggtitle(pid)
      
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
    ggsave(paste(save_to, fs, id, fs, "empirical_id_", emp_id, ".png", sep=""),
           p.speaker, height=12, width=20)
  }
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
  plotModelAndBehavioral("tables-model")
  plotModelAndBehavioral("tables-dirichlet")
  
  # normalized slider Ratings with produced utterance
  fn = paste(DATA$plot_dir, "by-participants-normalized-ratings-with-production", sep=fs)
  if(!dir.exists(fn)){dir.create(fn, recursive = TRUE)}
  plotSliderRatingsAndUtts(DATA$joint.smooth, fn)
}
