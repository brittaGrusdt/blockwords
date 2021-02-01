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
