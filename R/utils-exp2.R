expected.conjunction = c("if1_hh", "if1_lh", "if2_hl", "if2_ll",
                         "independent_hh", "independent_ll", "independent_hl",
                         "ind2");
expected.literal = c("independent_uh", "independent_ul");
expected.conditional = c("if1_uh", "if1_u-Lh", "if2_ul", "if2_u-Ll");
expected.both = c("if1_hh", "if2_hl", "independent_hh")
expected.none = c("if1_lh", "if2_ll", "independent_ll")

summarize_utts = function(df, w_pos, w_neg, utt){
  for(w in w_pos) {
    df[[paste('word', w, sep='_')]] = with(df, str_detect(response, w))
  }
  
  for(w in w_neg) {
    df[[paste('word_not', w, sep='_')]] = with(df, !str_detect(response, w))
  }
  
  dat <- df %>% group_by(prolific_id, id) %>% 
    pivot_longer(cols=starts_with('word_'), names_to='word',
                 values_to = "has_word") %>% 
    group_by(prolific_id, id) %>%
    # filter(has_word) %>% 
    mutate(has_all=sum(has_word),
           response=
             case_when(has_all ==  length(w_pos) + length(w_neg) ~ utt,
                       TRUE ~ response)
           ) %>%
    dplyr::select(-has_all, -word, -has_word) %>% ungroup() %>% distinct()
  return(dat)
}

standardized.sentences = list(
  bg = "both blocks fall",
  none = "neither block falls",
  g = "green falls but blue does not fall",
  b = "blue falls but green does not fall",
  
  if_gb = "if green falls blue falls",
  if_gnb = "if green falls blue does not fall",
  if_bg = "if blue falls green falls",
  if_bng = "if blue falls green does not fall",
  
  if_nbng = "if blue does not fall green does not fall",
  if_nbg = "if blue does not fall green falls",
  if_ngb = "if green does not fall blue falls",
  if_ngnb = "if green does not fall blue does not fall",
  
  might_g = "green might fall",
  might_b = "blue might fall",
  might_ng = "green might not fall",
  might_nb = "blue might not fall",
  
  only_g = "green falls",
  only_b = "blue falls",
  only_ng = "green does not fall",
  only_nb = "blue does not fall"
);

# (the green and the blue = the blue and the green, etc.)
standardize_sentences = function(df.test){
  df.test = df.test %>% mutate(response = as.character(response))
  test.standardized <- df.test %>%
    summarize_utts(c("and"), c("does not"), standardized.sentences$bg) %>%
    mutate(response=str_replace(response, "and", "but")) %>% 
    summarize_utts(c("neither"), c("does not"), standardized.sentences$none) %>%
    summarize_utts(c("but", "green falls", "blue does not"), c(), standardized.sentences$g) %>%
    summarize_utts(c("but", "blue falls", "green does not"), c(), standardized.sentences$b) %>% 
    summarize_utts(c("if green falls"), c("does not"), standardized.sentences$if_gb) %>%
    summarize_utts(c("if blue falls"), c("does not"), standardized.sentences$if_bg) %>%
    summarize_utts(c("if green does not fall", "blue does not fall"), c(),
                   standardized.sentences$if_ngnb) %>%
    summarize_utts(c("if blue does not fall", "green does not fall"), c(),
                   standardized.sentences$if_nbng) %>%
    summarize_utts(c("if blue falls", "green does not fall"), c(),
                   standardized.sentences$if_bng) %>%
    summarize_utts(c("if green does not fall", "blue falls"), c(),
                   standardized.sentences$if_ngb) %>% 
    summarize_utts(c("if green falls", "blue does not fall"), c(),
                   standardized.sentences$if_gnb) %>%
    summarize_utts(c("if blue does not fall", "green falls"), c(),
                   standardized.sentences$if_nbg) %>%
    
    summarize_utts(c("but", "the green block falls", "the blue block does not"),
                   c(), standardized.sentences$g) %>%
    summarize_utts(c("but", "the blue block falls", "the green block does not"),
                   c(), standardized.sentences$b) %>%
    summarize_utts(c("if the green block falls"), c("does not"), standardized.sentences$if_gb) %>%
    summarize_utts(c("if the blue block falls"), c("does not"), standardized.sentences$if_bg) %>%
    summarize_utts(c("if the green block does not fall", "the blue block does not fall"), c(),
                   standardized.sentences$if_ngnb) %>%
    summarize_utts(c("if the blue block does not fall", "the green block does not fall"), c(),
                   standardized.sentences$if_nbng) %>%
    
    summarize_utts(c("if the blue block falls", "the green block does not fall"), c(),
                   standardized.sentences$if_bng) %>%
    summarize_utts(c("if the green block does not fall", "the blue block falls"), c(),
                   standardized.sentences$if_ngb) %>% 
    summarize_utts(c("if the green block falls", "the blue block does not fall"), c(),
                   standardized.sentences$if_gnb) %>%
    summarize_utts(c("if the blue block does not fall", "the green block falls"), c(),
                   standardized.sentences$if_nbg) %>%
    
    summarize_utts(c("the green block might fall"), c(), standardized.sentences$might_g) %>%
    summarize_utts(c("the blue block might fall"), c(), standardized.sentences$might_b) %>%
    summarize_utts(c("the green block might not fall"), c(), standardized.sentences$might_ng) %>%
    summarize_utts(c("the blue block might not fall"), c(), standardized.sentences$might_nb) %>%
    summarize_utts(c("the green block falls"), c(), standardized.sentences$only_g) %>%
    summarize_utts(c("the blue block falls"), c(), standardized.sentences$only_b) %>%
    summarize_utts(c("the green block does not fall"), c(), standardized.sentences$only_ng) %>%
    summarize_utts(c("the blue block does not fall"), c(), standardized.sentences$only_nb);
    
  utterances <- test.standardized %>% dplyr::select(response) %>% unique()
  print('standardized responses:')
  print(utterances)
  return(test.standardized)
}

# translate A/C-responses to real colors
# @arg speaker.model: long format
translate_utterances = function(speaker.model, group="bg"){
  mapping = tribble(~group, ~A, ~`-A`, ~C, ~`-C`, 
                    "gb", "green falls", "green does not fall",
                    "blue falls", "blue does not fall",
                    "bg", "blue falls", "blue does not fall",
                    "green falls", "green does not fall"
  ) %>% filter(group == (!! group))
  
  df <- speaker.model %>%
    mutate(response=case_when(
      str_detect(response, "-C") ~ str_replace(response, "-C", mapping$`-C`),
      str_detect(response, "C") ~ str_replace(response, "C", mapping$`C`),
      TRUE ~ response)) %>%
    mutate(response=case_when(
      str_detect(response, "-A") ~ str_replace(response, "-A", mapping$`-A`),
      str_detect(response, "A") ~ str_replace(response, "A", mapping$`A`),
      TRUE ~ response))
  df = df %>% 
    mutate(response=case_when(str_detect(response, " >") ~
                                paste("if", str_replace(response, " >", "")),
                              TRUE ~ response)) %>%
    mutate(response=case_when(str_detect(response, "likely") ~ str_replace(response, "falls", "might fall"),
                              TRUE ~ response)) %>%
    mutate(response=case_when(str_detect(response, "likely") ~ str_replace(response, "does not fall", "might not fall"),
                              TRUE ~ response)) %>% 
    mutate(response=case_when(str_detect(response, "might") ~ str_replace(response, "likely", ""),
                              TRUE ~ response)) %>%
    mutate(response=case_when(response=="green falls and blue falls" ~ standardized.sentences$bg,
                              response=="blue falls and green falls" ~ standardized.sentences$bg,
                              response=="green does not fall and blue does not fall" ~
                                standardized.sentences$none,
                              response=="blue does not fall and green does not fall" ~
                                standardized.sentences$none,
                              TRUE ~ response)) %>%
    mutate(response=str_replace(response, "and", "but")) %>%
    mutate(response=case_when(
      response=="green does not fall but blue falls" ~
        "blue falls but green does not fall",
      response=="blue does not fall but green falls" ~
        "green falls but blue does not fall",
      TRUE ~ response)) %>%
    mutate(response=str_trim(response)) %>%
    ungroup() 
  return(df)
}

#@arg df: with columns 'utterance' and 'prob'
translate_probs_to_utts = function(df){
  dat = df %>% mutate(utterance=
           case_when(prob=="b" ~ standardized.sentences$b,
                     prob=="g" ~ standardized.sentences$g,
                     prob=="bg" ~ standardized.sentences$bg,
                     prob=="none" ~ standardized.sentences$none,
                     prob=="p_a" ~ "blue falls",
                     prob=="p_c" ~ "green falls",
                     prob=="p_na" ~ "blue does not fall",
                     prob=="p_nc" ~ "green does not fall",
                     prob=="p_c_given_a" ~ standardized.sentences$if_bg,
                     prob=="p_c_given_na" ~ standardized.sentences$if_nbg,
                     prob=="p_a_given_c" ~ standardized.sentences$if_gb,
                     prob=="p_a_given_nc" ~ standardized.sentences$if_ngb,
                     prob=="p_nc_given_a" ~ standardized.sentences$if_bng,
                     prob=="p_nc_given_na" ~ standardized.sentences$if_nbng,
                     prob=="p_na_given_c" ~ standardized.sentences$if_gnb,
                     prob=="p_na_given_nc" ~ standardized.sentences$if_ngnb,
                     prob=="p_likely_a" ~ "blue might fall",
                     prob=="p_likely_na" ~ "blue might not fall",
                     prob=="p_likely_c" ~ "green might fall",
                     prob=="p_likely_nc" ~ "green might not fall",
                     TRUE ~ NA_character_)
  )
 return(dat)
}

plotProductionTrials <- function(df.production.means, target_dir, min=0,
                                 dat.prior_empirical=tibble()){
  ids = df.production.means$id %>% unique()
  n = ids %>% length();
  brks=seq(0, 1, by=0.1)
  
  df.production.means <- df.production.means %>% filter(ratio>min)
    for(i in seq(1, n)) {
      df <- df.production.means %>% filter(id == ids[[i]]) %>%
        ungroup() %>%
        dplyr::select(-prolific_id) %>% distinct() %>%
        mutate(response=fct_reorder(response, ratio))
      p <- df %>%
        ggplot(aes(y=response, x=ratio)) +
        geom_bar(aes(fill=predictor), stat="identity",
                 position=position_dodge2(preserve="single")) +
        # geom_errorbar(data=df %>% filter(id==ids[[i]] & predictor=="model"),
        #               aes(xmin=ratio-sd, xmax=ratio+sd)) +
        theme_bw() +
        theme(text = element_text(size=20),
              axis.text.x=element_text(angle=45, vjust = 0.5)) +
        labs(x="ratio participants", y="response", title = ids[[i]]) +
        scale_y_discrete(labels = function(ylab) str_wrap(ylab, width = 27.5))
      if(nrow(dat.prior_empirical) != 0){
        priors = left_join(df.production.means %>% dplyr::select(prolific_id, id, response),
          dat.prior_empirical %>% dplyr::select(prolific_id, id, utterance, prob, val),
          by=c("prolific_id", "id")) %>%  filter(id == ids[[i]] & response==utterance) 
        priors.mean = priors %>% group_by(response) %>%
          summarize(m=mean(val), .groups="drop") %>%
          mutate(response=factor(response))
        p <- p + 
          geom_jitter(data=priors,
                      width=0, height=0.1,
                     aes(x=val, y=response, color=prolific_id), size=2, alpha=0.5) +
          geom_point(data=priors.mean, aes(x=m, y=response), shape='*', size=8, color='orange') +
          guides(color=FALSE)
      }
      ggsave(paste(target_dir,
                   paste(ids[[i]], ".png", sep=""), sep=.Platform$file.sep), p,
             width=8, height=10)
      
      print(p)
    }
}

plotSliderRatingsAndUtts <- function(dat, target_dir){
  stimuli = dat$id %>% unique()
  participants = dat$prolific_id %>% unique()
  df = dat %>% group_by(prolific_id, id, utterance) %>%
    mutate(utterance=factor(utterance, levels=levels.responses))
  brks=seq(0, 1, by=0.1)
  for(pid in participants){
    df.pid = df %>% filter(prolific_id == (!! pid))
    target = paste(target_dir, pid, sep=fs)
    dir.create(target)
    for(stimulus in stimuli) {
      df.stim <- df.pid %>%
        filter(id == (!! stimulus)) %>%
        arrange(human_exp1)
      levels.utt = df.stim$utterance %>% unique()
      uttered = df.stim %>% filter(!is.na(human_exp2)) %>% ungroup() %>% 
        dplyr::select(human_exp2, prolific_id, id, utterance) %>% distinct()
      
      df.stim = df.stim %>%
        mutate(utt.col=case_when(
          utterance %in% c(standardized.sentences$bg,
                           standardized.sentences$none,
                           standardized.sentences$b,
                           standardized.sentences$g) ~ "red",
          TRUE ~ "black"),
          utt.face = case_when(utterance == uttered$utterance ~ "bold",
                                TRUE ~ "plain"),
          utterance=factor(utterance, levels=levels.utt)) 
        p <- df.stim %>%
          ggplot() +
          geom_bar(aes(y=utterance, x=human_exp1), stat="identity") +
          geom_vline(aes(xintercept=0.7), color="grey", linetype='dashed') +
          geom_point(data=uttered, aes(x=human_exp2, y=utterance),
                     color='orange', size=2) +
          theme_bw() +
          theme(text = element_text(size=20),
                axis.text.x=element_text(angle=45, vjust = 0.5),
                axis.text.y=element_text(color=df.stim$utt.col, face=df.stim$utt.face)) +
          labs(x="rated probability", y="response", title = stimulus)
        
      ggsave(paste(target, paste(stimulus, "-", pid, ".png", sep=""), sep=fs),
             p ,width=8, height=9)
    }
  }
}

plotRatingsAndModel <- function(df, target_dir){
  stimuli = df$stimulus_id %>% unique()
  df.long = df %>% filter(!is.na(model_exp2)) %>% 
    dplyr::select(-table_id) %>% distinct() %>% 
    group_by(prolific_id, stimulus_id, response, cn) %>%
    pivot_longer(cols=c("model_exp2", "human_exp1"),
                 names_to="predictor", values_to="probs") %>%
    group_by(predictor, stimulus_id, prolific_id, response) %>%
    mutate(response=factor(response, levels=levels.responses))
  brks=seq(0, 1, by=0.1)
  for(stimulus in stimuli) {
    dat <- df.long %>% filter(stimulus_id == stimulus & !is.na(probs)) %>% distinct()
    uttered = dat %>% filter(!is.na(human_exp2)) %>% ungroup() %>% 
      dplyr::select(human_exp2, prolific_id, stimulus_id, response) %>% distinct()
    p <- dat %>%
      ggplot(aes(y=response, x=probs)) +
      geom_bar(aes(fill=predictor), stat="identity",
               position=position_dodge2(preserve="single")) +
      geom_vline(aes(xintercept=0.8), color="grey", linetype='dashed') +
      geom_point(data=uttered, aes(x=human_exp2, y=response), color='orange', size=5) +
      facet_wrap(~prolific_id) +
      theme_bw() +
      theme(text = element_text(size=20),
            axis.text.x=element_text(angle=45, vjust = 0.5)) +
      labs(x="ratio participants", y="response", title = stimulus)# +
      # scale_y_discrete(labels = function(ylab) str_wrap(ylab, width = 27.5))
    ggsave(paste(target_dir,
                 paste(stimulus, "-with-model.png", sep=""), sep=.Platform$file.sep), p,
           width=15, height=15)
    
    print(p)
  }
}

# computes counts of utterances per stimulus and corresponding ratio
task2_avg_per_stimulus = function(result_dir){
  behavioral = readRDS(paste(result_dir, "human-exp1-smoothed-exp2.rds", sep=fs))
  behav.n_per_stim = behavioral %>% dplyr::select(prolific_id, id) %>% 
    distinct() %>% group_by(id) %>% summarize(N=n(), .groups="drop_last")
  
  behav.count = behavioral %>%
    ungroup() %>% dplyr::select(-human_exp1, -prolific_id) %>%
    mutate(human_exp2 = case_when(is.na(human_exp2) ~ 0, 
                                  TRUE ~ 1)) %>%
    group_by(id, utterance) %>%
    summarize(count = sum(human_exp2), .groups="drop_last")
  
  behav.avg = left_join(behav.n_per_stim, behav.count, by="id") %>%
    mutate(ratio=count/N)
  return(behav.avg)
}

# across stimuli
average_predictions = function(dat.speaker, params, target_fn){
  # table_id can occur for different stimuli, model predictions were done only
  # based on table as stimulus doesnt have an influence on it!
  bn_samples = params$bns_sampled %>% group_by(table_id, stimulus) %>%
    mutate(stimulus=list(rep(stimulus, n))) %>% dplyr::select(-n)
  df = dat.speaker %>% ungroup() %>% dplyr::select(cn, table_id, utterance, probs)
  df.model = left_join(df %>% group_by(table_id), bn_samples, by=c("table_id"))
  
  df.bn_samples = df.model %>%
    group_by(table_id) %>% 
    pivot_wider(names_from="utterance", values_from="probs", names_prefix="utt.") %>%
    unnest(c(stimulus)) %>% 
    pivot_longer(cols=starts_with("utt."), names_to="response",
                 names_prefix="utt.",values_to="probs")
  # get counts of how often each stimulus appears
  if(str_detect(params$used_tables, "tables_model")){
    # for model tables stimuli are matched to generated tables, i.e. they are not 
    # generated for particular stimuli as we do for dirichlets, which means 
    # that one table may map to several stimuli
    df.bn_samples = df.bn_samples %>% rowid_to_column() %>%  unnest(c(stimulus)) %>% 
      distinct_at(vars(c(rowid, stimulus)), .keep_all = TRUE) %>%
      ungroup() %>% dplyr::select(-rowid)
  }
  df.n_stim = df.bn_samples %>% group_by(stimulus) %>%
    summarize(n.stim=n()/20, .groups="drop_last")
  
  model.avg.bn_samples = df.bn_samples %>% 
    group_by(stimulus, response) %>% 
    summarize(p=mean(probs), .groups="drop_last") %>%
    translate_utterances() %>% 
    add_column(predictor="model") %>%
    rename(utterance=response) %>%
    group_by(stimulus) %>% mutate(best.utt=p==max(p))
  
  # add nb of stimuli that were included in average
  model.avg = left_join(model.avg.bn_samples, df.n_stim, by="stimulus")
  
  save_data(model.avg, paste(params$target_dir, fs, target_fn, ".rds", sep=""))
  write_csv(model.avg, paste(params$target_dir, fs, target_fn, ".csv", sep=""))
  return(model.avg)
}

# put model prediction for particular table_id/empirical_ids together with
# corresponding behavioral observations, for states where both exist
join_model_behavioral_data = function(dat.speaker, params){
  # 1. Map Model data to empirical tables
  # model only predicts speaker once per table_id! (indep.of stimulus/cn)
  data.model <- dat.speaker %>%
    group_by(table_id, cn) %>%
    dplyr::select(table_id, cn, stimulus, AC, `A-C`, `-AC`, `-A-C`,
                  utterance, probs)
  
  path_empiric = paste(params$dir_empiric, "tables-empiric-pids.rds", sep=fs)
  tables.empiric = readRDS(path_empiric)
  message(paste("read empiric data from", path_empiric))
  pids = tables.empiric %>% ungroup() %>%
    dplyr::select(-bg, -b, -g, -none, -ends_with(".round")) %>% unnest(c(p_id))
  
  mapping.ids = readRDS(params$tables_mapping) %>% group_by(empirical_id)
  orig.tables = mapping.ids %>% filter(orig_table) %>%
    dplyr::select(empirical_id, table_id, AC, `A-C`, `-AC`, `-A-C`,
                  ends_with(".round"))
  
  # iterate through all empirical ids, and note all table_ids that are associated 
  # with that empirical id (as there are several since empirical tables were augmented)
  ids = mapping.ids %>% ungroup() %>% dplyr::select(table_id, empirical_id)
  empirical_ids = ids %>% pull(empirical_id) %>% unique()
  table_map = mapping.ids %>% group_by(empirical_id) %>%
    summarize(table_ids=list(table_id), .groups="drop_last")
  
  df = data.model %>% ungroup() %>% dplyr::select(table_id) %>% distinct() 
  mapping=map_dfr(empirical_ids, function(id){
    current = table_map %>% filter(empirical_id == id)
    if(nrow(current) != 0){
      table_ids =  current %>% unnest(c(table_ids)) %>% pull(table_ids)
      dat = df %>% mutate(empirical_id=case_when(table_id %in% table_ids ~ id)) %>%
        filter(!is.na(empirical_id))
    } else {
      dat = tibble()
    }
    return(dat)
  });
  predictions = data.model %>% ungroup() %>% 
    dplyr::select(-AC, -`A-C`, -`-AC`, -`-A-C`, -cn) %>%
    group_by(table_id)
  # model predictions may contain predictions for tables that no participant had actually used!
  # (empirical_id is NA)
  res.model = left_join(predictions, mapping, by="table_id") %>% 
    group_by(empirical_id) %>%
    rename(response=utterance) %>%
    translate_utterances() %>% group_by(empirical_id)

  # merge empirical data with model predictions for states we have observations
  # and model predictions for
  # add data which participant used which empirical id in which trial
  ids = left_join(pids, mapping, by="empirical_id") %>% group_by(empirical_id) %>%
    separate(p_id, into=c("prolific_id", "stimulus", "prior"), sep="_") %>%
    unite("id", c(stimulus, prior), sep="_")

  data.joint = readRDS(paste(params$dir_empiric, "human-exp1-smoothed-exp2.rds",
                             sep=.Platform$file.sep))
  res.behavioral = left_join(data.joint, ids, by=c("prolific_id", "id")) %>%
    group_by(prolific_id, id) 
  
  orig.ids = orig.tables$table_id
  behav_model = left_join(
    res.behavioral %>% filter(!is.na(table_id)),
    res.model %>% rename(utterance=response, model.p=probs) %>% filter(!is.na(empirical_id)),
    by=c("table_id", "empirical_id", "utterance")
  ) %>% group_by(prolific_id, id, table_id) %>%
    mutate(orig.table=case_when(table_id %in% orig.ids ~ TRUE,
                                TRUE ~ FALSE))
  # compute utterance probabilities, only needed for empirical tables (in plots)
  model.tables = mapping.ids %>% filter(!is.na(empirical_id)) %>% 
    dplyr::select(table_id, empirical_id, AC, `A-C`, `-AC`, `-A-C`) %>%
    rename(bg=AC, b=`A-C`, g=`-AC`, none=`-A-C`) %>% add_probs() %>% 
    group_by(table_id) %>% 
    pivot_longer(cols=c(bg, b, g, none, starts_with("p_")),
                 names_to="utterance", values_to="response") %>%
    rename(prob=utterance) %>% translate_probs_to_utts() %>% 
    rename(`model.table`= response) %>% dplyr::select(-prob)
    
  # data.behav_model = left_join(
  #   behav_model, model.tables,
  #   by=c("table_id", "empirical_id", "utterance")
  # )
  if(params$save) {
    save_data(behav_model,
              paste(params$target_dir, "model-behavioral-predictions.rds",
                    sep=.Platform$file.sep))
  }
  return(behav_model)
}
