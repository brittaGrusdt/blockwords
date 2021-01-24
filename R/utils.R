library(tidyverse)
source(here("model", "R", "helpers-tables.R"))

fs = .Platform$file.sep
epsilon = 0.000001
seed_fitted_tables = "20202020"

test_data <- function(path_to_csv) {
  data <- read_csv(path_to_csv) %>%
    mutate(prolific_id = str_trim(str_to_lower(prolific_id))) %>%
    filter(str_detect(prolific_id, "test-.*") | str_detect(prolific_id, "test "))
  return(data)
}

experimental_data <- function(path_to_csv){
  data <- read_csv(path_to_csv) %>%
    mutate(prolific_id = str_trim(str_to_lower(prolific_id))) %>%
    filter(!str_detect(prolific_id, "test.*") & prolific_id != "" &
           !is.na(prolific_id))
  return(data)
}

save_raw_data <- function(data_dir, data_fn, result_dir, result_fn, debug_run=FALSE){
  path_to_data <- paste(data_dir, data_fn, sep=.Platform$file.sep)
  if(debug_run){
    data <- test_data(path_to_data)
  } else {
    data <- experimental_data(path_to_data)
  }
  
  path_target <- paste(result_dir, paste(result_fn, "raw.csv", sep="_"), sep = .Platform$file.sep)
  write_excel_csv(data, path = path_target, delim = ",", append = FALSE, col_names=TRUE)
  print(paste('written raw data to:', path_target))
  return(data)
}

tidy_test_exp1 <- function(df){
  dat.test <- df %>% filter(str_detect(trial_name, "multiple_slider")) %>%
    dplyr::select(trial_name, trial_number,
           prolific_id, RT, QUD, id, group,
           question1, question2, question3, question4,
           response1, response2, response3, response4) %>%
    pivot_longer(cols=c(contains("response")),
                 names_to = "response_idx", names_prefix = "response",
                 values_to = "response") %>%
    pivot_longer(cols=c(contains("question")),
                 names_to = "question_idx", names_prefix = "question",
                 values_to = "question") %>%
    filter(response_idx == question_idx) %>%
    dplyr::select(-response_idx, -question_idx)

  dat.test <- dat.test %>%
    mutate(response = as.numeric(response),
           response = response/100,
           prolific_id = factor(prolific_id),
           group=case_when(id=="ind2" ~ "group1",
                           TRUE ~ group),
           id = factor(id)
  )
  return(dat.test)
}

tidy_test_exp2 <- function(df){
  dat.test <- df %>%
    filter(startsWith(trial_name, "fridge_view") | trial_name == "fridge_train") %>%
    dplyr::select(prolific_id, RT, QUD, id, group, response1, response2, trial_name,
           trial_number) %>%
    rename(custom_response=response2, response=response1)
  
  dat.test <- dat.test %>%
    mutate(prolific_id = factor(prolific_id),
           id = factor(id))
  return(dat.test)
}

tidy_test_joint <- function(df){
  data.prior = df %>% filter(str_detect(trial_name, "multiple_slider")) %>%
    tidy_test_exp1() %>%
    add_column(custom_response="", utterance="")
  data.production = df %>% filter(str_detect(trial_name, "fridge_")) %>%
    tidy_test_exp2() %>%
    rename(utterance=response) %>%
    add_column(question="")
  dat.test = bind_rows(data.prior, data.production)
  return(dat.test)
}

tidy_train <- function(df){
  dat.train <- df %>%
    filter(startsWith(trial_name, "animation") | trial_name == "multiple_slider_train") %>%
    dplyr::select(prolific_id, RT, expected, QUD, id, trial_name,
           trial_number,
           question1, question2, question3, question4,
           response1, response2, response3, response4
    ) %>%
    pivot_longer(cols=c(contains("response")),
                 names_to = "response_idx", names_prefix = "response",
                 values_to = "response") %>%
    pivot_longer(cols=c(contains("question")),
                 names_to = "question_idx", names_prefix = "question",
                 values_to = "question") %>%
    filter(response_idx == question_idx) %>%
    dplyr::select(-response_idx, -question_idx) %>%
    mutate(prolific_id = factor(prolific_id), id = factor(id)) %>%
    group_by(prolific_id, id) %>%
    mutate(response = as.numeric(response), response = response/100) %>%
    add_smoothed_exp1()
  
  dat.train.smooth = dat.train %>% rename(response=r_smooth) %>%
    dplyr::select(-r_orig, -n, -trial_name)
  dat.train.orig = dat.train %>% rename(response=r_orig) %>%
    dplyr::select(-r_smooth, -n, -trial_name)
  return(list(smooth=dat.train.smooth, orig=dat.train.orig))
}

tidy_data <- function(data, N_trials){
  # 1. dplyr::select only columns relevant for data analysis
  df <- data %>% dplyr::select(prolific_id, submission_id,
                        question, question1, question2, question3, question4,
                        QUD, response,
                        expected, response1, response2, response3, response4,
                        id, trial_name, trial_number, group,
                        timeSpent, RT,
                        education, comments, gender, age)
  # always use the same abbreviation
  df <- df %>% mutate(question1 = case_when(question1 == "gb" ~ "bg",
                                            question1 == "yr" ~ "ry",
                                           TRUE ~ question1),
                      response3 = as.character(response3),
                      response4 = as.character(response4));
  dat.color_vision <- tibble();
  if(N_trials$color_vision != 0) {
    dat.color_vision <- df %>%
      filter(startsWith(trial_name, "color-vision")) %>%
      dplyr::select(prolific_id, id, question, response, expected, QUD, trial_number)
    df <- df %>% filter(!startsWith(trial_name, "color-vision"));
  }
  dat.slider_choice=tibble()
  dat.attention_check=tibble()
  if(N_trials$slider_choice != 0){
    cols = c("prolific_id", "id", "question", "response", "expected", "trial_name", "trial_number")
    dat.slider_choice = df %>% filter(startsWith(trial_name, "slider_choice_training")) %>%
      dplyr::select(one_of(cols))
    dat.attention_check = df %>% filter(startsWith(trial_name, "attention_check")) %>%
      dplyr::select(one_of(cols))
  }
  N_participants <- df %>% dplyr::select(prolific_id) %>% unique() %>% nrow()
  stopifnot(nrow(df) == N_participants * (N_trials$test + N_trials$train));

  dat.comments <- df %>%
    dplyr::select(prolific_id, comments) %>%
    mutate(comments = as.character(comments),
           comments = if_else(is.na(comments), "", comments)) %>%
    unique()
  dat.info <- df %>% dplyr::select(prolific_id, education, gender, age, timeSpent) %>%
    unique()
  dat.train <- tidy_train(df)
  dat.test <- tidy_test_joint(df)
  dat.all <- list(test=dat.test, train.smooth=dat.train$smooth,
                  train.orig=dat.train$orig, color=dat.color_vision,
                  train.attention=dat.attention_check,
                  train.slider_choice=dat.slider_choice,
                  info=dat.info, comments=dat.comments)

  return(dat.all)
}

standardize_color_groups_exp1 <- function(df){
  # ind2 is used as single training example for production task (always group1!)
  df <- df %>%
    mutate(question =
             case_when((question == "bg" | question == "gb" |
                        question=="ry" | question == "yr") ~ "ac",
                        question == "none" ~ "none",
                        group == "group1" & (question == "b" | question=="r") ~ "a",
                        group == "group1" & (question == "g" | question=="y") ~ "c",
                        group == "group2" & question == "g"  ~ "a",
                        group == "group2" & question == "b" ~ "c"
                      ),
           group = "group1",
           question = case_when(question == "a" ~ "b",
                                question == "c" ~ "g",
                                question == "ac" ~ "bg",
                                question == "none" ~ "none")
           )
  return(df)
}

standardize_color_groups_exp2 <- function(df){
  df <- df %>%
    mutate(response = case_when(group == "group2" ~ str_replace_all(response, "blue", "G"),
                                TRUE ~ str_replace_all(response, "blue", "B")),
           custom_response = case_when(group == "group2" ~ str_replace_all(custom_response, "blue", "-G-"),
                                TRUE ~ str_replace_all(custom_response, "blue", "-B-"))) %>%

    mutate(response = case_when(group == "group2" ~ str_replace_all(response, "green", "B"),
                                TRUE ~ str_replace_all(response, "green", "G")),
           custom_response = case_when(group == "group2" ~ str_replace_all(custom_response, "green", "-B-"),
                                TRUE ~ str_replace_all(custom_response, "green", "-G-"))) %>%
    mutate(response = str_replace_all(response, "G", "green"),
           custom_response = str_replace_all(custom_response, "-G-", "green")) %>%
    mutate(response = str_replace_all(response, "B", "blue"),
           custom_response = str_replace_all(custom_response, "-B-", "blue"
           ));
  df <- df %>% mutate(group = "group1", 
                      response = as.factor(response),
                      custom_response = as.factor(custom_response)
                      );

  return(df)
}

# @arg df: data frame containing columns bg, b, g, none
add_probs <- function(df){
  df <- df %>% mutate(p_a=bg+b, p_c=bg+g, p_na=g+none, p_nc=b+none) %>%
    mutate(p_c_given_a = bg / p_a,
           p_c_given_na = g / p_na,
           p_a_given_c = bg / p_c, 
           p_a_given_nc = b / p_nc, 
           p_nc_given_a = b/p_a,
           p_nc_given_na = none/p_na,
           p_na_given_c = g/p_c,
           p_na_given_nc = none/p_nc,
           p_likely_a = p_a,
           p_likely_na=p_na,
           p_likely_c = p_c,
           p_likely_nc=p_nc
    )
  return(df)
}

# @arg quest: question which is used to generate the clusters, e.g. 'b'
cluster_responses <- function(dat, quest){
  dat.kmeans <- dat %>% filter(question == quest) %>%
    dplyr::select(prolific_id, id, response) %>% add_column(y=1) %>%
    group_by(prolific_id, id) %>%
    unite("rowid", "prolific_id", "id", sep="--") %>%
    column_to_rownames(var = "rowid")
  clusters <- kmeans(dat.kmeans, 2)

  df <- dat.kmeans %>%
    rownames_to_column(var = "rowid") %>%
    as_tibble() %>%
    separate(col="rowid", sep="--", into=c("prolific_id", "id")) %>%
    mutate(cluster=as.factor(clusters$cluster), id=as.factor(id),
           prolific_id = as.factor(prolific_id)) %>%
    dplyr::select(prolific_id, id, cluster)
  df <- left_join(dat, df) 
  df <- df %>% mutate(cluster = fct_explicit_na(df$cluster, na_level = 'not-clustered'))
  return(df)
}

# @arg df1 in long-format
# smooth slider ratings from prior elicitation experiment (exp1)
add_smoothed_exp1 <- function(df1){
  df = df1 %>% group_by(prolific_id, id) %>%
    filter(sum(response) != 0)
  # normalize such that slider responses sum up to 1 but also keep original response
  df.with_smoothed = df %>%
    mutate(n=sum(response + epsilon), r_smooth=(response + epsilon)/n) %>%
    rename(r_orig=response)
  return(df.with_smoothed)
}

save_prob_tables <- function(df, result_dir, result_fn){
  # Save all Tables (with smoothed values)
  tables.all <- df %>% dplyr::select(id, question, prolific_id, r_smooth) %>%
    group_by(id, prolific_id) %>%
    pivot_wider(names_from = question, values_from = r_smooth) %>%
    add_probs()
  fn_tables_all <- paste(result_fn, "_tables_smooth.csv", sep="");
  path_tables_all <- paste(result_dir, fn_tables_all, sep=.Platform$file.sep);
  write.table(tables.all, file=path_tables_all, sep = ",", row.names=FALSE)
  print(paste('written smoothed probability tables to:', path_tables_all))
  
  # enrich empirical tables, fine-grained tables mapped to more coarse tables
  tables.empiric.pids = tables.all %>%
    dplyr::select("bg", "b", "g", "none", "prolific_id", "id") %>% 
    unite("p_id", c(prolific_id, id)) %>% group_by(bg, b, g, none) %>%
    summarize(p_id=list(p_id), .groups="keep") %>% ungroup() %>% 
    mutate(bg.round=as.integer(round(bg,2)*100), b.round=as.integer(round(b,2)*100),
           g.round=as.integer(round(g,2)*100), none.round=as.integer(round(none,2)*100)) %>%
    distinct_at(vars(c(bg, b, g, none)), .keep_all = TRUE) %>% 
    rowid_to_column("empirical_id")
  save_data(tables.empiric.pids, paste(result_dir, "tables-empiric-pids.rds", sep=fs))

  # to summarize the empirically generated tables, i.e.to make them more
  # coarse-grained, we map each entry to the nearest next number that
  # is divisible by 5, e.g. 87 to 92 mapped to 90, 83 to 87 mapped to 85, then
  # normalize again to get well-defined probability distributions
  # for each empirical table, list all tables that would have been mapped to the
  # same table and check how many are then generated by the fitted prior distributions
  tables.emp.augmented=pmap_dfr(
    tables.empiric.pids %>% dplyr::select(-p_id), function(...){
    row=tibble(...)
    row.long = row %>%
      pivot_longer(cols=c(bg.round, b.round, g.round, none.round),
                   names_to="cell", values_to="p") %>%
      mutate(mod = p %% 10)
    vals.augmented = pmap_dfr(row.long, function(...){
      entry = tibble(...)
      if(entry$p %in% c(0,1,2)){
        vals = tibble(p=c(0, 1, 2))
      } else if(entry$p %in% c(98,99,100)){
        vals = tibble(p=c(98, 99, 100))
      } else if(entry$mod %in% c(3,4,5,6,7)) {
        vals = tibble(p=entry$p - entry$mod + c(3,4,5,6,7))
      } else if(entry$mod %in% c(8,9)){
        vals = tibble(p=entry$p - entry$mod + c(8,9, 10, 11, 12))
      } else if(entry$mod %in% c(0, 1, 2)){
        vals = tibble(p=entry$p - entry$mod + c(0, 1, 2, -2, -1))
      }
      vals %>% add_column(empirical_id = entry$empirical_id %>% unique(),
                          cell=entry$cell %>% unique())
    })
    # normalize new tables + make sure that distinct
    x = vals.augmented %>%
      pivot_wider(names_from="cell", values_from="p", values_fn = list)
    y=expand.grid(bg=x$bg.round[[1]], b=x$b.round[[1]], g=x$g.round[[1]],
                  none=x$none.round[[1]])
    tables.expanded = prop.table(as.matrix(y+epsilon), 1) %>% as_tibble() %>%
     mutate(bg.round=as.integer(round(bg, 2)*100), b.round=as.integer(round(b,2)*100),
            g.round=as.integer(round(g,2)*100), none.round=as.integer(round(none,2)*100)) %>%
      distinct_at(vars(ends_with(".round")), .keep_all = TRUE) %>%
      add_column(empirical_id=row$empirical_id %>% unique());
    
    # due to normalization, original values might have changed more than we want
    tables = tables.expanded %>%
      filter((bg*100 <= row$bg.round + 2 & (bg*100 >= row$bg.round-2)) & 
               (b*100 <= row$b.round + 2 & (b*100 >= row$b.round-2)) &
               (g*100 <= row$g.round + 2 & (g*100 >= row$g.round-2)) & 
               (none*100 <= row$none.round + 2 & (none*100 >= row$none-2)))
    return(tables)
  })
  tables.emp.augmented = tables.emp.augmented %>%
    distinct_at(vars(c(ends_with(".round"))), .keep_all = TRUE) %>% 
    add_column(augmented=TRUE)
  # merge augmented tables with original tables
  tables.emp.all = bind_rows(
    tables.empiric.pids %>% add_column(augmented=FALSE),
    tables.emp.augmented
  ) %>%
    distinct_at(vars(c(ends_with(".round"))), .keep_all = TRUE) %>%
    dplyr::select(-p_id) %>%
    rowid_to_column("row_id")
  
  save_data(tables.emp.all,
            paste(result_dir, "tables-empiric-augmented.rds", sep=fs))
}

process_data <- function(data_dir, data_fn, result_dir, result_fn, debug_run, N_trials){
  dat.anonym <- save_raw_data(data_dir, data_fn, result_dir, result_fn, debug_run)
  dat.tidy <- tidy_data(dat.anonym, N_trials);
  # Further process TEST-trial data --------------------------------------------
  data <- dat.tidy$test
  df1 <- data %>% filter(str_detect(trial_name, "multiple_slider"))
  df1 <- add_smoothed_exp1(df1);
  df1 <- standardize_color_groups_exp1(df1)
  save_prob_tables(df1, result_dir, result_fn);
  df2 <- data %>% filter(str_detect(trial_name, "fridge_")) %>%
    mutate(response=utterance) %>%
    dplyr::select(-utterance)
  df2 <- standardize_color_groups_exp2(df2)
  df2 <- standardize_sentences(df2)
  df <- bind_rows(df1 %>% rename(response=utterance), df2);

  # save processed data -----------------------------------------------------
  fn_tidy <- paste(result_fn, "_tidy.rds", sep="");
  path_target <- paste(result_dir, fn_tidy, sep=.Platform$file.sep)
  dat.tidy$test <- df
  save_data(dat.tidy, path_target)
  return(dat.tidy)
}

# sampled tables from fitted distributions may contain tables that 
# only match with augmented empirical tables, but not with the original empirical
# table, these original tables are added to set of sampled tables here
add_orig_empirical_only_augmented = function(tbls.gen.all){
  tbl_id.max = tbls.gen.all$table_id %>% unique() %>% length()
  ids.aug = tbls.gen.emp %>% filter(only_augmented) %>% pull(empirical_id) %>% unique()
  tbls.aug = tables.empiric.pids %>% filter(empirical_id %in% ids.aug) %>%
    dplyr::select(-p_id) %>% add_column(orig_table=TRUE)
  tbls.aug = tbls.aug %>%
    add_column(table_id = seq(tbl_id.max+1, tbl_id.max+nrow(tbls.aug)),
               row_id=NA_integer_, augmented = NA)
  map.ids = tables.empiric.pids %>% dplyr::select(empirical_id, p_id)
  tbls.aug = left_join(tbls.aug, map.ids, by=c("empirical_id"))  %>%
    add_column(stimulus="")
  
  tables.generated.all = bind_rows(tbls.gen.all, tbls.aug)
  return(tables.generated.all)
}

formatEmpiricTables = function(result_dir){
  tables.emp.augmented = readRDS(
    paste(result_dir, "tables-empiric-augmented.rds", sep=fs)
  ) %>% rename(AC=bg, `A-C`=b, `-AC`=g, `-A-C`=none,
               `AC.round`=bg.round, `A-C.round`=b.round,
               `-AC.round`=g.round, `-A-C.round`=none.round)
  
  # check how many tables were sampled from fitted priors given augmented tables
  tables.empiric.pids = readRDS(
    paste(result_dir, "tables-empiric-pids.rds", sep=fs)
  ) %>% rename(AC=bg, `A-C`=b, `-AC`=g, `-A-C`=none,
               AC.round=`bg.round`, `A-C.round`=`b.round`,
               `-AC.round`=`g.round`, `-A-C.round`=`none.round`)
  
  tbls.emp.augmented = left_join(
    tables.emp.augmented,
    tables.empiric.pids %>% dplyr::select(empirical_id, p_id),
    by=c("empirical_id")
  )
  return(tbls.emp.augmented)
}

# save mapping of table-empirical ids
formatGeneratedTables = function(tbls.joint){
  # one empirical id maps to several table_ids
  # generated + empirical
  tbls.gen.emp = tbls.joint %>% filter(empirical) %>% group_by(empirical_id) %>% 
    mutate(n=n(), s=sum(augmented), only_augmented=s==n) %>%
    mutate(orig_table=case_when(augmented ~ FALSE, 
                                TRUE ~ TRUE))
  # generated + not empirical
  tbls.gen.not_emp = tbls.joint %>% filter(!empirical) %>%
    dplyr::select(-ends_with(".y")) %>% 
    rename(`AC`=`AC.x`, `A-C`=`A-C.x`, `-AC`=`-AC.x`, `-A-C`=`-A-C.x`) %>%
    add_column(only_augmented=FALSE, orig_table=FALSE)
  
  # 1. use original empirical tables whenever possible, i.e. when match within 
  # generated and empirical tables where augmented is false
  # --> take table where augemented is false
  # generated + empirical + original
  tbls.gen.emp.orig = tbls.gen.emp %>% filter(orig_table) %>% 
    dplyr::select(-ends_with(".x")) %>% 
    rename(`AC.x`=`AC.y`, `A-C.x`=`A-C.y`, `-AC.x`=`-AC.y`, `-A-C.x`=`-A-C.y`)
  # 2. but some generated tables match only with augmented empirical tables
  # generated + empirical + not original
  tbls.gen.emp.not_orig = tbls.gen.emp %>% filter(!orig_table) %>%
    dplyr::select(-ends_with(".y"))
  
  tbls.gen.emp = bind_rows(tbls.gen.emp.orig, tbls.gen.emp.not_orig) %>%
    rename(`AC`=`AC.x`, `A-C`=`A-C.x`, `-AC`=`-AC.x`, `-A-C`=`-A-C.x`) %>%
    dplyr::select(-n, -s)
  # 3. merge again generated tables
  tables.generated.all = bind_rows(tbls.gen.emp, tbls.gen.not_emp)
  return(tables.generated.all)
}

checkRatioGeneratedEmpiricTables = function(tbls.joint, n.empiric){
  empirical_in_tables = tbls.joint %>% filter(empirical) %>%
    pull(empirical_id) %>% unique()
  return(length(empirical_in_tables) / n.empiric)
  # nrow(tables.empiric.pids)
}

makeModelTables = function(dir_empiric){
  dat.model = sampleModelTables()
  tables.model = dat.model$tables
  tables.generated = tables.model %>% unnest(c(vs, ps)) %>%
    pivot_wider(names_from="vs", values_from="ps") %>% 
    mutate(AC.round=as.integer(round(AC, 2) * 100),
           `A-C.round`=as.integer(round(`A-C`,2) * 100),
           `-AC.round`=as.integer(round(`-AC`, 2) * 100),
           `-A-C.round`=as.integer(round(`-A-C`, 2) * 100)) %>%
    ungroup() %>% dplyr::select(-bn_id) %>% 
    distinct_at(vars(c(ends_with(".round"))), .keep_all = TRUE) %>% 
    rowid_to_column("table_id")
  
  tbls.emp.augmented = formatEmpiricTables(dir_empiric)
    
  tbls.joint = left_join(tables.generated, tbls.emp.augmented,
                   by=c("AC.round", "A-C.round", "-AC.round", "-A-C.round")) %>% 
    mutate(empirical = !is.na(empirical_id)) %>% arrange(augmented)
  
  tables.generated.all = formatGeneratedTables(tbls.joint) %>%
    mutate(stimulus=cn)
  
  # for generated tables that match only with augmented empirical tables
  # check what the original table was and also add this one
  # tables.generated.all = add_orig_empirical_only_augmented(tables.generated.all)
  save_data(tables.generated.all, dat.model$params$target_mapping)
  
  tables.model = tables.generated.all %>%
    dplyr::select(-row_id, -id, -ends_with(".round"), -augmented, -only_augmented)
  tables.toWPPL = tables.model %>% 
    group_by(table_id) %>% 
    mutate(vs=list(c("AC", "A-C", "-AC", "-A-C")),
           ps=list(c(`AC`, `A-C`, `-AC`, `-A-C`))) %>% 
    add_column(indep_sigma=dat.model$params$indep_sigma)
  save_data(tables.toWPPL, dat.model$params$tables_empiric)
  return(tables.toWPPL)
}

# Model predictions different levels
load_model_data = function(fn_tables) {
  prior = readRDS(here("model", "results", fn_tables, "results-prior.rds")) %>% 
    dplyr::select(-bn.stimulus, -bias, -starts_with("p_"))
  
  prior.wide = prior %>% pivot_wider(names_from="cell", values_from="val") %>%
    ungroup() %>% dplyr::select(-level)
  
  speaker = readRDS(here("model", "results", fn_tables, "results-speaker.rds")) %>%
    dplyr::select(-starts_with("p_"), -stimulus, -bias) %>%
    mutate(AC=round(AC, 2), `A-C`=round(`A-C`, 2), `-AC`=round(`-AC`, 2),
           `-A-C`=round(`-A-C`, 2))
  
  # ll = readRDS(here("model", "results", fn_tables, "results-LL.rds")) %>%
  #   dplyr::select(-starts_with("p_"), -bn.stimulus, -bias) %>%
  #   mutate(val=round(val, 2)) %>%
  #   pivot_wider(names_from="cell", values_from="val") %>%
  #   ungroup() %>% dplyr::select(-level)
  
  pl = readRDS(here("model", "results", fn_tables, "results-PL.rds")) %>%
    dplyr::select(-starts_with("p_"), -bn.stimulus, -bias) %>%
    mutate(val=round(val, 2)) %>%
    pivot_wider(names_from="cell", values_from="val") %>%
    ungroup() %>% dplyr::select(-level)
  
  # tables = readRDS(here("model", "data", "tables-model-empirical.rds"))  %>%
  #   dplyr::select(-starts_with("logL_"), -cn, -vs, -ps, -stimulus) %>% 
  #   rename(bg=`AC`, b=`A-C`, g=`-AC`, none=`-A-C`) %>% 
  #   add_probs() %>%
  #   dplyr::select(-starts_with("p_likely"))
  # 
  
  return(list(pl=pl, speaker=speaker, prior=prior.wide))
}
