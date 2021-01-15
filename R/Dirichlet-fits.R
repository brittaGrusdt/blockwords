library(tidyverse)
library(greta)
library(here)
library(MCMCpack)
source(here("R", "utils.R"))

# Fit dirichlet distributions ---------------------------------------------
get_optimal_alphas <- function(table_data, st_id) {
  y <- table_data %>% 
    filter(id == st_id)
  y <- y[, 3:6] %>% 
    as.matrix()
  y <- prop.table(y + epsilon, 1)
  
  alpha <- uniform(0,20, 4)
  
  distribution(y) <- dirichlet(t(alpha), n_realisations = nrow(y))
  
  m <- model(alpha)
  
  fit_opt <- opt(m)
  
  tibble(
    id = st_id,
    alpha_1 = fit_opt$par$alpha[1],
    alpha_2 = fit_opt$par$alpha[2],
    alpha_3 = fit_opt$par$alpha[3],
    alpha_4 = fit_opt$par$alpha[4],
    )
}

fitDirichlets = function(path_smoothed_tables, target_dir){
  table_data <- read_csv(path_smoothed_tables) %>% arrange(id)
  stimulus_id_list <- table_data %>% pull(id) %>% unique()
  
  results <- map_df(
    stimulus_id_list,
    function(s) {
      print(s)
      get_optimal_alphas(table_data, st_id = s)
    }
  )
  write_csv(results, paste(target_dir, "results-dirichlet-fits.csv", sep=fs))
  return(results)
}

# sample tables from fitted distributions ---------------------------------
sample_dirichlet <- function(params, n){
  set.seed(seed_fitted_tables)
  return(
    pmap_dfr(params, function(...){
     row = tibble(...) 
     rdirichlet(n, row[1, 2:5] %>% as.numeric()) %>% as_tibble() %>%
       add_column(stimulus=row$id)
    })
  )
}

makeDirichletTables = function(params.fit, dir_empiric, n=2500){
  tables.generated = sample_dirichlet(params.fit, n) %>%
    rename(`AC`=V1, `A-C`=V2, `-AC`=V3, `-A-C`=V4) %>%
    mutate(`AC.round`=as.integer(round(AC, 2) * 100),
           `A-C.round`=as.integer(round(`A-C`, 2) * 100),
           `-AC.round`=as.integer(round(`-AC`, 2) * 100),
           `-A-C.round`=as.integer(round(`-A-C`, 2) * 100)) %>%
    distinct_at(vars(c(ends_with(".round"))), .keep_all = TRUE) %>%
    rowid_to_column("table_id")

  tbls.emp.augmented = formatEmpiricTables(dir_empiric)
  tbls.joint = left_join(tables.generated, tbls.emp.augmented,
                         by=c("AC.round", "A-C.round", "-AC.round", "-A-C.round")) %>% 
    mutate(empirical = !is.na(empirical_id)) %>% arrange(augmented)
  tables.generated.all = formatGeneratedTables(tbls.joint)
  # check how many tables were sampled from fitted priors given augmented tables
  #checkRatio..
  save_data(tables.generated.all, here("model", "data", "mapping-tables-dirichlet-ids.rds"))
  
  # save tables for input to webppl model -------------------------------
  tables.model = tables.generated.all %>%
    dplyr::select(-row_id, -ends_with(".round"), -augmented, -only_augmented)
  
  indep_sigma <- configure(c("model_tables"))$indep_sigma
  tables.toWPPL = tables.model %>% 
    group_by(table_id) %>% 
    mutate(vs=list(c("AC", "A-C", "-AC", "-A-C")),
           ps=list(c(`AC`, `A-C`, `-AC`, `-A-C`))) %>% 
    likelihood(indep_sigma)
  
  params.tbl <- configure(c("model_tables"))
  tbl.params =  list(n_best_cns = params.tbl$n_best_cns,
                     cns = params.tbl$cns)
  tables = tables_to_bns(tables.toWPPL, tbl.params)
  
  tables %>% save_data(here("model", "data", "tables-dirichlet-empirical.rds"))
  # likelihood and cns is not important for analysis, only tables!
  return(tables.generated.all)
}

# compute log likelihood for each sample table and each stimulus
# @arg tables: must be smoothed, with only 4 columns (bg,b,g,none)
ll_dirichlet = function(tables, par){
  tables.mat = tables %>% as.matrix()
  vec = rep(par %>% dplyr::select(-id) %>% as.matrix(), nrow(tables.mat))
  par.mat = matrix(vec, nrow(tables.mat), 4, byrow = TRUE)
  densities = MCMCpack::ddirichlet(tables.mat, par.mat)
  df = tables %>% add_column(ll=log(densities)) 
  return(df)
}

# Goodness fits -----------------------------------------------------------
ll_empirical_data = function(params, dir_empiric){
  tbls.empiric = readRDS(paste(dir_empiric, "human-exp1-smoothed.rds", sep=fs)) %>%
      dplyr::select(-utterance) %>% filter(!is.na(question)) %>% 
      pivot_wider(names_from = "question", values_from = "response")
  stimuli_ids = tbls.empiric %>% filter(id != "ind2") %>% 
    pull(id) %>% unique()
  # only exact empirical data ll 
  ll.empirical=map_dfr(stimuli_ids, function(id){
    par = params %>% filter(id == (!! id))
    ll = tbls.empiric %>% filter(id==(!!id)) %>% ungroup() %>% 
      dplyr::select(b, g, bg, none) %>% ll_dirichlet(par)
    name_ll = paste("ll", par$id, sep="__")
    ll = ll %>% rename(!!name_ll:=ll)
    tibble(stimulus_id=id, ll_sample=sum(ll[,ncol(ll)]))
  });
  return(ll.empirical)
}

goodness_fits_dirichlet = function(params, dir_empiric, n, N){
  ll.empirical = ll_empirical_data(params, dir_empiric)
  # Sample n times N=#participatns values for each stimulus
  # (each has a different fitted dirichlet distribution)
  p_values = pmap_dfr(params, function(...){
    par = tibble(...)
    print(par$id)
    ll.obs = ll.empirical %>% filter(stimulus_id == par$id) %>% pull(ll_sample)
    # sample n times N (N: #participants) tables and compute log likelihood
    par.vec = par[, 2:5] %>% as.numeric()
    probs = rdirichlet(N*n, par.vec) %>% as_tibble()
    ll.simulated = probs %>% ll_dirichlet(par) %>%
      add_column(idx=rep(seq(1, n), N)) %>%
      group_by(idx) %>% summarize(s=sum(ll), .groups = "drop_last") %>%
      dplyr::pull(s)
    p.val = (ll.simulated < ll.obs) %>% sum()/n
    return(ll.simulated %>% as_tibble() %>%
             mutate(stimulus_id=par$id, p.val=p.val, n=n) %>%
             rename(ll_sample=value))
  }) ;
  return(p_values)
}

compute_goodness_dirichlets = function(params, dir_empiric, N, n=10**4){
  res.goodness = goodness_fits_dirichlet(params, dir_empiric, n, N) %>% arrange(desc(p.val));
  p.vals = res.goodness %>% dplyr::select(-ll_sample) %>% distinct()
  write_csv(p.vals, paste(dir_empiric, "simulated-p-values-fitted-dirichlet.csv", sep=fs))
  return(res.goodness)
}

plot_goodness_dirichlets = function(res.goodness, params.fit, dir_empiric){
  ll.obs = ll_empirical_data(params.fit, dir_empiric)
  p = res.goodness %>% ggplot(aes(x=ll_sample)) +
    geom_density() +
    geom_point(data=ll.obs, aes(x=ll_sample, y=0), color='red', size=2) +
    geom_vline(data=ll.obs, aes(xintercept=ll_sample)) +
    facet_wrap(~stimulus_id) +
    theme_classic() +
    labs(x="log-likelihood simulated data")
  plot_dir = paste(dir_empiric, "plots", sep=fs)
  if(!dir.exists(plot_dir)){dir.create(plot_dir)}
  ggsave(paste(dir_empiric, "plots", "goodness-dirichlet-fits.png", sep=fs), p,
         height=6)
  return(p)
}
