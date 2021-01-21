library(tidyverse)
library(greta)
library(here)
library(MCMCpack)
library(rwebppl)
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
  return(results)
}

fitLatentMixture = function(path_smoothed_tables, target_dir){
  table_data <- read_csv(path_smoothed_tables) %>% arrange(id) %>% 
    dplyr::select(id, bg, b, g, none) %>% group_by(id) %>% 
    rename(AC=bg, `A-C`=b, `-AC`=g, `-A-C`=none) %>% 
    mutate(pa = AC + `A-C`, pna =`-AC` + `-A-C`, 
           pc = AC + `-AC`, pnc =`A-C` + `-A-C`,
           diff = AC - pa * pc,
           pca = AC/pa, pcna = `-AC`/ pna, 
           pac = AC/pc, panc = `A-C`/pnc,
           pnca = `A-C`/pa, pncna = `-A-C`/ pna, 
           pnac = `-AC`/pc, pnanc = `-A-C`/pnc, 
           ) %>% dplyr::select(-pna, -pnc, -`AC`, -`A-C`, -`-AC`, -`-A-C`)
  
  stimulus_id_list <- table_data %>% pull(id) %>% unique()
  
  results <- map_df(stimulus_id_list,
    function(s) {
      print(s)
      tbls.stim = table_data %>% filter(id == s)
        posteriorSamples = 
          webppl(program_file = here("model", "fit-latent-mixture.wppl"),
                 data = list(tables=tbls.stim), data_var = "data")
      
        samples = posteriorSamples %>% as_tibble() %>% 
          mutate(Parameter = as.character(Parameter)) %>%
          group_by(Parameter)
        
        pp = samples %>% summarize(ev=mean(value), .groups="keep") %>% 
          pivot_wider(names_from="Parameter", values_from="ev") %>%
          add_column(id=(!! s))
    return(pp)
    }
  )
  return(results)
}

# sample tables from fitted distributions ---------------------------------

get_dep_tables = function(df, cn){
  if(cn=="A implies C"){
    df = df %>% mutate(bg=pos*marg, b=(1-pos)*marg, g=neg*(1-marg), none=(1-neg)*(1-marg))
  } else if(cn == "A implies -C"){
    df = df %>% mutate(bg=(1-pos)*marg, b=pos*marg, g=(1-neg)*(1-marg), none=neg*(1-marg))
  } else if(cn == "C implies A"){
    df = df %>% mutate(bg=pos*marg, b=neg*(1-marg), g=(1-pos)*marg, none=(1-neg)*(1-marg))
  } else if(cn == "C implies -A"){
    df = df %>% mutate(bg=(1-pos)*marg, b=(1-neg)*(1-marg), g=pos*marg, none=neg*(1-marg))
  }else {
    stop(paste("unknown cn", cn))
  }
  return(df)
}

sample_latent_mixture <- function(params, n=2500){
  set.seed(seed_fitted_tables)
  ids = params$dep$id %>% unique()
  cns = c(params$dep$cn %>% unique, params$ind$cn %>% unique)
  samples = map_dfr(ids, function(stim){
    par.ind = params$ind %>% filter(id == stim)
    par.dep = params$dep %>% filter(id == stim)
    # sample each causal net according to fitted marginal
    cns <- sample(c(par.ind$cn, par.dep$cn), n, replace=TRUE,
                  prob=c(par.ind$p_cn, par.dep$p_cn)) %>%
      table() %>% as_tibble() %>% rename_all(~c("cn", "n"))
    n.ind = cns %>% filter(cn == "ind") %>% pull(n)
    ind.pa = rbeta(n.ind, par.ind$pa_a, par.ind$pa_b)
    ind.pc = rbeta(n.ind, par.ind$pc_a, par.ind$pc_b)
    ind.min = 1-(ind.pa+ind.pc)
    ind.min[ind.min>0] = 0
    ind.min[ind.min<0] = ind.min[ind.min<0] * -1
    ind.joint = rtruncnorm(n.ind, a=ind.min, b=pmin(ind.pa, ind.pc),
                           mean=par.ind$mu, sd=par.ind$sigma)
    tables.ind = tibble(bg=ind.joint, pa=ind.pa, pc=ind.pc) %>%
      mutate(b=pa-ind.joint, g=pc-ind.joint, none=1-bg-b-g) %>%
      dplyr::select(bg, b, g, none) %>% add_column(cn="ind")
      
    tables.dep = map_dfr(cns %>% filter(cn!="ind") %>% pull(cn), function(cn){
      par.cn = par.dep %>% filter(cn==(!! cn))
      n.cn = cns %>% filter(cn == (!! cn)) %>% pull(n)
      pos = rbeta(n.cn, par.cn$a, par.cn$b) 
      neg = rbeta(n.cn, par.cn$b, par.cn$a) 
      marg = rbeta(n.cn, par.cn$marg_a, par.cn$marg_b) 
      vals = tibble(pos=pos, neg=neg, marg=marg) %>%
        get_dep_tables(cn) %>% dplyr::select(bg, b, g, none) %>% 
        add_column(cn=(!! cn))
    })
    tables = bind_rows(tables.ind, tables.dep) %>% add_column(stimulus=stim)
    return(tables)
  })
  return(samples)
}

sample_dirichlet <- function(params, n=2500){
  set.seed(seed_fitted_tables)
  tables = pmap_dfr(params, function(...){
     row = tibble(...) 
     rdirichlet(n, row[1, 2:5] %>% as.numeric()) %>% as_tibble() %>%
       add_column(stimulus=row$id)
    }) %>% rename(bg=V1, b=V2, g=V3, none=V4)
  
  return(tables %>% add_column(cn="cn1"))
}

# @arg fn: "latent-mixture", "dirichlet", "model-tables"
format_and_save_fitted_tables = function(tables.fit, params.fit, dir_empiric, fn){
  tables.generated =  tables.fit %>% 
    rename(`AC`=bg, `A-C`=b, `-AC`=g, `-A-C`=none) %>%
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
  save_as = paste("mapping-tables-", fn, "-ids.rds", sep="")
  save_data(tables.generated.all, here("model", "data", save_as))
  
  # save tables for input to webppl model (with likelihoods) ------------------
  tables.model = tables.generated.all %>%
    dplyr::select(-row_id, -ends_with(".round"), -augmented, -only_augmented)
  
  if(fn=="dirichlet"){
    tbls = tables.model %>% group_by(stimulus) %>% 
      rename(bg=AC, b=`A-C`, g=`-AC`, none=`-A-C`) %>%
      dplyr::select(stimulus, table_id, bg, b, g, none)
    tables.ll = group_map(tbls, function(df, stim){
      stimulus = stim$stimulus
      tbls.model = tables.model %>% filter(table_id %in% df$table_id)
      par = params.fit %>% filter(id == (!! stimulus))
      ll = add_ll_dirichlet(df %>% dplyr::select(-table_id), par)
      tbls.model %>% add_column(ll=ll$`ll.table`)
    })
    tables.toWPPL = bind_rows(tables.ll) %>% group_by(table_id) %>% 
      mutate(vs=list(c("AC", "A-C", "-AC", "-A-C")),
             ps=list(c(`AC`, `A-C`, `-AC`, `-A-C`))) 
  } else if(fn=="latent-mixture"){
    # tables.toWPPL = 
  } else if(fn=="model-tables") {
    indep_sigma <- configure(c("model_tables"))$indep_sigma
    tables = tables.model %>% 
      group_by(table_id) %>% 
      mutate(vs=list(c("AC", "A-C", "-AC", "-A-C")),
             ps=list(c(`AC`, `A-C`, `-AC`, `-A-C`))) %>% 
      likelihood(indep_sigma)
    params.tbl <- configure(c("model_tables"))
    tbl.params =  list(n_best_cns = params.tbl$n_best_cns,
                       cns = params.tbl$cns)
    tables.toWPPL = tables_to_bns(tables, tbl.params)
  }
  else {
    stop("arg 'fn' must be one of: latent-mixture, dirichlet")
  }
  save_as = paste("tables-", fn, "-empirical.rds", sep="")
  tables.toWPPL %>% save_data(here("model", "data", save_as))
  # likelihood and cns is not important for analysis, so return without ll
  return(tables.generated.all)
}


# Log likelihoods ---------------------------------------------------------
# adds log likelihood for each table for stimulus it was generated for 
# with fitted params (params)
add_ll_dirichlet = function(tables, params) {
  tables.mat = tables %>% as.matrix()
  # iterate over all causal nets (if several)
  likelihoods = map_dfr(seq(1, nrow(params)), function(i){
    par = params[i,] %>% ungroup()
    vec = rep(par %>% dplyr::select(-id, -p_cn, -cn) %>% as.matrix(), nrow(tables.mat))
    par.mat = matrix(vec, nrow(tables.mat), 4, byrow = TRUE)
    densities = MCMCpack::ddirichlet(tables.mat, par.mat)
    df = tables %>% add_column(lik=densities * par$p_cn) %>% rowid_to_column() 
  })
  log_likelihoods = likelihoods %>% group_by(rowid) %>%
    summarize(ll.table=log(sum(lik)), .groups="drop_last") %>%
    dplyr::select(-rowid)
  
  return(log_likelihoods)
}

# compute log likelihood for set of all tables (considering different cns)
# @arg tables: must be smoothed, with only 4 columns: bg,b,g,none
# @arg params: columns: a, b, c, d, id, p_cn, cn
ll_dirichlet = function(tables, params){
  likelihoods = add_ll_dirichlet(tables, params)
  df = likelihoods %>% summarize(ll=sum(ll.table))
  return(df)
}

# Goodness fits -----------------------------------------------------------
# considers different causal nets, i.e. more than 1 dirichlet per stimulus
ll_dirichlet_empirical_data = function(params, dir_empiric){
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
    name_ll = paste("ll", id, sep="__")
    tibble(stimulus_id=id, ll_sample=ll$ll)
  });
  return(ll.empirical)
}

# @arg params: with columns: id, alpha_1, alpha_2, alpha_3, alpha_4, p_cn, cn
# name alpha1-alpha4 not important, but in columns 2:5!
goodness_fits_dirichlet = function(params, dir_empiric, n, N){
  ll.empirical = ll_dirichlet_empirical_data(params, dir_empiric)
  # Sample n times N=#participatns values for each stimulus
  # (each has a different fitted dirichlet distribution)
  ll_samples = group_map(params %>% group_by(id), function(par, stimulus){
    id = stimulus$id
    print(id)
    ll.obs = ll.empirical %>% filter(stimulus_id == (!! id)) %>% pull(ll_sample)
    # sample n times N (N: #participants) tables and compute log likelihood
    ll.simulated = map_dfr(seq(1, n), function(i){
      # generated set of tables
      tables.chunk = map_dfr(seq(1, nrow(par)), function(i_row){
        row = par[i_row, ]
        par.vec = row[, 2:5] %>% as.numeric()
        rdirichlet(round(row$p_cn * N), par.vec) %>% as_tibble()
      }) %>% rename(bg=V1, b=V2, g=V3, none=V4)
      
      tables.chunk %>% ll_dirichlet(par %>% add_column(id=(!! id)))
    })
    p.val = (ll.simulated$ll < ll.obs) %>% sum()/n
    return(ll.simulated %>% mutate(stimulus_id=id, p.val=p.val, n=n) %>%
           rename(ll_sample=ll))
  });
  return(bind_rows(ll_samples))
}

compute_goodness_dirichlets = function(params, dir_empiric, N, n=10**4){
  res.goodness = goodness_fits_dirichlet(params, dir_empiric, n, N) %>% arrange(desc(p.val));
  p.vals = res.goodness %>% dplyr::select(-ll_sample) %>% distinct()
  write_csv(p.vals, paste(dir_empiric, "simulated-p-values-fitted-dirichlet.csv", sep=fs))
  return(res.goodness)
}

plot_goodness_dirichlets = function(res.goodness, params.fit, dir_empiric){
  ll.obs = ll_dirichlet_empirical_data(params.fit, dir_empiric)
  p = res.goodness %>% ggplot(aes(x=ll_sample)) +
    geom_density() +
    geom_point(data=ll.obs, aes(x=ll_sample, y=0), color='red', size=2) +
    geom_vline(data=ll.obs, aes(xintercept=ll_sample)) +
    facet_wrap(~stimulus_id) +
    theme_classic() +
    labs(x="log-likelihood simulated data")
  plot_dir = paste(dir_empiric, "plots", sep=fs)
  if(!dir.exists(plot_dir)){dir.create(plot_dir)}
  save_as = paste(dir_empiric, "plots", "goodness-dirichlet-fits.png", sep=fs)
  ggsave(save_as, p, height=6)
  print(paste("saved plot to", save_as))
  return(p)
}
