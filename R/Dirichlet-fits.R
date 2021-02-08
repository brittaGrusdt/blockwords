library(tidyverse)
library(greta)
library(here)
library(MCMCpack)
library(rwebppl)
source(here("R", "utils.R"))
source(here("R", "utils-exp1.R"))

# Fit dirichlet distributions ---------------------------------------------
get_optimal_alphas <- function(table_data, st_id) {
  y <- table_data %>% 
    filter(id == st_id)
  y <- y[, 3:6] %>% 
    as.matrix()
  y <- prop.table(y + epsilon, 1)
  
  alpha <- uniform(0, 20, 4)
  
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

sample_dirichlet <- function(params, n=1000){
  set.seed(seed_fitted_tables)
  tables = map_dfr(seq(1, nrow(params)), function(i){
     row = params[i,]
     rdirichlet(n, row[1, 2:5] %>% as.numeric()) %>% as_tibble() %>%
       add_column(stimulus=row$id)
    }) %>% rename(bg=V1, b=V2, g=V3, none=V4)
  
  return(tables %>% add_column(cn="cn1"))
}

makeDirichletTables = function(df.params.fit, result_dir, fn_suffix, add_augmented=TRUE) {
  tables.dirichlet = sample_dirichlet(df.params.fit)
  formatted.dirichlet = format_and_save_fitted_tables(
    tables.dirichlet, df.params.fit, result_dir, fn_suffix, add_augmented
  )
  return(formatted.dirichlet)
}

# brings sampled tables into format for webppl model and if specified,
# add augmemted tables to sampled tables 
# @arg fn: dirichlet, dirichlet-filtered, model-tables, latent-mixture, latenet-mixture-filtered
format_and_save_fitted_tables = function(tables.fit, params.fit, dir_empiric, fn,
                                         add_augmented=FALSE){
  # table_ids are unique with respect to table, but not wrt stimulus, i.e.
  # a single table_id may appear for different stimuli
  tables.generated =  tables.fit %>% 
    rename(`AC`=bg, `A-C`=b, `-AC`=g, `-A-C`=none) %>%
    mutate(`AC.round`=as.integer(round(AC, 2) * 100),
           `A-C.round`=as.integer(round(`A-C`, 2) * 100),
           `-AC.round`=as.integer(round(`-AC`, 2) * 100),
           `-A-C.round`=as.integer(round(`-A-C`, 2) * 100)) %>%
    group_by(`AC.round`, `A-C.round`, `-AC.round`, `-A-C.round`)
  # all sampled tables retained but table_id should map to unique tables
  tables.generated$table_id = tables.generated %>%
    group_indices(`AC.round`, `A-C.round`, `-AC.round`, `-A-C.round`) 
  
  if(add_augmented) {
    save_mapping_to = here("model", "data", paste("mapping-tables-", fn, "-augmented-ids.rds", sep=""))
    tbls.all = add_augmented_to_sampled_tables(
      tables.generated %>% rowid_to_column() %>% group_by(rowid), dir_empiric
    ) 
    tbls = tbls.all$joint %>% group_by(stimulus) %>% 
      distinct_at(vars(c(rowid, AC, `A-C`, `-AC`, `-A-C`)), .keep_all = TRUE) %>%
      dplyr::select(-rowid, -augmented, -only_augmented)
    
    # --------- add empiric tables that did not match --------------- #
    # tid.max = tbls$table_id %>% max()
    # dirichlets = tables.generated$stimulus %>% unique()
    # tbls.miss = tbls.all$missing %>% rowid_to_column() %>% group_by(rowid) %>%
    #   add_column(empirical=TRUE, dirichlets=list(dirichlets)) %>%
    #   unnest_longer(c(dirichlets))
    # # get log likelihood for each table and dirichlet distribution
    # tbls.miss.ll = tbls.miss %>% likelihood_dirichlets(params.fit) %>% bind_rows() 
    # # but only take the best dirichlet per table
    # tbls.miss.ll = tbls.miss.ll %>% group_by(rowid) %>% mutate(best_ll=max(ll)) %>%
    #     filter(ll == best_ll) %>% dplyr::select(-best_ll)
    # tbls = bind_rows(
    #   tbls, tbls.miss.ll %>% rowid_to_column("table_id") %>% mutate(table_id=table_id + tid.max)
    # ) %>% dplyr::select(-rowid, -ll)
    # ll is computed later again, this is quick+dirty fix 
    # -----------------------------------------------------------------#
    # for empirical tables add stimulus for which participants created table
    # add this as list (we dont want to expand nb of tables here, just need the info)
    tbls.generated = tbls %>% rowid_to_column() %>% group_by(rowid) %>% 
      rename(stimulus.orig=stimulus) %>%
      unnest_longer(p_id, indices_include = FALSE) %>% 
      separate("p_id", into=c("prolific_id", "rel", "prior"), sep="_") %>%
      unite("stimulus", c("rel", "prior"), sep="_") %>%
      dplyr::select(-prolific_id) %>% 
      filter(stimulus != "ind2") %>%  # don't include training-test trial!!
      distinct_at(vars(c(rowid, stimulus.orig)), .keep_all = TRUE)
      
    tables.generated = tbls.generated %>% group_by(rowid) %>% 
      mutate(stimulus.participants=list(stimulus)) %>%
      distinct() %>% ungroup() %>% dplyr::select(-rowid)
    # make sure that tables without empirical match are retained (for set 
    # of input tables, we still want them, just not for speaker input), 
    # for computation of ll we want the original stimulus!
    tables.generated = tables.generated %>% 
        mutate(stimulus = case_when(stimulus == "NA_NA" ~ stimulus.orig,
                                    TRUE ~ stimulus))
    
    save_data(tables.generated, save_mapping_to)
    fn = paste(fn, "-with-augmented", sep="")
  }
  # save tables for input to webppl model (with likelihoods) ------------------
  tables.model = tables.generated %>% ungroup %>% dplyr::select(-ends_with(".round"))
  if(startsWith(fn, "dirichlet")){
    tbls = tables.model %>% group_by(stimulus)
    tables.ll = group_map(tbls, function(df, stim){
      stimulus = stim$stimulus
      par = params.fit %>% filter(id == (!! stimulus))
      ll = add_ll_dirichlet(df %>% dplyr::select(AC, `A-C`, `-AC`, `-A-C`), par)
      df %>% add_column(ll=ll$`ll.table`, stimulus=(!! stimulus))
    })
    tables.toWPPL = bind_rows(tables.ll) %>%
      rowid_to_column() %>% group_by(rowid) %>% 
      mutate(vs=list(c("AC", "A-C", "-AC", "-A-C")),
             ps=list(c(`AC`, `A-C`, `-AC`, `-A-C`))) %>% 
      ungroup() %>% dplyr::select(-rowid)
    
    # undo stimulus change for ll-computation
    if(add_augmented) {
      tables.toWPPL = tables.toWPPL %>% dplyr::select(-stimulus) %>% 
        rename(stimulus=stimulus.participants)
    }
    
  } else if(startsWith(fn, "latent-mixture")) {
    tbls = tables.model %>% group_by(stimulus) %>% 
      dplyr::select(stimulus, table_id, cn, AC, `A-C`, `-AC`, `-A-C`)
    
    # add log likelihood
    tables.ll = group_map(tbls, function(df, stim){
      # tbls.model = tbls %>% filter(table_id %in% df$table_id)
      par = list()
      par$dep = params.fit$dep %>% filter(id == (!! stim$stimulus))
      par$ind = params.fit$ind %>% filter(id== (!! stim$stimulus))
      df.cn_dep = add_cn_probs(df %>% filter(cn != "ind"), "dep")
      df.cn_ind = add_cn_probs(df %>% filter(cn=="ind"), "ind")
      par$ind = par$ind %>%
        rename(p_diff_sd=sigma, p_a1=pa_a, p_a2=pa_b, p_c1=pc_a, p_c2=pc_b)
      par$dep = par$dep %>%
        rename(pos1=a, pos2=b, marg1=marg_a, marg2=marg_b) %>%
        mutate(neg1=pos2, neg2=pos1)
      
      ll.ind = add_ll_latent(df.cn_ind, par$ind)
      ll.dep = add_ll_latent(df.cn_dep, par$dep)
      bind_rows(ll.ind, ll.dep)
    })
    tables.toWPPL = bind_rows(tables.ll) %>% 
      rowid_to_column() %>% group_by(rowid) %>% 
      mutate(vs=list(c("AC", "A-C", "-AC", "-A-C")),
             ps=list(c(`AC`, `A-C`, `-AC`, `-A-C`))) %>% 
      dplyr::select(-rowid)
    
  } else {
    stop("arg 'fn' must be one of: latent-mixture, dirichlet")
  }
  save_as = paste("tables-", fn, "-toWPPL.rds", sep="")
  tables.toWPPL %>% save_data(here("model", "data", save_as))
  return(tables.toWPPL)
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

# ** computes log likelihood for all given dirichlet distributions # ** 
likelihood_dirichlets <- function(tbls, params){
  df.ll = group_map(tbls %>% group_by(dirichlets), function(df.dir, dirichlet.stim){
    stim = dirichlet.stim$dirichlets
    par = params %>% filter(id == (!! stim))
    ll = add_ll_dirichlet(df.dir %>% dplyr::select(AC, `A-C`, `-AC`, `-A-C`), par)
    df.dir %>% add_column(ll=ll$`ll.table`, stimulus=(!! stim))
  })
  return(df.ll)
}



# fit single dirichlet distribution for each stimulus ---------------------
# @arg fn_suffix: "dirichlet", "dirichlet-filtered"
run_fit_dirichlet = function(result_dir, exp_name, fn_suffix){
  path_smoothed_tables <- paste(result_dir, fs, exp_name, "_tables_smooth.csv", sep="")
  table_data <- read_csv(path_smoothed_tables) %>% arrange(id)
  message(paste('load data for fitting dirichlets from', path_smoothed_tables))
  
  stimulus_id_list <- table_data %>% filter(id != "ind2") %>% pull(id) %>% unique()
  params.fit <- map_df(
    stimulus_id_list,
    function(s) {
      print(s)
      get_optimal_alphas(table_data, st_id = s)
    }
  )
  save_to = paste("params-fitted-", fn_suffix, ".csv", sep="")
  write_csv(params.fit, paste(result_dir, save_to, sep=fs))
  df.params.fit = params.fit %>% add_column(p_cn=1, cn="cn1")
  return(df.params.fit)
}

# plot fitted dirichlet
plot_dirichlet = function(df.params.fit){
  samples = map_dfr(seq(1, nrow(df.params.fit)), function(i){
    par = df.params.fit[i,]
    par.vec = par %>% dplyr::select(-id, -p_cn, -cn) %>% as.matrix()
    rdirichlet(5000, par.vec) %>% as_tibble() %>% add_column(id=par$id)
  }) %>% 
    rename(bg=V1, b=V2, g=V3, none=V4)
  
  p = samples %>%
    pivot_longer(cols=c(bg, b, g, none), names_to="cell", values_to="p") %>% 
    ggplot(aes(x=p, color=id)) + geom_density() + facet_grid(cell~id, scales="free") +
    ggtitle(df.params.fit[i,]$id) + theme(legend.position="none")
  print(p)
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

compute_goodness_dirichlets = function(params, dir_empiric, N, n=10**2){
  res.goodness = goodness_fits_dirichlet(params, dir_empiric, n, N) %>% arrange(desc(p.val));
  p.vals = res.goodness %>% dplyr::select(-ll_sample) %>% distinct() %>% 
    mutate(p.val=round(p.val, 3))
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
