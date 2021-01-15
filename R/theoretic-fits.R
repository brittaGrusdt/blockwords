library(tidyverse)
library(greta)
library(here)
library(MCMCpack)
source(here("R", "utils.R"))

# Fit params causal nets --------------------------------------------------
result_dir = here("data", "prolific", "results", "toy-blocks-pilot-2")
table_data = readRDS(paste(result_dir, "tables-empiric-augmented.rds", 
                           sep=fs)) %>%
  dplyr::select(row_id, empirical_id, bg, b, g, none) %>%
  mutate(p_blue = bg+b, p_green = bg + g, diff = bg - (p_blue * p_green))

  # rename(AC=bg, `A-C`=b, `-AC`=g, `-A-C`=none)

# Independent causal net
dat.kmeans = kmeans(table_data %>% dplyr::select(diff), 3)
closest0 = abs(dat.kmeans$centers) == min(abs(dat.kmeans$centers)) 

cluster.ind = closest0 %>% as_tibble() %>% rowid_to_column() %>%
  filter(diff) %>% pull(rowid)

table_data = table_data %>% 
  add_column(cluster_ind=dat.kmeans$cluster) %>%
  mutate(cluster_ind=case_when(cluster_ind == cluster.ind ~ TRUE,
                               TRUE ~ FALSE))
         
table_data %>% mutate(cluster_ind=as.factor(cluster_ind)) %>% 
  ggplot() +
  geom_density(aes(x=diff)) +
  geom_point(aes(x=diff, y=0, color=cluster_ind)) +
  labs(x="P(B,G)-P(B)*P(G)")


tables.ind = table_data %>% filter(cluster_ind)
tables.dep = table_data %>% filter(!cluster_ind) %>% 
  mutate(p_green_given_blue = bg/p_blue, 
         p_green_given_not_blue = g/(1-p_blue)) %>% 
  dplyr::select(-cluster_ind)

p.ind = tables.ind %>% 
  ggplot() +
  geom_density(aes(x=diff)) +
  geom_point(aes(x=diff, y=0, color=cluster_ind)) +
  labs(x="P(B,G)-P(B)*P(G)")

get_optimal_indep_params <- function(tables) {
  y <- tables %>% dplyr::select(diff) %>%  
    as.matrix()
  params <- uniform(0, 1, 2)
  
  distribution(y) <- greta::normal(mean=t(params[1]), sd=t(params[2]),
                                   truncation = c(-1, 1))
    
  m <- model(params)
  fit_opt <- opt(m)
  
  tibble(
    mu = fit_opt$par$params[1],
    sigma = fit_opt$par$params[2],
    value = fit_opt$value
  )
  
}
# results.ind <- get_optimal_indep_params(tables.ind)
# write_csv(results.ind, paste(result_dir, "results-indep-fit.csv", sep=fs))
results.ind = read_csv(paste(result_dir, "results-indep-fit.csv", sep=fs))

diff.min = tables.ind$diff %>% min()
diff.max = tables.ind$diff %>% max()
x = seq(diff.min, diff.max, length=1000)
pdf.ind = dtruncnorm(x, a=-1, b=1, mean=results.ind$mu, sd=results.ind$sigma)
p.ind + 
  geom_point(data=tibble(x=x, y=pdf.ind), aes(x=x, y=y), color='green', size=0.5)

# Dependent causal nets params
# P(C|A)
kmeans.pos = kmeans(tables.dep %>% dplyr::select(p_green_given_blue), 2)
closest1 = kmeans.pos$centers == max(kmeans.pos$centers) 
cl.pos = closest1 %>% as_tibble() %>% rowid_to_column() %>%
  filter(p_green_given_blue) %>% pull(rowid)

tbls.dep = tables.dep %>% add_column(cluster.pos=kmeans.pos$cluster)

# P(C|-A)
kmeans.neg = kmeans(tables.dep %>% dplyr::select(p_green_given_not_blue), 2)
closest0 = kmeans.neg$centers == min(kmeans.neg$centers) 
cl.neg = closest0 %>% as_tibble() %>% rowid_to_column() %>%
  filter(p_green_given_not_blue) %>% pull(rowid)

tbls.dep = tbls.dep %>% add_column(cluster.neg=kmeans.neg$cluster)

# tables for cns: A implies C / A implies -C
tbls.ac = tbls.dep %>% filter(cluster.pos == cl.pos & cluster.neg == cl.neg) %>%
  add_column(cn="A implies C")
tbls.anc = tbls.dep %>% filter(cluster.pos != cl.pos & cluster.neg != cl.neg) %>%
  add_column(cn = "A implies -C")

#  plot data
df.dep = bind_rows(tbls.ac, tbls.anc) %>%
  dplyr::select(row_id, empirical_id, cn, p_green_given_blue,
                p_green_given_not_blue, p_blue)
df.dep.long = df.dep %>% 
  pivot_longer(cols=c(p_green_given_blue, p_green_given_not_blue, p_blue),
               names_to="p.key", values_to="p.val")
p.dep = df.dep.long %>% ggplot() +
  geom_density(aes(x=p.val, color=cn)) +
  facet_wrap(~p.key)

# fit params
#@arg tbls is a matrix with single numeric column
get_optimal_dep_params <- function(df, marginal=FALSE) {
  y = df %>% as.matrix()
  if(marginal){
    params = uniform(0, 10, 2)
    distribution(y) <- greta::beta(shape1=t(params[1]), shape2=t(params[2]))
  } else {
    params = uniform(0, 1, 2)
    distribution(y) <-  greta::normal(mean=t(params[1]), sd=t(params[2]),
                                      truncation = c(0, 1))
  }
  m <- model(params)
  fit_opt <- opt(m)
  
  tibble(
    par1 = fit_opt$par$params[1],
    par2 = fit_opt$par$params[2],
    value = fit_opt$value
  )
}
# @arg cn: A implies C / A implies -C
fit_dep = function(df.dep, cn){
  dat = df.dep %>% filter(cn == (!! cn))
  tbls = dat %>% dplyr::select(p_green_given_blue, p_green_given_not_blue) %>%
    rename(p.pos=p_green_given_blue, p.neg=p_green_given_not_blue)
  if(cn=="A implies C"){ fn = "results-dep-a-implies-c-fit.csv"
  } else { fn = "results-dep-a-implies-not-c-fit.csv"
  }
  # results.pos <- get_optimal_dep_params(tbls %>% dplyr::select(p.pos)) %>%
  #   add_column(id="pos")
  # results.neg <- get_optimal_dep_params(tbls %>% dplyr::select(p.neg)) %>%
  #   add_column(id="neg")
  # results.marg = get_optimal_dep_params(dat %>% dplyr::select(p_blue), marginal=TRUE) %>%
  #   add_column(id="marg")
  # results = bind_rows(results.pos, results.neg, results.marg)
  # write_csv(results, paste(result_dir, fn, sep=fs))
  results = read_csv(paste(result_dir, fn, sep=fs))
  x = seq(0,1,length=1000)
  pdf.pos = dtruncnorm(x, a=0, b=1,
                       results %>% filter(id=="pos") %>% pull(par1),
                       results %>% filter(id=="pos") %>% pull(par2))
  pdf.neg = dtruncnorm(x, a=0, b=1,
                       results %>% filter(id=="neg") %>% pull(par1),
                       results %>% filter(id=="neg") %>% pull(par2))
  pdf.marg = dbeta(x, results %>% filter(id=="marg") %>% pull(par1),
                      results %>% filter(id=="marg") %>% pull(par2))
  
  fitted = tibble(pos=pdf.pos, neg=pdf.neg, marg=pdf.marg) %>%
    add_column(x=x) %>% 
    pivot_longer(cols=c(pos, neg, marg), names_to="p.key", values_to="p.val") %>%
    mutate(p.key = case_when(p.key == "pos" ~ "p_green_given_blue",
                             p.key=="neg" ~ "p_green_given_not_blue",
                             p.key == "marg" ~ "p_blue")) %>%
    add_column(cn=(!! cn))
  return(fitted)
}

fitted.ac = fit_dep(df.dep, "A implies C")
fitted.anc = fit_dep(df.dep, "A implies -C")

p.dep +  
  geom_point(data=fitted.ac, aes(x=x, y=p.val, color=cn), shape=6, size=0.25) +
  geom_point(data=fitted.anc, aes(x=x, y=p.val, color=cn), shape=6, size=0.25)
  
# Goodness fits -----------------------------------------------------------
# parameters
params.ac = read_csv(paste(result_dir, "results-dep-a-implies-c-fit.csv", sep=fs))
params.anc = read_csv(paste(result_dir, "results-dep-a-implies-not-c-fit.csv", sep=fs))
par.ind = read_csv(paste(result_dir, "results-indep-fit.csv", sep=fs))

par.ac.dep = params.ac %>% filter(id != "marg") %>%
  rename(mu=par1, sigma=par2)  %>% add_column(cn="A implies C")
par.anc.dep = params.anc %>% filter(id != "marg") %>% rename(mu=par1, sigma=par2) %>%
  add_column(cn="A implies -C")
par.dep.conditional = bind_rows(par.ac.dep, par.anc.dep)
par.dep.marginal = bind_rows(params.ac %>% filter(id=="marg"),
                             params.anc %>% filter(id=="marg"))

# log likelihood of empirical tables (used to fit distributions)
logL_truncnorm = function(data, par){
  data.mat = data %>% as.matrix()
  densities = dtruncnorm(data, mean=par$mu, sd=par$sigma)
  ll=sum(log(densities))
  return(ll)
}
logL_beta = function(data, par){
  data.mat = data %>% as.matrix()
  ll = dbeta(x=data, shape1=par$par1, shape2=par$par2, log=TRUE)
  return(sum(ll))
}
ll.ind = logL_truncnorm(tables.ind$diff, par.ind)
ll.ac = list(
  pos = logL_truncnorm(tbls.ac$p_green_given_blue, par.ac.dep %>% filter(id=="pos")),
  neg = logL_truncnorm(tbls.ac$p_green_given_not_blue, par.ac.dep %>% filter(id=="neg")),
  marg = logL_beta(tbls.ac$p_blue, params.ac %>% filter(id == "marg"))
)
ll.anc = list(
  pos = logL_truncnorm(tbls.anc$p_green_given_blue, par.anc.dep %>% filter(id=="pos")),
  neg = logL_truncnorm(tbls.anc$p_green_given_not_blue, par.anc.dep %>% filter(id=="neg")),
  marg = logL_beta(tbls.anc$p_blue, params.anc %>% filter(id == "marg"))
)

# compute log likelihood for each sample
goodness_fits = function(cn, n, N=30){
  tbls = ifelse(cn=="A implies C", tbls.ac, tbls.anc)
  N=nrow(tbls)
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
res.goodness = goodness_fits_dirichlet(cn="A implies C", 10**4) %>% arrange(desc(p.val));
p.vals = res.goodness %>% dplyr::select(-ll_sample) %>% distinct()
saveRDS(p.vals, paste(result_dir, "simulated-p-values.rds", sep=fs))

ll.obs = ll_empirical_data(params)
p = res.goodness %>% ggplot(aes(x=ll_sample)) +
  geom_density() +
  geom_point(data=ll.obs, aes(x=ll_sample, y=0), color='red', size=2) +
  geom_vline(data=ll.obs, aes(xintercept=ll_sample)) +
  facet_wrap(~stimulus_id) +
  theme_classic() +
  labs(x="log-likelihood simulated data")

p
ggsave(paste(result_dir, "plots", "goodness-theoretic-fits.png", sep=fs), p,
       height=6)



