library(cowplot)
library(ggpubr)
library(here)
library(gridExtra)
source(here("R", "analysis-utils.R"))
source(here("R", "plot-functions.R"))

exp.name = "blockwords"
use_filtered = TRUE


# Data --------------------------------------------------------------------
DATA = load_exp_data(exp.name, use_filtered)

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

# causality plays a role for utterance when no more informative utt applicable
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

p = df.unc %>%
  ggplot(aes(y=id)) +
  geom_bar(aes(x=ratio, fill=utt.type), stat="identity") +
  theme_classic(base_size = 20) +
  theme(legend.position="top") +
  labs(y="stimulus", x="ratio participants")
p
ggsave(paste(DATA$plot_dir, "behavioral-ifs.png", sep=fs), p, width=9, height=5)

df.unc %>% mutate(id=as.character(id), utt.type=as.character(utt.type)) %>%
  filter(utt.type=="conditional") %>% arrange(ratio)


# particular conditions ---------------------------------------------------
pids =  dat$prolific_id %>% unique
dat <- tables.long %>% ungroup() %>%
  filter(id %in% c("if1_uh", "independent_hl")) %>%
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
df = dat %>% filter(prolific_id %in% sample(pids, 6, replace=FALSE)) %>% 
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



# Averages ----------------------------------------------------------------
df.dirichlet = plotAveragePredictions("tables-dirichlet-filtered", DATA$result_dir) %>%
  add_column(predictor="context-dependent")
df.model = plotAveragePredictions("tables-model-filtered", DATA$result_dir) %>%
  add_column(predictor="context-independent")

df.joint = bind_rows(df.dirichlet, df.model)
p.scatter = ggscatter(df.joint, y = "behavioral", x = "model", add = "reg.line",
                      conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson",
                      ylab = "Empirical observations", xlab = "Model predictions") +
  geom_point(data=df.joint, aes(y=behavioral, x=model, color=utterance)) +
  theme_bw(base_size=20) + theme(legend.position = "top")  +
  facet_wrap(~predictor)
       # p.scatter, height=12, width=20)

ggsave(paste(DATA$plot_dir, "correlations.png", sep=fs), p.scatter, width=12, height=8)

# bar plots
data.joint.long = df.joint %>%  dplyr::select(-predictor) %>% 
  pivot_longer(cols=c(behavioral, model), names_to="predictor", values_to="p") %>%
  mutate(predictor=as.factor(predictor)) %>% 
  filter(stimulus %in% c("if1_uh", "independent_hl"))

p.bars = data.joint.long %>% filter(p > 0) %>% arrange(desc(p)) %>% 
  ggplot(aes(x=p, y=utterance, fill=predictor)) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme_classic(base_size=18) +
  theme(legend.position="bottom") +
  labs(x="ratio participants/average model prediction", y="utterance type") +
  facet_wrap(~stimulus)
ggsave(paste(DATA$plot_dir, "example_bars.png", sep=fs), p.bars, height=8, width=12)









