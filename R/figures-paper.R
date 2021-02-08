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
  mutate(utt.type=case_when(utt.type=="lit" ~ "literal",
                            utt.type=="might" ~ "might+literal",
                            utt.type=="conj" ~ "AND",
                            utt.type=="conditional" ~ "IF", 
                            TRUE ~ utt.type)) %>% 
  mutate(stimulus=as.factor(id),
         utt.type=factor(utt.type, levels=c("might+literal", "literal", "AND", "IF")))

p = df.unc %>%
  ggplot(aes(y=id)) +
  geom_bar(aes(x=ratio, fill=utt.type), stat="identity") +
  theme_classic() +
  theme(legend.position="top",
        legend.title = element_text(size = 8), 
        legend.text = element_text(size = 8)) +
  scale_fill_discrete(guide=guide_legend(title="utterance type", title.position="top")) +
  scale_y_discrete(position="right", name="") +
  scale_x_continuous(name="ratio participants")
p
ggsave(paste(DATA$plot_dir, "behavioral-ifs.png", sep=fs), p, width=3.8, height=2)

df.unc %>% mutate(id=as.character(id), utt.type=as.character(utt.type)) %>%
  filter(utt.type=="IF") %>% arrange(ratio)


# particular conditions ---------------------------------------------------
stim1 = "if1_uh"
stim2 = "independent_hl"
pids =  dat$prolific_id %>% unique
dat <- tables.long %>% ungroup() %>%
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
# first example
set.seed(123)
df = dat %>% filter(id == (!! stim1)) %>%
  filter(prolific_id %in% sample(pids, 6, replace=FALSE)) %>% 
  group_by(question, prolific_id)
p.ex1 <- dat %>% filter(id == (!! stim1)) %>%
  ggplot(aes(x=question, y=response)) +
  geom_violin() +
  geom_jitter(data=df, aes(shape=prolific_id), width=0.1, height = 0, show.legend=FALSE) +
  geom_line(data=df, aes(group=prolific_id), alpha=0.5) +
  theme_classic() +
  theme(legend.position="none",
        axis.text.x = element_text(size=8, angle=30, vjust = 1, hjust=1),
        axis.text.y=element_text(size=8)
  ) + labs(y="", x="")
p.ex1

# second example
set.seed(123)
df = dat %>% filter(id == (!! stim2)) %>%
  filter(prolific_id %in% sample(pids, 6, replace=FALSE)) %>% 
  group_by(question, prolific_id)
p.ex2 <- dat %>% filter(id == (!! stim2)) %>%
  ggplot(aes(x=question, y=response)) +
  geom_violin() +
  geom_jitter(data=df, aes(shape=prolific_id), width=0.1, height = 0, show.legend=FALSE) +
  geom_line(data=df, aes(group=prolific_id), alpha=0.5) +
  theme_classic() +
  theme(legend.position="none",
        axis.text.x = element_text(size=8, angle=30, vjust = 1, hjust=1),
        axis.text.y=element_text(size=8)
  ) + labs(y="", x="")
p.ex2

# Averages ----------------------------------------------------------------
df.dirichlet = plotAveragePredictions("tables-dirichlet-filtered-augmented", DATA$result_dir)
df.model = plotAveragePredictions("tables-model-filtered-augmented", DATA$result_dir)

results.joint = bind_rows(
  df.dirichlet %>% add_column(predictor="situation-specific prior"),
  df.model %>% add_column(predictor="abstract prior")
) %>% dplyr::select(-n.stim) %>% 
  mutate(utt.type=as.character(utt.type)) %>% 
  mutate(stimulus.type=case_when(str_detect(stimulus, "if") ~ "dependent",
                                 TRUE ~ "independent")) %>%
  mutate(utt.type=case_when(utt.type=="might + literal" ~ "might+literal",
                            utt.type=="conjunction" ~ "AND",
                            utt.type=="conditional" ~ "IF", 
                            TRUE ~ utt.type)) %>%
  mutate(utt.type=factor(utt.type, levels=c("might+literal", "literal", "IF", "AND")))

p.scatter = ggscatter(results.joint, y = "behavioral", x = "model", add = "reg.line",
                      conf.int = FALSE, cor.coef = FALSE, cor.method = "pearson",
                      ylab = "Empirical observations", xlab = "Model predictions",
                      ) +
  stat_cor(label.x = 0, label.y=0.6, method="pearson", label.sep = "\n", size = 3) +
  geom_point(data=results.joint, aes(y=behavioral, x=model, color=utt.type)) +
  theme_bw() +
  scale_color_discrete(name = 'utterance type') +
  theme(legend.position="top", legend.title = element_text(size = 10),
        legend.text = element_text(size = 10), axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10)) +
  facet_wrap(~predictor)
p.scatter
ggsave(paste(DATA$plot_dir, "correlations.png", sep=fs), p.scatter, width=6, height=3.3)


# Figure 3 ----------------------------------------------------------------
# C. Bar plots
results.joint.long = left_join(
  df.dirichlet %>% rename(model.dirichlet=model, sd.dirichlet=sd) %>% 
    dplyr::select(-n.stim),
  df.model %>% dplyr::select(-n.stim) %>% rename(model.abstract=model, sd.abstract=sd),
  by=c("utterance", "stimulus", "utt.type", "behavioral")
) %>% group_by(stimulus, utterance) %>% 
  pivot_longer(cols=c(behavioral, model.dirichlet, model.abstract), 
               names_to="predictor", values_to="p") %>% 
  mutate(sd=case_when(predictor=="model.dirichlet" ~ sd.dirichlet,
                      predictor=="model.abstract" ~ sd.abstract)) %>%
  dplyr::select(-sd.dirichlet, -sd.abstract)

results.joint.long.types = results.joint.long %>% 
  ungroup() %>% dplyr::select(-utterance) %>%
  group_by(stimulus, utt.type, predictor) %>% 
  summarize(p=sum(p), .groups="keep") %>%
  mutate(utt.type=as.character(utt.type),
         predictor=case_when(predictor=="model.abstract" ~ "abstract",
                             predictor=="model.dirichlet" ~ "specific",
                             TRUE ~ predictor),
         predictor=factor(predictor, levels=c("behavioral", "abstract", "specific")),
         utt.type=case_when(utt.type == "might + literal" ~ "might+literal",
                            utt.type=="conditional" ~ "IF",
                            utt.type=="literal" ~ "literal",
                            utt.type=="conjunction" ~ "AND"),
         utt.type=factor(utt.type, levels=c("might+literal", "IF", "literal", "AND")))

p.bars1 = results.joint.long.types %>% filter(p > 0) %>% arrange(desc(p)) %>% 
  filter(stimulus == (!! stim1)) %>%
  mutate(predictor=as.factor(predictor)) %>%
  ggplot(aes(y=p, x=utt.type, fill=predictor)) +
  geom_bar(stat="identity", position=position_dodge(0.9)) +
  # geom_errorbar(aes(ymin = p - sd, ymax = p + sd), position = position_dodge(0.9),
  #               width = 0.2) +
  theme_classic() + labs(y="", x="") +
  theme(axis.text.y=element_text(size=8),
        legend.title = element_text(size = 8),
        axis.text.x = element_text(angle=30, vjust=1, hjust=1, size=8)
  )
  # scale_fill_discrete(guide=guide_legend(nrow=2, title.position="top")) 
p.bars1

p.bars2 = results.joint.long.types %>%
  filter(p > 0) %>% arrange(desc(p)) %>% 
  filter(stimulus == (!! stim2)) %>%
  mutate(predictor=as.factor(predictor)) %>%
  ggplot(aes(y=p, x=utt.type, fill=predictor)) +
  geom_bar(stat="identity", position=position_dodge(0.9)) +
  # geom_errorbar(aes(ymin = p - sd, ymax = p + sd), position = position_dodge(0.9),
  #               width = 0.2) +
  theme_classic() + labs(x="", y="") +
  theme(legend.position="none",
        axis.text.x = element_text(angle=30, vjust=1, hjust=1, size=8), 
        axis.text.y=element_text(size=8)
        )
p.bars2

p.stim1 = readPNG("/home/britta/UNI/Osnabrueck/blockwords/stimuli/img/group1/if1_uh.png")
im.stim1 <- ggplot() + background_image(p.stim1)
p.stim2 = readPNG("/home/britta/UNI/Osnabrueck/blockwords/stimuli/img/group1/independent_hl.png")
im.stim2 <- ggplot() + background_image(p.stim2)

p = ggpubr::ggarrange(im.stim1, p.ex1, p.bars1, im.stim2, p.ex2, p.bars2, 
                      labels = c("A1", "A2", "A3", "B1", "B2", "B3"), 
                      common.legend = T, legend="bottom", nrow = 2, ncol = 3,
                      font.label = list(size = 8), align="h") +
  guides(fill = guide_legend(override.aes = list(size = 1)))
p
ggsave(paste(DATA$plot_dir, "fig3.png", sep=fs), p, width=6, height=3.5)


# why does the model predict more conditionals than participants in if1_uh?
df = DATA$production %>% dplyr::select(prolific_id, id, response) %>%
  filter(id == "if1_uh") %>% rename(utterance=response) %>% chunk_utterances()

df.prior = DATA$prior.smooth %>% filter(!is.na(question)) %>%
  dplyr::select(prolific_id, id, response, question) %>%
  filter(id == "if1_uh")
  
df.all = group_map(df %>% group_by(utterance), function(df.utt, utt){
  right_join(df.prior, df.utt) %>% add_column(uttered=utt$utterance)
}) %>% bind_rows() %>%
  mutate(question=factor(question, levels=questions.test))

p = df.all %>%
  ggplot(aes(x=question, y=response)) +
  geom_violin() +
  geom_jitter(aes(color=prolific_id), width=0.1, height = 0, alpha=0.5) +
  geom_hline(aes(yintercept=0.7), alpha=0.5) +
  theme_classic() +
  theme(legend.position="none", l) +
  labs(y="density slider rating", x="outcome") +
  facet_wrap(~uttered)

ggsave(paste(DATA$plot_dir, "if1_uh-empirical.png", sep=fs), p, width=8, height=6)
