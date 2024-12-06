##################################################################
# packages and data
##################################################################

dat_raw <- read.csv("Data/RedCapData-LC-Allvariable.csv")
#dat <- dat_full[-c(1,19:22), ]
dat <- dat_raw %>%
  dplyr::filter(!grepl("TEST", record_id)) %>%
  filter(redcap_event_name == "baseline_arm_1") %>%
  filter(grepl("^\\w{2}_\\w{2,3}$", record_id))


##################################################################
#wrangling
##################################################################

plotdat <- dat %>%
  select(tobacco, vape, alcohol, mj_thc, cocaine, 
         opiates, pcp, amphet, mdma, ghb_rohypnol, 
         huffing, hallucinogens, other) %>%
  mutate(across(everything(), ~replace(., . ==  888, 0)))  %>%
  rownames_to_column("person") %>%
  as_tibble %>%
  drop_na() %>%
  group_by(person) %>%
  mutate(totalP = nrow(.)) %>% 
  ungroup() %>%
  pivot_longer(cols = tobacco:other, values_to = "score",
               names_to = "DRUG") %>%
  filter(!score > 5) %>%
  mutate(DRUG = as.factor(DRUG)) %>%
  mutate(score=as.factor(score)) %>%
  group_by(DRUG, score, totalP) %>%
  count() %>%
  mutate(percent = (n / totalP)*100) %>%
  mutate(percent = round(percent, 0)) %>%
  mutate(bar_text = paste0(percent, "%")) %>%
  ungroup()

order <- plotdat %>%
  mutate(numericScore = as.numeric(as.character(score))) %>%
  mutate(weightedScore = numericScore * percent / 100) %>%
  group_by(DRUG) %>%
  summarise(aggScore = sum(weightedScore)) %>%
  arrange(aggScore) %>%
  pull(DRUG)

df_odered <- plotdat %>% 
  mutate(DRUG = factor(DRUG, levels = order))

##################################################################
#plotting
##################################################################
depn_insti <- "#f9570c"
col_dislike_alot <- "#e36c33"
col_dislike <- "#edad88"
col_neutral <- "#c7cdd1"
col_like <- "#829cb2"
col_like_alot <- "#3e6487"
missing <- "#636363"

showtext_auto()

theme_set(theme_minimal())

gg1 <- df_odered %>%
  mutate(DRUG = recode(DRUG,
                       "mj_thc" = "Cannabis",
                       "alcohol" = "Alcohol",
                       "vape" = "Vaping",
                       "tobacco" = "Tobacco",
                       "mdma" = "MDMA",
                       "hallucinogens" = "Hallucinogens",
                       "cocaine" = "Cocaine",
                       "pcp" = "PCP",
                       "other" = "Other",
                       "opiates" = "Opiates",
                       "huffing" = "Huffing",
                       "ghb_rohypnol" = "GHB",
                       "amphet" = "Amphetamines")) %>%
  mutate(score = recode(score,
                        "1" = "Abstinent",
                        "2" = "Use without impairment",
                        "3" = "Abuse",
                        "4" = "Dependence",
                        "5" = "Dependence with institutionalisation",
                        "0" = "Missing")) %>%
  ggplot(aes(y = DRUG, x = percent, fill = score)) + 
  geom_col(width = 0.8,  position = position_stack(reverse = TRUE)) +
  theme(legend.position = "top") + 
  theme(plot.margin = unit(rep(0.7, 4), "cm")) +
  scale_fill_manual(values = c(`Abstinent` = col_like_alot,
                               `Use without impairment` = col_like,
                               `Abuse` = col_neutral,
                               `Dependence` = col_dislike,
                               `Dependence with institutionalisation` = depn_insti,
                               `Missing` = missing)) +
  theme(legend.title = element_blank()) +
  theme(legend.key.height = unit(0.35, "cm")) +
  theme(legend.key.width = unit(0.27 * 3, "cm")) +
  theme(axis.title = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  theme(panel.grid.major.x = element_line(color = "grey",
                                          linewidth = 0.8)) +
  scale_x_continuous(expand = c(0, 0), labels = paste0(seq(0, 100, 25), "%")) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(axis.text = element_text(color = "black", size = 8)) +
  theme(plot.title = element_markdown(size = 2, face = "bold")) +
  theme(legend.text = element_text(size = 5))

gg2_drugsbar <- gg1 + geom_text(aes(label = bar_text),
                       position = position_stack(vjust = 0.5, reverse = TRUE),
                       colour="white",
                       size = 2)




