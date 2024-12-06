
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

# Remove the first and third rows from the mtcars dataset

nPeople<-dat %>%
  select(suicidal_ideation, suicide_attempts, nonsuicidal_selfinjury, 
          suicide_complete, legal_issues, jail_prison, 
          nights_jail_prison, court_ordered_treatment, violent_ideation, 
          violent_behavior, harmed_attack_assaulted) %>%
  nrow()


plotdat<-dat %>%
  select(suicidal_ideation, suicide_attempts, nonsuicidal_selfinjury, 
         suicide_complete, legal_issues, jail_prison, 
         nights_jail_prison, court_ordered_treatment, violent_ideation, 
         violent_behavior, harmed_attack_assaulted) %>%
  pivot_longer(cols = suicidal_ideation:harmed_attack_assaulted, values_to = "value",
               names_to = "allVs") %>%
  filter(value==1) %>%
  group_by(allVs) %>%
  summarise(`Total`=n()) %>%
  mutate(percent=(Total/nPeople)*100) %>%
  mutate(percent = round(percent, 0)) %>%
  mutate(bar_text = paste0(percent, "%")) %>%
  ungroup() 

order<-plotdat%>%
  arrange(percent) %>%
  pull(allVs)

df_odered <- plotdat %>% 
  mutate(allVs = factor(allVs, levels = order))

##################################################################
#plot
##################################################################

depn_insti <- "#f9570c"
col_dislike_alot <- "#e36c33"
col_dislike <- "#edad88"
col_neutral <- "#c7cdd1"
col_like <- "#829cb2"
col_like_alot <- "#3e6487"
missing <- "#636363"

theme_set(theme_minimal())

gg1 <- df_odered %>%
  mutate(allVs = recode(allVs, 
                          "harmed_attack_assaulted" = "Victim of violent\nattack",
                          "violent_behavior" = "Commited violent\nbehaviour",
                          "violent_ideation" = "Violent ideation",
                          "suicide_attempts" = "Suicide attempts",
                          "suicidal_ideation" = "Suicidal ideation",
                          "nonsuicidal_selfinjury" = "Self injury\n(non-suicidal)",
                          "court_ordered_treatment" = "Court ordered\ntreatment",
                          "jail_prison" = "incarceration",
                          "legal_issues" = "Legal issues",
                          "nights_jail_prison" = ">1 night\nin jail")) %>%
  ggplot(aes(y = allVs, x = percent, fill = percent)) + 
  geom_col(width = 0.8,  position = position_stack(reverse = TRUE)) +
  theme(legend.position = "top") + 
  theme(plot.margin = unit(rep(0.7, 4), "cm")) +
scale_fill_gradient2(
  name = waiver(),
  low = col_like_alot,
  mid = col_neutral,
  high = depn_insti,
  midpoint = 0,
  space = "Lab",
  na.value = "grey50",
  transform = "identity",
  guide = "colourbar",
  aesthetics = "fill"
) +
theme(plot.margin = unit(rep(0.7, 4), "cm")) +
  scale_y_discrete(expand = c(0, 0)) +
  theme(axis.text = element_text(color = "black", size = 8),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x=element_blank(),
        #axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "none",
        legend.title = element_blank())

gg2_bar <- gg1 + geom_text(aes(label = bar_text),
                       position = position_stack(vjust = 0.5, reverse = TRUE),
                       colour="white",
                       size = 4)

