
##################################################################
# packages and data
##################################################################

dat_raw <- read.csv("Data/RedCapData-LC-Allvariable.csv")
#dat <- dat_full[-c(1,19:22), ]
dat <- dat_raw %>%
  dplyr::filter(!grepl("TEST", record_id)) %>%
  filter(redcap_event_name == "baseline_arm_1") %>%
  filter(grepl("^\\w{2}_\\w{2,3}$", record_id))

################################
# data wrangling
################################
###### gender - 2 NA (?) ######

x <- dat %>%
  mutate(identified_gender=case_when(identified_gender == "1" ~ "Female",
                                     identified_gender == "2" ~ "Male")) %>%
  select(record_id, identified_gender, date_of_birth) %>%
  drop_na() %>%
  mutate(DOB=lubridate::as_date(date_of_birth)) %>%
  mutate(age=lubridate::interval(DOB, lubridate::now()) / years(1)) %>%
  mutate(ageGroups = case_when(age > 16 & age < 21 ~ "16-21yrs",
                               age > 21 & age < 25 ~ "22-26yrs",
                               age > 25 & age < 30 ~ "27-31yrs",
                               age > 30 & age < 36 ~ "32-35yrs")) %>%
  group_by(identified_gender, ageGroups) %>%
  summarise(pop = n(), frac = pop / nrow(.))

nudge_fun <- function(df) {
  ifelse(df$identified_gender == "Male", (sd(df$pop) / 3) * - 1, sd(df$pop) / 3)
}

xlimits <- c((length(x$identified_gender)*1.5) * -1, length(x$identified_gender)*1.5)

x2 <- x %>%
  mutate(
         pop = ifelse(identified_gender == "Male", pop * (-1), pop * 1),
         frac = ifelse(identified_gender == "Male", frac * (-1), frac * 1),
         share = paste0(abs(round(frac * 100, 1)), "%"),
         identified_gender = as.factor(identified_gender))



theme_set(theme_minimal())

pyramid_plot <- x2 %>%
  ggplot(data = ., aes(x = pop, y = ageGroups, label = share)) +
  geom_col(aes(fill = identified_gender)) +
  geom_text(aes(label = share),
            position = position_nudge(x = nudge_fun(x2)),
            size = 3) +
  geom_vline(xintercept = 0, linetype = "dotted",
             color = "black", size = 1.5) +
  scale_fill_manual("", values = c("#829cb2", "#e36c33")) +
  scale_x_continuous("", breaks = scales::pretty_breaks(n = 6),
    labels = function(br) {
      ifelse(abs(br) >= 1000,
             paste0(abs(br) / 1000, "k"),
             abs(br))
    }
  ) +
  theme(legend.position = "bottom", 
        legend.justification = "left",
        legend.text = element_text(size=8),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_text(size=8),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank())

######################
######## race ########
######################

totalP<- dat %>%
  `[`(33:44) %>%
  #drop_na() %>%
  nrow()

plotdat<- dat %>%
  `[`(33:44) %>%
  #drop_na() %>%
  pivot_longer(cols = race___1:race___11, values_to = "value",
               names_to = "Race") %>%
  filter(value==1) %>%
  group_by(Race) %>%
  summarise(`Total`=n()) %>%
  mutate(Percentage=(Total/totalP)*100) %>%
  mutate(Percentage = round(Percentage, 0)) %>%
  mutate(bar_text = paste0(Percentage, "%")) %>%
  ungroup() %>%
  mutate(Race = recode(Race,
                       "race___1" = "First Nations",
                       "race___2" = "East Asian",
                       "race___3" = "Southeast Asian",
                       "race___4" = "South Asian",
                       "race___5" = "Black",
                       "race___6" = "Central/ South American",
                       "race___7" = "Central Asia\n& Middle East",
                       "race___8" = "White",
                       "race___9" = "Hawaiian or\nPacific Islander",
                       "race___10" = "Interracial"))


theme_set(theme_minimal())

depn_insti <- "#f9570c"
col_dislike_alot <- "#e36c33"
col_dislike <- "#edad88"
col_neutral <- "#c7cdd1"
col_like <- "#829cb2"
col_like_alot <- "#3e6487"
darkerstill <- "#142c43"
missing <- "#636363"


gg3<-ggplot(data=plotdat, aes(x=Race, y=Percentage, fill=Race)) +
  geom_bar(stat="identity") +
  theme(axis.title.y = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(legend.title = element_blank()) + 
  theme(legend.position = "none") +
  scale_fill_manual(values = c(missing, col_like, depn_insti,col_neutral, darkerstill, col_dislike_alot))

gg4<- gg3 + scale_y_continuous(labels=function(x) paste0(x,"%"))




######################
###### income  #######
######################

theme_set(theme_minimal())

nPeople<-dat %>%
  select(household_income, participant_income) %>%
  nrow()


plotdat_householdInc <- dat %>%
  select(household_income, participant_income) %>%
  pivot_longer(cols = household_income:participant_income, values_to = "category",
               names_to = "incomeCat") %>% 
  group_by(incomeCat, category) %>% 
  summarise(n=n()) %>%
  mutate(category = recode(category, 
                           "1" = "Less than $10,000",
                           "2" = "$10,000 to $19,999",
                           "3" = "$20,000 to $39,999",
                           "4" = "$40,000 to $59,999",
                           "5" = "$60,000 to $99,999",
                           "6" = "$100,000 and above",
                           "7" = "Don't know",
                           "8" = "Prefer not to answer")) %>%
  mutate(percent=(n/nPeople)*100) %>%
  mutate(Percentage = round(percent, 0)) %>%
  mutate(bar_text = paste0(Percentage, "%")) %>%
  ungroup() %>% drop_na()

order<- plotdat_householdInc %>% 
  dplyr::filter(incomeCat=="household_income") %>%
  pull(category)

df_odered <- plotdat_householdInc %>% 
  mutate(category = factor(category, levels = order)) %>% 
  drop_na() %>%
  mutate(incomeCat = recode(incomeCat,
                            "household_income" = "Household Income",
                            "participant_income" = "Personal Income"))

incomeplot1 <- ggplot(data=df_odered, aes(y=category, x=Percentage, fill=incomeCat)) +
  geom_bar(position = "dodge", stat="identity") +
  theme(axis.title.y = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(legend.title = element_blank()) + 
  scale_fill_manual(values = c("Household Income" = col_like_alot,
                               "Personal Income" = col_like))


incomeplot <- incomeplot1 + scale_x_continuous(labels=function(x) paste0(x,"%"))





######################
######## bls  ########
######################

totalP<-dat %>%
  select(bls_code) %>%
  drop_na() %>%
  nrow(.)

plotdat<-dat %>%
  select(bls_code) %>%
  mutate(bls_code = recode(bls_code,
                           "1" = "Employed: F/T",
                           "2" = "Employed: P/T",
                           "3" = "Employed: Supported employment/training",
                           "4" = "Employed: Working without pay",
                           "5" = "Unemployed: Searching",
                           "6" = "Unemployed: Temporarily laid off",
                           "7" = "Student F/T",
                           "8" = "Caregiver F/T",
                           "9" = "NLFET")) %>%
  #drop_na() %>%
  group_by(bls_code) %>%
  summarise(Total = n()) %>%
  mutate(percent=(Total/totalP)*100) %>%
  mutate(percent = round(percent, 0)) %>%
  mutate(bar_text = paste0(percent, "%")) %>%
  ungroup() 

theme_set(theme_minimal())


gg1 <- plotdat %>%
  ggplot(aes(fill = bls_code, values = Total)) +
  geom_waffle(#n_col = 5,
    n_rows = 4, 
    size = 0.2, 
    colour = "white", 
    make_proportional = FALSE, 
    radius = unit(10, "pt")) +
  scale_fill_manual(name = NULL,
                    values = c(depn_insti, col_dislike_alot, col_dislike, 
                               col_like, col_like_alot, darkerstill)) +
  coord_equal() +
  theme_void() +
  theme(legend.position = "top",
        legend.text = element_text(color = "black", size = 8)) + labs(x="Test")


### new bls ########
all_categories <- data.frame(bls_code = c(1, 2, 3, 4, 5, 6, 7, 8, 9))

plotdat <- dat %>%
  select(bls_code) %>%
  group_by(bls_code) %>%
  summarise(Total = n()) %>%
  right_join(all_categories, by = "bls_code") %>%
  mutate(Total = ifelse(is.na(Total), 0, Total)) 

data <- data.frame(
  employment_status = plotdat$bls_code,
  count = plotdat$Total
)

data$group <- recode(data$employment_status,
                     "1" = "Employed",
                     "2" = "Employed",
                     "3" = "Employed",
                     "4" = "Employed",
                     "7" = "Employed",
                     "8" = "Employed",
                     "5" = "Unemployed",
                     "6" = "Unemployed",
                     "9" = "NLFET")
data$group <- factor(data$group, levels = c("Employed", "Unemployed", "NLFET"))

data$category <- recode(data$employment_status,
                        "1" = "Employed: F/T",
                        "2" = "Employed: P/T",
                        "3" = "Employed: Supported employment/training",
                        "4" = "Employed: Working without pay",
                        "5" = "Unemployed: Searching",
                        "6" = "Unemployed: Temporarily laid off",
                        "7" = "Student F/T",
                        "8" = "Caregiver F/T",
                        "9" = "NLFET")
data$category <- factor(data$category, 
                        levels = c("Employed: F/T", "Employed: P/T", 
                                   "Employed: Supported employment/training", 
                                   "Employed: Working without pay", 
                                   "Student F/T", "Caregiver F/T", 
                                   "Unemployed: Searching", 
                                   "Unemployed: Temporarily laid off", 
                                   "NLFET"))
# Plot: Grouped bar plot
gg1_new <- ggplot(data, aes(x = group, y = count, fill = category)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = " ", y = "Count", title = " ", fill = "Employment Status") +
  scale_fill_manual(values = c("#f9570c", "#e36c33", "#ebc48a", "#edad88", 
                               "#3e6487", "#829cb2", "#c7cdd1", "#636363",
                               "#142c43")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = -45, hjust = 0) )




