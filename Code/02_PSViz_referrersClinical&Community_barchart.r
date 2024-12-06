
depn_insti <- "#f9570c"
col_dislike_alot <- "#b29882"
col_dislike <- "#edad88"
col_neutral <- "#c7cdd1"
col_like <- "#829cb2"
col_like_alot <- "#3e6487"
missing <- "#636363"
new <- "#DB6B0B"
other <- "#F9CA0C"
purp <- "#b2829c"

ylim = 85

myColours = c("Other" = missing,
              "Unknown" = other,
              "Provider/clinician/agency" = new,
              "Presentation" = depn_insti,
              "Infoline/211/988" = col_like_alot,
              "Website/social media" = col_like,
              "Bus ad/billboard/cinema" = col_neutral,
              "Newspaper/radio/TV" = col_dislike,
              "Brochure/flyer/mailer" = col_dislike_alot)

#####################################################
# 1) Make Dataframes
#####################################################
dat <- temp  %>%
  filter(!is.na(`SCREEN NO.`)) %>%
  rename(Inqstat = as.factor("INQUIRY STATUS")) %>%
  rename(Inqdate = "INQUIRY DATE (VOICEMAIL LEFT)") %>%
  rename(IngLine = "Indicate whether inquiry was made via Learning collaborative phone line or other") %>%
  rename(ReasonNoEnroll3mo = `IF NO, PLEASE SELECT THE CODE THAT BEST DESCRIBES THE REASON`) %>%
  mutate(Inqdate = lubridate::as_date(Inqdate)) %>%
  mutate(Inqdate_month = month(Inqdate)) %>%
  mutate(Inqdate_day = mday(Inqdate)) %>%
  mutate(
    heardAboutInititative = as.factor(
      `SELECT CODE THAT BEST DESCRIBES HOW THEY HEARD ABOUT THIS INITIATIVE`
    )
  ) %>%
  mutate(REGION = recode(REGION, "STEP" = "2")) %>%
  mutate(REGION = as.factor(REGION)) %>%
  mutate(
    Community_Referral = as.factor(`SELECT THE COMMUNITY CODE THAT BEST CAPTURES THE REFERRER PROFILE`)
  ) %>%
  mutate(Clinical_Referral = as.factor(`SELECT THE CLINICAL CODE THAT BEST CAPTURES THE REFERRER PROFILE`)) %>%
  mutate(
    eligibility = as.factor(
      `PLEASE SELECT THE CODE THAT BEST SUITS THE ELIGIBILITY STATUS OF THIS PARTICIPANT`
    )
  ) %>%
  mutate(
    elig_YN = case_when(
      eligibility == "1 Eligible - met all inclusion criteria (including consent)" ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  mutate(Over18 = AGE >= 18) %>%
  rename(
    FU_admitted = as.factor(
      "HAS THE PATIENT BEEN SUCCESFULLY ADMITTED INTO TREATMENT BY THE 3 MOS MARK?"
    )
  ) %>%
  mutate(FU_admitted = as.character(FU_admitted)) %>%
  rename(DateConsent = "DATE OF CONSENT") %>%
  mutate(DateConsent = lubridate::as_date(DateConsent)) %>%
  rename(DateAdmittedLMHA = "IF YES, PLEASE INDICATE THE DATE OF ADMISSION") %>%
  mutate(DateAdmittedLMHA = lubridate::as_date(DateAdmittedLMHA)) %>%
  mutate(LMHA = as.factor(`NAME OF LMHA THE PATIENT HAS BEEN REFERRED TO`))


comm_df <- dat %>%
  dplyr::select(Community_Referral, heardAboutInititative) %>%
  mutate(
    heardAboutInititative = recode(
      heardAboutInititative,
      "9 Other (please specify in next column)" = "Other",
      "1 Brochure/flyer/mailer" = "Brochure/flyer/mailer",
      "2 Newspaper/radio/TV" = "Newspaper/radio/TV",
      "3 Bus ad/billboard/cinema ad" = "Bus ad/billboard/cinema",
      "4 website/social media" = "Website/social media",
      "5 Infoline/211/988" = "Infoline/211/988",
      "6 Presentation" = "Presentation",
      "7 Provider/clinician/agency" = "Provider/clinician/agency",
      "8 Unknown/cannot remember"	= "Unknown"
    )
  ) %>%
  mutate(
    Community_Referral = recode(
      Community_Referral,
      "1 - Patient" = "Patient",
      "2 - Family" = "Family",
      "3 - Police" = "Police",
      "4 - Teacher or guidance counselor" = "Teacher or guidance counselor",
      "5 - Other (community)" = "Other"
    )
  ) %>%
  filter(!Community_Referral == "NA") %>%
  filter(!heardAboutInititative == "NA") %>%
  filter(!is.na(Community_Referral)) %>%
  filter(!is.na(heardAboutInititative)) %>%
  group_by(Community_Referral) %>%
  reframe(Community_Referral,
          n = n(),
          heardAboutInititative = heardAboutInititative) %>%
  group_by(Community_Referral) 


clin_df <- dat %>%
  dplyr::select(Clinical_Referral, heardAboutInititative) %>%
  mutate(
    heardAboutInititative = recode(
      heardAboutInititative,
      "9 Other (please specify in next column)" = "Other",
      "1 Brochure/flyer/mailer" = "Brochure/flyer/mailer",
      "2 Newspaper/radio/TV" = "Newspaper/radio/TV",
      "3 Bus ad/billboard/cinema ad" = "Bus ad/billboard/cinema",
      "4 website/social media" = "Website/social media",
      "5 Infoline/211/988" = "Infoline/211/988",
      "6 Presentation" = "Presentation",
      "7 Provider/clinician/agency" = "Provider/clinician/agency",
      "8 Unknown/cannot remember"	= "Unknown"
    )
  ) %>%
  mutate(
    Clinical_Referral = recode(
      Clinical_Referral,
      "1 - Emergency department" = "Emergency department",
      "2 - Psychiatric inpatient"	= "Psychiatric inpatient",
      "3 - Intensive outpatient"	= "Intensive outpatient",
      "4 - Primary care provider" = "Primary care provider",
      "5 - Outpatient mental health"	=
        "Outpatient mental health",
      " 6 - Acute evaluation" = "Acute evaluation",
      "7 - mobile evaluation"	= "Mobile evaluation",
      "8 - Other mental health" = "Other mental health"	,
      "9 - other medical provider" = "Other medical provider",
      "10 - Other (clinical)" = "Other"
    )
  ) %>%
  filter(!Clinical_Referral == "NA") %>%
  filter(!heardAboutInititative == "NA") %>%
  filter(!is.na(Clinical_Referral)) %>%
  filter(!is.na(heardAboutInititative)) %>%
  group_by(Clinical_Referral) %>%
  reframe(Clinical_Referral,
          n = n(),
          heardAboutInititative = heardAboutInititative) %>%
  group_by(Clinical_Referral)


# Define the common order for 'heardAboutInititative'
common_levels <- c("Brochure/flyer/mailer",
                   "Newspaper/radio/TV",
                   "Bus ad/billboard/cinema",
                   "Website/social media",
                   "Infoline/211/988",
                   "Presentation",
                   "Provider/clinician/agency",
                   "Unknown", "Other") 

all_categories <- data.frame(heardAboutInititative = common_levels)

# Set the same factor levels in both datasets
comm_df$heardAboutInititative <- factor(comm_df$heardAboutInititative, levels = common_levels)
clin_df$heardAboutInititative <- factor(clin_df$heardAboutInititative, levels = common_levels)


# 2) Make plots

referrersCommunityPlot <- comm_df %>%
  ggplot(aes(y = Community_Referral)) +
  geom_bar(aes(fill = heardAboutInititative)) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title = element_blank()
  ) +
  # scale_x_discrete(drop = FALSE) +
  scale_y_discrete(labels = scales::label_wrap(10)) +
  scale_x_continuous( limits = c(0, ylim), breaks = seq(0, ylim, by = 10) ) +
  guides(fill = guide_legend(nrow = 3)) + 
  scale_fill_manual(name = "heardAboutInititative", values = myColours)


referrersClinicalPlot <- clin_df %>%
  ggplot(aes(y = Clinical_Referral)) +
  geom_bar(aes(fill = heardAboutInititative)) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title = element_blank()
  ) +
  # scale_y_discrete(drop = T) +
  # scale_y_discrete(labels = scales::label_wrap(10)) +
  scale_x_continuous( limits = c(0, ylim), breaks = seq(0, ylim, by = 10) ) +
  guides(fill = guide_legend(nrow = 3)) + 
  scale_fill_manual(name = "heardAboutInititative", values = myColours)


### only eligable ##########

comm_df_elig <- dat %>%
  filter(elig_YN == TRUE) %>%
  dplyr::select(Community_Referral, heardAboutInititative) %>%
  mutate(
    heardAboutInititative = recode(
      heardAboutInititative,
      "9 Other (please specify in next column)" = "Other",
      "1 Brochure/flyer/mailer" = "Brochure/flyer/mailer",
      "2 Newspaper/radio/TV" = "Newspaper/radio/TV",
      "3 Bus ad/billboard/cinema ad" = "Bus ad/billboard/cinema",
      "4 website/social media" = "Website/social media",
      "5 Infoline/211/988" = "Infoline/211/988",
      "6 Presentation" = "Presentation",
      "7 Provider/clinician/agency" = "Provider/clinician/agency",
      "8 Unknown/cannot remember"	= "Unknown"
    )
  ) %>%
  mutate(
    Community_Referral = recode(
      Community_Referral,
      "1 - Patient" = "Patient",
      "2 - Family" = "Family",
      "3 - Police" = "Police",
      "4 - Teacher or guidance counselor" = "Teacher or guidance counselor",
      "5 - Other (community)" = "Other"
    )
  ) %>%
  filter(!Community_Referral == "NA") %>%
  filter(!heardAboutInititative == "NA") %>%
  group_by(Community_Referral) %>%
  reframe(Community_Referral,
          n = n(),
          heardAboutInititative = heardAboutInititative) %>%
  group_by(Community_Referral) #%>%


clin_df_elig <- dat %>%
  filter(elig_YN == TRUE) %>%
  dplyr::select(Clinical_Referral, heardAboutInititative) %>%
  mutate(
    heardAboutInititative = recode(
      heardAboutInititative,
      "9 Other (please specify in next column)" = "Other",
      "1 Brochure/flyer/mailer" = "Brochure/flyer/mailer",
      "2 Newspaper/radio/TV" = "Newspaper/radio/TV",
      "3 Bus ad/billboard/cinema ad" = "Bus ad/billboard/cinema",
      "4 website/social media" = "Website/social media",
      "5 Infoline/211/988" = "Infoline/211/988",
      "6 Presentation" = "Presentation",
      "7 Provider/clinician/agency" = "Provider/clinician/agency",
      "8 Unknown/cannot remember"	= "Unknown"
    )
  ) %>%
  mutate(
    Clinical_Referral = recode(
      Clinical_Referral,
      "1 - Emergency department" = "Emergency department",
      "2 - Psychiatric inpatient"	= "Psychiatric inpatient",
      "3 - Intensive outpatient"	= "Intensive outpatient",
      "4 - Primary care provider" = "Primary care provider",
      "5 - Outpatient mental health"	=
        "Outpatient mental health",
      " 6 - Acute evaluation" = "Acute evaluation",
      "7 - mobile evaluation"	= "Mobile evaluation",
      "8 - Other mental health" = "Other mental health"	,
      "9 - other medical provider" = "Other medical provider",
      "10 - Other (clinical)" = "Other"
    )
  ) %>%
  filter(!Clinical_Referral == "NA") %>%
  filter(!heardAboutInititative == "NA") %>%
  group_by(Clinical_Referral) %>%
  reframe(Clinical_Referral,
          n = n(),
          heardAboutInititative = heardAboutInititative) %>%
  group_by(Clinical_Referral)


# Define the common order for 'heardAboutInititative'
common_levels <- c("Brochure/flyer/mailer",
                   "Newspaper/radio/TV",
                   "Bus ad/billboard/cinema",
                   "Website/social media",
                   "Infoline/211/988",
                   "Presentation",
                   "Provider/clinician/agency",
                   "Unknown", "Other") 

all_categories <- data.frame(heardAboutInititative = common_levels)

# Set the same factor levels in both datasets
comm_df_elig$heardAboutInititative <- factor(comm_df_elig$heardAboutInititative, levels = common_levels)
clin_df_elig$heardAboutInititative <- factor(clin_df_elig$heardAboutInititative, levels = common_levels)


# 2) Make plots

referrersCommunityPlot_elig <- comm_df_elig %>%
  ggplot(aes(y = Community_Referral)) +
  geom_bar(aes(fill = heardAboutInititative)) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title = element_blank()
  ) +
  # scale_x_discrete(drop = FALSE) +
  scale_y_discrete(labels = scales::label_wrap(10)) +
  scale_x_continuous( limits = c(0, ylim), breaks = seq(0, ylim, by = 10) ) +
  guides(fill = guide_legend(nrow = 3)) + 
  scale_fill_manual(name = "heardAboutInititative", values = myColours)


referrersClinicalPlot_elig <- clin_df_elig %>%
  ggplot(aes(y = Clinical_Referral)) +
  geom_bar(aes(fill = heardAboutInititative)) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title = element_blank()
  ) +
  # scale_y_discrete(drop = T) +
  # scale_y_discrete(labels = scales::label_wrap(10)) +
  scale_x_continuous( limits = c(0, ylim), breaks = seq(0, ylim, by = 10) ) +
  guides(fill = guide_legend(nrow = 3)) + 
  scale_fill_manual(name = "heardAboutInititative", values = myColours)





# combine plots ####
# plot1 = referrersCommunityPlot + ggtitle("(a) Community referral nodes") +
#   theme(plot.title = element_text(size = 10, hjust = 0, lineheight = 1.2) )
# 
# plot2 = referrersClinicalPlot + ggtitle("(b) Clinical referral nodes") +
#   theme(plot.title = element_text(size = 10, hjust = 0, lineheight = 1.2) )
# 
# plot3 = referrersCommunityPlot_elig + 
#   ggtitle("(c) Community referral nodes among eligible people") +
#   theme(plot.title = element_text(size = 10, hjust = 0, lineheight = 1.2) )
# 
# plot4 = referrersClinicalPlot_elig + 
#   ggtitle("(d) Clinical referral nodes among eligible people") +
#   theme(plot.title = element_text(size = 10, hjust = 0, lineheight = 1.2) )




legend_labels <- names(myColours)
legend_colors <- unname(myColours)

custom_legend <- ggplot() +
  geom_rect(aes(
    xmin = 0, xmax = 0.8, 
    ymin = seq_along(legend_labels) - 0.4, 
    ymax = seq_along(legend_labels) + 0.4,
    fill = factor(seq_along(legend_labels))
  ), show.legend = FALSE) +
  geom_text(aes( x = 1.1, y = seq_along(legend_labels), label = legend_labels), 
            hjust = 0, size = 3) +
  geom_text(aes( x = 0, y = 10, label = "Heard About Inititative"), 
            hjust = 0, size = 4) +
  scale_fill_manual(values = legend_colors) +
  theme_void() +
  theme(legend.position = "none") +
  coord_fixed(ratio = 1, clip = "off") +
  xlim(0, 4) 


# Combine_plots <- (plot1 | plot2) / (plot3 | plot4) +
#   plot_layout(guides = "collect") & theme(legend.position = "none")
# 
# Combine_plots_legend <- plot_grid(Combine_plots, custom_legend, ncol = 2, 
#             rel_heights = c(6, 0.1), rel_widths = c(4, 1))


plot1 = plot_grid( referrersCommunityPlot + theme(legend.position = "none"),
                   custom_legend, ncol = 2, 
                   rel_heights = c(6, 0.1), rel_widths = c(3, 1))
plot2 = plot_grid( referrersClinicalPlot + theme(legend.position = "none"),
                   custom_legend, ncol = 2, 
                   rel_heights = c(6, 0.1), rel_widths = c(3, 1))
plot3 = plot_grid( referrersCommunityPlot_elig + theme(legend.position = "none"),
                   custom_legend, ncol = 2, 
                   rel_heights = c(6, 0.1), rel_widths = c(3, 1))
plot4 = plot_grid( referrersClinicalPlot_elig + theme(legend.position = "none"),
                   custom_legend, ncol = 2, 
                   rel_heights = c(6, 0.1), rel_widths = c(3, 1))







