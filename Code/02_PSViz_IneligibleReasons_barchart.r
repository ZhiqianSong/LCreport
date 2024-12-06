



col_neutral <- "#c7cdd1"
col_like <- "#829cb2"
col_like_alot <- "#3e6487"

theme_set(theme_minimal())

#####################################################
# 1) Make Dataframe
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
  #mutate(DateConsent=as.numeric(DateConsent)) %>%
  mutate(DateConsent = lubridate::as_date(DateConsent)) %>%
  rename(DateAdmittedLMHA = "IF YES, PLEASE INDICATE THE DATE OF ADMISSION") %>%
  mutate(DateAdmittedLMHA = lubridate::as_date(DateAdmittedLMHA)) %>%
  mutate(LMHA = as.factor(`NAME OF LMHA THE PATIENT HAS BEEN REFERRED TO`))

pie_Ineligibility <- dat %>%
  mutate(
    eligibility = recode(
      eligibility,
      "1 Eligible - met all inclusion criteria (including consent)" = "Eligible",
      "2 Ineligible - Unable to provide consent/assent, or was not able to understand the nature of the study" = "Unable to provide consent",
      "3 Ineligible - Patient does not live in Connecticut" = "Outside Connecticut",
      "4 Ineligible - Does not meet psychosis threhold according to SIPS" = "Does not meet psychosis threshold",
      "5 Ineligible - It is over 3yrs since psychosis onset" = "Over 3yrs since psychosis onset",
      "6 Ineligible - Diagnosis of Affective psychosis or psychosis secondary to substance use, or medical illness" = "Psychosis secondary to other diagnosis",
      "7 Ineligible - Unable to communicate in English" = "Unable to communicate in English",
      "8 Ineligible - IQ less than 70 or DDS eligible (autism, developmental disorder)" = "DDS eligible",
      "9 Ineligible - Unstable serios medical issue" = "Unstable/serious medical issue",
      "10 - Eligible but refused to participate in the LHN initiative (did not consent)" = "Eligible but refused",
      "11 - Ineligible due to age" = "Ineligible due to age",
      "12 - ELIGIBLE BUT CONSENT UNABLE TO BE OBTAINED" = "Eligible but unable to obtain consent",
      "13 - Ineligible: already in satisfactory care" = "Already in satisfactory care"
    )
  )  %>%
  group_by(`SCREEN NO.`) %>%
  filter(!eligibility == "Eligible") %>%
  mutate(totalP = nrow(.)) %>%
  group_by(eligibility, totalP = totalP) %>%
  summarise(N = n()) %>%
  mutate(percent = (N / totalP) * 100) %>%
  mutate(percent = round(percent, 0)) %>%
  mutate(bar_text = paste0(percent, "%")) %>%
  ungroup()

odered <- pie_Ineligibility %>%
  arrange(percent) %>%
  pull(eligibility)

df_odered <- pie_Ineligibility %>%
  mutate(allVs = factor(eligibility, levels = odered))

#####################################################
# Make plot
#####################################################

plot <- df_odered %>%
  ggplot(aes(y = allVs, x = percent, fill = percent)) +
  geom_col(width = 0.8,  position = position_stack(reverse = TRUE)) +
  theme(plot.margin = unit(rep(0.7, 4), "cm")) +
  scale_y_discrete(expand = c(0, 0))

gg2_bar <- plot + geom_text(
  aes(label = bar_text),
  position = position_stack(vjust = 0.5, reverse = TRUE),
  colour = "white",
  size = 3
)

gg3_bar <- gg2_bar + scale_fill_gradient2(
  name = waiver(),
  low = col_neutral,
  mid = col_like,
  high = col_like_alot,
  midpoint = 0
) +
  theme(
    axis.text = element_text(color = "black", size = 8),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none",
    legend.title = element_blank()
  )


