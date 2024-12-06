



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

myColours1 = c(
  "1" = depn_insti,
  "2" = col_dislike_alot,
  "3" = col_dislike,
  "4" = col_neutral,
  "5" = col_like,
  "STEP" = col_like_alot,
  "All" = "#000000"
)


#####################################
#Inquiries by week, by region
#####################################
dat_weeklyqueries_region <- dat %>%
  mutate(Inqweek = floor_date(Inqdate, "week")) %>%
  group_by(Inqweek, REGION, .drop = FALSE) %>%
  summarise(n = n()) %>%
  group_by(REGION) %>%
  mutate(cumsum = cumsum(n))

regions <- dat_weeklyqueries_region$REGION %>% unique

missingRows <-
  data.frame(
    Inqweek = rep(lubridate::as_date("2024-01-26 UTC"), length(regions)),
    REGION =
      regions,
    n =
      rep(0, length(regions)),
    cumsum =
      rep(0, length(regions))
  ) %>%
  as_tibble

dat_weeklyqueries_region <-
  rbind(missingRows, dat_weeklyqueries_region)


#####################################
#Inquiries by week, by region PLOT
#####################################
inq_tabxRegion <-  dat %>%
  group_by(REGION) %>%
  mutate(
    REGION = recode(
      REGION,
      "1" = "Region 1",
      "2" = "Region 2",
      "3" = "Region 3",
      "4" = "Region 4",
      "5" = "Region 5",
      "Out of catchment/no location given" = "Location undetermined"
    )
  ) %>%
  rename(Location = "REGION") %>%
  #filter(!REGION=="Out of catchment") %>%
  select(Inqstat) %>%
  group_by(Location) %>%
  drop_na() %>%
  summarise(`No. Inquiries` = n()) %>%
  mutate(Location = as.character(Location))

table1 <- knitr::kable(inq_tabxRegion, 
                       label = 'Table of inquiries by DMHAS Region') 



x <- dat_weeklyqueries_region  %>%
  filter(REGION %in% c(1:5)) %>%
  drop_na() %>%
  mutate(REGION_numeric = as.numeric(REGION))

dat_weeklyqueries <- dat %>%
  mutate(Inqweek = floor_date(Inqdate, "week")) %>%
  group_by(Inqweek, .drop = FALSE) %>%
  summarise(n = n()) %>%
  mutate(cumsum = cumsum(n), REGION = "All")

dat_weeklyqueries[nrow(dat_weeklyqueries) + 1, ] <-
  list(date("2024-01-26 UTC"), 0, 0, "All")

RegionsXY <- x %>%
  group_by(REGION) %>%
  summarize(
    pos = which.max(Inqweek),
    x = Inqweek[pos],
    y = cumsum[pos],
    label = REGION[pos]
  )


AllcumsumXY <- dat_weeklyqueries %>%
  group_by(REGION) %>%
  summarize(
    pos = which.max(Inqweek),
    x = Inqweek[pos],
    y = cumsum[pos],
    label = REGION[pos]
  )

xy_labs <- full_join(RegionsXY, AllcumsumXY)

inquiryRunchart <- dat_weeklyqueries_region %>%
  filter(REGION %in% c(1:5)) %>%
  ggplot() +
  geom_line(aes(x = Inqweek, y = cumsum, color = REGION)) +
  geom_line(data = dat_weeklyqueries, aes(x = Inqweek, y = cumsum, colour =
                                            REGION)) +
  theme_classic() +
  # theme(legend.text = element_blank()) +
  theme(legend.position = "right") +
  ylab("Cumulative Inquiries") +
  xlab(" ") +
  scale_color_manual(name = "Region", values = myColours1) +
  geom_label_repel(
    data = xy_labs,
    aes(x = x, y = y + 1, label = label),
    label.size = 0.03,
    nudge_x = 0.0,
    inherit.aes = F,
    na.rm = FALSE
  ) 




