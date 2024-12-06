


#####################################################
# 1) Make Dataframe
#####################################################
dat <- temp %>%
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


patients_notadmitted_table <- dat %>%
  filter(elig_YN == "TRUE") %>%
  mutate(
    FU_admitted_numeric = case_when(
      FU_admitted == "AWAITING CONFIRMATION" ~ 0,
      FU_admitted == "YES" ~ 1,
      FU_admitted == "NO" ~ 2,
      TRUE ~ 0
    )
  ) %>%
  filter(FU_admitted_numeric == 2) %>%
  mutate(
    ReasonNoEnroll3mo = recode(
      ReasonNoEnroll3mo,
      "1" = "Subject doesn't like the LMHA",
      "2" = "Subject decided on another providor",
      "3" = "Subject doesn't like program location",
      "4" = "Subject can't get to location",
      "5" = "Subject doesn't want to share information",
      "6" = "Subject finds it too time consuming",
      "7" = "Subject got a job",
      "8" = "Subject went back to school",
      "9" = "Subject does not have time",
      "10" = "Subject believes they don't have a problem",
      "11" = "Subject is doing better now",
      "12" = "Subject believes the program isn't helping",
      "13" = "Subject had psychiatric hospitalisation",
      "14" = "Subject has medical problem",
      "15" = "Subject death - mental illness related",
      "16" = "Subject death - not mental illness related",
      "17" = "Suject's family refused participation",
      "18" = "Subject has intervening family circumstances",
      "19" = "Subject moved",
      "20" = "Incarceration",
      "21" = "Other",
      "22" = "Met SIPS criteria for clinical high risk"
    )
  ) %>%
  select(LMHA, REGION, FU_admitted_numeric, ReasonNoEnroll3mo) %>%
  group_by(LMHA) %>%
  summarise(
    `Region` = REGION,
    `Patients not admitted` = n(),
    `Reason` = ReasonNoEnroll3mo
  )

##need to add in the reason
tablePatientsNotAdmitted <-
  knitr::kable(head(patients_notadmitted_table), format = "markdown")

