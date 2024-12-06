

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


tabLMHArefs_consent_new <- dat %>%
  filter(elig_YN == "TRUE") %>%
  mutate(LMHA = recode(LMHA, "STEP" = "CMHC (STEP)")) %>%
  rename(`Admitted to` = "IF YES AND OTHER, SPECIFIY") %>%
  mutate(`Admitted to` = na_if(`Admitted to`, "NA")) %>%
  mutate(`Admitted to` = recode(`Admitted to`,
                                "Cornell Scott Hill Health Center" =
                                  "CSHC")) %>%
  mutate(`Admitted to` = as.factor(`Admitted to`)) %>%
  rename(`Referred to` = LMHA) %>%
  mutate(
    `Referred to` = recode(
      `Referred to`,
      "Western CT mental health network - waterbury area" =
        "Western CT",
      "BHCare (Shoreline offices)" = "BHCare (Shoreline)"
    )
  ) %>%
  mutate(waitTime_inq2admit = DateAdmittedLMHA - DateConsent) %>%
  mutate(
    FU_admitted_numeric = case_when(
      FU_admitted == "AWAITING CONFIRMATION" ~ 0,
      FU_admitted == "YES" ~ 1,
      FU_admitted == "NO" ~ 2,
      TRUE ~ 0
    )
  ) %>%
  filter(FU_admitted_numeric == 1) %>%
  select(`Referred to`,
         REGION,
         FU_admitted_numeric,
         waitTime_inq2admit,
         `Admitted to`) %>%
  group_by(`Referred to`, `Admitted to`) %>%
  summarise(
    `No. admitted` = sum(FU_admitted_numeric, na.rm=T),
    `Median  wait time (days)` = median(as.numeric(waitTime_inq2admit), na.rm=T),
    `IQR` = IQR(as.numeric(waitTime_inq2admit), na.rm=T)
  ) %>%
  mutate_if(is.numeric, round, 1) %>%
  mutate(`Admitted to` = replace_na(as.character(`Admitted to`), "Matches referral"))

DUPTable <-
  knitr::kable((tabLMHArefs_consent_new), format = "markdown")
