





    
    dat <- temp %>%     
      filter(!is.na(`SCREEN NO.`)) %>%
      
      mutate(Inqdate = `INQUIRY DATE (VOICEMAIL LEFT)`) %>%
      mutate(Inqdate=lubridate::as_date(Inqdate)) %>%
      mutate(Inqdate_month = month(Inqdate)) %>%
      mutate(Inqdate_day = mday(Inqdate)) %>%
      mutate(DateConsent = `DATE OF CONSENT`) %>%
      mutate(DateConsent = lubridate::as_date(DateConsent)) %>%
      mutate(DateAdmittedLMHA = `IF YES, PLEASE INDICATE THE DATE OF ADMISSION`) %>%
      mutate(DateAdmittedLMHA = lubridate::as_date(DateAdmittedLMHA)) %>%
      mutate(Inqstat = as.factor(`INQUIRY STATUS`)) %>%
      mutate(IngLine = `Indicate whether inquiry was made via Learning collaborative phone line or other`) %>%
      mutate(ReasonNoEnroll3mo = `IF NO, PLEASE SELECT THE CODE THAT BEST DESCRIBES THE REASON`) %>%
      mutate(heardAboutInititative = as.factor(`SELECT CODE THAT BEST DESCRIBES HOW THEY HEARD ABOUT THIS INITIATIVE`)) %>%
      mutate(REGION = recode(REGION,"STEP"="2")) %>%
      mutate(REGION = as.factor(REGION)) %>%
      mutate(Community_Referral = as.factor(`SELECT THE COMMUNITY CODE THAT BEST CAPTURES THE REFERRER PROFILE`)) %>%
      mutate(Clinical_Referral = as.factor(`SELECT THE CLINICAL CODE THAT BEST CAPTURES THE REFERRER PROFILE`)) %>%    
      mutate(eligibility = as.factor(`PLEASE SELECT THE CODE THAT BEST SUITS THE ELIGIBILITY STATUS OF THIS PARTICIPANT`)) %>%
      mutate(elig_YN = case_when(eligibility == "1 Eligible - met all inclusion criteria (including consent)" ~ TRUE,   TRUE ~ FALSE)) %>%
      mutate(Over18 = AGE>=18) %>%
      mutate(FU_admitted = as.factor(`HAS THE PATIENT BEEN SUCCESFULLY ADMITTED INTO TREATMENT BY THE 3 MOS MARK?`)) %>%
      mutate(FU_admitted = as.character(FU_admitted)) %>%
      mutate(LMHA=as.factor(`NAME OF LMHA THE PATIENT HAS BEEN REFERRED TO`)) 

#############################
#Numbers for text informatics   
##############################  
    
    # No. of inquries made overall - for text
    tot_inq <- dat %>%
      # group_by("SCREEN NO.") %>%
      nrow()
    
    # monthly queries
    dat_monthlylyqueries <- dat %>%
      mutate(Inqmonth = month(Inqdate, label = TRUE)) %>%
      group_by(Inqmonth) %>%
      summarise(n = n()) %>%
      drop_na() %>%
      mutate(cumulativeN = cumsum(n))
    
    monthlyqueries <- dat_monthlylyqueries %>%  tail(1)
    
    # WEEKLY queries
    dat_weeklyqueries <- dat %>%
      mutate(Inqweek = floor_date(Inqdate, "week")) %>%
      group_by(Inqweek, .drop = FALSE) %>%
      summarise(n = n()) %>%
      mutate(cumsum = cumsum(n), REGION = "All")
    
    dat_weeklyqueries[nrow(dat_weeklyqueries) + 1, ] <- 
      list(date("2024-01-26 UTC"), 0, 0, "All")
    
    numWeeks <- nrow(dat_weeklyqueries)
    # weeklyquerychange <-
    #   dat_weeklyqueries$n[numWeeks] - dat_weeklyqueries$n[numWeeks - 1]
    
    Other_line <- dat %>%
      filter(IngLine == "OTHER") %>%
      select(IngLine) %>% 
      nrow()

    LC_line <- dat %>%
      filter(!IngLine == "OTHER") %>%
      select(IngLine) %>%
      nrow()
      
    
    ########################
    #quarterly comparisons
    ########################
    Quarter1total <-  dat %>%
      filter(between(Inqdate, as.Date("2024-01-31"), as.Date("2024-05-01"))) %>%
      nrow()
    
    Quarter2total <- dat %>%
      filter(between(Inqdate, as.Date("2024-05-01"), as.Date("2024-08-01"))) %>%
      nrow()
    
    Quarter3total <- dat %>%
      filter(between(Inqdate, as.Date("2024-08-01"), as.Date("2024-11-01"))) %>%
      nrow()
    
    Quarter4total <- dat %>%
      filter(between(Inqdate, as.Date("2024-11-01"), as.Date("2025-02-01"))) %>%
      nrow()
    
    percentageChange_raw1 <-
      ((Quarter2total - Quarter1total) / Quarter1total) * 100
    percentageChange_raw2 <-
      ((Quarter3total - Quarter2total) / Quarter1total) * 100
    percentageChange_raw3 <-
      ((Quarter4total - Quarter3total) / Quarter1total) * 100
    
    percentageChange1 <-
      formatC(as.numeric(as.character(round( percentageChange_raw1, 0 ))),
              digits = 0, format = "f")
    percentageChange2 <-
      formatC(as.numeric(as.character(round( percentageChange_raw2, 0 ))),
              digits = 0, format = "f")
    percentageChange3 <-
      formatC(as.numeric(as.character(round( percentageChange_raw3, 0 ))),
              digits = 0, format = "f")
    
    Eligbile_Text <- dat %>%
      filter(elig_YN == TRUE) %>%
      group_by(eligibility) %>%
      summarise(n = n())
    
    Ineligible_Text <- dat %>%
      filter(elig_YN == FALSE) %>%
      group_by(eligibility) %>%
      summarise(n = n())
    
    PendingActivecases_text <- dat %>%
      filter(Inqstat %in% "Active") %>%
      group_by(Inqstat) %>%
      summarise(n = n())
    
    PendingInactivecases_text <- dat %>%
      filter(Inqstat %in% "Inactive") %>%
      group_by(Inqstat) %>%
      summarise(n = n())
    
    Under18_Text <- dat %>%
      filter(elig_YN == TRUE) %>%
      group_by(Over18) %>%
      filter(Over18 == FALSE) %>%
      summarise(n = n())
    
    number_admitted <- dat %>%
      filter(elig_YN == "TRUE") %>%
      mutate(waitTime_inq2admit = DateAdmittedLMHA - DateConsent) %>%
      mutate(
        FU_admitted_numeric = case_when(
          FU_admitted == "AWAITING CONFIRMATION" ~ 0,
          FU_admitted == "YES" ~ 1,
          FU_admitted == "NO" ~ 2,
          TRUE ~ 0
        )
      ) %>%
      filter(FU_admitted_numeric == 1) %>% nrow()
    
    number_not_admitted <- dat %>%
      filter(elig_YN == "TRUE") %>%
      mutate(waitTime_inq2admit = DateAdmittedLMHA - DateConsent) %>%
      mutate(
        FU_admitted_numeric = case_when(
          FU_admitted == "AWAITING CONFIRMATION" ~ 0,
          FU_admitted == "YES" ~ 1,
          FU_admitted == "NO" ~ 2,
          TRUE ~ 0
        )
      ) %>%
      filter(FU_admitted_numeric == 2) %>% nrow()
    
    number_awaiting_admission <- dat %>%
      filter(elig_YN == "TRUE") %>%
      mutate(waitTime_inq2admit = DateAdmittedLMHA - DateConsent) %>%
      mutate(
        FU_admitted_numeric = case_when(
          FU_admitted == "AWAITING CONFIRMATION" ~ 0,
          FU_admitted == "YES" ~ 1,
          FU_admitted == "NO" ~ 2,
          TRUE ~ 0
        )
      ) %>%
      filter(FU_admitted_numeric == 0) %>% nrow()
    
    statewideAverage <- dat %>%
      filter(elig_YN == "TRUE") %>%
      mutate(waitTime_inq2admit = DateAdmittedLMHA - DateConsent) %>%
      filter(!FU_admitted == "NO") %>%
      mutate(
        FU_admitted_numeric = case_when(
          FU_admitted == "AWAITING CONFIRMATION" ~ 0,
          FU_admitted == "YES" ~ 1,
          FU_admitted == "NO" ~ 2,
          TRUE ~ 0
        )
      ) %>%
      filter(FU_admitted_numeric == 1) %>%
      select(LMHA, REGION, FU_admitted_numeric, waitTime_inq2admit) %>%
      summarise(
        `Patients admitted` = sum(FU_admitted_numeric),
        `Average wait time` = mean(waitTime_inq2admit),
        `Median wait time` = median(waitTime_inq2admit)
      )
    