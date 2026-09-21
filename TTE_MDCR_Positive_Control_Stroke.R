stroke_ICD9_prefix <-
  c(
    "430",
    "431",
    "436",
    "43301",
    "43311",
    "43321",
    "43331",
    "43381",
    "43391",
    "43401",
    "43411",
    "43491"
  )

stroke_ICD10_prefix <-
  c(
    "I60",
    "I61",
    "I63",
    "I64"
  )

###############################################################
## REVIEWER QUICK 8
## POSITIVE CONTROL: STROKE
###############################################################

stroke_diagnoses <- all_diagnoses %>%
  mutate(
    diagnosis_code_clean =
        diagnosis_code
  ) %>%
  filter(
      diagnosis_code_clean
     %in%
      c(
        stroke_ICD9_prefix,
        stroke_ICD10_prefix
      )
  ) %>%
  semi_join(
    baseline_characteristics %>%
      select(
        ENROLID
      ),
    by = "ENROLID"
  )


stroke_first_date <- stroke_diagnoses %>%
  group_by(
    ENROLID
  ) %>%
  summarise(
    stroke_date =
      min(
        diagnosis_date
      ),
    .groups = "drop"
  )


stroke_outcome <- baseline_characteristics %>%
  left_join(
    stroke_first_date,
    by = "ENROLID"
  ) %>%
  left_join(
    switch_dates,
    by = "ENROLID"
  ) %>%
  mutate(
    risk_start_date =
      index_date +
      days(
        EXPOSURE_WINDOW_DAYS
      )
  ) %>%
  
#############################################################
## Exclude stroke before risk window
#############################################################

filter(
  is.na(stroke_date) |
    stroke_date >=
    risk_start_date
) %>%
  
  mutate(
    censor_date =
      pmin(
        switch_date,
        last_enroll,
        study_end_date,
        na.rm = TRUE
      ),
    
    event_stroke =
      ifelse(
        !is.na(stroke_date) &
          stroke_date >= risk_start_date &
          stroke_date <= censor_date,
        1,
        0
      ),
    
    end_date =
      if_else(
        event_stroke == 1,
        stroke_date,
        censor_date
      ),
    
    followup_time =
      as.numeric(
        end_date -
          risk_start_date
      )
  ) %>%
  filter(
    followup_time > 0
  )

