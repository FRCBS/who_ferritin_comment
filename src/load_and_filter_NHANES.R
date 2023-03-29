# Load NHANES data

library(nhanesA)

# Do this only if the data doesn't yet exist on our drive
if (!file.exists("~/who_ferritin_comment/data/NHANES_variables_filtered.rds")) {

    # "-- using cross-sectional NHANES data from 2003-06, 2007-10, 2015-18"
    # I think this maps to  C, D, E, F and I, J for DEMO (demographic) and RHQ (reproductive/pregnancy)
    # All the others map to D, E, F and I, J, but not to C, because 2003-2004 was somehow different
    # Additionally, CRP doesn't have I and J, for those years the code is HSCRP
    # We'll capture everything mappable and then treat 2003-2004 separately
    suffix_complete <- c("C", "D", "E", "F", "I", "J")
    suffix_incomplete <- c("D", "E", "F", "I", "J")
    suffix_HSCRP <- c("I", "J")
    suffix_CRP <- c("D", "E", "F")
    demographics_complete <- bind_rows(lapply(suffix_complete, function(s) nhanes(paste0("DEMO_", s))))
    pregnancy_complete <- bind_rows(lapply(suffix_complete, function(s) nhanes(paste0("RHQ_", s))))
    ferritin_incomplete <- bind_rows(lapply(suffix_incomplete, function(s) nhanes(paste0("FERTIN_", s))))
    sTfR_incomplete <- bind_rows(lapply(suffix_incomplete, function(s) nhanes(paste0("TFR_", s))))
    CRP_incomplete <- bind_rows(lapply(suffix_CRP, function(s) nhanes(paste0("CRP_", s))))
    HSCRP_incomplete <- bind_rows(lapply(suffix_HSCRP, function(s) nhanes(paste0("HSCRP_", s))))
    blood_incomplete <- bind_rows(lapply(suffix_incomplete, function(s) nhanes(paste0("CBC_", s))))
    biopro_incomplete <- bind_rows(lapply(suffix_incomplete, function(s) nhanes(paste0("BIOPRO_", s))))

    # Select only variables of interest
    # SEQN: identifier
    # RIDAGEYR: Age in years
    # RIAGENDR: Gender, 1 Male, 2 Female, . missing
    # RIDEXPRG: Pregnancy status at Exam, 1 Yes, 2 No, 3, Unsure, . Missing
    # LBXFER: Ferritin ng/mL
    # LBXTFR: sTfR mg/L
    # LBXWBCSI: White blood cell count, 1000 cells/uL
    # LBXHGB: Hemoglobin, g/dL
    # LBXCRP: CRP mg/dL
    # LBXSASSI: Aspartate aminotransferase (AST) U/L
    # LBXSATSI: Alanine aminotransferase (ALT) U/L

    demo <- demographics_complete %>%
        select(SEQN, RIDAGEYR, RIAGENDR, RIDEXPRG)
    preg <- pregnancy_complete %>%
        select(SEQN, RHD143)
    fer_incomplt <- ferritin_incomplete %>%
        select(SEQN, LBXFER)
    stfr_incomplt <- sTfR_incomplete %>%
        select(SEQN, LBXTFR)
    blood_incomplt <- blood_incomplete %>%
        select(SEQN, LBXWBCSI, LBXHGB)
    CRP_incomplt <- CRP_incomplete %>%
        select(SEQN, LBXCRP)
    HSCRP_incomplt <- HSCRP_incomplete %>%
        select(SEQN, LBXHSCRP) %>%
        rename(LBXCRP = LBXHSCRP) # To conform with other CRPs
    biopro_incomplt <- biopro_incomplete %>%
        select(SEQN, LBXSASSI, LBXSATSI)

    # For the 2003-2004 data we have the following tables:
    # ferritin & stfr: L06TFR_C
    # biochem profile: L40_C
    # blood: L25_C
    # CRP: L11_C
    blood_C <- nhanes("L25_C") %>% select(SEQN, LBXWBCSI, LBXHGB)
    biopro_C <- nhanes("L40_C") %>% select(SEQN, LBXSASSI, LBXSATSI)
    CRP_C <- nhanes("L11_C") %>% select(SEQN, LBXCRP)
    fer_tfr_C <- nhanes("L06TFR_C") %>% select(SEQN, LBDFER, LBXTFR) %>% rename(LBXFER = LBDFER)
    fer_C <- fer_tfr_C[, c("SEQN", "LBXFER")]
    stfr_C <- fer_tfr_C[, c("SEQN", "LBXTFR")]

    # Make incomplete variables whole
    fer <- bind_rows(fer_C, fer_incomplt)
    stfr <- bind_rows(stfr_C, stfr_incomplt)
    blood <- bind_rows(blood_C, blood_incomplt)
    CRP <- bind_rows(CRP_C, CRP_incomplt, HSCRP_incomplt)
    biopro <- bind_rows(biopro_C, biopro_incomplt)

    # Join everything by SEQN
    unfiltered_variables <- demo %>%
        left_join(preg, by = "SEQN") %>%
        left_join(fer, by = "SEQN") %>%
        left_join(stfr, by = "SEQN") %>%
        left_join(blood, by = "SEQN") %>%
        left_join(CRP, by = "SEQN") %>%
        left_join(biopro, by = "SEQN")

    # Rename columns for readability
    unfiltered_renamed <- unfiltered_variables %>%
      rename(
        Age = RIDAGEYR,
        Gender = RIAGENDR,
        Pregnant = RIDEXPRG,
        Pregnant2 = RHD143,
        Ferritin = LBXFER,
        sTfR = LBXTFR,
        WBC = LBXWBCSI,
        Hemoglobin = LBXHGB,
        CRP = LBXCRP,
        AST = LBXSASSI,
        ALT = LBXSATSI
      )

    # Save full set
    saveRDS(unfiltered_renamed, "~/who_ferritin_comment/data/NHANES_variables_unfiltered.rds")

    # Filter to match Mei & Addo
    filtered_data <- unfiltered_renamed %>%
      filter(Age >= 15 & Age <= 49,
             Gender == 2,
             Pregnant == 2 | Pregnant2 == 2,
             !is.na(Ferritin) & !is.na(sTfR) & !is.na(Hemoglobin) & !is.na(WBC) & !is.na(CRP) & !is.na(AST) & !is.na(ALT), # Exclude missing measurements
             WBC <= 10, # Exclude individuals with infection (WBC > 10 x 10^9/L)
             ALT <= 70,
             AST <= 70 # Exclude if possible liver disease
             #CRP <= 5 # Exclude women with inflammation
        )

    # Save filtered
    saveRDS(filtered_data, "~/who_ferritin_comment/data/NHANES_variables_filtered.rds")

} else {
    data <- readRDS("~/who_ferritin_comment/data/NHANES_variables_filtered.rds")
    }

# NB! Our N does not match with Mei & Addo
# We only get 4,588 non-pregnant women. Mei & Addo have 7,498. Why?
# TODO: Figure out!
