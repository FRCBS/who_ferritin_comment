# Function for data processing (rename variables etc.)
# For situations where already have the subsets we need
# (so we aren't running 0_LoadData.qmd)
# but they aren't conforming to the standards of 2_Analysis.qmd

# These steps are taken straight from 0_LoadData.qmd
# I've just functionalized them.

conform <- function(data) {
  
  # Rename columns to conform
  data <- data %>% 
    rename(EINnummer = donationID,
           KeyID = donor,
           Geslacht = gender,
           Geboortedatum = dob,
           Donatiedatum = date,
           Ferritine = Ferritin,
           Donatiesoortcode = donat_phleb,
           Lengte = height,
           Gewicht = weight)
  
  # Transform variables for conformity
  data <- data %>% 
    mutate(Hb = Hb * 0.06195) %>%  # Our Hb measurements are in g/L. This conversion to mmol/L comes from Google + calculator!
    mutate(Geslacht = as.factor(ifelse(Geslacht == "Men", "M", "F"))) %>% 
    mutate(Donatiesoortcode = ifelse(Donatiesoortcode == "K", "Volbloed", NA)) %>% 
    filter(Donatiesoortcode == "Volbloed") %>% mutate(Donatiesoortcode = as.factor(Donatiesoortcode))
  
  # Create numdon, prev_fer, and RefDonDate
  data <- data %>% 
    arrange(KeyID, as.Date(Donatiedatum, format = "%Y-%m-%d")) %>% 
    mutate(numdon = seq_along(KeyID)) %>% 
    group_by(KeyID) %>% 
    arrange(KeyID, by_group = T) %>% 
    mutate(prev_fer = lag(Ferritine, order_by = KeyID), RefDonDate = min(Donatiedatum), prev_DonDate = lag(Donatiedatum, order_by = KeyID))
  
  # Create RefHb
  ReferenceHb <- data %>%
    filter(numdon == 1 | numdon == 2) %>%
    group_by(KeyID) %>% mutate(RefHb = mean(Hb)) %>%
    ungroup() %>%
    filter(numdon == 1 & !is.na(RefHb)) %>%
    dplyr::select(c(KeyID, RefHb))
  
  data_2 <- merge(data, ReferenceHb, by = "KeyID")
  
  # Create numfer
  data_3 <- data_2 %>% filter(!is.na(Ferritine)) 
  numfer <- data_3 %>% 
    arrange(KeyID, as.Date(data_3$Donatiedatum, format = "%Y-%m-%d")) %>% 
    mutate(numfer = with(data_3, ave(rep(1, nrow(data_3)), KeyID, FUN = seq_along))) %>% 
    select(c(EINnummer, numfer))
  
  data <- merge(data_3, numfer, by = "EINnummer")
  
  # Adjust length, width
  # Create BMI, Age, LogFer, Dhb, premenopausal
  data_1 <- data%>%
    filter((Donatiesoortcode == "Nieuwe donorkeuring" | Donatiesoortcode == "Volbloed") & !is.na(Hb) & !is.na(Ferritine) & Hb > 0) %>%
    mutate(Lengte = replace(Lengte, Lengte < 130, NA), 
           Gewicht = replace(Gewicht, Gewicht > 150 | Gewicht < 51, NA)) %>%
    mutate(BMI = Gewicht / ((Lengte/100)*(Lengte/100)), 
           Leeftijd = round(as.numeric((Donatiedatum-Geboortedatum)/365.25), 0), 
           LogFer = log10(Ferritine), 
           DHb = Hb - RefHb,
           premenopausal = case_when(Geslacht == "F" & Leeftijd <=50 ~1, Geslacht == "F" & Leeftijd >50 ~0))%>%
    filter(!is.na(BMI))
  
  data <- data_1
  
  return(data)
}