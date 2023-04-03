library(dplyr)

# Load data
load("~/CRP_enrichment/data/thldata.rdata")
H2000 <- thldata$h2000

# Remove leftovers
rm(thldata)

# Rename
H2000 <- rename(H2000, Ferritin = FERRITIINI, Hemoglobin = B_Hb)

# Apply desired filters
## Women | Menstruating
subgroup <- H2000 %>%
    filter(SP2 == 2) %>% # sex: female
    filter(BD03 == 1 | BD03 == 2) %>% # menstruation: regular or irregular
    filter(IKA2 >= 18 & IKA2 < 55) # age over 18, under 55

## Women | Non-menstruating
# subgroup <- H2000 %>%
#     filter(SP2 == 2) %>% # sex: female
#     filter(BD03 == 3) %>% # menstruation: no
#     filter(IKA2 >= 55) # age over 55

## Women | Non-menstruating
# subgroup <- H2000 %>%
#     filter(SP2 == 1) %>% # sex: male
#     filter(IKA2 >= 18) # age over 18

# Apply additional filters to filtered subgroup, e.g. health status
subgroup_filtered <- subgroup  %>%
    filter(BMII_PAINO.x >= 50 & BMII_PAINO.x <= 200) %>% # Filter away people <50kg and >200kg
    filter(IKA2 >= 20 & IKA2 <= 49) %>% # Filter away too young and too old
    filter(BD07 == 0) %>% # filter out pregnant women
    filter(BA08 == 0) %>% # filter out people with heart attacks
    filter(BA09 == 0) %>% # filter out people with angina
    filter(BA10 == 0) %>% # cardiac insufficiency / heart failure
    filter(!(BA26 == 1 & ATC_A10A == 1)) %>% # filter out people who are diabetic AND use insulin
    filter(BA01 < 4) %>% # filter out "Bad" or "Very bad" SRH
    select(Ferritin, Hemoglobin) %>%
    na.omit()

# Save
saveRDS(subgroup_filtered, file = "~/who_ferritin_comment/data/h2000_menstr_women_apparently_healthy.rds")
