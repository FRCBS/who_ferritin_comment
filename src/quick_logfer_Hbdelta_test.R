library(mcp)
library(dplyr)
library(tidyr)
library(ggplot2)

# Load data on individual donations
load("~/CRP_enrichment/data/r02.fd.bd.all.rdata") # outputs an object called "output" into the environment
donations <- output

# Load FinDonor demographic data
load("~/CRP_enrichment/data/r02ds.donorData.rdata") # outputs an object called "output" into the environment
findonor <- output

# Combine the FinDonor datasets
FinDonor <- left_join(donations, findonor, by = "donor")

# Group and arrange
FinDonor <- FinDonor %>%
    group_by(donor) %>%
    arrange(donor, date) %>%
    drop_na(date)


# Create Hb reference diff column
wRef <- FinDonor %>%
    mutate(reference_delta = Hb - first(Hb)) %>%
    mutate(fer_log10 = log10(Ferritin)) %>%
    select(donor, reference_delta, fer_log10, Gender, QR79) %>%
    slice(-1) %>%
    ungroup

ggplot(wRef %>% filter(Gender == "Men"), aes(x = fer_log10, y = reference_delta)) +
    geom_point(alpha = 0.2) +
    theme_minimal()

wRef_men <- wRef %>% filter(Gender == "Men") %>% select(reference_delta, fer_log10) %>% drop_na()
wRef_women <- wRef %>% filter(Gender == "Women")
wRef_premeno <- wRef %>% filter(Gender == "Women" & QR79 != "no_period")
wRef_postmeno <- wRef %>% filter(Gender == "Women" & QR79 == "no_period")
# Define breakpoint formula
bp_spec <- list(reference_delta ~ fer_log10,
                ~ 0 + fer_log10)

# Fit men
bp_fit_men <- mcp(
    bp_spec,
    data = wRef_men,
    adapt = 10000,
    iter = 10000,
    chains = 4,
    inits = list(cp_1 = 1.45),
    cores = 4
)

# Fit women
bp_fit_women <- mcp(
    bp_spec,
    data = wRef_women,
    adapt = 500,
    iter = 500,
    chains = 4,
    inits = list(cp_1 = 1.45),
    cores = 4
)
# Fit premeno
bp_fit_premeno <- mcp(
    bp_spec,
    data = wRef_premeno,
    adapt = 500,
    iter = 500,
    chains = 4,
    inits = list(cp_1 = 1.45),
    cores = 4
)
# Fit postmeno
bp_fit_postmeno <- mcp(
    bp_spec,
    data = wRef_postmeno,
    adapt = 500,
    iter = 500,
    chains = 4,
    inits = list(cp_1 = 1.45),
    cores = 4
)

# Plot men
plot(bp_fit_men) +
    geom_vline(xintercept = 1.38, linetype = "dashed", size = 1) +
    scale_x_continuous(breaks = seq(0, 3, 0.1), labels = round(10**seq(0, 3, 0.1), 0)) +
    labs(title = "FinDonor | Men",
         subtitle = "Breakpoint estimate: approx. 24 (μg/l)",
         x = "Ferritin (μg/l), log-scale",
         y = "Hb difference to first measurement (g/l)") +
    theme_minimal(17) +
    theme(axis.text.x = element_text(angle = 45))

# Plot women
plot(bp_fit_women) +
    theme_minimal() +
    #geom_vline(xintercept = 1.4, linetype = "dashed", color = "red") +
    scale_x_continuous(breaks = seq(0, 3, 0.1)) +
    labs(title = "FinDonor | Women",
         subtitle = "Breakpoint estimate: ??",
         x = "Ferritin (log10)",
         y = "Hb difference to first measurement")
# Plot premeno
plot(bp_fit_premeno) +
    theme_minimal() +
    #geom_vline(xintercept = 1.4, linetype = "dashed", color = "red") +
    scale_x_continuous(breaks = seq(0, 3, 0.1)) +
    labs(title = "FinDonor | Women premenopausal",
         subtitle = "Breakpoint estimate: ??",
         x = "Ferritin (log10)",
         y = "Hb difference to first measurement")
# Plot women
plot(bp_fit_postmeno) +
    theme_minimal() +
    #geom_vline(xintercept = 1.4, linetype = "dashed", color = "red") +
    scale_x_continuous(breaks = seq(0, 3, 0.1)) +
    labs(title = "FinDonor | Women postmenopausal",
         subtitle = "Breakpoint estimate: ??",
         x = "Ferritin (log10)",
         y = "Hb difference to first measurement")
