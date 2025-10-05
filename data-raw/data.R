library(conflictoR)
library(poorman)

# Set seed for reproducibility
set.seed(123)

# Get real data (you need to run this with internet connection)
state_conflicts <- cl_get_data("ucdpprioconflict", "24.1")
nonstate_conflicts <- cl_get_data("nonstate", "24.1")
onesided_conflicts <- cl_get_data("onesided", "24.1")


# Create inst/extdata directory if it doesn't exist
if (!dir.exists("inst/extdata")) {
  dir.create("inst/extdata", recursive = TRUE)
}

# Save all sample data in one .rda file for the vignette
save(state_conflicts, 
     nonstate_conflicts, 
     onesided_conflicts,
     file = "inst/extdata/vignette_sample_data.rda")
