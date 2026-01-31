# Rebuild snt_var_tree data object from YAML
# This script regenerates the snt_var_tree.rda file from var_tree.yml

library(yaml)

# Read the YAML file from the development directory
snt_var_tree <- yaml::read_yaml(
  file.path("inst", "extdata", "var_tree.yml")
)

# Save to data directory
usethis::use_data(snt_var_tree, overwrite = TRUE)

cat("snt_var_tree data object has been rebuilt from var_tree.yml\n")
