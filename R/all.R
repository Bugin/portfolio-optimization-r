# Source every module in dependency order. Assumes the working directory is the
# repository root. Later tasks append their files here.
source("R/params.R")
source("R/data.R")
source("R/simulation.R")
source("R/metrics.R")
source("R/optimisation.R")
