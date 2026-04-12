## ------------------------------------------------------- ##
# Silica WG - Extract Land Cover
## ------------------------------------------------------- ##
# Legacy compatibility stub.
# The land cover extraction is not part of the current production workflow and
# is skipped by default in total-workflow.R. The retained implementation lives
# under deprecated/ for historical reruns only.

message("extract-landcover.R is legacy and not part of the current production workflow.")
source(file.path("deprecated", "extract-landcover.R"), echo = TRUE)
