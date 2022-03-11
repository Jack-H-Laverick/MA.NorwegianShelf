
## Initialise model

implementation <- "Norwegian_Shelf"
R.utils::copyDirectory("../Celtic Sea/Data/Celtic_Sea_ERSEM/", stringr::str_glue("./StrathE2E/{implementation}/")) # Copy example model

#!!# Rename folder to appropriate reference period