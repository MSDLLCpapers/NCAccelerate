# Entry point for deploying to shinyapps.io / Posit Connect from source.
# For local use, prefer: NCAcelerate::run_app()
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
NCAcelerate::run_app()
