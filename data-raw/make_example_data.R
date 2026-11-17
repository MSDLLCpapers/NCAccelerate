# Generates the simulated example datasets shipped in inst/extdata.
# Run from the package root: source("data-raw/make_example_data.R")
# All values are simulated (one-compartment oral model) - no real study data.

set.seed(20260701)

times <- c(0, 0.5, 1, 2, 4, 8, 12, 24)
design <- data.frame(panel = c("A", "B", "C"), dose = c(10, 50, 200))
n_subj <- 6
ka <- 1.2; ke_pop <- 0.1; v_pop <- 50 # 1/h, 1/h, L

conc_1cmt <- function(dose, t, ka, ke, v, n_doses = 1, tau = 24) {
  # Superposition of n_doses given every tau; t is relative to the last dose
  total <- 0
  for (k in seq_len(n_doses) - 1) {
    tt <- t + k * tau
    total <- total + dose * 1000 * ka / (v * (ka - ke)) * (exp(-ke * tt) - exp(-ka * tt))
  }
  total # ng/mL when dose in mg and V in L
}

simulate <- function(days = 1) {
  rows <- list()
  subj <- 100
  for (i in seq_len(nrow(design))) {
    for (s in seq_len(n_subj)) {
      subj <- subj + 1
      ke <- ke_pop * exp(rnorm(1, 0, 0.2))
      v <- v_pop * exp(rnorm(1, 0, 0.2))
      for (day in days) {
        n_doses <- if (day == 1) 1 else day
        conc <- conc_1cmt(design$dose[i], times, ka, ke, v, n_doses)
        conc <- round(conc * exp(rnorm(length(times), 0, 0.1)), 3)
        if (day == 1) conc[1] <- 0
        rows[[length(rows) + 1]] <- data.frame(
          PKEXTSTUDYID = "EX-001", PKEXTSUBJID = subj,
          PKEXTSRCANL = "DRUG-X", PKEXTSPECAB = "Plasma",
          PKEXTPKSEQ = design$panel[i], PKEXTPERIOD = 1,
          PKEXTSTDYDY = day, PKEXTNMHR = times, PKEXTORRES = conc,
          PKEXTDOSE = design$dose[i], ke = ke
        )
      }
    }
  }
  do.call(rbind, rows)
}

pk_params <- function(conc) {
  groups <- split(conc, list(conc$PKEXTSUBJID, conc$PKEXTSTDYDY), drop = TRUE)
  rows <- lapply(groups, function(g) {
    auclast <- sum(diff(g$PKEXTNMHR) * (head(g$PKEXTORRES, -1) + tail(g$PKEXTORRES, -1)) / 2)
    clast <- tail(g$PKEXTORRES, 1)
    ke <- g$ke[1]
    id <- g[1, c("PKEXTSUBJID", "PKEXTSRCANL", "PKEXTSPECAB", "PKEXTDOSE",
                 "PKEXTPKSEQ", "PKEXTPERIOD", "PKEXTSTDYDY")]
    data.frame(
      id[rep(1, 6), ],
      Parameter = c("Cmax", "Tmax", "AUClast", "AUCINF_pred", "HL_Lambda_z",
                    "Flag_AUC_%Ext_pred"),
      Units = c("ng/mL", "h", "h*ng/mL", "h*ng/mL", "h", ""),
      Estimate = round(c(max(g$PKEXTORRES), g$PKEXTNMHR[which.max(g$PKEXTORRES)],
                         auclast, auclast + clast / ke, log(2) / ke, 0), 3),
      row.names = NULL, check.names = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out[order(out$PKEXTSUBJID, out$PKEXTSTDYDY), ]
}

write_with_units_row <- function(data, units, path) {
  data <- data.frame(lapply(data, as.character), check.names = FALSE)
  units_row <- as.data.frame(as.list(setNames(rep("", ncol(data)), names(data))),
                             check.names = FALSE)
  units_row[names(units)] <- units
  utils::write.csv(rbind(units_row, data), path, row.names = FALSE, na = "")
}

out <- file.path("inst", "extdata")

# SAD, Watson-styled: dose value only, units in the second header row
sad <- simulate(days = 1)
write_with_units_row(sad[, setdiff(names(sad), "ke")],
                     c(PKEXTNMHR = "h", PKEXTORRES = "ng/mL", PKEXTDOSE = "mg"),
                     file.path(out, "example_concentration.csv"))
sad_pk <- pk_params(sad)
sad_pk$PKEXTPERIOD <- NULL
write_with_units_row(sad_pk, c(PKEXTDOSE = "mg"),
                     file.path(out, "example_pk_parameters.csv"))

# MAD, Ardena-styled: PKEXTNAM column, "25 mg" dose, empty period, "Day1" days
mad <- simulate(days = c(1, 7))
mad_out <- data.frame(PKEXTNAM = "ARDENA",
                      mad[, setdiff(names(mad), c("ke", "PKEXTDOSE"))],
                      PKEXTDOSE = paste(mad$PKEXTDOSE, "mg"))
mad_out$PKEXTPERIOD <- NA
mad_out$PKEXTSTDYDY <- paste0("Day", mad_out$PKEXTSTDYDY)
write_with_units_row(mad_out, c(PKEXTNMHR = "h", PKEXTORRES = "ng/mL"),
                     file.path(out, "example_concentration_mad.csv"))
mad_pk <- pk_params(mad)
mad_pk$PKEXTDOSE <- paste(mad_pk$PKEXTDOSE, "mg")
mad_pk$PKEXTPERIOD <- NA
utils::write.csv(mad_pk, file.path(out, "example_pk_parameters_mad.csv"),
                 row.names = FALSE, na = "")
