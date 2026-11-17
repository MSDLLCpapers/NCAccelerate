example_path <- function(name) {
  system.file("extdata", name, package = "NCAcelerate", mustWork = TRUE)
}

# Minimal concentration data in the shape returned by read_concentration_data()
make_conc_data <- function(conc, nmhr = rep(c(0, 1, 2, 4), length.out = length(conc)),
                           subj = rep(seq_len(length(conc) / length(unique(nmhr))),
                                      each = length(unique(nmhr))),
                           dose = 10, day = 1, panel = "A") {
  data.frame(
    PKEXTSTUDYID = "S1", PKEXTSUBJID = subj, PKEXTPKSEQ = panel,
    PKEXTPERIOD = 1, PKEXTSTDYDY = day, PKEXTNMHR = nmhr, NMHRU = "h",
    PKEXTORRES = conc, ORRESU = "ng/mL", DOSE = dose, DOSEUNIT = "mg",
    PKEXTSRCANL = "DRUG", PKEXTSPECAB = "Plasma"
  )
}

# Minimal wide PK parameter data in the shape of transform_pk_parameters()
make_pk_wide <- function() {
  data.frame(
    PKEXTSUBJID = rep(1:6, each = 2),
    Analyte = "DRUG", Matrix = "Plasma",
    Dose = rep(c(10, 10, 10, 100, 100, 100), each = 2),
    Panel = rep(c("A", "A", "A", "B", "B", "B"), each = 2),
    Period = 1,
    Day = rep(c(1, 7), 6),
    Cmax = c(10, 20, 12, 24, 8, 16, 100, 150, 120, 180, 80, 120),
    Tmax = c(1, 2, 1, 1, 2, 2, 1, 1, 2, 2, 4, 4)
  )
}
