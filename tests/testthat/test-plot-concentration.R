sad_summary <- function() {
  conc <- read_concentration_data(example_path("example_concentration.csv"))
  summarise_concentration(conc)
}

mad_summary <- function() {
  conc <- read_concentration_data(example_path("example_concentration_mad.csv"))
  summarise_concentration(conc)
}

test_that("SAD plot has one line per panel - period - dose and default labels", {
  p <- plot_concentration_time(sad_summary())
  expect_s3_class(p, "ggplot")
  built <- ggplot2::ggplot_build(p)
  expect_equal(levels(p$data$interaction_var), c("A - 1 - 10", "B - 1 - 50", "C - 1 - 200"))
  expect_equal(p$labels$x, "Time (h)")
  expect_equal(p$labels$y, "DRUG-X Concentration (ng/mL)")
  expect_equal(p$labels$colour, "Panel - Period - Dose (mg)")
  expect_null(p$labels$caption)
  expect_s3_class(built, "ggplot_built")
})

test_that("custom titles, reference lines and captions are applied", {
  p <- plot_concentration_time(sad_summary(), pk_target = "100", lloq = 1,
                               title = "My plot", x_title = "X", y_title = "Y")
  expect_equal(p$labels$title, "My plot")
  expect_equal(p$labels$x, "X")
  expect_equal(p$labels$y, "Y")
  expect_match(p$labels$caption, "PK Target \\(red dashed line\\): 100 ng/mL")
  expect_match(p$labels$caption, "LLOQ \\(black dashed line\\): 1 ng/mL")
  hlines <- vapply(p$layers, function(l) inherits(l$geom, "GeomHline"), logical(1))
  expect_equal(sum(hlines), 2)
})

test_that("GM plots drop NC time points and use geometric SD error bars", {
  s <- sad_summary()
  s$GM[2] <- "NC"
  p <- plot_concentration_time(s, stat = "GM", error_bar = TRUE)
  expect_equal(nrow(p$data), nrow(s) - 1)
  expect_type(p$data$GM, "double")
  expect_true(any(vapply(p$layers, function(l) inherits(l$geom, "GeomErrorbar"), logical(1))))
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("BLQ excluded time points are not plotted", {
  s <- sad_summary()
  s$BLQ_EXCLUDED[1:3] <- "Y"
  p <- plot_concentration_time(s)
  expect_equal(nrow(p$data), nrow(s) - 3)
})

test_that("log scale and x-axis range are applied", {
  p <- plot_concentration_time(sad_summary(), log_y = TRUE,
                               x_limits = c("0", "24"), x_interval = 6)
  expect_equal(p$scales$get_scales("y")$trans$name, "log-10")
  expect_equal(p$scales$get_scales("x")$breaks, c(0, 6, 12, 18, 24))
  expect_equal(p$coordinates$limits$x, c(0, 24))
})

test_that("invalid x ranges are ignored", {
  p <- plot_concentration_time(sad_summary(), x_limits = c("", ""), x_interval = NA)
  expect_null(p$coordinates$limits$x)
})

test_that("MAD plot facets by selected days", {
  p <- plot_concentration_time(mad_summary(), days = c("1", "7"))
  expect_s3_class(p$facet, "FacetWrap")
  expect_equal(sort(unique(p$data$DAY)), c(1, 7))
  p7 <- plot_concentration_time(mad_summary(), days = 7)
  expect_equal(unique(p7$data$DAY), 7)
})

test_that("free x scale ignores x range", {
  p <- plot_concentration_time(mad_summary(), days = 1, free_x = TRUE,
                               x_limits = c(0, 12), x_interval = 6)
  expect_null(p$coordinates$limits$x)
  expect_true(p$facet$params$free$x)
})

test_that("empty selections return a message plot", {
  p <- plot_concentration_time(mad_summary(), days = character(0))
  expect_equal(p$labels$title, "Select day(s) to plot")
  p2 <- plot_concentration_time(mad_summary(), days = 99)
  expect_equal(p2$labels$title, "No data points in the selected X-axis range")
})

test_that("log10_breaks covers the data", {
  expect_equal(log10_breaks(250), c(1, 10, 100, 1000))
  expect_equal(log10_breaks(5, from = -2), c(0.01, 0.1, 1, 10))
})
