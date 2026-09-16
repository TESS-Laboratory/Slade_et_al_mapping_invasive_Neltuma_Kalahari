testthat::test_that("Table S8 phase thresholds: boundaries go to the lower phase", {
  th <- list(dominance = 15, expansion = 1.5, incursion = 0.1)
  cover <- c(0, 0.05, 0.1, 1.0, 1.5, 10, 15, 15.01, 40)
  ph <- cut(cover, breaks = c(-Inf, th$incursion, th$expansion, th$dominance, Inf),
            labels = c("Pre-Incursion", "Initial Incursion", "Expansion", "Dominance"), right = FALSE)
  testthat::expect_equal(as.character(ph),
    c("Pre-Incursion", "Pre-Incursion", "Initial Incursion", "Initial Incursion",
      "Expansion", "Expansion", "Dominance", "Dominance", "Dominance"))
})
