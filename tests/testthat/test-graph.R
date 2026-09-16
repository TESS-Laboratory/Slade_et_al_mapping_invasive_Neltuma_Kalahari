testthat::test_that("the generated graph has v2.0's shape under the full config", {
  cfg <- read_sensors_yml(); sites <- read_sites()$site; tags <- read_stacks()$tag
  tg <- task_grid(cfg, sites, tags)
  testthat::expect_equal(nrow(tg), 38L)                       # 28 drone + 4 wv2 + 3 planet + 3 s2
  testthat::expect_equal(sum(tg$sensor == "drone"), 28L)
  testthat::expect_setequal(unique(tg$source), c("field", "archived", "purity_raw", "purity_smooth"))
  fg <- fit_grid(tg, letters[1:7])
  testthat::expect_equal(nrow(fg), 38L * 7L)
  pg <- pred_grid(tg, cfg, "5_CHM_ALLVI", c("svm"))
  testthat::expect_equal(nrow(pg), 10L)                       # 7 sites + 3 scenes
  testthat::expect_true(all(pg$source[pg$sensor != "drone"] == "archived"))
  testthat::expect_equal(legacy_site_label("wv2", "scene", "purity_raw"), "wv2_dr_raw")
  testthat::expect_equal(legacy_site_label("drone", "bokspits_1", "field"), "bokspits_1")
})
testthat::test_that("task-grid layer symbols point at shared per-sensor targets", {
  cfg <- read_sensors_yml(); tg <- task_grid(cfg, "bokspits_1", "5")
  syms <- vapply(tg$layer_sym, as.character, "")
  testthat::expect_true("field_paths_bokspits_1" %in% syms)
  testthat::expect_true("purity_layer_wv2_raw" %in% syms)
  testthat::expect_true("layer_archived_s2" %in% syms)
  testthat::expect_true("field_layer_wv2" %in% syms)
})
