# Conformal coverage must hold on held-out data by construction. Synthetic
# 3-class problem with a genuinely uncertain classifier.
make_probs <- function(n, k = 3, seed = 1) {
  set.seed(seed)
  truth <- factor(sample(seq_len(k), n, replace = TRUE))
  # noisy but informative probabilities
  p <- matrix(runif(n * k, 0.05, 1), n, k)
  for (i in seq_len(n)) p[i, as.integer(truth[i])] <- p[i, as.integer(truth[i])] + runif(1, 0, 1.5)
  p <- p / rowSums(p); colnames(p) <- as.character(seq_len(k))
  list(prob = p, truth = truth)
}

testthat::test_that("Mondrian LAC gives >= 1-alpha coverage per class on held-out data", {
  cal <- make_probs(4000, seed = 1); test <- make_probs(4000, seed = 2)
  for (alpha in c(0.05, 0.10, 0.20)) {
    th <- mondrian_thresholds(cal$prob, cal$truth, alpha)
    cov <- conformal_coverage(test$prob, test$truth, th)
    # per-class coverage at least 1 - alpha, allowing a small finite-sample slack
    testthat::expect_true(all(cov$by_class$coverage >= (1 - alpha) - 0.03),
                          info = sprintf("alpha=%.2f class coverages: %s", alpha,
                                         paste(round(cov$by_class$coverage, 3), collapse = " ")))
  }
})

testthat::test_that("smaller alpha yields larger sets; scores and thresholds are sane", {
  d <- make_probs(3000, seed = 3)
  th05 <- mondrian_thresholds(d$prob, d$truth, 0.05)
  th20 <- mondrian_thresholds(d$prob, d$truth, 0.20)
  s05 <- conformal_coverage(d$prob, d$truth, th05)$mean_set_size
  s20 <- conformal_coverage(d$prob, d$truth, th20)$mean_set_size
  testthat::expect_gt(s05, s20)
  testthat::expect_true(all(lac_scores(d$prob, d$truth) >= 0 & lac_scores(d$prob, d$truth) <= 1))
})

testthat::test_that("softvote_oof averages aligned by row id; tidy_softvote scores it", {
  a <- make_probs(50, seed = 4); b <- make_probs(50, seed = 5)
  # same truth/ids, different probs (two 'learners')
  o1 <- list(row_ids = 1:50, prob = a$prob, truth = a$truth)
  o2 <- list(row_ids = 50:1, prob = b$prob[50:1, ], truth = a$truth[50:1])  # shuffled ids
  sv <- softvote_oof(list(o1, o2))
  testthat::expect_equal(sv$row_ids, 1:50)
  testthat::expect_equal(unname(sv$prob[1, ]), unname((a$prob[1, ] + b$prob[1, ]) / 2))
  r <- tidy_softvote(sv, "s", "t", neltuma_code = 1L)
  testthat::expect_equal(r$score$learner, "average")
  testthat::expect_equal(r$class$learner, "average")
  testthat::expect_true(r$score$classif.acc >= 0 && r$score$classif.acc <= 1)
  testthat::expect_named(r, c("score", "class"))
})

testthat::test_that("honest K-fold coverage is near nominal and neltuma coverage present", {
  d <- (function(n){set.seed(7); truth<-factor(sample(1:4,n,TRUE)); p<-matrix(runif(n*4,0.05,1),n,4)
    for(i in seq_len(n)) p[i,as.integer(truth[i])]<-p[i,as.integer(truth[i])]+runif(1,0,1.5)
    p<-p/rowSums(p); colnames(p)<-as.character(1:4); list(row_ids=seq_len(n),prob=p,truth=truth)})(4000)
  h <- honest_coverage(d, c(0.10), neltuma_code=1L, "s", "t")
  testthat::expect_true(abs(h$overall - 0.90) < 0.03)
  testthat::expect_false(is.na(h$neltuma_coverage))
  testthat::expect_true(h$neltuma_coverage >= 0.85)
})
