# run script

invisible(lapply(list.files("R", full.names = TRUE, recursive = TRUE), source))

#============ Data Prep =====================

x <- build_cube(site_name = "Bokspits_1")

# x <- rast("data_out/Bokspits_1/Bokspits_1_stack.tif") #access the intermediate layer

v <- build_ml_df(cube = x,
                 site_name = "Bokspits_1",
                 df_type="grid")

v <- "data_out/Bokspits_1/Bokspits_1ML_in_grid_level.rds"

#================ ML pipeline ================
# Auto ML ------------------

task <- build_task(v, site_name="Bokspits_1")

tune_spcv <- mlr3::rsmp("spcv_coords", folds = 10)

# #inspect the spatial cv
autoplot(tune_spcv, task=task, 1:3)


# ================ model tuning. =================
# xgboost first.

xgb.lrn <- lts('classif.xgboost.rbv2')$get_learner()

# here we can add the option for filtering based on importance as a hyper parameter.
xgb.lrn.filt <- po("filter", filter=flt("importance"), filter.frac=to_tune(0.1, 1))%>>%
  xgb.lrn 

xgb_tune <- tune_lrnr(
  .task = task,
  .lrnr = xgb.lrn.filt,
  .resamp = tune_spcv,
  .measure = msr("classif.acc"),
  .n_evals = 10,
  sub.sample = 0.1,
  .tune_method = "mbo",
  .test.scale = TRUE,
  .test.pca = TRUE,
  .workers = future::availableCores() - 2,
  .verbose = FALSE,
  .seed = 5446
)

plot(xgb_tune$graph)
xgb_tune$tun_inst$tuning_result
xgb_tune$tun_inst$learner$model$importance$features # view selected features if filter is used.


# now for svm
svm.lrn <- lts('classif.svm.rbv2', type=to_tune("C-classification"))$get_learner()

svm_tune <- tune_lrnr(
  .task = task,
  .lrnr = svm.lrn,
  .resamp = tune_spcv,
  .measure = msr("classif.acc"),
  sub.sample = 0.1,
  .n_evals = 10,
  .tune_method = "random_search",
  .test.scale = TRUE,
  .test.pca = TRUE
)
plot(svm_tune$graph)
svm_tune$tun_inst$tuning_result

# ranger random forest.

rf.lrn <- lts('classif.ranger.rbv2')$get_learner()

rf.lrn.filt <- po("filter", filter=flt("importance"), 
                  filter.frac=to_tune(0.1, 1)) %>>%
  rf.lrn 

rf_tune <- tune_lrnr(
  .task = task,
  .lrnr = rf.lrn.filt,
  .resamp = tune_spcv,
  .measure = msr("classif.acc"),
  sub.sample = 0.1,
  .n_evals = 10,
  .tune_method = "random_search",
  .test.scale = TRUE,
  .test.pca = TRUE
)

plot(rf_tune$graph)
rf_tune$tun_inst$tuning_result
rf_tune$tun_inst$learner$model$importance$scores
# ensemble example


ens.lrn.tune <- gunion(list(
  po("learner_cv", xgb.lrn, id = "ens_xgb"),
  po("learner_cv", svm.lrn, id = "ens_rf"),
  po("learner_cv", rf.lrn, id = "ens_svm"),
  po("nop", "ens_nop")
)) %>>%
  po("featureunion", id = "ens_union") %>>%
  lrn("classif.ranger", id = "master_rf")


ens_tune <- tune_lrnr(
  .task = task,
  .lrnr = ens.lrn.tune,
  .resamp = tune_spcv,
  .measure = msr("classif.acc"),
  sub.sample = 0.1,
  .n_evals = 10,
  .tune_method = "random_search", # definitely use "mbo" for this!
  .test.scale = TRUE,
  .test.pca = TRUE
)

# You can just remove the PO("filter"..) or copy it from the above xfboost filter bit 

plot(ens_tune$graph)
ens_tune$tun_inst$tuning_result




# =================== benchmarking =================================
bench.mark <- benchmark_lrnrs(xgb_tune$tun_inst$learner, 
                              svm_tune$tun_inst$learner, 
                              rf_tune$tun_inst$learner, 
                              ens_tune$tun_inst$learner, 
                             lrn("classif.ranger", id="ranger.untuned"), #untuned random forest
                             .task = task, 
                             .resamp=tune_spcv)

bench.mark$aggregate(c(msr("classif.ce"),msr("classif.acc")))
bench.mark$aggregate(msr("classif.acc"))

autoplot(bench.mark, measure=msr("classif.acc"))

# decide which is the best...

# Resampling ------------------------------
# now proper resampling...here I'm using svm as it looks the best just switch the learner as needed.
resamps <- ml_resample(.task = task,
            .learner = svm_tune$tun_inst$learner)

resamps$aggregate(c(msr("classif.acc"),
                    msr("classif.ce"))) # get aggregate scores
res.preds <-
  resamps$predictions() |>
  purrr::map(function(x = .x) x$confusion) |> 
  Reduce("+", x = _)
print(res.preds)

 autoplot(resamps, measure=msr("classif.acc"))

# Prediction ----------------------------------

tic()
mod.pred <- predict_terra(x, 
                  mod= svm_tune$tun_inst$learner,
                  site_name="Bokspits_1",
                  .workers=1)
toc()
# quick n dirt map.
par(mfrow=c(1,2))
plot(mod.pred, col=rev(brewer.pal(6, name="Dark2")), axes=FALSE)
terra::plotRGB(x[[c(3,2,1)]], scale=0.6, smooth=TRUE, axes=FALSE)
par(mfrow=c(1,1))



#


