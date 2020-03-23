source(fs::path("R", "packages.R"), encoding = "UTF-8")
source_all()

plan <- drake::drake_plan(
    data = import_data(),
    #fit_result = fit_with_greta(data)
    #fit_result_all = fit_with_greta_all(data)
    #fit_result_all_2 = fit_with_greta_all(data,
            #i = vec_c(0, 100),
            #e = vec_c(0, 0.35),
            #lambda_p = vec_c(0, 360),
            #omega = vec_c(0, 360),
            #phi_p = vec_c(80, 120)),
    fit_result_all_3 = fit_with_greta_all(data,
            i = vec_c(0, 100),
            e = vec_c(0, 0.35),
            lambda_p = vec_c(0, 360),
            omega = vec_c(0, 360),
            phi_p = vec_c(0, 360))
    )

drake::drake_config(
    plan,
    log_make = stdout(),
    verbose = 4L,
    log_progress = TRUE)