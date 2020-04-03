source(fs::path("R", "packages.R"), encoding = "UTF-8")
source_all()

plan <- drake::drake_plan(
    data = import_data(),
    data_2 = data %>% filter(Filter %vin% vec_c("B", "V")),
    #fit_result = fit_with_greta(data)
    #fit_result_all = fit_with_greta_all(data)
    #fit_result_all_2 = fit_with_greta_all(data,
            #i = vec_c(0, 100),
            #e = vec_c(0, 0.35),
            #lambda_p = vec_c(0, 360),
            #omega = vec_c(0, 360),
            #phi_p = vec_c(80, 120)),
    #fit_result_all_3 = fit_with_greta_all(data,
            #i = vec_c(0, 100),
            #e = vec_c(0, 0.35),
            #lambda_p = vec_c(0, 360),
            #omega = vec_c(0, 360),
            #phi_p = vec_c(0, 360))
    #fit_2_result_all = fit_with_greta_all(
            #data,
            #q_func = q_int_2, u_func = u_int_2,
            #tau = vec_c(0, 10),
            #n_warmup = 1e3,
            #n_samples = 2.5e4),
    fit_3_result_all = fit_with_greta_all(
            data,
            i = vec_c(0, 100),
            e = vec_c(1e-3, 0.35),
            lambda_p = vec_c(0, 270),
            omega = vec_c(0, 180),
            phi_p = vec_c(0, 360),
            q_func = q_int_2, u_func = u_int_2,
            tau = vec_c(0, 10),
            n_warmup = 7.5e3,
            n_samples = 5.7e4)
    )

drake::drake_config(
    plan,
    log_make = stdout(),
    verbose = 4L,
    log_progress = TRUE)