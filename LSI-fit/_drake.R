source(fs::path("R", "packages.R"), encoding = "UTF-8")
source_all()

plan <- drake::drake_plan(
    data = import_data(),
    fit_result = fit_with_greta(data)
    )

drake::drake_config(plan, console_log_file = stdout(), verbose = 4L, log_progress = TRUE)