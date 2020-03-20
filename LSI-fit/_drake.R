source(fs::path("R", "packages.R"), encoding = "UTF-8")
source_all()

plan <- drake::drake_plan(
    data = import_data()
    )

drake::drake_config(plan)