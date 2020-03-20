source(fs::path("R", "packages.R"), encoding = "UTF-8")
source_all()

plan <- drake::drake_plan(
   target = 1
    )

drake::drake_config(plan)