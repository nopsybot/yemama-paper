# Youth Ecological Momentary Assessment Meta-Analysis (YEMAMA) Update

This is the code repository for "Readdressing the Ongoing Challenge of Missing Data in Youth Ecological Momentary Assessment Studies: A Meta-Analysis Update."

All data and materials are available via our research compendium on the Open Science Framework: [https://osf.io/8nkeu/](https://osf.io/8nkeu/?view_only=f87987d1419d4b3b9f2b6499a3686460)

Packages are managed using the `renv` package, and the environment can be restored by running `renv::restore()`.

The preprocessing and statistical pipeline was designed using the [{targets}](https://docs.ropensci.org/targets/) package and paradigm for improved reproducibility and efficiency. The pipelines are defined in the `pipelines/.../_targets.R` files, and can be executed by running the `targets::tar_make()` function.

