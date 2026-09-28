# TODO on the main machine: make the repo runnable on a fresh clone

Written 2026-09-28 from the laptop (repos in `C:\GitHubRepos`). Delete this
file, and the pointer in `CLAUDE.md`, once everything below is done.

## Why

These files are read by this repo's `.qmd` files but are git-ignored, so they
were missing on the laptop:

```
data/bh_dep_ch_ethnicity_hrp.csv            <- postcode_school_pop.qmd
data/bn_postcodes_pop1.csv                  <- postcode_school_pop.qmd
data/bh_hh_comp.csv                         <- equity_in_responses.qmd
data/bn_all_deprivation.csv                 <- equity_in_responses.qmd
data/bn_pcd_dist_hh_stats.csv               <- equity_in_responses.qmd
data/bn_pcd_sect_hh_stats.csv               <- equity_in_responses.qmd
data/pcd_p001.csv                           <- equity_in_responses.qmd
data/pcd_p002.csv                           <- equity_in_responses.qmd
data/pcds_p003.csv                          <- equity_in_responses.qmd
data/current_catchments_fix.geojson         <- catchment_map.qmd
data/optionZ_Mar25.geojson                  <- catchment_map.qmd
```

Other repos also read `data/lsoa_age_11_props_totals.xlsx` and
`bn_postcodes_pop1.csv` (repo root) from here. Both were present on the laptop.

`postcode_school_pop.qmd` may be reading from the wrong folder: the only copy
of `bn_postcodes_pop1.csv` on the laptop is in the repo root, not `data/`.

## Steps

### 1. Make the files above available

### How to get ignored data onto GitHub

Check sizes first:

```r
f <- c(...)  # the list above
data.frame(f, MB = round(file.size(f) / 1e6, 1))
```

Then, for each file:
- **Small (under ~50 MB), licence allows it, not embargoed:** commit it. Add a
  `!path/to/file` rule to `.gitignore`. Git can't re-include a file whose
  parent folder is ignored, so change a folder rule like `data/` to `data/*`
  first.
- **Large:** upload as a GitHub Release asset (e.g. `piggyback::pb_upload()`)
  and add a small fetch script that downloads whatever is missing.
- **Restricted / embargoed / personal data:** leave it out, and say in the
  README where it comes from.

Check whether the repo is public before committing anything.

### 2. `postgres_import.qmd` uses `D:/` paths

It reads national datasets (AddressBase, ONSPD, Code-Point) from `D:/`. This
looks like a one-off import, so these can probably stay as they are. If it's
still needed, read the path from an environment variable instead, e.g.
`Sys.getenv("ONSPD_PATH")`.

### Check it

Pull on the laptop and run `quarto render equity_in_responses.qmd`.
