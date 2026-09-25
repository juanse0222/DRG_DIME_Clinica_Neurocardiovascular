# Dashboard workflow — shiny_grd (and friends)

Quick reference for updating, previewing, and deploying the DIME Shiny
dashboards (`shiny_grd`, `shiny_los`, `shiny_mortalidad`). Three separate
flows: **update**, **view locally**, **deploy**.

## 1. Update the dashboard

**Code change** (new tab, tweak a chart, fix a bug)
- Edit `app.R` / `global.R` directly in VS Code.

**Data change** (new month of costs, new egresos file, refreshed geocoding, etc.)
- Drop the fresh source file into `data/` — copy it in, don't read straight
  from another project's iCloud folder (keeps paths stable and matches every
  existing script's convention).
- Re-run whichever prep script feeds the tab you're updating. It writes
  compact, privacy-safe `.rds` files into `shiny_grd/data/` — that is the
  *only* place `global.R` reads from when the app actually runs:
  - `Rscript prep_data.R` (run from inside `shiny_grd/`) →
    cost/CACI tabs + Perfil de pacientes
  - `Rscript scripts/prep_discharges_trend.R` (run from project root) →
    Egresos y Reingresos
  - `Rscript scripts/geocode_addresses.R` (run from project root, needs the
    local Nominatim container running) → Mapa

### LOS census — different mechanism, don't use prep scripts for this

`shiny_los` doesn't read a `prep_*.R`-generated file for occupancy/LOS data —
it processes the raw census itself, with a caching layer. Two ways to update:

- **Monthly update (normal case):** drop the new month's export into
  `shiny_los/data/cense_updates/`, named
  `data_cense_update_YYYY_MM.rds` / `.xlsx` / `.xls`
  (e.g. `data_cense_update_2026_08.xlsx` — the raw hospital-system export
  works as-is, no conversion needed). `global.R` auto-detects it by filename
  pattern and merges it with the base census. See the README in that folder.
- **Full replace (rare):** overwrite `shiny_los/data/data_cense_2017_2026.rds`
  directly (and its canonical copy at `data/data_cense_2017_2026.rds`, project
  root) with a fresh full export.

Either way: delete `shiny_los/data/data_los_processed.rds` (the cache)
afterward, then run the app locally once — that forces `global.R` to
reprocess (~30s) and regenerate the cache before you deploy. This whole
folder and the raw census are **local-only**: `scripts/deploy_app.R` ships
only the already-processed `data/data_los_processed.rds` for LOS, never the
raw census or its monthly updates (see `LOS_FILES` in that script).

## 2. View it locally

In VS Code, put your cursor on one of these lines and press `Cmd+Enter`
(run from the R console, not a Terminal):
```r
shiny::runApp("shiny_grd")   # from the project root
shiny::runApp(".")           # if already inside shiny_grd/
```
It opens in your browser. The R console stays "busy" the whole time the app
is running — that's normal, it means it's serving requests. To stop it:
click back into that console panel and hit the red square/stop icon (or
`Ctrl+C`).

## 3. Deploy to shinyapps.io

**Pre-deploy checklist** (not optional — this is what has caught real bugs
before a broken tab reached production):
1. Regenerate any `.rds` files affected by your change (step 1 above).
2. Syntax-check: `parse("app.R")` and `parse("global.R")` shouldn't error.
3. Run it locally (step 2) and actually click into the tab you touched —
   confirm it renders with real data, not just that the app boots. A root-page
   HTTP 200 check alone does **not** exercise hidden-tab reactives.

Then deploy — use a **Terminal** panel (`Terminal → New Terminal`, not the R
console, since this is a batch script rather than interactive code):
```
Rscript scripts/deploy_app.R grd    # shiny_grd
Rscript scripts/deploy_app.R mort   # shiny_mortalidad
Rscript scripts/deploy_app.R los    # shiny_los
```
Run these **one at a time**, waiting for each to finish — shinyapps.io only
allows one task in flight per account; overlapping deploys throw a 409.

Live URLs (account `8cq8ch-juan0sebastian-hurtado0zapata`):
- GRD: https://8cq8ch-juan0sebastian-hurtado0zapata.shinyapps.io/grd-dime/
- Mortalidad: https://8cq8ch-juan0sebastian-hurtado0zapata.shinyapps.io/mortalidad-dime/
- LOS: https://8cq8ch-juan0sebastian-hurtado0zapata.shinyapps.io/los-dime/

Never paste an rsconnect token into chat. If a fresh machine ever needs
credentials, run `rsconnect::setAccountInfo(...)` yourself in your own R
console.

## VS Code cheat-sheet (the basics you actually need)

- **Activity Bar** (far-left thin strip): Explorer (files), and the **R**
  icon — click it to open the Workspace/Environment viewer, RStudio-style.
- **`Cmd+Enter`**: run the line (or whole multi-line block) your cursor is
  on, sending it to the R console at the bottom. This is the shortcut you'll
  use constantly.
- **R console vs. Terminal**: the R console (opened automatically by your
  first `Cmd+Enter`) is a live, interactive R session — good for exploring
  data line by line. A plain Terminal (`Terminal → New Terminal`) is a shell
  — use it for `Rscript some_script.R`, `git`, etc.
- Packages loaded with `library(...)` don't show up in the Workspace panel —
  only actual variables (`x <- ...`) do.
