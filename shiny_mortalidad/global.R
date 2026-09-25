################################################################################
# global.R — Mortalidad Hospitalaria Ajustada al Riesgo · DIME
################################################################################

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(scales)

# ── Load pre-computed results ──────────────────────────────────────────────────
pc_path <- file.path(getwd(), "data", "mort_precomputed.rds")
if (!file.exists(pc_path)) stop("mort_precomputed.rds not found. Run prep_mortality.R first.")

pc <- readRDS(pc_path)

# Unpack all pre-computed objects into global scope
tbl_anual             <- pc$tbl_anual
df_bim                <- pc$df_bim
df_mes_act            <- pc$df_mes_act
dist_acum             <- pc$dist_acum
trend_anual           <- pc$trend_anual
demo_year             <- pc$demo_year
box_data              <- pc$box_data
comorb_data           <- pc$comorb_data
dept_mort             <- pc$dept_mort
pal                   <- pc$pal
ano_actual            <- pc$ano_actual
total_discharges_dime <- pc$total_discharges_dime
total_mort_dime       <- pc$total_mort_dime
tasa_bruta_dime       <- pc$tasa_bruta_dime
tot_egresos           <- pc$tot_egresos
tot_muertes           <- pc$tot_muertes
tasa_bruta_grd        <- pc$tasa_bruta_grd
act_egresos_dime      <- pc$act_egresos_dime
act_egresos           <- pc$act_egresos
act_mort_dime         <- pc$act_mort_dime
act_muertes           <- pc$act_muertes
act_exp               <- pc$act_exp
act_tmar              <- pc$act_tmar
tmar_reciente         <- pc$tmar_reciente
tmar_color            <- pc$tmar_color
otras_total           <- pc$otras_total
otras_pct_dime        <- pc$otras_pct_dime
otras_top_cat         <- pc$otras_top_cat
otras_trend           <- pc$otras_trend
otras_mes_act         <- pc$otras_mes_act
otras_tbl             <- pc$otras_tbl
otras_dist            <- pc$otras_dist
otras_pal             <- pc$otras_pal
otras_diag_tbl        <- pc$otras_diag_tbl
hsmr_anual            <- pc$hsmr_anual
hsmr_mes_act_g        <- pc$hsmr_mes_act_g
hsmr_mensual_todos    <- pc$hsmr_mensual_todos
dist_global           <- pc$dist_global
dist_global_acum      <- pc$dist_global_acum
comorb_global         <- pc$comorb_global
coef_global           <- pc$coef_global
pal_global            <- pc$pal_global
tm_ref_global         <- pc$tm_ref_global
n_cv_train            <- pc$n_cv_train
hsmr_actual_val       <- pc$hsmr_actual_val
hsmr_esp_actual       <- pc$hsmr_esp_actual
total_egresos_global  <- pc$total_egresos_global
total_muertes_global  <- pc$total_muertes_global
tasa_global           <- pc$tasa_global
hsmr_color_g          <- pc$hsmr_color_g

grd_choices <- sort(unique(tbl_anual$GRD))

message("[Mortalidad] Pre-computed data loaded | ano_actual = ", ano_actual,
        " | GRDs = ", paste(grd_choices, collapse = ", "))
