library(vars); library(dplyr); library(janitor); library(readr); library(here)

vr <- read_csv(here("data","processed","tasas","var_ready.csv"), show_col_types = FALSE) |> clean_names()
vr$share_extranjeros_prop <- vr$share_extranjeros / 100
dlog <- function(x) { x <- as.numeric(x); if (all(is.na(x))) return(x); c(NA_real_, diff(log(x))) }
vr$d_l_share_extranjeros <- if (!"d_l_share_extranjeros" %in% names(vr)) dlog(vr$share_extranjeros_prop) else vr$d_l_share_extranjeros

mk_irf <- function(y_nm, file_stub, p=1, h=6){
  Z <- vr |> transmute(d_l_share_extranjeros, !!y_nm := dlog(.data[[sub("^d_l_", "", y_nm)]])) |> stats::na.omit()
  # Nota: si ya tienes d_l_* en var_ready, usa directamente:
  Z <- vr |> select(d_l_share_extranjeros, all_of(y_nm)) |> stats::na.omit()
  v  <- VAR(Z, p=p, type="const")
  ir <- irf(v, impulse="d_l_share_extranjeros", response=y_nm, n.ahead=h, ortho=FALSE, boot=TRUE, runs=200)
  dir.create(here("output","figures"), recursive = TRUE, showWarnings = FALSE)
  png(here("output","figures", paste0(file_stub, "_irf_p", p, ".png")), width=900, height=600, res=120)
  plot(ir, main=paste0("IRF: Δlog(% extranjeros) → ", y_nm, " (p=", p, ")"))
  dev.off()
}

mk_irf("d_l_det_tot_rate", "irf_det_tot", p=1, h=6)
mk_irf("d_l_det_ext_rate", "irf_det_ext", p=1, h=6)
# Si quieres probar p=2:
# mk_irf("d_l_det_tot_rate", "irf_det_tot", p=2, h=6)
# mk_irf("d_l_det_ext_rate", "irf_det_ext", p=2, h=6)
