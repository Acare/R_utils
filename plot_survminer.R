plot_survminer <- function(fit, type, 
                           palette = NULL, strata_names = NULL, 
                           xlim = c(0, 120), break_time_by = 12) {
  
  require(survival)
  require(survminer, quietly = TRUE)
  
  ggsurvplot(fit,
             data = dplyr::sym(as.character(fit$call)[3]),
             risk.table = TRUE,
             pval = TRUE,
             conf.int = F,
             legend = "right",
             palette = palette,
             xlim = xlim,
             break.time.by = break_time_by,
             # risk.table.col = "strata",
             # surv.median.line = "hv",
             # risk.table.pos = "in",
             legend.labs = strata_names,
             ggtheme = ggplot2::theme_bw(),
             xlab = "Time (months)", ylab = stringr::str_c(type, " probability"),
             risk.table.y.text = FALSE)
}