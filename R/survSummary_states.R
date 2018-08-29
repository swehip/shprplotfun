#' Survival analysis summary for competing risk results
#'
#' Summary of a survfit object with more than one model from survival package.
#' If there is not enough follow up time the latest results is extrapolated.
#' @param surv_object Object returned from survfit function in survival package.
#' @param times Time to be summarized.
#' @return Data frame summarizing surv_object.
#' @export
survSummary_states <- function(surv_object, times) {
  # If there is not enough follow up time the latest results is extrapolated

  result <- NULL
  group_vars <- dplyr::select(surv_object, -models)

  for (i in seq_along(surv_object$models)) {
    sum_temp <- summary(surv_object$models[[i]], times, extend = TRUE)
    cens_index <- which(sum_temp$states == "")
    n <- data.frame(n = sum_temp$n)
    n_risk <- data.frame(n_risk = sum_temp$n.risk[, cens_index])

    n_event <- data.frame(
      n_event = sum_temp$n.event[, -cens_index],
      states = paste0("event_", sum_temp$states[-cens_index])) %>%
      tidyr::spread(states, n_event)

    pstates <- data.frame(
      props = sum_temp$pstate[, -cens_index],
      states = paste0("prop_", sum_temp$states[-cens_index])) %>%
      tidyr::spread(states, props)

    lower <- data.frame(
      props = sum_temp$lower[, -cens_index],
      states = paste0("lower_", sum_temp$states[-cens_index])) %>%
      tidyr::spread(states, props)

    upper <- data.frame(
      props = sum_temp$upper[, -cens_index],
      states = paste0("upper_", sum_temp$states[-cens_index])) %>%
      tidyr::spread(states, props)

    sd <- data.frame(
      sd = sum_temp$std.err[, -cens_index],
      states = paste0("sd_", sum_temp$states[-cens_index])) %>%
      tidyr::spread(states, sd)

    temp_result <-
      dplyr::bind_cols(n, n_risk, n_event, pstates, lower, upper, sd)
    result <- dplyr::bind_rows(result, temp_result)
  }

  dplyr::bind_cols(group_vars, result)
}
