#' Survival analysis summary
#'
#' Summary of a survfit object with more than one model from survival package.
#' If there is not enough follow up time the latest results is extrapolated.
#' @param surv_object Object returned from survfit function in survival package.
#' @param times Time to be summarized.
#' @return Data frame summarizing surv_object.
#' @export
survSummary <- function(surv_object, times) {
  # If there is not enough follow up time the latest results is extrapolated

  n         <- NULL
  n_risk    <- NULL
  surv_prob <- NULL
  lower     <- NULL
  upper     <- NULL
  sd        <- NULL

  group_vars <- dplyr::select_(surv_object, ~-models)

  for (i in seq_along(surv_object$models)) {
    n[i] <- ifelse(
      is.null(
          summary(surv_object$models[[i]], times, extend = TRUE)$n), NA,
          summary(surv_object$models[[i]], times, extend = TRUE)$n
        )


    n_risk[i] <- ifelse(
      is.null(summary(surv_object$models[[i]], times, extend = TRUE)$n.risk), NA,
              summary(surv_object$models[[i]], times, extend = TRUE)$n.risk
    )

    surv_prob[i] <- ifelse(
      is.null(summary(surv_object$models[[i]], times, extend = TRUE)$surv), NA,
              summary(surv_object$models[[i]], times, extend = TRUE)$surv
    )


    lower[i] <- ifelse(
      is.null(summary(surv_object$models[[i]], times, extend = TRUE)$lower), NA,
              summary(surv_object$models[[i]], times, extend = TRUE)$lower
    )


    upper[i] <-  ifelse(
      is.null(summary(surv_object$models[[i]], times, extend = TRUE)$upper), NA,
              summary(surv_object$models[[i]], times, extend = TRUE)$upper
    )

    sd[i] <-
      ifelse(is.null(summary(surv_object$models[[i]], times)$std.err), NA,
                     summary(surv_object$models[[i]], times)$std.err)
  }

  result <-
    data.frame(
      antal     = n,
      at_risk   = n_risk,
      surv_prob = surv_prob,
      lower     = lower,
      upper     = upper,
      sd        = sd
    )

  dplyr::bind_cols(group_vars, result)
}
