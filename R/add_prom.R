#' Add PROM to operation data
#'
#' Easy way to add PROM data to operation data. Preoperation and postoperation
#' 1 year, 6 years and 10 years are added.
#' Also an option to add reoperation PROM.
#'
#' @param dataOp Corresponds to dataOperations in SHPR datalayer.
#'   Variables needed are SubjectKey, P_Side, P_ProstType and P_SurgDate.
#' @param dataPREP Corresponds to dataPROMBefore. Variables SubjectKey,
#'   PREP_HipPain[L|R] must be included.
#' @param dataPOSTP Corresponds to dataPROMAfter. Variables SubjectKey,
#'   POSTP_HipPain[L|R], POSTP_Satisfaction, POSTP_Satisfaction[L|R]
#'   must be included.
#' @param cens_date Censor date for PROM, example if cens_date = '2018-12-31'
#'   there will be no PROMS after that date.
#' @param primary_prom_start_year Start year for PROM, should probably
#'   not be changed.
#' @param add_reop_prom Boolean if reoperation PROM should be added, that PROM
#'   is marked with _reop after every variable.
#' @param reop_prom_start_year Start year for reop PROM,
#'   should probably not be changed.
#'
#' @return dataOp with added PROM variables
add_prom <- function(dataOp,
                     dataPREP,
                     dataPOSTP,
                     cens_date = NULL,
                     primary_prom_start_year = 2002,
                     add_reop_prom = FALSE,
                     reop_prom_start_year = 2017) {

  # To avoid check notes! Should be changed to NSE later!
  DateOfDeath <- POSTP_Date <- PREP_Date <- P_ProstType <- P_Side <- P_SurgDate <-
    P_SurgDate_L <- P_SurgDate_R <- R_SurgDate <- R_SurgDate2 <- SubjectKey <-
    fit_values <- hospital <- op_time <- prim_time <- reg_time <- reop_time <-
    reop_time_L <- reop_time_R <-
    NULL

  # Tried to make it work without these variables included but no time,
  # these are used for this
  pain_and_satis_vars <- c(
    "PREP_HipPainL",
    "PREP_HipPainR",
    "POSTP_HipPainL",
    "POSTP_HipPainR",
    "POSTP_Satisfaction",
    "POSTP_SatisfactionL",
    "POSTP_SatisfactionR"
  )
  postp_vars <- dplyr::intersect(names(dataPOSTP), pain_and_satis_vars)
  prep_vars  <- dplyr::intersect(names(dataPREP), pain_and_satis_vars)
  satis_vars <-
    dplyr::intersect(
      names(dataPOSTP),
      c( "POSTP_Satisfaction", "POSTP_SatisfactionL", "POSTP_SatisfactionR")
    )

  # To work for factor data

  side_factor           <- is.factor(dataOp$P_Side)
  prosttype_factor      <- is.factor(dataOp$P_ProstType)
  prep_hippainL_factor  <- is.factor(dataPREP$PREP_HipPainL)
  prep_hippainR_factor  <- is.factor(dataPREP$PREP_HipPainL)
  postp_hippainL_factor <- is.factor(dataPOSTP$POSTP_HipPainL)
  postp_hippainR_factor <- is.factor(dataPOSTP$POSTP_HipPainR)
  postp_satis_factor    <- is.factor(dataPOSTP$POSTP_Satisfaction)
  postp_satisL_factor   <- is.factor(dataPOSTP$POSTP_SatisfactionL)
  postp_satisR_factor   <- is.factor(dataPOSTP$POSTP_SatisfactionR)


  if (side_factor) {
    side_levels <- levels(dataOp$P_Side)
    dataOp$P_Side <- as.numeric(dataOp$P_Side)
  }

  if (prosttype_factor) {
    prosttype_levels <- levels(dataOp$P_ProstType)
    dataOp$P_ProstType <- as.numeric(dataOp$P_ProstType)
  }

  if (prep_hippainL_factor) {
    prep_hippainL_levels <- levels(dataPREP$PREP_HipPainL)
    dataPREP$PREP_HipPainL <- as.numeric(dataPREP$PREP_HipPainL)
  }

  if (prep_hippainR_factor) {
    prep_hippainR_levels <- levels(dataPREP$PREP_HipPainR)
    dataPREP$PREP_HipPainR <- as.numeric(dataPREP$PREP_HipPainR)
  }

  if (postp_hippainL_factor) {
    postp_hippainL_levels <- levels(dataPOSTP$POSTP_HipPainL)
    dataPOSTP$POSTP_HipPainL <- as.numeric(dataPOSTP$POSTP_HipPainL)
  }

  if (postp_hippainR_factor) {
    postp_hippainR_levels <- levels(dataPOSTP$POSTP_HipPainR)
    dataPOSTP$POSTP_HipPainR <- as.numeric(dataPOSTP$POSTP_HipPainR)
  }

  if (postp_satis_factor) {
    postp_satis_levels <- levels(dataPOSTP$POSTP_Satisfaction)
    dataPOSTP$POSTP_Satisfaction <- as.numeric(dataPOSTP$POSTP_Satisfaction)
  }


  if (postp_satisL_factor) {
    postp_satisL_levels <- levels(dataPOSTP$POSTP_SatisfactionL)
    dataPOSTP$POSTP_SatisfactionL <-
      as.numeric(dataPOSTP$POSTP_SatisfactionL)
  }


  if (postp_satisR_factor) {
    postp_satisR_levels <- levels(dataPOSTP$POSTP_SatisfactionR)
    dataPOSTP$POSTP_SatisfactionR <- as.numeric(dataPOSTP$POSTP_SatisfactionR)
  }



  # Add PROM to data

  # For preop:
  # Consider all primary operation DATES
  # All reoperations that occur before a primary operation (other side)

  # For postop:
  # Consider all primary operation DATES
  # Find the first reoperation (only current side is relevant for this)
  # This because PROM poll taking into bilateral operations into account
  # Need to filter out satisfaction if any reoperation occurs between PROM and primary

  reops <- dataOp %>%
    dplyr::filter(!is.na(R_SurgDate), P_ProstType == 1) %>%
    dplyr::select(SubjectKey, R_SurgDate)

  operations <- dataOp %>%
    # PROM only exist from 2002
    dplyr::filter(lubridate::year(P_SurgDate) >= primary_prom_start_year) %>%
    # For postop (reoperations on same side):
    dplyr::select(SubjectKey, P_SurgDate, R_SurgDate) %>%
    dplyr::arrange(SubjectKey, R_SurgDate) %>%
    dplyr::distinct(SubjectKey, P_SurgDate, .keep_all = TRUE) %>%
    dplyr::mutate(reop_time = as.numeric(difftime(R_SurgDate, P_SurgDate, units = "days"))) %>%
    dplyr::select(-R_SurgDate) %>%
    # For preop (reoperations on other side):
    dplyr::left_join(reops, by = "SubjectKey") %>%
    dplyr::mutate(prim_time = as.numeric(difftime(P_SurgDate, R_SurgDate, units = "days")),
                  prim_time = dplyr::case_when(prim_time >= 0 ~ prim_time)) %>%
    dplyr::arrange(SubjectKey, prim_time) %>%
    dplyr::distinct(SubjectKey, P_SurgDate, .keep_all = TRUE) %>%
    dplyr::select(SubjectKey, P_SurgDate, prim_time, reop_time)

  dataPREP <- dataPREP %>%
    dplyr::filter(PREP_Date <= DateOfDeath | is.na(DateOfDeath)) %>%
    dplyr::select(SubjectKey, dplyr::setdiff(names(dataPREP), names(dataOp))) %>%
    dplyr::mutate(PREP_Date = as.Date(PREP_Date, tz = "CET")) %>%
    # For people operating both hips there are sometimes two PROMs
    # This operation simply populates both hip pain in both those rows
    # This makes us be able to only use one of the PROMs
    dplyr::group_by(SubjectKey, PREP_Date) %>%
    dplyr::mutate_at(prep_vars, pick_valid) %>%
    dplyr::ungroup()

  dataPOSTP <- dataPOSTP %>%
    dplyr::filter(POSTP_Date <= DateOfDeath | is.na(DateOfDeath)) %>%
    dplyr::select(SubjectKey, dplyr::setdiff(names(dataPOSTP), names(dataOp))) %>%
    # Populate Satisfaction[L|R] using HipPain[L|R] and Satisfaction
    dplyr::mutate(
      POSTP_Date = as.Date(POSTP_Date, tz = "CET"),
      POSTP_SatisfactionL = dplyr::case_when(
        !is.na(POSTP_Satisfaction) &
          !is.na(POSTP_HipPainL) ~ POSTP_Satisfaction,
        TRUE ~ POSTP_SatisfactionL
      ),
      POSTP_SatisfactionR = dplyr::case_when(
        !is.na(POSTP_Satisfaction) &
          !is.na(POSTP_HipPainR) ~ POSTP_Satisfaction,
        TRUE ~ POSTP_SatisfactionR
      ),
      POSTP_Satisfaction = dplyr::case_when(
        is.na(POSTP_SatisfactionL) &
          is.na(POSTP_SatisfactionR) ~ POSTP_Satisfaction
      )
    ) %>%
    dplyr::group_by(SubjectKey, POSTP_Date) %>%
    dplyr::mutate_at(postp_vars, pick_valid) %>%
    dplyr::ungroup()


  if (prep_hippainL_factor) {
    dataPREP$PREP_HipPainL <-
      factor(dataPREP$PREP_HipPainL, labels = prep_hippainL_levels)
  }

  if (prep_hippainR_factor) {
    dataPREP$PREP_HipPainR <-
      factor(dataPREP$PREP_HipPainR, labels = prep_hippainR_levels)
  }

  if (postp_hippainL_factor) {
    dataPOSTP$POSTP_HipPainL <-
      factor(dataPOSTP$POSTP_HipPainL, labels = postp_hippainL_levels)
  }

  if (postp_hippainR_factor) {
    dataPOSTP$POSTP_HipPainR <-
      factor(dataPOSTP$POSTP_HipPainR, labels = postp_hippainR_levels)
  }

  if (postp_satis_factor) {
    dataPOSTP$POSTP_Satisfaction <-
      factor(dataPOSTP$POSTP_Satisfaction, labels = postp_satis_levels)
  }


  if (postp_satisL_factor) {
    dataPOSTP$POSTP_SatisfactionL <-
      factor(dataPOSTP$POSTP_SatisfactionL, labels = postp_satisL_levels)
  }


  if (postp_satisR_factor) {
    dataPOSTP$POSTP_SatisfactionR <-
      factor(dataPOSTP$POSTP_SatisfactionR, labels = postp_satisR_levels)
  }


  if (!is.null(cens_date)) {
    dataPREP <- dataPREP %>%
      dplyr::filter(PREP_Date <= as.Date(cens_date))

    dataPOSTP <- dataPOSTP %>%
      dplyr::filter(POSTP_Date <= as.Date(cens_date))
  }

  pre_prom <- dataPREP %>%
    # Find the proms that is likely to be attached to that operation by using left_join
    dplyr::left_join(operations, by = "SubjectKey") %>%
    dplyr::mutate(reg_time = as.numeric(difftime(P_SurgDate, PREP_Date, units = "days"))) %>%
    # Consider only PROMs that was made at most 180 days before operation
    # (decided by the register directors)
    dplyr::filter(dplyr::between(reg_time, 0, 180)) %>%
    # Take the PROM which is closest to the operation date
    # One PROM per operation date
    # Can exist same PROM for both proms
    dplyr::arrange(SubjectKey, P_SurgDate, reg_time) %>%
    dplyr::distinct(SubjectKey, P_SurgDate, .keep_all = TRUE) %>%
    # If a reop exist between primary and PROM, remove it
    dplyr::filter(prim_time > reg_time | is.na(prim_time)) %>%
    dplyr::select(-prim_time,-reg_time,-reop_time)

  post_prom <- dataPOSTP %>%
    dplyr::left_join(operations, by = "SubjectKey") %>%
    dplyr::mutate(reg_time = as.numeric(difftime(POSTP_Date, P_SurgDate, units = "days")))


  # Add preoperative PROM to operation data
  dataOp <- dplyr::left_join(dataOp, pre_prom, by = c("SubjectKey", "P_SurgDate"))


  # Add postoperative 1 year PROM to operation data
  post_prom1 <- post_prom %>%
    dplyr::filter(dplyr::between(reg_time, 365 - 90, 365 + 180)) %>%
    dplyr::mutate(reg_time = abs(365 - reg_time)) %>%
    # Take the PROM closest to 1 year follow-up
    dplyr::arrange(SubjectKey, P_SurgDate, reg_time) %>%
    dplyr::distinct(SubjectKey, P_SurgDate, .keep_all = TRUE) %>%
    # Satisfaction is for the latest operation
    dplyr::mutate_at(satis_vars, list( ~ dplyr::case_when(
      reop_time > reg_time |
        is.na(reop_time) ~ .
    ))) %>%
    dplyr::select(-prim_time,-reg_time,-reop_time)

  names(post_prom1)  <- paste(names(post_prom1), '1yr', sep = '_')

  dataOp <-
    dplyr::left_join(
      dataOp,
      post_prom1,
      by = c("SubjectKey" = "SubjectKey_1yr", "P_SurgDate" = "P_SurgDate_1yr")
    )

  # Add postoperative 6 year PROM to operation data

  post_prom6 <- post_prom %>%
    dplyr::filter(dplyr::between(reg_time, 6 * 365 - 365, 6 * 365 + 365)) %>%
    dplyr::mutate(reg_time = abs(6 * 365 - reg_time)) %>%
    dplyr::arrange(SubjectKey, P_SurgDate, reg_time) %>%
    dplyr::distinct(SubjectKey, P_SurgDate, .keep_all = TRUE) %>%
    # Satisfaction is for the latest operation
    dplyr::mutate_at(satis_vars, list( ~ dplyr::case_when(
      reop_time > reg_time |
        is.na(reop_time) ~ .
    ))) %>%
    dplyr::select(-prim_time,-reg_time,-reop_time)

  names(post_prom6)  <- paste(names(post_prom6), '6yrs', sep = '_')

  dataOp <-
    dplyr::left_join(
      dataOp,
      post_prom6,
      by = c("SubjectKey" = "SubjectKey_6yrs",
             "P_SurgDate" = "P_SurgDate_6yrs")
    )

  # Add postoperative 10 year PROM to operation data
  post_prom10 <- post_prom %>%
    dplyr::filter(dplyr::between(reg_time, 10 * 365 - 365, 10 * 365 + 365)) %>%
    dplyr::mutate(reg_time = abs(10 * 365 - reg_time)) %>%
    dplyr::arrange(SubjectKey, P_SurgDate, reg_time) %>%
    dplyr::distinct(SubjectKey, P_SurgDate, .keep_all = TRUE) %>%
    # Satisfaction is for the latest operation
    dplyr::mutate_at(satis_vars, list( ~ dplyr::case_when(
      reop_time > reg_time |
        is.na(reop_time) ~ .
    ))) %>%
    dplyr::select(-prim_time,-reg_time,-reop_time)

  names(post_prom10)  <-
    paste(names(post_prom10), '10yrs', sep = '_')

  dataOp <-
    dplyr::left_join(
      dataOp,
      post_prom10,
      by = c("SubjectKey" = "SubjectKey_10yrs", "P_SurgDate" = "P_SurgDate_10yrs")
    )

  # Make one pain/satisfaction variable for each year
  # If right hip is operated,
  # we are only interested in pain/satisfaction for right hip
  # in this analysis

  dataOp <- dataOp %>%
    dplyr::mutate(
      PREP_HipPain = dplyr::case_when(P_Side == 1 ~ PREP_HipPainR,
                               P_Side == 2 ~ PREP_HipPainL),
      POSTP_HipPain_1yr = dplyr::case_when(
        P_Side == 1 ~ POSTP_HipPainR_1yr,
        P_Side == 2 ~ POSTP_HipPainL_1yr
      ),
      POSTP_HipPain_6yrs = dplyr::case_when(
        P_Side == 1 ~ POSTP_HipPainR_6yrs,
        P_Side == 2 ~ POSTP_HipPainL_6yrs
      ),
      POSTP_HipPain_10yrs = dplyr::case_when(
        P_Side == 1 ~ POSTP_HipPainR_10yrs,
        P_Side == 2 ~ POSTP_HipPainL_10yrs
      ),
      POSTP_Satisfaction_1yr = dplyr::case_when(
        !is.na(POSTP_Satisfaction_1yr) ~ POSTP_Satisfaction_1yr,
        P_Side == 1 ~ POSTP_SatisfactionR_1yr,
        P_Side == 2 ~ POSTP_SatisfactionL_1yr
      ),
      POSTP_Satisfaction_6yrs = dplyr::case_when(
        !is.na(POSTP_Satisfaction_6yrs) ~ POSTP_Satisfaction_6yrs,
        P_Side == 1 ~ POSTP_SatisfactionR_6yrs,
        P_Side == 2 ~ POSTP_SatisfactionL_6yrs
      ),
      POSTP_Satisfaction_10yrs = dplyr::case_when(
        !is.na(POSTP_Satisfaction_10yrs) ~ POSTP_Satisfaction_10yrs,
        P_Side == 1 ~ POSTP_SatisfactionR_10yrs,
        P_Side == 2 ~ POSTP_SatisfactionL_10yrs
      )
    )

  if (add_reop_prom) {
    # Need to extract the primary closest to the reoperation to make sure PROM is not connected to any primary
    # If ANY operation happens between PROM and reoperation, the PROM is not valid as reoperation PROM.
    # Therefore need the time between the closest primary to the reoperation.

    reops <- dataOp %>%
      dplyr::filter(
        lubridate::year(R_SurgDate) >= reop_prom_start_year,
        !is.na(R_SurgDate),
        P_ProstType == 1
      ) %>%
      dplyr::select(SubjectKey, P_Side, R_SurgDate)

    right_hips <- dplyr::filter(dataOp, P_Side == 1) %>%
      dplyr::distinct(SubjectKey, P_SurgDate) %>%
      dplyr::rename(P_SurgDate_R = P_SurgDate)

    left_hips <-  dplyr::filter(dataOp, P_Side == 2) %>%
      dplyr::distinct(SubjectKey, P_SurgDate) %>%
      dplyr::rename(P_SurgDate_L = P_SurgDate)

    both_hips <- dplyr::full_join(right_hips, left_hips, by = "SubjectKey")
    reops <- dplyr::left_join(reops, both_hips, by = "SubjectKey")

    reops <-
      dplyr::mutate(reops,
        reop_time_R = as.numeric(difftime(R_SurgDate, P_SurgDate_R, units = "days")),
        reop_time_L = as.numeric(difftime(R_SurgDate, P_SurgDate_L, units = "days")),
        reop_time_R = dplyr::case_when(reop_time_R >= 0 ~ reop_time_R),
        reop_time_L = dplyr::case_when(reop_time_L >= 0 ~ reop_time_L),
        reop_time   = pmin(reop_time_R, reop_time_L, na.rm = TRUE)) %>%
      dplyr::select(SubjectKey, P_Side, R_SurgDate, reop_time)

    all_reops <- reops %>%
      dplyr::distinct(SubjectKey, P_Side, R_SurgDate, .keep_all = TRUE) %>%
      # Attach next reoperation to same row
      dplyr::arrange(SubjectKey, P_Side, R_SurgDate) %>%
      dplyr::group_by(SubjectKey, P_Side) %>%
      dplyr::mutate(R_SurgDate2 = dplyr::lead(R_SurgDate)) %>%
      dplyr::ungroup() %>%
      # Calculate time to the next reoperation
      dplyr::mutate(op_time = as.numeric(difftime(R_SurgDate2, R_SurgDate, units = "days"))) %>%
      dplyr::arrange(SubjectKey, R_SurgDate, R_SurgDate2) %>%
      dplyr::distinct(SubjectKey, R_SurgDate, .keep_all = TRUE) %>%
      dplyr::select(-P_Side,-R_SurgDate2)

    pre_prom <- dataPREP %>%
      # Find the proms that is likely to be attached to that operation by using left_join
      dplyr::left_join(all_reops, by = "SubjectKey") %>%
      dplyr::mutate(reg_time = as.numeric(difftime(R_SurgDate, PREP_Date, units = "days"))) %>%
      # Consider only PROMs that was made at most 180 days before operation
      # (decided by the register directors)
      dplyr::filter(dplyr::between(reg_time, 0, 180),
                    # No operation between the PROM and the reoperation
                    reop_time > reg_time) %>%
      # Take the PROM which is closest to the operation date
      # Only one PROM per operation date
      dplyr::arrange(SubjectKey, reg_time) %>%
      # Two distincts to make sure the closest reoperation is the one getting the preop PROM
      dplyr::distinct(SubjectKey, R_SurgDate, .keep_all = TRUE) %>%
      dplyr::distinct(SubjectKey, PREP_Date, .keep_all = TRUE) %>%
      dplyr::select(-reop_time,-op_time)

    names(pre_prom)  <- paste(names(pre_prom), 'reop', sep = '_')

    dataOp <-
      dplyr::left_join(
        dataOp,
        pre_prom,
        by = c("SubjectKey" = "SubjectKey_reop",
               "R_SurgDate" = "R_SurgDate_reop")
      )


    post_prom <- dataPOSTP %>%
      dplyr::left_join(all_reops, by = "SubjectKey") %>%
      dplyr::mutate(reg_time = as.numeric(difftime(POSTP_Date, R_SurgDate, units = "days")))

    post_prom1 <- post_prom %>%
      dplyr::filter(dplyr::between(reg_time, 365 - 90, 365 + 180)) %>%
      dplyr::mutate(reg_time = abs(365 - reg_time)) %>%
      dplyr::arrange(SubjectKey, R_SurgDate, reg_time) %>%
      dplyr::distinct(SubjectKey, R_SurgDate, .keep_all = TRUE) %>%
      # Satisfaction is for the latest operation
      dplyr::mutate_at(satis_vars, list( ~ dplyr::case_when(
        op_time > reg_time |
          is.na(reop_time) ~ .
      ))) %>%
      dplyr::select(-reg_time,-reop_time,-op_time)

    names(post_prom1)  <-
      paste(names(post_prom1), 'reop1yr', sep = '_')


    dataOp <-
      dplyr::left_join(
        dataOp,
        post_prom1,
        by = c("SubjectKey" = "SubjectKey_reop1yr",
               "R_SurgDate" = "R_SurgDate_reop1yr")
      )

    dataOp <- dataOp %>%
      dplyr::mutate(
        PREP_HipPain_reop = dplyr::case_when(
          P_Side == 1 ~ PREP_HipPainR_reop,
          P_Side == 2 ~ PREP_HipPainL_reop
        ),
        POSTP_HipPain_reop1yr = dplyr::case_when(
          P_Side == 1 ~ POSTP_HipPainR_reop1yr,
          P_Side == 2 ~ POSTP_HipPainL_reop1yr
        ),
        POSTP_Satisfaction_reop1yr = dplyr::case_when(
          !is.na(POSTP_Satisfaction_reop1yr) ~ POSTP_Satisfaction_reop1yr,
          P_Side == 1 ~ POSTP_SatisfactionR_reop1yr,
          P_Side == 2 ~ POSTP_SatisfactionL_reop1yr
        )
      )

    # When 6 year reop prom exist

    if (lubridate::year(Sys.Date()) >= 2023) {
      post_prom6 <- post_prom %>%
        dplyr::filter(dplyr::between(reg_time, 6 * 365 - 365, 6 * 365 + 365)) %>%
        dplyr::mutate(reg_time = abs(6 * 365 - reg_time)) %>%
        dplyr::arrange(SubjectKey, R_SurgDate, reg_time) %>%
        dplyr::distinct(SubjectKey, R_SurgDate, .keep_all = TRUE) %>%
        # Satisfaction is for the latest operation
        dplyr::mutate_at(satis_vars, list( ~ dplyr::case_when(
          op_time > reg_time |
            is.na(reop_time) ~ .
        ))) %>%
        dplyr::select(-reg_time,-reop_time,-op_time)

      names(post_prom6)  <-
        paste(names(post_prom6), 'reop6yrs', sep = '_')

      dataOp <-
        dplyr::left_join(
          dataOp,
          post_prom6,
          by = c("SubjectKey" = "SubjectKey_reop6yrs",
                 "R_SurgDate" = "R_SurgDate_reop6yrs")
        )

      dataOp <- dataOp %>%
        dplyr::mutate(
          POSTP_HipPain_reop6yrs = dplyr::case_when(
            P_Side == 1 ~ POSTP_HipPainR_reop6yrs,
            P_Side == 2 ~ POSTP_HipPainL_reop6yrs
          ),
          POSTP_Satisfaction_reop6yrs = dplyr::case_when(
            !is.na(POSTP_Satisfaction_reop6yrs) ~ POSTP_Satisfaction_reop6yrs,
            P_Side == 1 ~ POSTP_SatisfactionR_reop6yrs,
            P_Side == 2 ~ POSTP_SatisfactionL_reop6yrs
          )
        )
    }

    # When 10 year reop prom exist

    if (lubridate::year(Sys.Date()) >= 2027) {
      post_prom10 <- post_prom %>%
        dplyr::filter(dplyr::between(reg_time, 10 * 365 - 365, 10 * 365 + 365)) %>%
        dplyr::mutate(reg_time = abs(10 * 365 - reg_time)) %>%
        dplyr::arrange(SubjectKey, R_SurgDate, reg_time) %>%
        dplyr::distinct(SubjectKey, R_SurgDate, .keep_all = TRUE) %>%
        # Satisfaction is for the latest operation
        dplyr::mutate_at(satis_vars, list( ~ dplyr::case_when(
          op_time > reg_time |
            is.na(reop_time) ~ .
        ))) %>%
        dplyr::select(-reg_time,-reop_time,-op_time)

      names(post_prom10)  <-
        paste(names(post_prom10), 'reop10yrs', sep = '_')

      dataOp <-
        dplyr::left_join(
          dataOp,
          post_prom10,
          by = c("SubjectKey" = "SubjectKey_reop10yrs",
                 "R_SurgDate" = "R_SurgDate_reop10yrs")
        )

      dataOp <- dataOp %>%
        dplyr::mutate(
          POSTP_HipPain_reop10yrs = dplyr::case_when(
            P_Side == 1 ~ POSTP_HipPainR_reop10yrs,
            P_Side == 2 ~ POSTP_HipPainL_reop10yrs
          ),
          POSTP_Satisfaction_reop10yrs = dplyr::case_when(
            !is.na(POSTP_Satisfaction_reop10yrs) ~ POSTP_Satisfaction_reop10yrs,
            P_Side == 1 ~ POSTP_SatisfactionR_reop10yrs,
            P_Side == 2 ~ POSTP_SatisfactionL_reop10yrs
          )
        )
    }
  }

  if (side_factor) {
    dataOp$P_Side <- factor(dataOp$P_Side, labels = side_levels)
  }

  if (prosttype_factor) {
    dataOp$P_ProstType <- factor(dataOp$P_ProstType, labels = prosttype_levels)
  }

  return(dataOp)
}
