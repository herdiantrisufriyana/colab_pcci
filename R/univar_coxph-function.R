univar_coxph <- function(data, covariates = NULL){
  coxph_data <- function(data, variable){
    suppressWarnings(coxph(
      paste0(
          "Surv(interview_onset, outcome, type = 'left')~"
          , paste0(c(variable, covariates), collapse = "+")
        ) |>
        as.formula()
      , data = data
      , id = id
    ))
  }
  colnames(data)[!colnames(data) %in%non_indep] |>
    lapply(
      \(x)
      data |>
        select_at(c(non_indep, c(x, covariates))) |>
        mutate_at("outcome", as.factor) |>
        coxph_data(x) |>
        tidy() |>
        filter(str_detect(term, paste0("^", x))) |>
        mutate(
          variable = x
          , term = str_remove_all(term, paste0("^", variable))
          , reference =
            ifelse(is.factor(data[[x]]), levels(data[[x]])[1], "")
        )
    ) |>
    reduce(rbind) |>
    mutate(
      HR = sprintf("%.3f", exp(estimate))
      , LB = sprintf("%.3f", exp(estimate - qnorm(0.975) * std.error))
      , UB = sprintf("%.3f", exp(estimate + qnorm(0.975) * std.error))
      , p_value =
        ifelse(
          p.value < 0.001
          , "<0.001", ifelse(p.value > 0.05, ">0.05", sprintf("%.3f", p.value))
        )
    ) |>
    select(variable, term, reference, HR, LB, UB, p_value) |>
    unite(term_reference, term, reference, sep = " vs. ") |>
    mutate_at("term_reference", str_remove_all, "^ vs. $")
}