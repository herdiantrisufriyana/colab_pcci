univar_lr <- function(data){
  glm_data <- function(data, variable){
    suppressWarnings(
      glm(
        as.formula(paste0("outcome~", variable))  
        , family=binomial(link = "logit")
        , data = data
      )
    )
  }
  
  colnames(data)[!colnames(data) %in% non_indep] |>
    lapply(
      \(x)
      data |>
        select_at(c(non_indep, x)) |>
        mutate_at("outcome", as.factor) |>
        glm_data(x) |>
        tidy() |>
        filter(term != "(Intercept)") |>
        mutate(
          variable = x
          , term = str_remove_all(term, paste0("^", variable))
          , reference = ifelse(is.factor(data[[x]]), levels(data[[x]])[1], "")
        )
    ) |>
    reduce(rbind) |>
    mutate(
      OR = sprintf("%.3f", exp(estimate))
      , LB = sprintf("%.3f", exp(estimate - qnorm(0.975) * std.error))
      , UB = sprintf("%.3f", exp(estimate + qnorm(0.975) * std.error))
      , p_value =
          ifelse(
            p.value < 0.001
            , "<0.001"
            , ifelse(p.value > 0.05, ">0.05", sprintf("%.3f", p.value))
          )
    ) |>
    select(variable, term, reference, OR, LB, UB, p_value) |>
    unite(term_reference, term, reference, sep = " vs. ") |>
    mutate_at("term_reference", str_remove_all, "^ vs. $")
}