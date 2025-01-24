intervar_lr <- function(unicox_sum, data){
  variable <-
    expand.grid(
      x = unique(unicox_sum$variable)
      , y =
        unicox_sum |>
        filter(type == "Independent variable") |>
        pull(variable) |>
        unique()
      , stringsAsFactors = FALSE
    ) |>
    filter(x != y) |>
    unite(y_x, y, x, sep = "~") |>
    pull(y_x)
  
  variable |>
    `names<-`(as.character(variable)) |>
    lapply(as.formula) |>
    lapply(
      \(x)
      x |>
        glm(family = binomial(link = "logit"), data = data) |>
        tidy() |>
        filter(term != "(Intercept)") |>
        mutate(
          outcome = as.character(x)[2]
          , variable = as.character(x)[3]
          , term = str_remove_all(term, paste0("^", as.character(x)[3]))
          , reference =
              ifelse(
                is.factor(data[[as.character(x)[3]]])
                , levels(data[[as.character(x)[3]]])[1]
                , ""
              )
        )
    ) |>
    reduce(rbind) |>
    mutate(
      OR = sprintf("%.3f", exp(estimate))
      , LB = sprintf("%.3f", exp(estimate - qnorm(0.975) * std.error))
      , UB = sprintf("%.3f", exp(estimate + qnorm(0.975) * std.error))
      , p_value=
          ifelse(
            p.value < 0.001
            , "<0.001", ifelse(p.value > 0.05, ">0.05", sprintf("%.3f",p.value))
          )
    ) |>
    select(outcome, variable, term, reference, OR, LB, UB, p_value) |>
    unite(term_reference, term, reference, sep = " vs. ") |>
    mutate_at("term_reference", str_remove_all, "^ vs. $")
}