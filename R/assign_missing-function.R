assign_missing <-function(data, var_med_iqr){
  # Intersect available variables with var_med_iqr
  var_med_iqr <-
    data |>
    colnames() |>
    intersect(var_med_iqr)
  
  # Assign missing to outliers
  var_med_iqr |>
    lapply(
      \(x)
      data |>
        select_at(x) |>
        mutate(variable = x) |>
        rename_at(x, \(x) "value") |>
        mutate(
          q1 = quantile(value, 0.25, na.rm = TRUE)
          , q3 = quantile(value, 0.75, na.rm = TRUE)
        ) |>
        mutate(
          value =
            ifelse(
              is.na(value)
              , NA
              , ifelse(
                  variable %in% var_med_iqr
                  , value
                  , ifelse(
                      value < (q1 - 1.5 * (q3 - q1))
                      | value > (q3 + 1.5 * (q3 - q1))
                      , NA, value
                    )
                )
            )
        ) |>
        select_at("value") |>
        rename_at("value", \(y) x)
    ) |>
    reduce(cbind) |>
    cbind(select_if(data, !colnames(data) %in% var_med_iqr)) |>
    select_at(colnames(data))
}