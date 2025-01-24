desc_stats <- function(data, var_med_iqr, outlier_per_batch = FALSE){
  
  # Functions to handle numeric p-value for different numbers of groups
  numeric_p_value_fn <- function(data){
    if(length(unique(data$batch)) <= 2){
      t.test(
        x = filter(data, batch == levels(batch)[1])$value
        , y = filter(data, batch == levels(batch)[2])$value
        , paired = FALSE
      )$p.value
    }else{
      summary(aov(value ~ batch, data = data))[[1]][["Pr(>F)"]][1]
    }
  }
  
  numeric_p_value2_fn <- function(data){
    if(length(unique(data$batch))<=2){
      wilcox.test(
        x = filter(data, batch == levels(batch)[1])$value
        , y = filter(data, batch == levels(batch)[2])$value
        , paired = FALSE
      )$p.value
    }else{
      kruskal.test(value ~ batch, data = data)$p.value
    }
  }
  
  format_p_N_p_value <- function(data){
    data |>
      group_by(batch, variable) |>
      mutate(N = sum(n)) |>
      ungroup() |>
      mutate(
        p = round(n / N * 100, 2)
        , p = paste0("(", sprintf("%.2f", p), ")")
        , N = paste0("(n=", N,")")
        , p_value =
            ifelse(
              p_value < 0.001
              , "<0.001"
              , ifelse(p_value>0.05, ">0.05", sprintf("%.3f", p_value))
            )
      )
  }
  
  # Intersect available variables with var_med_iqr
  var_med_iqr <- intersect(colnames(data), var_med_iqr)
  
  # Functions to compute descriptive statistics
  desc_stats_factor <- function(colname, data){
    data_factor <-
      data |>
      select_at(c("batch", colname)) |>
      mutate(variable = colname) |>
      rename_at(colname, \(x) "category") |>
      mutate_at("batch", as.factor)
    
    data_factor <-
      data_factor |>
      cbind(
        filter(data_factor, !is.na(category)) |>
          summarize(
            p_value =
              ifelse(
                length(unique(category)) == 1
                , NA
                , fisher.test(batch, category, simulate.p.value = TRUE)$p.value
              )
          )
      )
    
    data_factor <-
      data_factor |>
      cbind(
        mutate(data_factor, category = !is.na(category)) |>
          summarize(
            p_value2 =
              ifelse(
                length(unique(category)) == 1
                , NA
                , fisher.test(batch, category, simulate.p.value = TRUE)$p.value
              )
          )
      )
    
    data_factor |>
      group_by(batch, variable, category, p_value, p_value2) |>
      summarize(n = n(), .groups = "drop") |>
      mutate(p_value = ifelse(is.na(category), p_value2, p_value)) |>
      select(-p_value2) |>
      format_p_N_p_value() |>
      unite(n_p, n, p, sep = " ") |>
      unite(batch_N, batch, N, sep = "\n") |>
      spread(batch_N, n_p, fill = "0 (0.00)") |>
      arrange(variable, category) |>
      mutate_at("category", as.character) |>
      mutate(
        category = ifelse(is.na(category), "Missing", category)
        ,category = paste0(category, " - n (%)")
      )
  }
  
  desc_stats_numeric <- function(colname, data){
    data_numeric <-
      data |>
      select_at(c("batch", colname)) |>
      mutate(variable = colname) |>
      rename_at(colname, \(x) "value") |>
      mutate_at("batch", as.factor) |>
      mutate(
        outlier_per_batch_ = outlier_per_batch
        , batch2 = ifelse(outlier_per_batch_, batch, "non-batch")
      ) |>
      group_by(batch2) |>
      mutate(
        med = median(value, na.rm = TRUE)
        , q1 = quantile(value, 0.25, na.rm = TRUE)
        , q3 = quantile(value, 0.75, na.rm = TRUE)
        , min = min(value, na.rm = TRUE)
        , max = max(value, na.rm = TRUE)
      ) |>
      ungroup() |>
      select(-outlier_per_batch_, -batch2) |>
      mutate(
        category =
          ifelse(
            is.na(value)
            , "Missing"
            , ifelse(
                variable %in% var_med_iqr
                , "Non-missing"
                , ifelse(
                    value < (q1 - 1.5 * (q3 - q1))
                    | value > (q3 + 1.5 * (q3 - q1))
                    ,"Outlier", "Non-missing, non-outlier"
                  )
              )
          ) |>
          factor(
            c("Non-missing, non-outlier", "Non-missing", "Outlier", "Missing")
          )
        , value =
            ifelse(
              category %in% c("Non-missing, non-outlier", "Non-missing")
              , value, NA
            )
      ) |>
      select(-q1, -q3)
    
    data_numeric_non_missing_outlier <-
      filter(
        data_numeric
        , category %in% c("Non-missing, non-outlier", "Non-missing")
      )
    
    data_numeric <-
      data_numeric |>
      cbind(
        data_numeric_non_missing_outlier |>
          summarize(
            p_value =
              numeric_p_value_fn(data = data_numeric_non_missing_outlier)
          )
      ) |>
      cbind(
        data_numeric_non_missing_outlier |>
          summarize(
            p_value2 =
              numeric_p_value2_fn(data = data_numeric_non_missing_outlier)
          )
      )
    
    data_numeric <-
      data_numeric |>
      cbind(
        mutate(
            data_numeric
            , category =
              category
              %in% c("Non-missing, non-outlier", "Non-missing", "Outlier")
          ) |>
          summarize(
            p_value3 =
              ifelse(
                length(unique(category)) == 1
                , NA
                , fisher.test(batch, category, simulate.p.value = TRUE)$p.value
              )
          )
      )
    
    data_numeric <-
      data_numeric |>
      cbind(
        mutate(
            data_numeric
            , category =
              category %in% c("Non-missing, non-outlier","Non-missing")
          ) |>
          summarize(
            p_value4 =
              ifelse(
                length(unique(category)) == 1
                , NA
                , fisher.test(batch, category, simulate.p.value = TRUE)$p.value
              )
          )
      ) |>
      mutate(p_value4 = ifelse(variable %in% var_med_iqr, NA, p_value4)) |>
      group_by(
        batch, variable, category, med, min, max
        , p_value, p_value2, p_value3, p_value4
      ) |>
      summarize(
        n = n()
        , avg = mean(value, na.rm = TRUE)
        , std = sd(value, na.rm = TRUE)
        , ci = qnorm(0.975) * std / sqrt(n)
        , lb = avg - ci
        , ub = avg + ci
        , .groups = "drop"
      ) |>
      mutate(
        avg = ifelse(category == "Non-missing, non-outlier", avg, med)
        , lb = ifelse(category == "Non-missing, non-outlier", lb, min)
        , ub = ifelse(category == "Non-missing, non-outlier", ub, max)
      ) |>
      select(-std, -ci, -med, -min, -max) |>
      mutate(
        p_value =
          ifelse(
            category %in% c("Non-missing, non-outlier", "Non-missing")
            , ifelse(category == "Non-missing, non-outlier", p_value, p_value2)
            , ifelse(category %in% c("Missing"), p_value3, p_value4)
          )
      ) |>
      select(-p_value2, -p_value3, -p_value4) |>
      format_p_N_p_value() |>
      mutate_at(
        c("avg", "lb", "ub"), \(x) ifelse(is.nan(x), NA, sprintf("%.2f", x))
      ) |>
      unite(n_p, n, p, sep = " ") |>
      unite(ci, lb, ub, sep = ", ") |>
      mutate(ci = paste0("(", ci, ")")) |>
      unite(avg_ci, avg, ci, sep = " ") |>
      mutate(
        n_p =
          ifelse(
            category %in% c("Non-missing, non-outlier", "Non-missing")
            , avg_ci, n_p
          )
      ) |>
      select(-avg_ci) |>
      unite(batch_N, batch, N, sep = "\n") |>
      spread(batch_N, n_p, fill = "0 (0.00)") |>
      arrange(variable, category) |>
      mutate_at("category", as.character) |>
      mutate(
        category=
          ifelse(
            category %in% c("Non-missing, non-outlier","Non-missing")
            , ifelse(
                category == "Non-missing, non-outlier"
                , "Avg. (95% CI)", "Med. (min.-max.)"
              )
            , paste0(category, " - n (%)")
          )
      )
  }
  
  # Compute descriptive statistics
  desc0 <-
    data |>
    select(-id)
  
  data_batch_factor <-
    data |>
    select_if(
      colnames(data) == "batch"
      | sapply(data, class) == "factor"
    )
  
  data_batch_numeric <-
    data |>
    select_if(
      colnames(data) == "batch"
      | sapply(data, class) == "numeric"
    )
  
  desc0 <-
    list(
      factor=
        colnames(data_batch_factor)[colnames(data_batch_factor) != "batch"] |>
        lapply(\(x) desc_stats_factor(x, data_batch_factor)) |>
        reduce(rbind)
      ,numeric=
        colnames(data_batch_numeric)[colnames(data_batch_numeric) != "batch"] |>
        lapply(\(x) desc_stats_numeric(x, data_batch_numeric)) |>
        reduce(rbind)
    ) |>
    reduce(rbind)
  
  desc0 <-
    desc0 |>
    select_at(c(colnames(desc0)[colnames(desc0) != "p_value"], "p_value")) |>
    arrange(factor(variable, colnames(data)))
  
  # Compute descriptive statistics for med-iqr
  desc0_var_med_iqr <-
    data |>
    select_at(c("batch", var_med_iqr)) |>
    gather(variable, value, -batch) |>
    mutate(
      category =
        ifelse(is.na(value), "Missing", ifelse(value > 0, ">0", "0")) |>
        factor(c("0", ">0", "Missing"))
    )
  
  desc0_var_med_iqr <-
    rbind(
      desc0_var_med_iqr |>
        filter(category != "Missing") |>
        group_by(batch, variable, category) |>
        summarize(
          n = n()
          , med = median(value, na.rm = TRUE)
          , min = min(value, na.rm = TRUE)
          , max = max(value, na.rm = TRUE)
          , .groups = "drop"
        )
      , desc0_var_med_iqr |>
        filter(category == "Missing") |>
        group_by(batch, variable, category) |>
        summarize(n = n(), med = NA , min = NA, max = NA, .groups="drop")
    ) |>
    mutate(p_value = NA) |>
    format_p_N_p_value() |>
    select(-p_value) |>
    mutate_at(
      c("med", "min", "max"), \(x) ifelse(is.nan(x), NA ,sprintf("%.2f", x))
    ) |>
    unite(n_p, n, p, sep = " ") |>
    unite(min_max, min, max, sep = ", ") |>
    mutate(min_max = paste0("(", min_max, ")")) |>
    unite(med_min_max, med,min_max, sep = " ") |>
    mutate(n_p = ifelse(category %in% c(">0"), med_min_max, n_p)) |>
    select(-med_min_max) |>
    unite(batch_N, batch, N, sep = "\n") |>
    spread(batch_N, n_p, fill = "0 (0.00)") |>
    arrange(variable, category) |>
    mutate_at("category", as.character) |>
    mutate(
      category =
        ifelse(
          category %in% c(">0")
          , paste0(category, " - Med. (min.-max.)")
          , paste0(category, " - n (%)")
        )
    )
  
  desc0_var_med_iqr2 <-
    desc0 |>
    filter(variable%in%var_med_iqr)
  
  desc0_var_med_iqr2 <-
    rbind(
      desc0_var_med_iqr2 |>
        filter(category == "Med. (min.-max.)") |>
        mutate(category = "0 - n (%)")
      , desc0_var_med_iqr2 |>
        filter(category == "Med. (min.-max.)") |>
        mutate(category = ">0 - Med. (min.-max.)")
      , desc0_var_med_iqr2 |>
        filter(category != "Med. (min.-max.)")
    ) |>
    select(variable, category, p_value)
  
  # Combine the descriptive statistics
  desc1 <-
    desc0_var_med_iqr |>
    left_join(desc0_var_med_iqr2, by = join_by(variable, category)) |>
    rbind(filter(desc0, !variable %in% var_med_iqr)) |>
    arrange(factor(variable, unique(desc0$variable))) |>
    filter(variable != "id")
  
  desc1
}