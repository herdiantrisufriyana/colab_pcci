multivar_coxph <-
  function(
    data
    , causal_factor
    , covariates = NULL
    , unicox_result_df
  ){
    multivar_data <-
      data |>
      select_at(c(non_indep, c(causal_factor, covariates))) |>
      mutate_at("outcome", as.factor)
    
    if(length(covariates) > 1){
      multivar_data_covariate <-
        multivar_data |>
        select_at(covariates)
      
      if(sum(sapply(multivar_data_covariate, is.factor)) > 0){
        sig_covariate <-
          unicox_result_df |>
          filter(variable %in% covariates) |>
          filter(str_detect(term_reference, "vs\\.")) |>
          # filter(!(LB == "0.000" & UB == "Inf")) |>
          # filter(sum(p_value != ">0.05") > 0) |>
          # filter(as.numeric(LB) > 1 | as.numeric(UB) < 1) |>
          separate(term_reference, c("term", "reference"), sep = " vs\\.") |>
          unite(variable, variable, term, sep = "=") |>
          pull(variable)
        
        multivar_data_covariate_factor <-
          multivar_data_covariate[
            , sapply(multivar_data_covariate, is.factor), drop = FALSE
          ]
        
        multivar_data_covariate_factor_mod <-
          multivar_data_covariate_factor |>
          mutate_all(as.character) |>
          mutate(seq = seq(n())) |>
          gather(variable, value, -seq) |>
          unite(variable, variable, value, sep = "=") |>
          mutate(variable = factor(variable, unique(variable)), value = 1) |>
          spread(variable, value, fill = 0) |>
          arrange(seq) |>
          select(-seq) |>
          select_at(sig_covariate)
      }
      
      if(sum(sapply(multivar_data_covariate, is.numeric)) > 0){
        multivar_data_covariate_numeric <-
          multivar_data_covariate[
            , sapply(multivar_data_covariate, is.numeric), drop = FALSE
          ]
      }
      
      if(sum(sapply(multivar_data_covariate, is.factor)) > 0
         & sum(sapply(multivar_data_covariate, is.numeric)) > 0
      ){
        multivar_data_covariate_mod <-
          multivar_data_covariate_numeric |>
          cbind(multivar_data_covariate_factor_mod)
      }else if(sum(sapply(multivar_data_covariate,is.factor)) > 0){
        multivar_data_covariate_mod <- multivar_data_covariate_factor_mod
      }else{
        multivar_data_covariate_mod <- multivar_data_covariate_numeric
      }
      
      num_cols <-
        multivar_data_covariate |>
        sapply(is.numeric)
      
      num_cols <- names(num_cols[num_cols])
      
      pca_model <-
        multivar_data_covariate_mod |>
        mutate_if(colnames(multivar_data_covariate_mod) %in% num_cols, scale) |>
        prcomp(retx = TRUE)
      
      selected_pc <- select(as.data.frame(pca_model$x), PC1)
      
      multivar_data_mod <-
        multivar_data |>
        select_at(
          colnames(multivar_data)[!colnames(multivar_data) %in%covariates]
        ) |>
        cbind(selected_pc)
      
      covariates <- colnames(selected_pc)
    }else{
      pca_model <- NULL
      multivar_data_mod <- multivar_data
    }
    
    results <-
      suppressWarnings(
        coxph(
          paste0(
              "Surv(interview_onset,outcome,type='left')~"
              , paste0(c(causal_factor, covariates), collapse="+")
            ) |>
            as.formula()
          , data = multivar_data_mod
          , id = id
        )
      ) |>
      tidy() |>
      filter(str_detect(term, paste0("^", causal_factor))) |>
      mutate(
        variable = causal_factor
        , term = str_remove_all(term, paste0("^", variable))
        , reference =
            ifelse(
              is.factor(multivar_data_mod[[causal_factor]])
              , levels(multivar_data_mod[[causal_factor]])[1], ""
            )
      ) |>
      mutate(
        HR = sprintf("%.3f", exp(estimate))
        , LB = sprintf("%.3f", exp(estimate - qnorm(0.975)*std.error))
        , UB = sprintf("%.3f", exp(estimate + qnorm(0.975)*std.error))
        ,p_value =
          ifelse(
            p.value < 0.001
            , "<0.001"
            , ifelse(p.value > 0.05, ">0.05", sprintf("%.3f", p.value))
          )
      ) |>
      select(variable, term, reference, HR, LB, UB, p_value) |>
      unite(term_reference, term, reference, sep = " vs. ") |>
      mutate_at("term_reference", str_remove_all, "^ vs. $")
    
    list(results = results, pca_model = pca_model)
  }