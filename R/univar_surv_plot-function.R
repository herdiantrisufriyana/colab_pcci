# Function to create survival plot per strata
univar_surv_plot=
  function(
    data
    , strata
    , outcome
    , anno_x = 0
    , anno_y = 0
    , anno_label = ""
    , modified_data = TRUE
  ){
    data <-
      data |>
      select_at(c(non_indep, strata)) |>
      rename_at(strata, \(x) "strata")
    
    if(modified_data){
      data <-
        data |>
        mutate(
          strata =
            ifelse(strata != 0, "Yes", "No") |>
            factor(c("No", "Yes"))
        )
    }
    
    data<-
      data |>
      mutate(interview_onset = max(interview_onset) - interview_onset)
    
    data <-
      ggsurvplot(
        fit =
          suppressWarnings(
            survfit(
              Surv(interview_onset, outcome, type="left") ~ strata
              , data = data
            )
          )
        , data = data
        , risk.table = TRUE
        , pval = FALSE
        , conf.int = FALSE
        , break.time.by = 1
      )[1:2]
    
    suppressMessages(
      ggarrange(
        data$plot +
          annotate(
            geom = "text"
            , x = anno_x
            , y = anno_y
            , label = anno_label
            , size = 5
            , hjust = 0
            , vjust = 0
          ) +
          xlab("Month") +
          scale_y_continuous(paste0(outcome,"\nevent rate"), labels = \(x) x) +
          scale_color_discrete(
            filter(variable_label_type, variable == strata)$label
          )
        , data$table +
          ggtitle("Sample size") +
          xlab("Month") +
          ylab(
            filter(variable_label_type, variable == strata)$label |>
              str_replace_all("COVID-19", "COVID-19\n")
          )
        , ncol = 1
        , nrow = 2
        , heights = c(5, 2)
      )
    )
  }