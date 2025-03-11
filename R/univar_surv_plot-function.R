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
    , xmin
    , xmax
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
        data$plot$data |>
          group_by(strata) |>
          slice(-1) |>
          ungroup() |>
          rbind(
            data$plot$data |>
              group_by(strata) |>
              slice(2) |>
              ungroup() |>
              select(-time) |>
              left_join(
                data$plot$data |>
                  group_by(strata) |>
                  slice(1) |>
                  ungroup() |>
                  select(time, strata)
              )
          ) |>
          ggplot(aes(time, surv, color = strata)) +
          geom_step(linewidth = 1) +
          annotate(
            geom = "text"
            , x = anno_x
            , y = anno_y
            , label = anno_label
            , size = 4
            , hjust = 0
            , vjust = 0
          ) +
          scale_x_continuous(
            "Month"
            , breaks =
              seq(min(data$plot$data$time), max(data$plot$data$time), 1)
          ) +
          scale_y_continuous(paste0(outcome,"\nevent rate"), labels = \(x) x) +
          scale_color_discrete(
            filter(variable_label_type, variable == strata)$label
          ) +
          theme_classic() +
          theme(
            axis.title.y.left = element_text(size = 14)
            , axis.text = element_text(size = 12)
            , legend.position = "top"
          )
        , data$table$data |>
          filter(
            time >= min(data$plot$data$time)
            & time <= max(data$plot$data$time)
          ) |>
          mutate_at("strata", \(x) factor(x, rev(unique(x)))) |>
          ggplot(aes(time, strata)) +
          geom_text(aes(label = n.risk), size = 4) +
          ggtitle("Sample size") +
          scale_x_continuous(
            "Month"
            , breaks =
              seq(min(data$plot$data$time), max(data$plot$data$time), 1)
          ) +
          ylab(
            filter(variable_label_type, variable == strata)$label |>
              str_replace_all("COVID-19", "COVID-19\n")
          ) +
          theme_classic() +
          theme(
            axis.title.y.left = element_text(size = 14)
            , axis.text = element_text(size = 12)
          )
        , ncol = 1
        , nrow = 2
        , heights = c(5, 2)
      )
    )
  }