desc_stats_vertical <- function(data, p_row = FALSE){
  data <-
    data |>
    mutate(
      exposure =
        case_when(
          exposure == 0 ~ "Mild COVID-19 in the past year"
          , exposure == 1 ~ "Mild COVID-19 within the past year"
          , exposure == 2 ~ "Moderate-to-severe COVID-19 at any time"
        ) |>
        factor(
          c("Mild COVID-19 in the past year"
            , "Mild COVID-19 within the past year"
            , "Moderate-to-severe COVID-19 at any time"
          )
        )
      ,vaccine =
        case_when(
          vaccine == 0 ~ "Never received COVID-19 vaccination"
          , vaccine == 1
            ~ "Received the first dose of COVID-19 vaccine after their COVID-19"
          , vaccine == 2
            ~ paste0(
                "Received the first dose of COVID-19 vaccine before COVID "
                , "infection"
              )
        ) |>
        factor(
          c("Never received COVID-19 vaccination"
            , "Received the first dose of COVID-19 vaccine after their COVID-19"
            , paste0(
                "Received the first dose of COVID-19 vaccine before COVID "
                , "infection"
              )
          )
        )
    ) |>
    mutate_at(
      c("age_timestamp", "age_inf_date")
      , \(x)
        case_when(
            floor(x) <= 19 ~ "<20"
            , floor(x) >= 20 & floor(x) <= 29 ~ "20-29"
            , floor(x) >= 30 & floor(x) <= 39 ~ "30-39"
            , floor(x) >= 40 & floor(x) <= 49 ~ "40-49"
            , floor(x) >= 50 & floor(x) <= 59 ~ "50-59"
            , floor(x) >= 60 & floor(x) <= 69 ~ "60-69"
            , floor(x) >= 70 & floor(x) <= 79 ~ "70-79"
            , floor(x) >= 80 & floor(x) <= 89 ~ "80-89"
            , floor(x) >= 90 ~ "90+"
          ) |>
          factor(
            c("<20", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79",
              "80-89", "90+"
            )
          )
    ) |>
    mutate(
      prev_inf = as.numeric(as.character(prev_inf))
      , prev_inf = factor(paste0(prev_inf," time",ifelse(prev_inf>1,"s","")))
      , bmi = weight/((height/100)^2)
      , bmi =
          case_when(
            bmi < 18.5 ~ "Underweight"
            , bmi >= 18.5 & bmi < 25 ~ "Normal"
            , bmi >= 25 & bmi < 30 ~ "Overweight"
            , bmi >= 30 ~ "Obesity"
          ) |>
          factor(c("Underweight", "Normal", "Overweight", "Obesity"))
      , los =
          case_when(
            los == 0 ~ "0 day"
            , los > 0 ~ "1+ day(s)"
          ) |>
          factor()
      , cig_smoking_d =
          case_when(
            cig_smoking_d < 1 ~ "<1 year"
            , cig_smoking_d >= 1 ~ "1+ year(s)"
          )
    ) |>
    select(
      -id, -interview_onset, -weight, -height
      , -interviewer_sbq_lc, -interviewer_moca_ina
      , -memory_thinking_communication_score, -moca_ina_delayed_recall
    )
  
  if(p_row){
    data |>
      mutate(batch = ifelse(batch %in% levels(batch)[1], 0, 1)) |>
      mutate_all(as.character) |>
      gather(variable, category, -batch) |>
      group_by(variable, category, batch) |>
      summarize(n = n(), .groups = "drop") |>
      group_by(variable, category) |>
      mutate(subtotal = sum(n)) |>
      ungroup() |>
      mutate(p = format(n / subtotal * 100, digits = 3)) |>
      filter(batch == 1) |>
      select(-batch) |>
      mutate(n = subtotal) |>
      select(-subtotal) |>
      mutate(
        category =
          case_when(
            category == 0 ~ "No"
            , category == 1 ~ "Yes"
            , TRUE ~ category
          )
      ) |>
      arrange(
        factor(variable, colnames(data))
        , factor(category, unique(unlist(lapply(data, levels))))
      )
  }else{
    data |>
      mutate_all(as.character) |>
      gather(variable, category) |>
      group_by(variable, category) |>
      summarize(n = n(), .groups="drop") |>
      group_by(variable) |>
      mutate(p = format(n / sum(n) * 100, digits = 3)) |>
      ungroup() |>
      mutate(
        category =
          case_when(
            category == 0 ~ "No"
            , category == 1 ~ "Yes"
            , TRUE ~ category
          )
      ) |>
      arrange(
        factor(variable, colnames(data))
        , factor(category, unique(unlist(lapply(data, levels))))
      )
  }
}