data_batch1_2_raw5_txt_to_cat <- function(x){
  if(x %in% non_cat_txt_cols){
    txt_cols_gather |>
      left_join(data_batch1_2_new_txts, by = join_by(old_txt)) |>
      filter(colname == x) |>
      select(-colname) |>
      filter(!is.na(new_txt)) |>
      select(-old_txt) |>
      unique() |>
      mutate(value = "1") |>
      spread(new_txt, value, fill = "0") |>
      right_join(
        data_batch1_2_raw5 |>
          mutate(seq=seq(nrow(data_batch1_2_raw5))) |>
          select(seq)
        , by = join_by(seq)
      ) |>
      mutate_all(\(x) ifelse(is.na(x), "0", x)) |>
      arrange(seq) |>
      select(-seq)
  }else{
    select_at(data_batch1_2_raw5, x)
  }
}