source("src/load_packages.R")
load_packages(c(
  "tidyverse",
  "magrittr"
))
# character型の列をキーとしてそれぞれの統計量を出力します。（適当に追加してください）
get.BS <- function(input.df ,filedir){
  dir.create(paste0(filedir))
  dir.create(paste0(filedir ,"/BS"))
  colnames.df <- expand.grid(
    character = select_if(input.df, is.character) %>% 
      colnames(),
    numeric = select_if(input.df, is.numeric) %>% 
      colnames()) %>% 
    mutate_if(is.factor, as.character)
  
  for(i in 1:NROW(colnames.df)){
    chr.col <- colnames.df[i,] %>% 
      select(character) %>% 
      as.character()
    num.col <- colnames.df[i,] %>% 
      select(numeric) %>% 
      as.character()
    dist.name <- input.df %>% 
      select_(chr.col) %>% 
      distinct_(chr.col) %>% 
      mutate_if(is.factor, as.character)
    
    input.df %>% 
      summarise_(
        sum = paste0("sum(",num.col,")"),
        mean = paste0("mean(",num.col,")"),
        count = paste0("NROW(",num.col,")"),
        max = paste0("max(",num.col,")"),
        min = paste0("min(",num.col,")")
      ) %>% 
      data.frame(name = "all", .) %>% 
      write.csv(paste0(filedir ,"/BS/choose_all_BSby_", num.col, ".csv"), row.names = F)
    
    if(NROW(dist.name) != NROW(input.df)){
      input.df %>% 
        group_by_(chr.col) %>% 
        summarise_(
          sum = paste0("sum(",num.col,")"),
          mean = paste0("mean(",num.col,")"),
          count = paste0("NROW(",num.col,")"),
          max = paste0("max(",num.col,")"),
          min = paste0("min(",num.col,")")
          ) %>% 
        write.csv(paste0(filedir ,"/BS/choose_", chr.col, "_BSby_", num.col, ".csv"), row.names = F)
      message(paste0(filedir ,"/BS/choose_", chr.col, "_BSby_", num.col))
    }
  }
  message("...and all!")
}