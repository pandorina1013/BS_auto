source("src/load_packages.R")
load_packages(c(
  "tidyverse",
  "magrittr"
))

# character型の列をキーとしてそれぞれのランキングを出力します。
get.rank <- function(input.df ,filedir){
  dir.create(paste0(filedir ,"/rank"))
  
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
    for(j in 1:NROW(dist.name)){
      input.df %>% 
        filter_(paste0(chr.col ,"==", "'", dist.name[j,], "'")) %>% 
        mutate_(min_rank=paste0("min_rank(desc(",num.col,"))")) %>% 
        arrange(min_rank) %>% 
        write.csv(paste0(filedir ,"/rank/choose_", chr.col, "-", dist.name[j,], "_rankby_", num.col, ".csv"))
      message(paste0(filedir ,"/rank/choose_", chr.col, "-", dist.name[j,], "_rankby_", num.col))
    }
    input.df %>% 
      mutate_(min_rank=paste0("min_rank(desc(",num.col,"))")) %>% 
      arrange(min_rank) %>% 
      write.csv(paste0(filedir ,"/rank/choose_all", "_rankby_", num.col, ".csv"), row.names = F)
  }
  message("...and all!")
}