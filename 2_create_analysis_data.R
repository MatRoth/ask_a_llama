### Important: This code extracts the relevant data from the raw synthetic survey
# data file. However, I changed it to work in a single git-repository for replication. 
# I have yet to check if it actually works.

library(tidyverse)

#read in data
data<- list.files()|>
  keep(\(x) str_detect("n_info"))|>
  as_tibble()|>
  rename(file_names = 1)|>
  mutate(
    model = file_names |> map_chr(\(cur_file) str_match(cur_file,"TheBloke_(.*)-GGUF.*")[,2]),
    n_info = map_chr(file_names,\(cur_file) str_match(cur_file,"(\\d*)_\\.rds$")[,2])|>as.numeric(),
    data = map(file_names,read_rds,.progress = T))

data <- data |>
  group_by(model,n_info)|>
  nest(.key = "sorted_files")

data<- data |>
  ungroup()|>
  mutate(sorted_files = map(sorted_files,\(x) x$data |> bind_rows()))


library(tidyverse)
data <- data |> 
  unnest(sorted_files)|>
  mutate(probs = map(model_results,\(cur_res) if(is.list(cur_res)) cur_res$probs else cur_res),
         logits = map_dbl(model_results,\(cur_res) if(is.list(cur_res)) max(cur_res$logprobs) else NA_real_)) |> 
  select(-model_results)
write_rds(data,"complete_unpacked_probs_maxlogit.rds")