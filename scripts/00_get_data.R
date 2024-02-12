library(osfr)
library(dplyr)



osf_retrieve_node("6sxpe") %>% 
  osf_ls_files() %>% 
  filter(name == "Inputs") %>% 
  osf_download(recurse = TRUE, path = "data") 

file.copy(list.files("data/Inputs/data", full.names = TRUE), 
          "data")
file.remove(list.files("data/Inputs/data", full.names = TRUE))

file.copy("data/Inputs/CombinedCosts", "data", recursive = TRUE)

unlink("data/Inputs", recursive = TRUE)
