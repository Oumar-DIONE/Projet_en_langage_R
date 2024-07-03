#lire_yaml_file
library("yaml")
config <- yaml::yaml.load_file(paste0(Boston_data_path, "config2.yaml"))
