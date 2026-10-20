library(dplyr) # for data manipulation
library(stringr) # for working with strings

# load data that we saved in data_import
load("processed_data/sample_metadata.rda")

# clean data
clean_metadata <- sample_metadata %>%
  # remove all rows in data that are missing sample ID
  dplyr::filter(!is.na(SampleID)) %>% 
  # clean study name to only include last name of first author
  mutate(Study = 
           # remove _year from study name
           stringr::str_remove(Study, "\\_.*") %>% 
           # remove first name initial from study name
           stringr::str_remove_all("(?<=.)[A-Z]")) %>% 
  # remove spaces from names of variables 
  dplyr::rename(sampling_paper = `Sampling from papers`, 
         sampling_day = `Sampling, day`) %>%
  # recode "NA" as NA and make sampling_day a numeric variable
  mutate(sampling_day = na_if(sampling_day, "NA") %>%
           as.numeric()) 

# in Yassour study, sample IDs are not unique to samples 
# first inspect this to confirm
clean_metadata %>%
  filter(Study == "Yassour") %>%
  dplyr::select(Study, Category, SampleID, sampling_day) %>%
  head()

# create new unique sample IDs for Yassour study
clean_metadata <- clean_metadata %>%
  # check if study is Yassour
  mutate(SampleID = ifelse(Study == "Yassour", 
                           # if so, combine sample ID and sampling day
                           paste0(SampleID, sampling_day),
                           # if not, leave sample ID as is 
                           SampleID)) 

# confirm that we've fixed this problem
clean_metadata %>%
  filter(Study == "Yassour") %>%
  select(Study, Category, SampleID, sampling_day) %>%
  head()

# two studies (Parnanen and Yassour) took samples 
# from mothers during pregnancy
# remove these samples, because they don't fit into
# the time range that we want to consider 
clean_metadata <- clean_metadata %>%
  filter(!(sampling_paper %in% c("32WK", "Gest")))

# save cleaned data
save(clean_metadata, file = "processed_data/clean_sample_metadata.rda")
