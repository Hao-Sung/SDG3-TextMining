library(tidyverse)

read.text.csv = function(path) {
  res = read.csv(
    path,
    sep = ",",
    header = TRUE,
    fill = TRUE,
    strip.white = TRUE,
    fileEncoding = "BIG5"
  ) %>% select("文獻名稱.", "年份", "摘要", "索引關鍵字")
  
  return(res)
}

read.document = function(file_list) {
  res = list()
  college_list = unique(str_match(file_list, "Abstract/(.+)/")[, 2])
  for (college in college_list) {
    files = file_list[str_detect(file_list, college)]
    if (length(files) == 1) {
      res[[college]] = read.text.csv(files)
      print(paste(college, ": imported successfully"))
    } else {
      res[[college]] = foreach(
        f = files,
        .combine = "rbind",
        .inorder = FALSE,
        .export = c("read.text.csv"),
        .packages = c("tidyverse")
      ) %dopar% {
        read.text.csv(f)
      }
      print(paste(college, ": imported successfully"))
    }
  }
  return(res)
}


nl_preprocess = function(doc_vec) {
  char_rep_by_space = c("/", "@", "\\|", "-", "–")
  replacement = rep(" ", length(char_rep_by_space))
  names(replacement) = char_rep_by_space
  res = doc_vec %>%
    tolower() %>%
    str_replace_all(replacement) %>%
    str_replace_all("http[^[:space:]]*", "") %>%
    removeNumbers() %>%
    removePunctuation() %>%
    stemDocument(language = "english") %>%
    stripWhitespace()
  return(res)
}

document_preprocess = function(doc_list) {
  res = foreach(
    doc = doc_list,
    .export = c("nl_preprocess"),
    .packages = c("tidyverse", "tm", "SnowballC")
  ) %dopar% {
    doc %>%
      mutate(document = str_c(摘要, 索引關鍵字, sep = " ")) %>%
      filter(!grepl("[\\p{Han}]", document, perl = T)) %>%
      mutate(document = nl_preprocess(document)) %>%
      select("document")
  }
  names(res) = names(doc_list)
  return(res)
}
