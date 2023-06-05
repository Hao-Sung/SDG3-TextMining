
.group_docs = function(doc_list, group_list) {
  group_name = names(group_list)
  return(foreach(
    colleges = group_list,
    name = group_name,
    .combine = "rbind"
  ) %do% {
    do.call(rbind, doc_list[colleges]) %>%
      mutate(label = name)
  })
}

.get_unigram = function(docs) {
  stop_w = nl_preprocess(stop_words$word) %>% unique()
  return(
    docs %>%
      unnest_tokens(words, document) %>%
      filter(!(words %in% stop_w))
  )
}

.get_bigrams = function(docs) {
  stop_w = nl_preprocess(stop_words$word) %>% unique()
  return(
    docs %>%
      unnest_tokens(words, document, token = "ngrams", n = 2) %>%
      separate(words, c("word1", "word2"), sep = " ") %>%
      filter((!word1 %in% stop_w) & (!word2 %in% stop_w)) %>%
      unite(words, word1, word2, sep = " ")
  )
}

representative_keywords = function(doc_list, group_list, n = 1, threshold = 100) {
  docs = .group_docs(doc_list, group_list)
  # generate ngrams data
  if (n == 1) {
    ngram = .get_unigram(docs)
  } else if (n== 2) {
    ngram = .get_bigrams(docs)
  } else {
    stop("Sorry, n_gram >= 3 is not available.")
  }
  # extract features
  res = ngram %>%
    count(label, words) %>%
    bind_tf_idf(words, label, n) %>%
    arrange(desc(tf_idf)) %>%
    group_by(label) %>%
    top_n(threshold) %>%
    ungroup()
  
  invisible(gc())
  return(res)
}

.get_keyword_pos = function(docs, keyword) {
  doc_vec = docs$document
  doc_idx = 1:length(doc_vec)
  keyword_pos = doc_idx[str_detect(doc_vec, keyword)]
  return(tibble(label = keyword_pos, words = keyword))
}

scival_related_keyword = function(doc_list, doc_vec, keywords, measrue="counting") {
  docs = do.call(rbind, doc_list[doc_vec]) %>%
    mutate(label = 1:n())
  # generate bigrams
  bigrams = .get_bigrams(docs)
  # include keywords that are not bigrams when compute correlation
  not_bigram_key = keywords %>%
    rowwise() %>%
    mutate(word.number = str_split_1(keyword, pattern=" ") %>% length()) %>%
    ungroup() %>%
    filter(word.number != 2)
  not_bigram_pos = foreach(key=not_bigram_key$keyword, .combine="rbind") %do% {
    .get_keyword_pos(docs, key)
  }
  
  if(measrue=="counting") {
    # compute pairwise count for all keywords
    res = bigrams %>%
      rbind(not_bigram_pos) %>%
      pairwise_count(words, label, sort = TRUE) %>%
      filter(item1 %in% keywords$keyword |
               item2 %in% keywords$keyword)
  } else if(measrue=="correlating") {
    # compute pairwise correlation for all keywords
    res = bigrams %>%
      rbind(not_bigram_pos) %>%
      add_count(words) %>%
      filter(n >= 10) %>% 
      select(-n) %>%
      pairwise_cor(words, label, sort = TRUE) %>%
      filter(item1 %in% keywords$keyword |
               item2 %in% keywords$keyword)
  } else {
    stop("Invalid 'measrue' value.")
  }
  return(res)
}

