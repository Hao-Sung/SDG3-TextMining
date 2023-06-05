# packages for nature language processing
library(tidyverse)
library(tm)
library(widyr)
library(SnowballC)
library(tidytext)
library(reshape2)

# packages for parallel computing
library(foreach)
library(parallel)
library(doParallel)

# packages for visualization
library(heatmaply)
library(wordcloud)
library(wordcloud2)
library(dendextend)
library(circlize)
library(plotrix)
library(leaflet)

# import functions for preprocess, analysis and visualization...
sep = .Platform$file.sep
source(paste(c(".", "functionality", "preprocess.R"), collapse = sep))
source(paste(c(".", "functionality", "summary.R"), collapse = sep))
source(paste(c(".", "functionality", "graphic.R"), collapse = sep))
source(paste(c(".", "functionality", "wrapper.R"), collapse = sep))

# read SDG3 keywords recommended by SciVal
sdg3_keywords = read.csv(
  paste(c(".", "data", "SDG keywords", "SDG3.csv"), collapse = sep),
  sep = ",",
  header = FALSE,
  fill = TRUE,
  strip.white = TRUE,
  fileEncoding = "BIG5",
  col.names = c("keyword", "keyword in Chinese")
)

# read documents for each college in parallel
files = list.files(paste(c(".", "data", "SDG3 Paper Abstract"), collapse = sep),
                   recursive = TRUE,
                   full.names = TRUE)
sdg3_documents = parallel_wrapper(read.document, file_list = files)

# Available colleges:
# - Taiwan: NCKU, NCTU, NTHU, NTU, NTUST, NYMU, TCU
# - Asia (Taiwan excluded): HONGKONG, INDONESIA, KHU, MALAYA, NAGOYA, SCU, KKU
# - Oceania: AUCKLAND, SYDNEY
# - United Kingdom: KCL
# - Canada: McMU

############### Preprocess ###############

sdg3_keywords = sdg3_keywords %>%
  mutate(keyword = nl_preprocess(keyword)) %>%
  distinct(keyword, .keep_all = TRUE)

sdg3_documents = parallel_wrapper(document_preprocess, doc_list = sdg3_documents)


############### Task.1: Find representative keywords for each group ###############

# * Select representative keywords based on TF-IDF values

# Taiwan
group_by_tw_college = list(
  NCKU = c("NCKU"),
  NTU = c("NTU"),
  NYMU = c("NYMU"),
  NCTU = c("NCTU"),
  NTHU = c("NTHU"),
  NTUST = c("NTUST"),
  TCU = c("TCU")
)
rep_key_tw = representative_keywords(sdg3_documents, group_by_tw_college, n=1)
rep_key_tw

# Asia
group_by_asia_country = list(
  Taiwan = c("NCKU", "NTU", "NYMU"),
  HongKong = c("HONGKONG"),
  China = c("SCU"),
  Korea = c("KHU"),
  Japan =c("NAGOYA"),
  Indonesia = c("INDONESIA"),
  Thailand = c("KKU"),
  Malaysia =c("MALAYA")
)
rep_key_asia = representative_keywords(sdg3_documents, group_by_asia_country, n=1)
rep_key_asia

# Western World
group_by_western_country = list(
  Taiwan = c("NCKU", "NTU", "NYMU"),
  Oceania = c("AUCKLAND", "SYDNEY"),
  Canada = c("McMU"),
  UnitedKingdom = c("KCL")
)
rep_key_western = representative_keywords(sdg3_documents, group_by_western_country, n=1)
rep_key_western
bi_rep_key_western = representative_keywords(sdg3_documents, group_by_western_country, n=2)
bi_rep_key_western


############### Task.2: Compare occurrence of SciVal keywords ###############

# between colleges in Taiwan 
parallel_wrapper(
  keywords_freq_compare,
  doc_list = sdg3_documents,
  doc_vec = unlist(group_by_tw_college),
  keywords = sdg3_keywords,
  filename="scival_in_taiwan.html"
)

# between colleges in Asia
parallel_wrapper(
  keywords_freq_compare,
  doc_list = sdg3_documents,
  doc_vec = unlist(group_by_asia_country),
  keywords = sdg3_keywords,
  filename="scival_in_asia.html"
)

# between colleges in Western World
parallel_wrapper(
  keywords_freq_compare,
  doc_list = sdg3_documents,
  doc_vec = unlist(group_by_western_country),
  keywords = sdg3_keywords,
  filename="scival_in_west.html"
)


############### Task.3: Compute pairwise correlation between words and SciVal keywords ###############

# ? visualization with comparison/common cloud between multiple college?

# use all documents available
all_docs = c("NCKU", "NCTU", "NTHU", "NTU", "NTUST",
             "NYMU", "TCU", "HONGKONG", "INDONESIA", "KHU",
             "MALAYA", "NAGOYA", "SCU", "KKU", "AUCKLAND",
             "SYDNEY", "KCL", "McMU")

# by co-occurrence
corr_by_count = scival_related_keyword(
  doc_list = sdg3_documents,
  doc_vec = all_docs,
  keywords = sdg3_keywords,
  measrue="counting"
)
corr_by_count

# by phi coefficient
corr = scival_related_keyword(
  doc_list = sdg3_documents,
  doc_vec = all_docs,
  keywords = sdg3_keywords,
  measrue="correlating"
)
corr


############### Task.4: Find correlation between representative keywords and SciVal keywords ###############

# use representative keywords of Taiwan
parallel_wrapper(
  keywords_correlation,
  corr = corr,
  sci_key = sdg3_keywords,
  rep_key = bi_rep_key_western %>% filter(label == "Taiwan"),
  filename = "Taiwan_corr_with_scival.html"
)

# use representative keywords of Oceania(AUS, NZ)
parallel_wrapper(
  keywords_correlation,
  corr = corr,
  sci_key = sdg3_keywords,
  rep_key = bi_rep_key_western %>% filter(label == "Oceania"),
  filename = "Oceania_corr_with_scival.html"
)

# use representative keywords of Canada
parallel_wrapper(
  keywords_correlation,
  corr = corr,
  sci_key = sdg3_keywords,
  rep_key = bi_rep_key_western %>% filter(label == "Canada"),
  filename = "Canada_corr_with_scival.html"
)

# use representative keywords of United Kingdom
parallel_wrapper(
  keywords_correlation,
  corr = corr,
  sci_key = sdg3_keywords,
  rep_key = bi_rep_key_western %>% filter(label == "UnitedKingdom"),
  filename = "UK_corr_with_scival.html"
)


############### Task.5: Visualize differences in representative keyword #####################

# between colleges in Taiwan
key_comparison_cloud(
  rep_key = rep_key_tw,
  filename = "cf_taiwan_six.png"
)
key_comparison_cloud(
  rep_key = rep_key_tw,
  group_vec = c("NCKU", "NTU", "NYMU"),
  filename = "cf_taiwan_three.png"
)

# between colleges in Asia
key_comparison_cloud(
  rep_key = rep_key_asia,
  filename = "cf_asia.png"
)

# between colleges in Western World
key_comparison_cloud(
  rep_key = rep_key_western, 
  filename = "cf_west.png"
)


################ Task.6: Visualize commonality in representative keyword #####################

# It's hard to find commonality when taking too many colleges into account...
key_commonality_cloud(rep_key_tw)
key_commonality_cloud(rep_key_asia)
key_commonality_cloud(rep_key_western)

# turn to pairwise commonality...
# NCKU versus NTU
key_commonality_cloud(
  rep_key = rep_key_tw,
  group_vec = c("NCKU", "NTU"),
  filename = "common_NCKU_NTU.png"
)
plot_pyramid(
  rep_key = rep_key_tw,
  pair = c("NCKU", "NTU"),
  filename = "common_NCKU_NTU_pyramid.png"
)

# NCKU versus NYMU
key_commonality_cloud(
  rep_key = rep_key_tw,
  group_vec = c("NCKU", "NYMU"),
  filename = "common_NCKU_NYMU.png"
)
plot_pyramid(
  rep_key = rep_key_tw,
  pair = c("NCKU", "NYMU"),
  filename = "common_NCKU_NYMU_pyramid.png"
)


############### Task.7: SciVal keyword clustering #####################

# clustering analysis using term-document matrix
keyword_cluter(sdg3_documents, sdg3_keywords, "scival_clustering.png")


############### Task.8: Impact Ranking 2020 SDG3 - EDA #####################

SDG3_college_ranking = read.csv(
  paste(c(".", "data", "SDG3 impact ranking 2020.csv"), collapse = sep),
  sep = ",",
  fill = TRUE,
  strip.white = TRUE
)

# stacked bar chart
plot_ranking_bar(SDG3_college_ranking, "SDG3_ranking_decomp.png")
# line chart
plot_ranking_segment(SDG3_college_ranking, "SDG3_prop_ranking.png")


############### Task.9: Impact Ranking 2020 SDG3 - world map #####################

plot_world_map(SDG3_college_ranking, "map.html")

