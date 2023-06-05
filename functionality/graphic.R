library(htmlwidgets)

# define macro variable
GRAPH_EXPORT_PATH = paste(c(".", "figs") ,collapse = sep)

.ggplot_export = function(graph, width, height, filename) {
  ggsave(filename,
         path = GRAPH_EXPORT_PATH,
         width = width,
         height = height)
}

.filename_check = function(filename) {
  if (is.null(filename)) {
    file = paste(c(".", "figs", "temp.png"), collapse = sep)
  } else {
    file = paste(c(GRAPH_EXPORT_PATH, filename), collapse = sep)
  }
  return(file)
}

.get_keywords_freq = function(doc_vec, keywords) {
  doc_num = length(doc_vec)
  res = keywords %>%
    rowwise() %>%
    mutate(freqency = sum(str_detect(doc_vec, keyword))) %>%
    ungroup() %>%
    mutate(ratio = freqency / doc_num)
  return(res)
}

keywords_freq_compare = function(doc_list, doc_vec, keywords, filename = NULL) {
  docs = doc_list[doc_vec]
  freq_dataframe = foreach(
    doc = docs,
    .combine = "cbind",
    .export = c(".get_keywords_freq", "keywords"),
    .packages = c("tidyverse")
  ) %dopar% {
    doc$document %>%
      .get_keywords_freq(keywords=keywords) %>%
      select("ratio")
  }
  colnames(freq_dataframe) = names(docs)
  rownames(freq_dataframe) = keywords$keyword
  heatmaply(
    freq_dataframe * 100 ,
    xlab = "Universities",
    ylab = "Keywords",
    main = "Tendency to use scival keywords in abstract (%)",
    dendrogram = "column",
    file = paste(c(GRAPH_EXPORT_PATH, filename), collapse = sep)
  )
}

keywords_correlation = function(corr, sci_key, rep_key, filename = NULL) {
  sci_key = sci_key[["keyword"]]
  # college_key = rep_key[["words"]]
  college_key = rep_key %>%
    arrange(desc(tf_idf)) %>%
    top_n(50) %>%
    pull(words)
  res = foreach(
    rk = college_key,
    .combine = "rbind",
    .packages = c("tidyverse", "foreach"),
    .export = c("sci_key", "corr")
  ) %dopar% {
    foreach(sk = sci_key, .combine = "c") %do% {
      res = filter(corr, item1 == rk & item2 == sk)[["correlation"]]
      res = if(length(res) == 0) 0 else max(res, 0)
    }
  }
  colnames(res) = sci_key
  rownames(res) = college_key
  heatmaply(res ,
            xlab = "Scival Keywords",
            ylab = "representative keywords",
            main = "Keywords correlation",
            dendrogram = "none",
            colors = viridis(n = 512, alpha = 1, begin = 0, end = 1, option = "viridis"),
            file = paste(c(GRAPH_EXPORT_PATH, filename), collapse = sep))
}


key_comparison_cloud = function(rep_key,
                                group_vec = NULL,
                                filename = NULL) {
  if (is.null(group_vec)) {
    rep_key = rep_key %>%
      acast(words ~ label, value.var = "tf", fill = 0)
  } else {
    rep_key = rep_key %>%
      filter(label %in% group_vec) %>%
      acast(words ~ label, value.var = "tf", fill = 0)
  }
  
  filename = .filename_check(filename)
  png(filename = filename,
      width = 1200,
      height = 1000)
  comparison.cloud(
    rep_key,
    max.words = 800,
    rot.per = .2,
    title.bg.colors = "#ffffff"
  )
  dev.off()
}

.common_word_detect = function(target) {
  n_group = dim(target)[2]
  is_commom = rowSums(target != 0) == n_group
  common_words = rownames(target)[is_commom]
  if(length(common_words) <= 1) {
    print("No common word detected.")
    return(FALSE)
  } else {
    return(common_words)
  }
}

key_commonality_cloud = function(rep_key,
                                 group_vec = NULL,
                                 filename = NULL) {
  if (is.null(group_vec)) {
    rep_key = rep_key %>%
      acast(words ~ label, value.var = "tf", fill = 0)
  } else {
    rep_key = rep_key %>%
      filter(label %in% group_vec) %>%
      acast(words ~ label, value.var = "tf", fill = 0)
  }
  
  common_words = .common_word_detect(rep_key)
  if(is.character(common_words)) {
    rep_key = rep_key[c(common_words), ]
    
    filename = .filename_check(filename)
    png(filename = filename,
        width = 800,
        height = 600)
    commonality.cloud(
      rep_key,
      max.words = 800,
      rot.per = .2,
      random.order = FALSE,
      colors = brewer.pal(8, "Dark2")
    )
    dev.off()
  }
}

plot_pyramid = function(rep_key, pair, lab_grid=0.01, ndig=4, filename = NULL) {
  rep_key = rep_key %>%
    filter(label %in% pair) %>%
    acast(words ~ label, value.var = "tf", fill = 0)
  
  common_words = .common_word_detect(rep_key)
  if(is.character(common_words)) {
    rep_key = rep_key[common_words, ] * 1000
    
    maximum = max(rep_key)
    quo = maximum %/% lab_grid
    lab = seq(0,(quo+1)*lab_grid,lab_grid)
    
    filename = .filename_check(filename)
    png(filename = filename,
        width = 1000,
        height = 600)
    pyramid.plot(
      rep_key[, pair[1]],
      rep_key[, pair[2]],
      main = "Words in Common",
      unit = "term frequency (‰)",
      top.labels = c(pair[1], "Words", pair[2]),
      labels = rownames(rep_key),
      show.values = TRUE,
      space=0.1,
      gap=0.25,
      lxcol=hcl.colors(dim(rep_key)[1]),
      rxcol=hcl.colors(dim(rep_key)[1]),
      laxlab = lab,
      raxlab = lab,
      xlim=c(maximum + lab_grid, maximum + lab_grid),
      labelcex = 1,
      ndig=4,
      ppmar=c(5,4,6,4)
    )
    dev.off()
  }
}


# pair_commonality_cloud = function(rep_key, pair) {
#   target = rep_key %>%
#     filter(label %in% pair)
#   
#   is_common_exist = .common_word_detect(target, pair)
#   if(is_common_exist) {
#     common_words = target %>%
#       add_count(words, name="freq") %>%
#       filter(freq > 1) %>%
#       acast(words ~ label, value.var = "tf")
#     common_words = common_words * 1000
#     
#     commonality = data.frame(
#       words = rownames(common_words),
#       abs_diff_tf = abs(common_words[, pair[1]] - common_words[, pair[2]])
#     )
#     
#     color = ifelse(common_words[, pair[1]] >
#                      common_words[, pair[2]], "#00a5f7", "#0058b3")
#     wordcloud2(commonality, size=0.5, color= unname(color), backgroundColor="#cccccc")
#   }
# }


# https://cran.r-project.org/web/packages/dendextend/vignettes/FAQ.html
keyword_cluter = function(doc_list, keywords, filename=NULL) {
  get_tdm = function(doc_list, keywords) {
    keys = keywords[["keyword"]]
    docs = do.call(rbind, doc_list)[["document"]]
    res = foreach(
      key = keys,
      .combine = "rbind",
      .export = c("docs"),
      .packages = c("tidyverse")
    ) %dopar% {
      str_count(docs, key)
    }
    rownames(res) = keys
    return(res)
  }
  tdm = parallel_wrapper(
    get_tdm,
    doc_list = doc_list,
    keywords = keywords
  )
  # measure distance with term-document matrix
  hc = hclust(d = dist(tdm, method = "euclidean"), method = "ward.D")
  
  filename = .filename_check(filename)
  png(filename = filename,
      width = 800,
      height = 800)
  par(mar = rep(0,4))
  hc %>%
    as.dendrogram() %>%
    color_branches(k=3) %>% 
    color_labels() %>%
    circlize_dendrogram(labels_track_height = NA, dend_track_height = .3) 
  dev.off()
}

.add_missing_continent = function(data, continent_vec) {
  is_exist = continent_vec %in% (data %>% pull(Continent))
  if (sum(is_exist) != length(continent_vec)) {
    missing_continent = continent_vec[!is_exist]
    return(data %>% add_row(
      Ranking = unique(data %>% pull(Ranking)),
      Continent = missing_continent,
      n = 0,
      ratio = 0,
      label=""
    ))
  } else {
    return(data)
  }
  
}

.ranking_group_decompose = function(ranking_dat) {
  continent_vec = unique(ranking_dat$Continent)
  return(
    ranking_dat %>%
      group_by(Ranking, Continent) %>%
      count() %>%
      group_by(Ranking) %>%
      mutate(ratio = n / sum(n)) %>%
      mutate(label = if_else(ratio > 0.05, paste0(
        round(ratio * 100, 1), "%"
      ), "")) %>%
      do(.add_missing_continent(., continent_vec)) %>%
      mutate(Number.ranking = dense_rank(desc(n))) %>%
      mutate(
        color = case_when(
          Continent == "Africa" ~ "#e01e00",
          Continent == "Asia" ~ "#b1b1b1",
          Continent == "Asia(ASEAN)" ~ "#5d6074",
          Continent == "Europe" ~ "#fdcf2c",
          Continent == "North America" ~ "#e98602",
          Continent == "Oceania" ~ "#1971ff",
          Continent == "South America" ~ "#4ecbff",
        )
      ) %>%
      ungroup()
  )
}

plot_ranking_bar = function(ranking_dat, filename = NULL) {
  decompose_dat = .ranking_group_decompose(ranking_dat)
  p = ggplot(decompose_dat, aes(
    x = Ranking,
    y = ratio,
    label = label,
    fill = Continent
  )) +
    geom_bar(stat = "identity") +
    geom_text(size = 3, position = position_stack(vjust = 0.5)) +
    scale_fill_manual(values = c(
      "#e01e00",
      "#b1b1b1",
      "#5d6074",
      "#fdcf2c",
      "#e98602",
      "#1971ff",
      "#4ecbff"
    )) +
    labs(title = "SDG3 Percent stack barchart") +
    theme(plot.title = element_text(size = 20, hjust = 0.5)) +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "white"))
  p %>% .ggplot_export(width = 5, height = 6, filename = filename)
  
  return(p)
}

plot_ranking_segment = function(ranking_dat, filename = NULL) {
  theme.porttheme <-
    theme(text = element_text(family = "Gill Sans", color = "#444444")) +
    theme(plot.title = element_text(size = 24)) +
    theme(plot.subtitle = element_text(size = 18)) +
    theme(axis.title = element_text(size = 14)) +
    theme(axis.title.y = element_text(
      angle = 0,
      vjust = .5,
      margin = margin(r = 15)
    )) +
    theme(axis.text = element_text(size = 10)) +
    theme(axis.title.x = element_text(margin = margin(t = 20))) +
    theme(legend.title = element_blank()) +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "white"))
  
  decompose_dat = .ranking_group_decompose(ranking_dat)
  decompose_dat %>%
    ggplot(aes(x = Ranking, y = Number.ranking, group = Continent)) +
    geom_line(aes(color = I(color)), linewidth = 2.5) +
    geom_point(aes(color = I(color)), size = 3.5) +
    geom_point(color = "#FFFFFF",
               alpha = .8,
               size = .3) +
    geom_text(
      data = decompose_dat %>% filter(Ranking == '401-600'),
      aes(label = Continent, x = '401-600') ,
      hjust = -.05,
      color = "#888888",
      size = 4
    ) +
    geom_text(
      data = decompose_dat %>% filter(Ranking == "1-100"),
      aes(label = Continent, x = '1-100') ,
      hjust = 1.05,
      color = "#888888",
      size = 4
    ) +
    scale_x_discrete(expand = c(.2, .2)) +
    scale_y_reverse(breaks = 1:7) +
    scale_alpha_discrete(range = c(.4, .9)) +
    labs(title = "Ranking of Continents in Each Ranking Interval") +
    labs(subtitle = "(by proportion)") +
    labs(x = "Interval", y = "Ranking of Proportion") +
    theme.porttheme +
    theme(panel.grid.major.x = element_line(color = "#f3f3f3")) +
    theme(panel.grid.major.y = element_blank()) +
    theme(panel.grid.minor = element_blank()) +
    theme(legend.position = "none")
  p %>% .ggplot_export(width = 8, height = 6, filename = filename)
  
  return(p)
}

.add_popup_style = function(ranking, university) {
  style_div = "background-color: #ddd; color: #246; padding: 2em; box-shadow: 1px 2px 4px #0003; width: 80%; border-radius: 15px;"
  style_h4 = "margin: 0 auto;"
  style_ul = "padding-inline-start: 1.5em; font-weight: 400;"
  style_li = "padding-inline-start: 0.5em; padding-top: 1em; border-bottom: 1px solid #48b2;"
  return(
    str_glue(
      '
    <div style="{style_div}">
    <h4 style="{style_h4}">Impact Ranking 2020</h4>
    <ul style="{style_ul}">
    <li style="{style_li}">Objective: SDG3</li>
    <li style="{style_li}">Ranking: {ranking}</li>
    <li style="{style_li}">University: {university}</li>
    </ul>
    </div>
    '
    )
  )
  
}


plot_world_map = function(college_ranking, filename = NULL) {
  college_ranking = college_ranking %>%
    rowwise() %>%
    mutate(marker.content = .add_popup_style(Ranking, Uni.Name)) %>%
    ungroup() %>%
    mutate(
      marker.color = case_when(
        Ranking == "1-100" ~ "darkred",
        Ranking == "101-200" ~ "darkpurple",
        Ranking == "201-300" ~ "lightblue",
        Ranking == "301-400" ~ "blue",
        Ranking == "401-600" ~ "darkblue",
      )
    )
  map = leaflet() %>% addTiles() %>% addProviderTiles(providers$Esri.WorldStreetMap)
  
  ranking_group = unique(college_ranking$Ranking)
  for(r in ranking_group) {
    data = college_ranking %>% filter(Ranking == r)
    icons = awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = data$marker.color
    )
    map = map %>% addAwesomeMarkers(
      data = data,
      lng = ~ Longitude,
      lat = ~ Latitude,
      label = ~ Uni.Name,
      popup = ~ marker.content,
      icon = icons,
      clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = T),
      group = r,
      labelOptions = labelOptions(noHide = F, direction = 'auto')
    )
  }
  map = map %>% addMeasure(
    position = "bottomleft",
    primaryLengthUnit = "meters",
    primaryAreaUnit = "sqmeters",
    activeColor = "#3D535D",
    completedColor = "#7D4479") %>%
    
    addMiniMap(
      tiles = providers$Esri.WorldStreetMap,
      toggleDisplay = TRUE) %>%
    
    addEasyButton(easyButton(
      icon="fa-globe", title="Zoom to Level 1",
      onClick=JS("function(btn, map){ map.setZoom(1); }"))) %>%
    
    addEasyButton(easyButton(
      icon="fa-crosshairs", title="Locate Me",
      onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
    
    addLayersControl(
      overlayGroups = ranking_group,
      options = layersControlOptions(collapsed = FALSE)
    )
  saveWidget(map, file=paste(c(GRAPH_EXPORT_PATH, filename), collapse = sep))
  return(map)
}
