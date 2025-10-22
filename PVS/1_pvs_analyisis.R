#Notes----------------------------------
#This is the analysis code for the empirical applications of 
#"Decoding German Politics with StateParl – a text corpus of plenary protocols in the 16 German Länder parliaments"

#This code can also be adapted to own analytical applications 
#Important: This analysis may take a fairly long time on regular machines. 
#Use sample = TRUE runs a subset analysis that is faster. For full replication set this to FALSE

USE_SAMPLE <- FALSE  # Set to FALSE for full analysis

#Setup------------------------------------------------------------------------------
##Check and Install Required Packages--------------------------------------------------------
# List of required CRAN packages
required_cran_packages <- c(
  "tidyverse",      # Basic Data Manipulation
  "quanteda",       # Analysis of Text data
  "quanteda.textmodels", # Extra Models for quanteda
  "scales",         # Adjust Y Axis Code
  "lubridate",      # Date Handling
  "mgcv",           # GAM models / Spline in figure 
  "ggformula"       # Formula interface for ggplot
)

# Check which packages are missing
missing_packages <- required_cran_packages[!required_cran_packages %in% installed.packages()[,"Package"]]

# Install missing packages
if (length(missing_packages) > 0) {
  message(paste("Installing missing packages:", paste(missing_packages, collapse = ", ")))
  install.packages(missing_packages, dependencies = TRUE)
} else {
  message("All CRAN packages already installed!")
}

# Check for quanteda.sentiment (GitHub package)
if (!"quanteda.sentiment" %in% installed.packages()[,"Package"]) {
  message("Installing quanteda.sentiment from GitHub...")
  if (!"remotes" %in% installed.packages()[,"Package"]) {
    install.packages("remotes")
  }
  remotes::install_github("quanteda/quanteda.sentiment")
}
##Load Libraries--------------------------------------------------------
library(tidyverse)      #Basic Data Manipulation
library(quanteda) #Analysis of Text data 
library(quanteda.textmodels) #Extra Models/ Sentiment Analysis
library(quanteda.sentiment) #Sentiment Analysis (from GitHub)

##Extra Packages for Presentation

library(scales) #Adjust Y Axis Code
library(lubridate) #Date Handling
library(mgcv) 
library(ggformula)

#Randomization Setup
quanteda_options(threads=6)
set.seed(42)

##Load in Data-------------------
#Requires 0_corpus_setup.R Code
corpus_quanteda <- read_rds("stateparl_pvs.rds")

##Example Code with subset----------------------
sample_perc <- 0.01 # 1% of corpus, can be changed 

if (USE_SAMPLE) {
  message(paste0("Using ", sample_perc * 100, "% sample of corpus for faster analysis"))
  corpus_working <- corpus_sample(corpus_quanteda, size=sample_perc*ndoc(corpus_quanteda))
} else {
  message("Using full corpus - this may take a long time")
  corpus_working <- corpus_quanteda
}

##Tokenize and DFM corpus-----------
#Note: This can also take a long time, recommend saving these
stateparl_token <-
  corpus_working %>%
  tokens(verbose=T,
         remove_punct=T,
         remove_numbers=T,
         remove_symbols=T) %>%
  tokens_tolower()

stateparl_dfm <-
  stateparl_token %>%
  dfm()

#Define Terms/Dictionary For Analysis-----------------
  #Note:  stopword removal "und" since "bund und länder" is otherwise filtered
  reference_dict <- dictionary(list(
    federal = c("*föderal*","bund länder*","*bundesstaat*"),
    eu = c("europäische* union*", "eu ","vertrag* von maastricht", "vertrag* von amsterdam", "vertrag* von nizza", "vertrag* von lissabon",
           "maastricht-vertrag*", "amsterdam-vertrag*", "nizza-vertrag*", "lissabon-vertrag*",
           "lissabonner vertrag*", "amsterdamer vertrag*")))

  ##Validation of Dictionary Terms-------------------
  #Note: This is not included in the analysis, but is highly recommended 
  ###Check the tokens the dictionary finds
  # tokens_lookup_results <- tokens_lookup(
  #   stateparl_token,
  #   dictionary=reference_dict,
  #   exclusive=T,
  #   nested_scope = "dictionary", #to account for multiple terms
  #   append_key=T,
  #   concatenator=" ")
  # 
  # tokens_lookup_list <- as.list(tokens_lookup_results)
  # ###Make these outputs readable
  # # create named array of equal lengths
  # x <- sapply(tokens_lookup_list, '[', seq(max(lengths(tokens_lookup_list))))
  # 
  # results_toks <- x %>%
  #   as_tibble() %>%
  #   pivot_longer(cols = everything(), names_to = "text", values_to = "tokens") %>%
  #   arrange(text) %>%
  #   filter(!is.na(tokens)) %>%  # remove NA values of each text
  #   mutate(id=as.numeric(str_remove(text,"text"))) %>%
  #   arrange(id) %>%
  #   mutate(pattern_result=str_split(tokens,"/")) %>%
  #   mutate(
  #     level = map_chr(pattern_result, ~.x[2]),
  #     pattern_result = map_chr(pattern_result, ~.x[1])
  #   ) %>%
  #   select(id,pattern_result)
  # 
  # ###See which terms are frequent/infrequent
  # results_toks %>%
  #   group_by(pattern_result) %>%
  #   summarize(n=n()) %>%
  #   arrange(-n) %>%
  #   print(n=50)


#Analysis 1: Salience--------------
  ##Case 1a: Salience of Topic in State Parliaments: Federalism over time--------
  ###Calculate proprotion of terms------------
  fed_time <- stateparl_dfm %>%
    dfm_tolower() %>%  # Convert tokens to lowercase
    dfm_remove(pattern = stopwords("de")) %>%  # Remove German stopwords
    dfm_group(groups = year_month) %>% #group by year 
    dfm_weight(scheme = "prop") %>% #Weigh terms by total terms used in that group
    dfm_lookup(dictionary = reference_dict) #Define Dictionary 

  ### Convert to data frame for analysis-------
  fed_time_data <-
    fed_time %>%
    convert(to="data.frame") %>%
    pivot_longer(cols = c("federal","eu"), names_to = "reference", values_to = "total") %>%
    mutate(date = ym(doc_id)) %>%  # Ensure date is in the correct format
    filter(total > 0) %>% #Exclude Empty observations
    filter(total < quantile(total, 0.95, na.rm = TRUE))  # Exclude strong outliers

  ##Quick exploratory plot for data validation
  fed_time_data %>%
    ggplot(aes(x=date, y=total, color=reference)) +
    geom_point(alpha=0.3, size=1) +
    geom_smooth()+
    theme_minimal()

#Analysis 2a: Sentiment in General--------------------------
  ##Data Preparation-----------------------------
  ###Clean tokens----------
  toks_clean <- stateparl_token %>%
    tokens_remove(pattern = stopwords("de"))
  
  ###Identify Salient tokens/parts of speeches-------------
  toks_fed <- tokens_keep(toks_clean, pattern = reference_dict$federal, window = 20)
  toks_eu <- tokens_keep(toks_clean, pattern = reference_dict$eu, window = 20)
  
  ### Convert tokens to dfm for each subdimension------
  dfm_fed <- dfm(toks_fed)
  dfm_eu <- dfm(toks_eu)
  
  ##Calculate Overall Sentiment Scores over Time------------
  dfm_fed_time <- dfm_fed %>%
    dfm_group(groups = year_month) %>%
    textstat_valence(dictionary = data_dictionary_sentiws, normalization = "all") %>%
    mutate(reference = "federal")  # Label for later merging
  
  dfm_eu_time <- dfm_eu %>%
    dfm_group(groups = year_month) %>%
    textstat_valence(dictionary = data_dictionary_sentiws, normalization = "all") %>%
    mutate(reference = "eu")  # Label for later merging
  
  dfm_sentiment_time_data <- bind_rows(dfm_fed_time, dfm_eu_time) %>%
    mutate(date = ym(doc_id)) %>%  # Ensure correct date format
    filter(sentiment != 0) %>%
    filter(sentiment < quantile(sentiment, 0.95, na.rm = TRUE)) %>%  # Exclude strong outliers top
    filter(sentiment > quantile(sentiment, 0.01, na.rm = TRUE))  # Exclude strong outliers bottom
  
  ##Quick exploratory plot for sentiment validation----------
  dfm_sentiment_time_data %>%
    ggplot(aes(x=date, y=sentiment, color=reference)) +
    geom_smooth(alpha=0.5) +
    geom_point(alpha=0.3, size=1) +
    geom_hline(yintercept=0, linetype="dashed") +
    theme_minimal()
  
#Analysis 2b: Sentiment by Party-----------
  ###Calculate Sentiment scores-----------
  dfm_fed_time_party <- dfm_fed %>%
    dfm_group(groups = interaction(year_month, affiliation, drop = TRUE)) %>%
    textstat_valence(dictionary = data_dictionary_sentiws, normalization = "all") %>%
    mutate(reference = "federal")  # Label for merging

  dfm_eu_time_party <- dfm_eu %>%
    dfm_group(groups = interaction(year_month, affiliation, drop = TRUE)) %>%
    textstat_valence(dictionary = data_dictionary_sentiws, normalization = "all") %>%
    mutate(reference = "eu")  # Label for merging

  ###Prepare Data for Analysis-----------
  dfm_sentiment_time_party_data <- bind_rows(dfm_fed_time_party, dfm_eu_time_party) %>%
    mutate(
      date = ym(sub("\\..*", "", doc_id)),  # Extract YYYY-MM (before ".")
      party = sub(".*\\.", "", doc_id)  # Extract party (after ".")
    ) %>%
    left_join(party_info, by = c("party" = "doc_id")) %>%  # Add full party names & colors
    # **Filter and Remove Extreme Outliers**
    filter(sentiment != 0) %>%
    filter(sentiment < quantile(sentiment, 0.95, na.rm = TRUE)) %>%  # Remove upper 5% outliers
    filter(sentiment > quantile(sentiment, 0.05, na.rm = TRUE)) %>%  # Remove lower 5% outliers
    filter(!is.na(full_name)) # Remove non party actors (include interjection and executive)
  
  ##Quick Visual Inspection----------
   ggplot(dfm_sentiment_time_party, aes(x = date, y = sentiment, color = reference, group = reference)) +
    geom_point(alpha = 0.25) +  # Show data points
    geom_smooth() +  # Smoothed trend line
    theme_minimal() +
    xlab("Year") +
    ylab("Sentiment around Federalism & EU \nin German State Parliaments") +
    facet_wrap(.~ full_name, ncol = 3, axes="all_x")  # Display each party separately
  
#Prepare Output for Publication----------------------
  ##Define Visuals and Themes---------
  ###Define Color Palette--------------
  event_colors <- list(
    federal = "#CC9900",
    eu = "#0057B7",
    other = "#000000"
  )
  
  reference_colors <- c("eu" = event_colors$eu, "federal" = event_colors$federal)
  reference_labels <- c("eu" = "References to the European Union", "federal" = "References to Federalism")
  sentiment_labels <- c("eu" = "Sentiment surrounding the EU", "federal" = "Sentiment surrounding Federalism")
  
  ###Define Event Markers for Visualizations--------------
  # Define individual event groups for readability
  federalism_events <- data.frame(
    date = ym(c("2006-07", "2009-08", "2017-06")),
    label = c("Federalism Reform 1", "Federalism Reform 2", "Silent Federalism Reform 3"),
    type = "federal"
  )
  
  eu_events <- data.frame(
    date = ym(c("2001-02", "2004-05", "2007-12", "2016-06")),
    label = c("Treaty of Nice", "EU Expansion 2004", "Lisbon Treaty", "Brexit Referendum"),
    type = "eu"
  )
  
  other_events <- data.frame(
    date = ym("2020-03"),
    label = "COVID-19 Pandemic",
    type = "other"
  )
  
  ### Merge all events and add programmatic styling
  all_events <- bind_rows(federalism_events, eu_events, other_events) %>%
    mutate(
      # Assign colors based on event type (reference centrally defined colors)
      event_color = case_when(
        type == "federal" ~ event_colors$federal,
        type == "eu" ~ event_colors$eu,
        type == "other" ~ event_colors$other
      ),
      # Assign vertical positioning (top or bottom of plot)
      v_position = case_when(
        type == "federal" ~ "top",
        type == "eu" ~ "bottom",
        type == "other" ~ "top"
      ),
      # Assign text justification based on position
      vjust_val = ifelse(v_position == "top", -1, 1),
      hjust_val = ifelse(v_position == "top", 0, 1)
    )
  
  ###Define Custom Theme--------------
  publication_theme <- theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 0, hjust = 1),
      plot.title = element_text(size = 12, face = "bold"),
      axis.title = element_text(size = 10)
    )
  
  
  ##Figure 1: Salience Over Time (All States Combined)--------------
  # Calculate y-axis extremes and add to events
  max_salience <- max(fed_time_data$total, na.rm = TRUE)
  
  events_salience <- all_events %>%
    mutate(y_position = ifelse(v_position == "top", max_salience, 0))
  
  fed_time_graph <- fed_time_data %>%
    ggplot() +
    aes(x=date, y=total, group=reference, color=reference) +
    stat_spline(df=20, linewidth=1) +
    geom_point(alpha=0.2) +
    xlab("Year") +
    ylab("Relative Frequency of References") +
    scale_y_continuous(labels = scales::percent) +
    scale_color_manual(values = reference_colors, labels = reference_labels) +
    guides(color = guide_legend(title = NULL)) +
    
    # Add event markers (vertical lines) - one layer for all events
    geom_vline(data = events_salience, 
               aes(xintercept = date), 
               linetype = "dashed", 
               color = events_salience$event_color,
               alpha = 0.7)+
    
    # Add event labels - one layer for all events
    geom_text(data = events_salience, 
              aes(x = date, y = y_position, label = label), 
              color = events_salience$event_color,
              angle = 270, 
              vjust = events_salience$vjust_val, 
              hjust = events_salience$hjust_val, 
              size = 2, 
              alpha = 1,
              inherit.aes = FALSE) +
    publication_theme
  
  fed_time_graph
  
  # Save figure 
  ggsave(plot = fed_time_graph, filename = "./Figures/fed_time.png", 
         width = 10, height = 7, scale = 0.75)
  
  ##Figure 2: Sentiment Over Time (All States and Parties Combined)--------------
  # Calculate y-axis extremes and add to events
  max_sentiment <- max(dfm_sentiment_time_data$sentiment, na.rm = TRUE)
  min_sentiment <- min(dfm_sentiment_time_data$sentiment, na.rm = TRUE)
  
  events_sentiment <- all_events %>%
    mutate(y_position = ifelse(v_position == "top", max_sentiment, min_sentiment))
  
  fig_sentiment_time <- dfm_sentiment_time_data %>%
    ggplot() +
    aes(x = date, y = sentiment, color = reference, group = reference) +
    geom_point(alpha = 0.3) +
    stat_spline(df = 25, linewidth = 1) +
    xlab("Year") +
    ylab("Sentiment around Federalism and EU \nin German State Parliaments") +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.8) +
    scale_color_manual(values = reference_colors, labels = sentiment_labels) +
    guides(color = guide_legend(title = NULL)) +
    
    # Add event markers (vertical lines) - one layer for all events
    geom_vline(data = events_sentiment, 
               aes(xintercept = date), 
               linetype = "dashed", 
               color = events_sentiment$event_color,
               alpha = 0.7)+
    
    # Add event labels - one layer for all events
    geom_text(data = events_sentiment, 
              aes(x = date, y = y_position, label = label), 
              color = events_sentiment$event_color,
              angle = 270, 
              vjust = events_sentiment$vjust_val, 
              hjust = events_sentiment$hjust_val, 
              size = 3, 
              alpha = 1,
              inherit.aes = FALSE) +
    
    publication_theme
  
  fig_sentiment_time
  
  # Save figure
  ggsave(plot = fig_sentiment_time, filename = "./Figures/fed_sentiment.png", 
         width = 10, height = 7, scale = 0.75)
  
  ##Figure 3: Sentiment Over Time by Party--------------
  fig_sentiment_time_party <- dfm_sentiment_time_party_data %>%
    ggplot() +
    aes(x = date, y = sentiment, color = reference, group = reference) +
    geom_point(alpha = 0.25) +  # Show data points
    stat_spline(df = 15, linewidth = 1) +  # Smoothed trend line
    xlab("Year") +
    ylab("Sentiment around Federalism & EU \nin German State Parliaments") +
    geom_hline(yintercept = 0, linetype = "dashed") +  # Neutral sentiment line
    facet_wrap(.~ full_name, ncol = 3, axes = "all_x") +  # Display each party separately
    scale_color_manual(values = reference_colors, labels = sentiment_labels) +
    guides(color = guide_legend(title = NULL)) +
    ylim(-0.01, 0.01) +
    publication_theme
  
  fig_sentiment_time_party
  
  # Save figure
  ggsave(plot = fig_sentiment_time_party, filename = "./Figures/sentiment_party.png", 
         width = 12, height = 10, scale = 0.75)
  