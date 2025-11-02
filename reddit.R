# 📌 Install Required Packages (Run Once)
install.packages(c("tidyverse","RColorBrewer","SnowballC","shiny", "shinydashboard", "ggplot2", "tidyr", "dplyr", "httr", "jsonlite", "DT","rvest","stringr","plotly","wordcloud","tm"))



# Load Libraries
library(tidyverse)
library(SnowballC)
library(RColorBrewer)
library(DT)
library(tidyr)
library(tm)  
library(wordcloud)
library(plotly)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(httr)
library(jsonlite)
library(DT)  # For interactive tables
library(rvest)
library(dplyr)
library(stringr)

# ----------------------------------------
# 1. Scrape Reddit Data (webscraping)
# ----------------------------------------

scrape_reddit <- function(subreddits, pages_per_sub = 20) {
  reddit_data <- data.frame()
  
  for (subreddit in subreddits) {
    cat("\nScraping Subreddit:", subreddit, "\n")
    base_url <- paste0("https://old.reddit.com/r/", subreddit, "/top/?t=week")
    
    for (i in 1:pages_per_sub) {
      cat("Scraping page:", i, "\n")
      Sys.sleep(runif(1, 2, 4))  # Add a delay of 2-4 seconds to prevent rate limiting
      
      page <- tryCatch(read_html(base_url), error = function(e) return(NULL))
      if (is.null(page)) break  # Stop if page fails
      
      # Extract Data
      titles <- page %>% html_nodes(".title a") %>% html_text()
      links <- page %>% html_nodes(".title a") %>% html_attr("href")
      authors <- page %>% html_nodes(".author") %>% html_text()
      scores <- page %>% html_nodes(".score.unvoted") %>% html_text()
      comments <- page %>% html_nodes(".comments") %>% html_text() %>% str_extract("\\d+") %>% as.numeric()
      post_time <- page %>% html_nodes("time") %>% html_text()
      flair <- page %>% html_nodes(".linkflairlabel") %>% html_text()
      
      # Handle missing values
      scores <- ifelse(scores == "", NA, as.numeric(scores))
      flair <- ifelse(length(flair) == 0, NA, flair)
      post_time <- ifelse(length(post_time) == 0, NA, post_time)
      
      # Detect post type
      post_type <- ifelse(grepl("\\.jpg|\\.png|\\.gif|\\.jpeg", links), "Image",
                          ifelse(grepl("v.redd.it", links), "Video", "Text"))
      
      # Extract text content (if available)
      text_content <- page %>% html_nodes(".expando .md p") %>% html_text()
      text_content <- ifelse(length(text_content) == 0, NA, text_content)
      
      # Adjust lengths to avoid mismatches
      max_length <- max(length(titles), length(links), length(authors), length(scores), 
                        length(comments), length(post_time), length(flair), length(post_type), length(text_content))
      
      # Ensure equal lengths
      titles <- c(titles, rep(NA, max_length - length(titles)))
      links <- c(links, rep(NA, max_length - length(links)))
      authors <- c(authors, rep(NA, max_length - length(authors)))
      scores <- c(scores, rep(NA, max_length - length(scores)))
      comments <- c(comments, rep(NA, max_length - length(comments)))
      post_time <- c(post_time, rep(NA, max_length - length(post_time)))
      flair <- c(flair, rep(NA, max_length - length(flair)))
      post_type <- c(post_type, rep(NA, max_length - length(post_type)))
      text_content <- c(text_content, rep(NA, max_length - length(text_content)))
      
      # Create a data frame
      temp_data <- data.frame(
        Title = titles,
        Link = links,
        Author = authors,
        Score = scores,
        Comments = comments,
        Post_Time = post_time,
        Flair = flair,
        Post_Type = post_type,
        Text_Content = text_content,
        Subreddit = subreddit,
        stringsAsFactors = FALSE
      )
      
      # Append to main data
      reddit_data <- rbind(reddit_data, temp_data)
      
      # Check next page
      next_page <- page %>% html_node(".next-button a") %>% html_attr("href")
      if (is.na(next_page)) break  # Stop if no next page
      base_url <- next_page
    }
  }
  return(reddit_data)
}


# Scrape from multiple subreddits 
subreddits <- c("andhra_pradesh", "ArunachalPradesh", "assam","bihar","Chhattisgarh","Goa","gujarat","Haryana","HimachalPradesh","Jharkhand","karnataka","Kerala","MadhyaPradesh","Maharashtra","manipur","Meghalaya","mizo","NAGALAND","Odisha","punjab","Rajasthan","sikkim","TamilNadu","Telangana","TripuraNE","uttarpradesh","uttarakhand","westbengal")
reddit_data <- scrape_reddit(subreddits, pages_per_sub = 30)

# Replace Missing values
clean_data <- function(df) {
  df$Author[is.na(df$Author)] <- "Unknown"
  df$Score[is.na(df$Score)] <- 0
  df$Comments[is.na(df$Comments)] <- 0
  df$Post_Time[is.na(df$Post_Time)] <- "Not Available"
  df$Flair[is.na(df$Flair)] <- "No Flair"
  df$Text_Content[is.na(df$Text_Content)] <- "No Text"
  return(df)
}

reddit_data <- clean_data(reddit_data)

summary(reddit_data)
str(reddit_data)

# Save to CSV
write.csv(reddit_data, "reddit_data.csv", row.names = FALSE)

# Check record count
cat("Total records scraped:", nrow(reddit_data), "\n")

# Preview data
head(reddit_data)



#--------------------------

reddit_data <- read.csv("reddit_data.csv", stringsAsFactors = FALSE)

# Convert Score & Comments to numeric
reddit_data$Score <- as.numeric(reddit_data$Score)
reddit_data$Comments <- as.numeric(reddit_data$Comments)

# Convert Post_Time to Date (if applicable)
reddit_data$Post_Time <- as.Date(reddit_data$Post_Time, format="%Y-%m-%d")

#-------------------------------------------------------------------





# ----------------------------------------
#  2. Build Shiny Dashboard
# ----------------------------------------



# Load dataset (ensure your CSV has 'Subreddit', 'Title', 'Text_Content' columns)
data <- read.csv("reddit_data.csv")

# Define word categories
word_categories <- list(
  happiness = c("joy", "happy", "excited", "cheerful","yay","hurray"),
  depression = c("sad", "lonely", "anxious", "depressed","alone","low"),
  anger = c("angry", "hate", "furious", "rage", "fuck", "bsdk", "lawde", "chutiya"),
  gratitude = c("thank", "grateful", "appreciate", "kind"),
  romantic = c("love", "crush", "date", "relationship", "sex", "beautiful", "boyfriend","girlfriend"),
  religious = c("god", "church", "prayer", "faith", "muslim","christian","Hindu","sikh","jain","Buddhist"),
  political = c("election", "government", "bjp", "congress", "vote", "dmk", "admk", "IND","TDP","YSRCP","IUML"),
  tech = c("ai", "coding", "startup", "technology","gpt","developer","money"),
  foodie = c("food", "restaurant", "cooking", "biryani", "chai","chicken", "fish", "mutton"),
  fitness = c("gym", "workout", "running", "calories", "exercise","military","marathon"),
  formal = c("sir", "respectfully", "regards", "namasthe","namaste","vanakkam","namaskaram","namaskar"),
  informal = c("dude", "bro", "lol", "chill","yo","guys","chick"),
  literary = c("complex", "philosophy", "literature", "poetry","writing","novel"),
  meme = c("meme", "lol", "lmao", "troll","lol","hehe","funny","ngl","irl"),
  questions = c("how", "why", "what", "help","where","when","is"),
  complaints = c("bad", "problem", "issue","terrible","horrible"),
  career = c("job", "interview", "career", "salary","money","business","startup","founder","company"),
  travel = c("trip", "travel", "vacation","long","tour","flight","train"),
  debate = c("disagree", "argue", "vs","fight",""),
  crypto = c("bitcoin", "stocks", "trading","eth","crypto","etherium","tokens","USD","block","chain","blockchain"),
  superstition = c("astrology", "ghost", "superstition","bhoot","kalajadu","blackmagic","religion")
)

# Function to count word occurrences in a text
count_words <- function(text, words) {
  sum(str_count(tolower(text), paste(words, collapse = "|")))
}

# Apply word counting to the dataset
for (category in names(word_categories)) {
  data[[category]] <- apply(data, 1, function(row) count_words(paste(row["Title"], row["Text_Content"]), word_categories[[category]]))
}

# UI
ui <- fluidPage(
  titlePanel("Reddit Subreddit Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selectedCategory", "Select Category", choices = names(word_categories)),
      selectInput("selectedSubreddit", "Select Subreddit for Word Cloud", choices = unique(data$Subreddit))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Score Trend", plotlyOutput("scoreTrend")),
        tabPanel("Comment Trend", plotlyOutput("commentTrend")),
        tabPanel("Flair Distribution", plotlyOutput("flairPie")),
        
        tabPanel("Subreddit Rankings", plotlyOutput("subredditPlot")),
        tabPanel("Word Cloud", plotOutput("wordCloud")),
        tabPanel("Most Active Subreddits", plotlyOutput("activeSubredditPlot")),
        tabPanel("Sentiment Distribution", plotlyOutput("sentimentPlot")),
        tabPanel("Topic Heatmap", plotlyOutput("heatmapPlot")),
        tabPanel("Word Frequency", plotlyOutput("wordFreqPlot")),
        tabPanel("Data Table", DTOutput("dataTable"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Most Active Subreddits (based on number of posts)
  output$activeSubredditPlot <- renderPlotly({
    data %>%
      count(Subreddit, sort = TRUE) %>%
      head(10) %>%
      plot_ly(x = ~Subreddit, y = ~n, type = "bar", name = "Post Count") %>%
      layout(title = "Most Active Subreddits")
  })
  
  # Subreddit Ranking Plot
  output$subredditPlot <- renderPlotly({
    data %>%
      group_by(Subreddit) %>%
      summarise(score = sum(.data[[input$selectedCategory]], na.rm = TRUE)) %>%
      arrange(desc(score)) %>%
      head(10) %>%
      plot_ly(x = ~Subreddit, y = ~score, type = "bar", name = input$selectedCategory) %>%
      layout(title = paste("Top Subreddits by", input$selectedCategory))
  })
  
  # Flair Distribution Pie Chart
  output$flairPie <- renderPlotly({
    subset_data <- data %>% filter(Subreddit == input$selectedSubreddit) %>%
      count(Flair)
    
    plot_ly(subset_data, labels = ~Flair, values = ~n, type = "pie", textinfo = "label+percent",
            marker = list(colors = brewer.pal(8, "Set3"))) %>%
      layout(title = paste("Flair Distribution in", input$selectedSubreddit))
  })
  
  # Score Trend Line Chart (Using Row Index)
  output$scoreTrend <- renderPlotly({
    subset_data <- data %>% filter(Subreddit == input$selectedSubreddit)
    
    plot_ly(subset_data, x = ~seq_along(Score), y = ~Score, type = "scatter", mode = "lines+markers",
            line = list(color = "blue")) %>%
      layout(title = paste("Score Trend for", input$selectedSubreddit),
             xaxis = list(title = "Post Index"),
             yaxis = list(title = "Score"))
  })
  
  # Comment Trend Line Chart (Using Row Index)
  output$commentTrend <- renderPlotly({
    subset_data <- data %>% filter(Subreddit == input$selectedSubreddit)
    
    plot_ly(subset_data, x = ~seq_along(Comments), y = ~Comments, type = "scatter", mode = "lines+markers",
            line = list(color = "red")) %>%
      layout(title = paste("Comment Trend for", input$selectedSubreddit),
             xaxis = list(title = "Post Index"),
             yaxis = list(title = "Comments"))
  })
  
  # Sentiment Distribution Per Subreddit
  output$sentimentPlot <- renderPlotly({
    data %>%
      group_by(Subreddit) %>%
      summarise(Happiness = sum(happiness),
                Depression = sum(depression),
                Anger = sum(anger),
                Gratitude = sum(gratitude)) %>%
      gather(key = "Sentiment", value = "Count", -Subreddit) %>%
      arrange(desc(Count)) %>%
      head(40) %>%
      plot_ly(x = ~Subreddit, y = ~Count, color = ~Sentiment, type = "bar") %>%
      layout(title = "Sentiment Distribution Across Subreddits")
  })
  
  # Topic-Based Heatmap
  output$heatmapPlot <- renderPlotly({
    heatmap_data <- data %>%
      group_by(Subreddit) %>%
      summarise(Tech = sum(tech),
                Political = sum(political),
                Foodie = sum(foodie),
                Fitness = sum(fitness)) %>%
      gather(key = "Topic", value = "Count", -Subreddit)
    
    plot_ly(heatmap_data, x = ~Subreddit, y = ~Topic, z = ~Count, type = "heatmap", colors = "Blues") %>%
      layout(title = "Topic Intensity Heatmap")
  })
  
  
  
  # Word Cloud for Selected Subreddit
  output$wordCloud <- renderPlot({
    subset_data <- data %>% filter(Subreddit == input$selectedSubreddit)
    text_corpus <- Corpus(VectorSource(paste(subset_data$Title, subset_data$Text_Content)))
    text_corpus <- tm_map(text_corpus, content_transformer(tolower))
    text_corpus <- tm_map(text_corpus, removePunctuation)
    text_corpus <- tm_map(text_corpus, removeWords, c(stopwords("english"), "text"))  # Remove "text"
    
    wordcloud(text_corpus, max.words = 100, random.order = FALSE, colors = brewer.pal(8, "Dark2"))
  })
  
  
  
  # Word Frequency Bar Chart for Selected Subreddit
  output$wordFreqPlot <- renderPlotly({
    subset_data <- data %>% filter(Subreddit == input$selectedSubreddit)
    words <- unlist(strsplit(tolower(paste(subset_data$Title, subset_data$Text_Content)), "\\s+"))
    words <- words[!words %in% stopwords("english") & nchar(words) > 3]
    word_counts <- as.data.frame(table(words)) %>% arrange(desc(Freq)) %>% head(10)
    
    plot_ly(word_counts, x = ~words, y = ~Freq, type = "bar", name = "Word Frequency") %>%
      layout(title = paste("Most Common Words in", input$selectedSubreddit))
  })
  
  # Data Table
  output$dataTable <- renderDT({
    datatable(data)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
