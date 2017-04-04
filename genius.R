artists <- read.csv(file="c:/Documents/Python-64/artists.csv", header=TRUE, sep=",")
artists1 <- as.matrix(artists)

token <- 'OiLj59jzZdtQnz4Bqd2HAniqJwG5lhXOH4B7QApsL2PXcXdovqkmJHw_mEgfBQn7'

genius_get_artists <- function(artist_name, n_results = 10) {
  baseURL <- 'https://api.genius.com/search?q=' 
  requestURL <- paste0(baseURL, gsub(' ', '%20', artist_name),
                       '&per_page=', n_results,
                       '&access_token=', token)
  
  res <- GET(requestURL) %>% content %>% .$response %>% .$hits
  
  map_df(1:length(res), function(x) {
    tmp <- res[[x]]$result$primary_artist
    list(
      artist_id = tmp$id,
      artist_name = tmp$name
    )
  }) %>% unique
}

g_artists <- data.frame(matrix(ncol = 2, nrow = 0))
for(x in artists$Artists[[x]]){
g_artists <- merge(g_artists, genius_get_artists(x), all=TRUE)
}

for (artist_id in g_artists){
baseURL <- 'https://api.genius.com/artists/' 
requestURL <- paste0(baseURL, artist_id, '/songs')

track_lyric_urls <- list()
i <- 1
while (i > 0) {
  tmp <- GET(requestURL, query = list(access_token = token, per_page = 50, page = i)) %>% content %>% .$response
  track_lyric_urls <- c(track_lyric_urls, tmp$songs)
  if (!is.null(tmp$next_page)) {
    i <- tmp$next_page
  } else {
    break
  }
}

}

library(rvest)

lyric_scraper <- function(url) {
  read_html(url) %>% 
    html_node('lyrics') %>% 
    html_text
}

genius_df <- map_df(1:length(track_lyric_urls), function(x) {
  lyrics <- lyric_scraper(track_lyric_urls[[x]]$url)
  # strip out non-lyric text and extra spaces
  lyrics <- str_replace_all(lyrics, '\\[(Verse [[:digit:]]|Chorus|Outro|Verse|Refrain|Hook|Bridge|Intro|Instrumental)\\]|[[:digit:]]', '')
  lyrics <- str_replace_all(lyrics, '\\n', ' ')
  lyrics <- str_replace_all(lyrics, '([A-Z])', ' \\1')
  lyrics <- str_replace_all(lyrics, ' {2,}', ' ')
  lyrics <- tolower(str_trim(lyrics))
  tots <- list(
    track_name = track_lyric_urls[[x]]$title,
    lyrics = lyrics
  )
  return(tots)
})

str(genius_df)


track_df <- spotify_df %>%
  mutate(track_name_join = tolower(str_replace(track_name, '[[:punct:]]', ''))) %>%
  left_join(genius_df, by = 'track_name_join') %>%
  select(track_name, valence, duration_ms, lyrics, album_name, album_release_year, album_img)

str(track_df)

library(tidytext)

sad_words <- sentiments %>% 
  filter(lexicon == 'nrc', sentiment == 'sadness') %>% 
  select(word) %>% 
  mutate(sad = T)

sent_df <- track_df %>% 
  unnest_tokens(word, lyrics) %>%
  anti_join(stop_words, by = 'word') %>%
  left_join(sad_words, by = 'word') %>%
  group_by(track_name) %>% 
  summarise(pct_sad = round(sum(sad, na.rm = T) / n(), 4),
            word_count = n()) %>% 
  ungroup


library(scales)

track_df <- track_df %>%
  left_join(sent_df, by = 'track_name') %>%
  mutate_at(c('pct_sad', 'word_count'), funs(ifelse(is.na(.), 0, .))) %>% 
  mutate(lyrical_density = word_count / duration_ms * 1000,
         happiness = round(rescale(1 - ((1 - valence) + (pct_sad * (1 + lyrical_density))) / 2, to = c(1, 100)), 2)) 

avg_lineB <- tribe_tracksB %>% 
  group_by(album_release_year, album_name, album_img) %>% 
  summarise(avg = mean(gloom_index)) %>% 
  ungroup 

library("highcharter")

hchart(track_df, "scatter", hcaes(x = as.numeric(as.factor(album_release_year)), y = happiness, group = album_name)) %>%
  hc_add_series(data = avg_line, type = 'line') %>%
  hc_xAxis(title = list(text = 'Album'), labels = list(enabled = F)) %>% 
  hc_yAxis(max = 100, title = list(text = 'Happiness Rating')) %>% 
  hc_title(text = 'Song Sadness')