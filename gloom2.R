library(tidyverse)
library(httr)
library(stringr)
library(magrittr)

get_artists <- function(artist_name) {
  
  # Search Spotify API for artist name
  res <- GET('https://api.spotify.com/v1/search', query = list(q = artist_name, type = 'artist')) %>%
    content %>% .$artists %>% .$items
  
  # Clean response and combine all returned artists into a dataframe
  artists <- map_df(seq_len(length(res)), function(x) {
    list(
      artist_name = res[[x]]$name,
      artist_uri = str_replace(res[[x]]$uri, 'spotify:artist:', '') # remove meta info from the uri string
    )
  })
  
  return(artists)
}

#artist_info <- get_artists(artists$Artists)

artist_info <- data.frame()
n <- nrow(artists)
for (i in 1:n) {
  artist_info <- merge(artist_info, get_artists(artists$Artists[i]), all=TRUE)
  
}
#artist_info <- merge(get_artists('Radiohead'), get_artists('The Beatles'), all=TRUE)

str(artist_info)

# Filter out other artist matches
artist_info <- artist_info %>% 
  filter(artist_name %in% artists$Artists)

#Filter out duplicates
#artist_info <- artist_info[-c(20,22,36,40,41,42,43,44,49,50,51,66,78,85),]

#End up with only 84 of the original artists 


library(lubridate)
library(rvest)

get_albums <- function(artist_uri) {
  albums <- GET(paste0('https://api.spotify.com/v1/artists/', artist_uri,'/albums')) %>% content
  
  map_df(1:length(albums$items), function(x) {
    tmp <- albums$items[[x]]
    
    # Make sure the album_type is not "single"
    if (tmp$album_type == 'album') {
      data.frame(album_uri = str_replace(tmp$uri, 'spotify:album:', ''),
                 album_name = str_replace_all(tmp$name, '\'', ''),
                 stringsAsFactors = F) %>%
        mutate(album_release_date = GET(paste0('https://api.spotify.com/v1/albums/', str_replace(tmp$uri, 'spotify:album:', ''))) %>% content %>% .$release_date, # you need a separate call to on "albums" to get release date.
               album_release_year = ifelse(nchar(album_release_date) == 4, year(as.Date(album_release_date, '%Y')), year(as.Date(album_release_date, '%Y-%m-%d'))) # not all album_release_dates have months, so I created album_release year for sorting
        )
    } else {
      NULL
    }
    
  }) %>% filter(!duplicated(tolower(album_name))) %>%  # Sometimes there are multiple versions (just with different capitalizations) of the same album
    arrange(album_release_year)
}

#album_info <- get_albums(artist_info$artist_uri)
album_info <- matrix()
artist_matrix <- as.matrix(artist_info)
n <- nrow(artist_info)
for(i in 1:n){
  album <- get_albums(artist_uri[i])
album_info <- rbind(album)
}

str(album_info)



get_tracks <- function(album_info2) {
 
  client_id <- 'c001c9fe501341baa4c3136c3fa76cda'
  client_secret <- '395cdbbb53744241a0183cb7e7d32b73'
  access_token <- POST('https://accounts.spotify.com/api/token',
                       accept_json(), authenticate(client_id, client_secret),
                       body = list(grant_type='client_credentials'),
                       encode = 'form', httr::config(http_version=2)) %>% content %>% .$access_token
  
  track_info <- map_df(album_info$album_uri, function(x) {
    tracks <- GET(paste0('https://api.spotify.com/v1/albums/', x, '/tracks')) %>% 
      content %>% 
      .$items 
    
    uris <- map(1:length(tracks), function(z) {
      gsub('spotify:track:', '', tracks[z][[1]]$uri)
    }) %>% unlist %>% paste0(collapse=',')
    
    res <- GET(paste0('https://api.spotify.com/v1/audio-features/?ids=', uris),
               query = list(access_token = access_token)) %>% content %>% .$audio_features
    df <- unlist(res) %>% 
      matrix(nrow = length(res), byrow = T) %>% 
      as.data.frame(stringsAsFactors = F)
    names(df) <- names(res[[1]])
    df <- df %>% 
      mutate(album_uri = x,
             track_number = row_number()) %>% 
      rowwise %>% 
      mutate(track_name = tracks[[track_number]]$name) %>%
      ungroup %>% 
      left_join(album_info, by = 'album_uri') %>% 
      rename(track_uri = id) %>% 
      select(-c(type, track_href, analysis_url, uri))
    return(df)
  }) %>%
    mutate_at(c('album_uri', 'track_uri', 'album_release_date', 'track_name', 'album_name', 'artist_img'), funs(as.character)) %>%
    mutate_at(c('danceability', 'energy', 'key', 'loudness', 'mode', 'speechiness', 'acousticness', 'album_release_year',
                'instrumentalness', 'liveness', 'valence', 'tempo', 'duration_ms', 'time_signature', 'track_number'), funs(as.numeric(gsub('[^0-9.-]+', '', as.character(.))))) # for some reason parse_number() from readr doesn't work here
  return(merge(track_info, track_info, all=TRUE))
  }
         
spotify_df <- data.frame()
for(album_uri in album_info2){
spotify_df <- merge(spotify_df, get_tracks(album_uri), ALL=TRUE)
}
str(spotify_df)


