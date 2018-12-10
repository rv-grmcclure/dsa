library("spotifyr")
library("knitr")
library("tidyverse")
library("magrittr") 

Sys.setenv(SPOTIFY_CLIENT_ID = '8d06ab962d224ff49254ee612b925af1')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '024ab8415a424459be1df5f98d0b926c')
access_token <- get_spotify_access_token()
thankUNext <- get_artist_audio_features("Ariana Grande",access_token)
#85x23
names(thankUNext)
length(unique(thankUNext$album_name)) #6

lengthOfAlbums <- nchar(thankUNext$album_name)
uniqueAlbumNames <- unique(thankUNext$album_name)
unique(lengthOfAlbums)


uniqueAlbumNames <- thankUNext %>% mutate(len = nchar(thankUNext$album_name)) %>% select(album_name,len) %>% distinct() %>% arrange(desc(len))

#2c
?ggplot
ggplot(uniqueAlbumNames,aes(x=album_name,y=len)) + 
  geom_bar(stat = "identity")

#3
thankUNext %>% group_by(album_name) %>% summarize(tracks = length(unique(track_name)))

#4
