norm_key <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  gsub("_+", "_", x)
}

lter_name_key <- data.frame(
  source = norm_key(c("Swedish Goverment", "Swedish Government", "Carey")),
  target = norm_key(c("Sweden", "Sweden", "PIE")),
  stringsAsFactors = FALSE
)

stream_name_key <- data.frame(
  lter = norm_key(c("Catalina Jemez", "Catalina Jemez", "USGS", "Walker Branch", "Walker Branch")),
  source = norm_key(c("MG_WEIR", "OR_low", "Sopchoppy River", "East Fork", "West Fork")),
  target = norm_key(c("MGWEIR", "ORlow", "SOPCHOPPY RIVER", "east fork", "west fork")),
  stringsAsFactors = FALSE
)

clean_lter_key <- function(lter) {
  out <- norm_key(lter)
  hit <- match(out, lter_name_key$source)
  out[!is.na(hit)] <- lter_name_key$target[hit[!is.na(hit)]]
  out
}

clean_stream_key <- function(lter_key, stream_name) {
  out <- norm_key(stream_name)
  if (!nrow(stream_name_key)) {
    return(out)
  }

  for (i in seq_len(nrow(stream_name_key))) {
    hit <- lter_key == stream_name_key$lter[[i]] &
      (out == stream_name_key$source[[i]] | out == stream_name_key$target[[i]])
    out[hit] <- stream_name_key$target[[i]]
  }
  out
}

split_stream_id <- function(stream_id) {
  stream_id <- as.character(stream_id)
  has_sep <- grepl("__", stream_id, fixed = TRUE)
  data.frame(
    LTER = ifelse(has_sep, sub("__.*$", "", stream_id), NA_character_),
    Stream_Name = ifelse(has_sep, sub("^[^_]*__", "", stream_id), stream_id),
    stringsAsFactors = FALSE
  )
}

site_key_from_parts <- function(lter, stream_name) {
  lter_key <- clean_lter_key(lter)
  stream_key <- clean_stream_key(lter_key, stream_name)
  paste(lter_key, stream_key, sep = "||")
}

site_key_from_stream_id <- function(stream_id) {
  parts <- split_stream_id(stream_id)
  site_key_from_parts(parts$LTER, parts$Stream_Name)
}
