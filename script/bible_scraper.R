setwd('~/Github/Racz2026past/')

library(rvest)
library(httr)
library(xml2)

# Define the gospels and their chapters
gospels = list(
  Matthew = 28,
  Mark = 16,
  Luke = 24,
  John = 21
)

# Initialize results
all_headings = data.frame(
  book = character(),
  chapter = integer(),
  verse = integer(),
  heading = character(),
  stringsAsFactors = FALSE
)

# Loop through each gospel
for (book in names(gospels)) {
  for (chapter in 1:gospels[[book]]) {
    
    # Construct URL
    url = paste0("https://www.biblegateway.com/passage/?search=",
                  book, "%20", chapter, "&version=NIV")
    
    # Get page
    page = read_html(url)
    
    # Get the main passage content
    passage = page %>% html_node(".passage-content")
    
    # Get all headings with their following verse numbers
    headings = passage %>% 
      html_nodes("h3, .heading")
    
    for (heading in headings) {
      heading_text = html_text(heading, trim = TRUE)
      
      # Find the next verse number after this heading
      next_verse = heading %>%
        xml_find_first("following::sup[@class='versenum']")
      
      verse_num = NA
      if (!is.na(next_verse)) {
        verse_text = html_text(next_verse, trim = TRUE)
        verse_num = as.integer(gsub("[^0-9]", "", verse_text))
      }
      
      # Store result
      temp = data.frame(
        book = book,
        chapter = chapter,
        verse = verse_num,
        heading = heading_text,
        stringsAsFactors = FALSE
      )
      all_headings = rbind(all_headings, temp)
    }
    
    # Print progress
    cat(book, chapter, "\n")
    
    # Wait to avoid getting blocked
    Sys.sleep(2)
  }
}

# Save results
write.csv(all_headings, "dat/niv_gospel_headings.csv", row.names = FALSE)

# View
print(all_headings)
