print_question <- function (rnd, q) {
  q_row <- quiz %>% 
    filter(round == rnd & num == q)
  
  HTML(paste0("<p class='question'>", q_row$question,"</p>",
              "<img class='quarto-figure r-stretch quarto-figure-center' src='img/", q_row$img1,"'>",
       if (q_row$options != "") {
         paste0("<div class='fragment' data-fragment-index='0'><p>", q_row$options, "</p></div>")
       }))
  
}

recap_round <- function (rnd) {
  round_q <- quiz %>% 
    filter(round == rnd)
  
  ol <- ""
  
  for (i in 1:nrow(round_q)) {
    q_row <- round_q %>% 
      filter(num == i)
    
    ol <- (paste0(ol, "<li><p class='question'>", q_row$question, " ", gsub("<br/>", " ", q_row$options, fixed = TRUE), "</p></li>"))
  }
  
  HTML("<ol>", ol, "</ol>")
  
}

print_answer <- function (rnd, q) {
  q_row <- quiz %>% 
    filter(round == rnd & num == q)
  
  HTML(paste0("<p></p>",
              "<img class='quarto-figure r-stretch quarto-figure-center' src='img/", q_row$img2,"'>"),
       "<p class='question'>", q_row$answer, "</p>",)
  
}

music_question <- function (q) {
  HTML(paste0('<audio src="music/question_', q, '.mp3" controls data-autoplay data-ignore>'))
}

round_name <- function (rnd) {
  rounds %>% 
    filter(round == rnd) %>% 
    pull(title) %>% 
    toupper()
}