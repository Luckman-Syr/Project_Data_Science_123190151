library(rvest)
baseUrl = "https://www.tripadvisor.com"
classificationScore <- function(score){
  if("bubble_50" == score){
    score = 5
    #return(5)
  }else if("bubble_40" == score){
    score = 4
    #return(4)
  }else if("bubble_30" == score){
    score = 3
    #return(3)
  }else if("bubble_20" == score){
    score = 2
    #return(2)
  }else if("bubble_10" == score){
    score = 1
    #return(1)
  }
}

get_hotel_reviews <- function(url, size = -1, incProgress = NULL) {
    #deklarasi data frame
    reviews <- character()
    reviewers <- character()
    
    url = paste(baseUrl, url, sep="")

    reviewPage <- read_html(url)
    #reviewPage

    #comment review
    review <- reviewPage %>%
      html_nodes('.IRsGHoPm') %>%
      html_text()
    
    #data nama
    reviewer <- reviewPage %>%
      html_nodes('._1r_My98y') %>%
      html_text()
    
    
    #data framenya
    reviews <- c(reviews, review)
    reviewers <- c(reviewers, reviewer)
    
    #mengecheck data apakah kosong atau tidak
    if(!is.null(incProgress)) {
      incProgress(5/size) 
    }
    
    #next page data 
    nextPage <- reviewPage %>%
      html_nodes('._16gKMTFp .next') %>%
      html_attr('href')
    
    #mengecheck next page hrefnya ada atau kosong
    if(is_empty(nextPage)) {
      nextPage = NA
    }
    
    while (!is.na(nextPage) & (length(reviews) < size | size == -1)) {
      print(paste(length(reviews), "data", "collected"))
      
      reviewUrl <- paste(baseUrl, nextPage, sep = "")
      #print(reviewUrl)
      
      reviewPage <- read_html(reviewUrl)
      
      #comment review
      review <- reviewPage %>%
        html_nodes('.IRsGHoPm') %>%
        html_text()
      
      #data nama
      reviewer <- reviewPage %>%
        html_nodes('._1r_My98y') %>%
        html_text()

      #data framenya
      reviews <- c(reviews, review)
      reviewers <- c(reviewers, reviewer)
      
      #mengecheck data apakah kosong atau tidak
      nextPage <- reviewPage %>%
        html_nodes('._16gKMTFp .next') %>%
        html_attr('href')
      
      #mengecheck next page hrefnya ada atau kosong
      if(is_empty(nextPage)) {
        nextPage = NA
      }
      
      if(!is.null(incProgress)) {
        incProgress(5/size) 
      }
    }
    
    totalReviews <- length(reviews)
    if(totalReviews < size || size == -1) {
      size = totalReviews
    }
    print(paste(length(reviews), "data", "collected"))
    
    return(data.frame(reviewer = reviewers, review = reviews, stringsAsFactors = FALSE)[1 : size,])
}
