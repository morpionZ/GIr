myIP <- function() {
  # Uses jsonip.com to retrieve computer's public IP address.
  #
  # Args:
  #   No arguments. 
  #
  # Returns:
  #   A character string containing the computer's public IP address. 
  
  json <- getURL("http://jsonip.com/")
  json <- fromJSON(json)
  json$ip
}

getIMGs <- function(keywords, number = 5, WRITE = TRUE, RETURN = FALSE, imgtype = "all") {
  # Retrieve and save pictures from a Google Images search.
  #
  # Args:
  #   keywords: A vector containing the keywords for the Google Images search.
  #   number: The number of results to return (default: 5). 
  #   WRITE: Logical. Should the function save pictures to hard drive (default: 
  #     TRUE). If TRUE, the pictures will be saved in the working directory in a 
  #     folder named after the provided keywords.
  #   RETURN: Logical. Should the function return a list of the images (default: 
  #     FALSE). Users should use this argument carefully, especially if they 
  #     request a large number of images, to avoid running out of memory. 
  #   imgtype: A character string indicating the type of images returned by the 
  #     Google Images search (default: "all"). Possible values are "all" (all 
  #     types of picture), "photo" (only photographs), "face" (only pictures 
  #     containing faces), "clipart" (only cliparts) and "lineart" (only line 
  #     art).
  #
  # Returns:
  #   If RETURN is TRUE, a list of objects of EBImage class Image. If FALSE, the 
  #   function does not return anything
  
  ## Verifications
  if (!is.vector(keywords)) {
    stop("Error! The first argument ('keywords') must be a vector.")
  }
  if (number < 1 | number%%1!=0) {
    stop("Error! The 'number' argument must be a positive, non-zero whole number.")
  }
  if (!is.logical(WRITE)) {
    stop("Error! The 'WRITE' argument must be of type 'logical' (i.e., TRUE or FALSE)")
  }
  if (!is.logical(RETURN)) {
    stop("Error! The 'RETURN' argument must be of type 'logical' (i.e., TRUE or FALSE)")
  }
  if (!WRITE & !RETURN) {
    stop("Error! At least one of 'WRITE' and 'RETURN' must be TRUE.")
  }
  if (!any(c("all", "photo", "face", "clipart", "lineart") == imgtype)) {
    stop("Error! The only authorized image types ('imgtype') are: 'all', 'photo', 'face', 'clipart' and 'lineart'.")
  }
  
  ## Prepare environment
  keywords <- paste0(keywords, collapse = "+")
  
  if (WRITE) {
    dir.create(keywords)
  }
  
  start <- 0
  counter <- 0
  IP <- myIP()
  img.url <- NULL
  
  if (RETURN) {
    imgs <- list()
  }
  
  ## Collect images
  print(paste0("Collecting valid images for '", keywords, "'"))
  pb <- txtProgressBar(min = 0, max = number, style = 3)
  
  while (counter < number) {
    search.url <- paste0("https://ajax.googleapis.com/ajax/services/search/images?v=1.0", 
                         "&q=", keywords, 
                         "&start=", start,
                         "&rsz=", 8,
                         "&userip=", IP,
                         "&imgtype=", imgtype)
    raw.json <- getURL(search.url)
    parsed.json <- fromJSON(raw.json)
    img.url <- parsed.json$responseData$results$unescapedUrl
    
    for (i in 1:length(img.url)) {
      if (counter < number) {
        try({
          img <- readImage(img.url[i])
          
          counter = counter + 1
          
          if (WRITE) {
            writeImage(img, 
                       files = paste0(keywords, "/", counter, ".jpg"), 
                       type = "jpeg")
          }
          
          if (RETURN) {
            imgs[[counter]] <- img
          }
          
          setTxtProgressBar(pb, counter)},
          
          silent = TRUE
        )
      }
    }
    
    start <- start + 7
  }
  
  close(pb)
  
  if (RETURN) {
    return(imgs)
  } else {
    return(NULL)
  }
}
