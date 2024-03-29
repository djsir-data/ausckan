
ulst <- as.list.environment(ausckan:::urls)

test_that("urls are current", {

  for (u in 1:length(ulst)){
    expect_true(RCurl::url.exists(ulst[[u]]))
  }

})




test_that("each ckan repo can be searched", {

  suppressMessages({

    suppressWarnings({

      for (n in names(ulst)){
        search <- tryCatch({
          do.call(paste0('search_ckan_', n), args = list(search_term = 'schools'))
        }, error = function(e){print(paste0(n, ' failed'))})
        expect_s3_class(search, 'data.frame')
      }

    })

  })

})
