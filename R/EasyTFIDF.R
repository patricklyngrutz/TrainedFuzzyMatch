# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'
char_vector <- company_names_data
replace_words <- c('\t'=' ','llc'='limited liability company','inc'='incorporated')
EasyTFIDF <- function(char_vector, replace_words = c('\t'=' ')) {

    easytfidf <- list()

    if(is.null(names(char_vector))){
        names(char_vector) <- seq_along(char_vector)
    }

    char_vector[] <- char_vector %>%
        stringr::str_to_lower() %>%
        stringr::str_replace_all('[.,;:()\\[\\]]', '') %>%
        stringr::str_replace_all('[-~\\n/!?]', ' ') %>%
        stringr::str_replace_all(' +', ' ') %>%
        stringr::str_trim()

    for (i in seq_along(replace_words)){
        char_vector[] <- stringr::str_replace_all(
            char_vector, paste0('\\b',
                                stringr::str_to_lower(names(replace_words[i])),
                                '\\b'), replace_words[i]
        )
    }

    easytfidf$doc_keys <- char_vector

    my_corp <- tm::VCorpus(tm::VectorSource(char_vector))

easytfidf$dtm <- tm::DocumentTermMatrix(
    my_corp,
    control = list(weighting = function(x) tm::weightTfIdf(x, normalize = F),
        stopwords = F,
        removeNumbers = F,
        stopwords = F,
        stemming = F,
        wordLengths = c(1,Inf)
    )
)

CosineSimFlat <- function(A, B){
    row_sums(A * B) / sqrt(row_sums(A * A) * row_sums(B * B))
}

easytfidf$CosineSimVector <- function(key_a, keys_b, return_sorted = T, top = length(keys_b)){
    scores <- slam::tcrossprod_simple_triplet_matrix(
        easytfidf$dtm[key_a,]/row_norms(easytfidf$dtm[key_a,]),
        easytfidf$dtm[keys_b,]/row_norms(easytfidf$dtm[keys_b,])
    )[,]
    if (return_sorted) {
        scores <- scores %>%
            sort(decreasing = T) %>%
            `[`(seq_len(top))
    }
    scores
}

easytfidf$CosineSimMatrix <- function(keys_a = keys_a, keys_b){
    slam::tcrossprod_simple_triplet_matrix(
        easytfidf$dtm[keys_a,]/row_norms(easytfidf$dtm[keys_a,]),
        easytfidf$dtm[keys_b,]/row_norms(easytfidf$dtm[keys_b,])
    )
}

easytfidf$lookup <- function(key){
    for (i in key){
        print(easytfidf$doc_keys[i])
        easytfidf$dtm[i,easytfidf$dtm[i,]$j] %>%
            as.matrix() %>%
            `[`(1,) %>%
            sort(decreasing = T) %>%
            print()
    }
}

easytfidf

}

x <- EasyTFIDF(company_names_data)


#TODO:
#use slam package to do vector 1-length normalization
#allow for char vector names as keys
#create function that gets best matches given keys



