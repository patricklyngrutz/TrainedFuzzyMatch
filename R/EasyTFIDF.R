# EasyTFIDF



#' An easy function for processing TFIDF
#'
#' This function returns an EasyTFIDF list containing handy functions
#' @param char_vector A character vector of documents. To be passed as a VectorSource (tm package). The values may be duplicated but the names may not.
#' @param replace_words A named character vector. The element names will be replaced with the elements.
#' @keywords nlp, fuzzy matching, tfidf, tm, easy
#' @export
#' @examples
#' super_heroes <- c('The Flash', 'The HULK', 'she-hulk', 'ant-man', 'iron-man', 'bat-man', 'super-man', 'the green arrow')
#' names(super_heroes) <- super_heroes
#' x <- EasyTFIDF(
#'     super_heroes, replace_words = c('-' = ' ')
#' )
#' x$doc_keys[1:3]  # look up by index
#' x$doc_keys['The HULK']  # see how names have been transformed
#' x$lookup(2:3)  # breakdown docs by dimension scores
#' x$CosineSimVector(2, return_sorted = T, top = 3)  # top 3 for she-hulk
#' x$CosineSimMatrix(2)  # all results

#char_vector <- company_names_data
#replace_words <- c('\t'=' ','llc'='limited liability company','inc'='incorporated')
EasyTFIDF <- function(char_vector, replace_words = c('\t'=' ')) {

    easytfidf <- list()

    # Default cleanup for punctuation and whitespace
    char_vector[] <- char_vector %>%
        stringr::str_to_lower() %>%
        stringr::str_replace_all('[.,;:()\\[\\]]', '') %>%
        stringr::str_replace_all('[-~\\n/!?]', ' ') %>%
        stringr::str_replace_all(' +', ' ') %>%
        stringr::str_trim()

    # Always lower case treatment
    replace_words[] <- stringr::str_to_lower(replace_words)
    names(replace_words) <- stringr::str_to_lower(names(replace_words))

    # Iterate through explicit word replacements
    for (i in seq_along(replace_words)){
        char_vector[] <- stringr::str_replace_all(
            char_vector, paste0('\\b',
                                names(replace_words[i]),
                                '\\b'), replace_words[i]
        )
    }

    # This will expose any translation along with assigned keys
    easytfidf$docs <- char_vector

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

    easytfidf$values <- function(keys){
        lapply(keys, FUN = function(key){
            easytfidf$dtm[key, easytfidf$dtm[key,]$j] %>%
                as.matrix() %>%
                .[,order(., decreasing = T)]
        })
    }

    CosineSimFlat <- function(A, B){
        slam::row_sums(A * B) / sqrt(slam::row_sums(A * A) * slam::row_sums(B * B))
    }

    easytfidf$CosineSimVector <- function(key_a, keys_b, return_sorted = T, top = length(keys_b)){
        scores <- slam::tcrossprod_simple_triplet_matrix(
            easytfidf$dtm[key_a,]/slam::row_norms(easytfidf$dtm[key_a,]),
            easytfidf$dtm[keys_b,]/slam::row_norms(easytfidf$dtm[keys_b,])
        )
        names(scores) <- names(easytfidf$dtm[keys_b,])
        if (return_sorted) {
            scores <- scores %>%
                sort(decreasing = T) %>%
                `[`(seq_len(top))
        }
        scores
    }

    easytfidf$CosineSimMatrix <- function(keys_a = keys_a, keys_b){
        slam::tcrossprod_simple_triplet_matrix(
            easytfidf$dtm[keys_a,]/slam::row_norms(easytfidf$dtm[keys_a,]),
            easytfidf$dtm[keys_b,]/slam::row_norms(easytfidf$dtm[keys_b,])
        )
    }


    easytfidf

}

#x <- EasyTFIDF(company_names_data)


#TODO:
#use slam package to do vector 1-length normalization
#allow for char vector names as keys
#create function that gets best matches given keys



