# eztfidf

#' A function for turning character vectors into useful distances and analysis quickly.
#'
#' This function returns an eztfidf list containing convenient functions.
#' @param char_vector A character vector of documents. To be passed as a VectorSource (tm package). The values may be duplicated but the names may not.
#' @param replace_words A named character vector. The element names will be replaced with the elements.
#' @keywords nlp, fuzzy matching, tfidf, tm, easy
#' @export
#' @importFrom magrittr %>%
#' @examples
#' super_heroes <- c('The Flash', 'The HULK', 'she-hulk', 'ant-man', 'iron-man', 'bat-man', 'super-man', 'the green arrow')
#' names(super_heroes) <- super_heroes
#' x <- eztfidf(
#'     super_heroes, replace_words = c('-' = ' ')
#' )
#' x$doc_keys[1:3]  # look up by index
#' x$doc_keys['The HULK']  # see how names have been transformed
#' x$lookup(2:3)  # breakdown docs by dimension scores
#' x$CosineSimVector(2, return_sorted = T, top = 3)  # top 3 for she-hulk
#' x$CosineSimMatrix(2)  # all results

#char_vector <- company_names_data
#char_vector <- setNames(eztfidf::company_names_data, as.character(seq_along(eztfidf::company_names_data)))
#char_vector <- setNames(eztfidf::company_names_data, eztfidf::company_names_data)
#replace_words <- c('\t'=' ','llc'='limited liability company','inc'='incorporated')
eztfidf <- function(char_vector, replace_words = c('\t'=' ')) {

    eztfidf <- list()

    # Default cleanup for punctuation and whitespace
    char_vector[] <- char_vector %>%
        stringr::str_to_lower() %>%
        stringr::str_replace_all('[.,;:()\\[\\]]', '') %>%
        stringr::str_replace_all('[-~\\n/!?]', ' ') %>%
        stringr::str_replace_all(' +', ' ') %>%
        stringr::str_trim()

    # Iterate through explicit word replacements (always lower case)
    replace_words[] <- stringr::str_to_lower(replace_words)
    names(replace_words) <- stringr::str_to_lower(names(replace_words))
    for (i in seq_along(replace_words)){
        char_vector[] <- stringr::str_replace_all(
            char_vector, paste0('\\b',
                                names(replace_words[i]),
                                '\\b'), replace_words[i]
        )
    }

    # This will expose any translation along with assigned keys
    eztfidf$docs <- char_vector

    my_corp <- tm::VCorpus(tm::VectorSource(char_vector))

    eztfidf$dtm <- tm::DocumentTermMatrix(
        my_corp,
        control = list(
            stopwords = F,
            removePunctuation = F,
            removeNumbers = F,
            stopwords = F,
            stemming = F,
            wordLengths = c(1,Inf),
            weighting = function(x) tm::weightTfIdf(x, normalize = F)
            #weighting = function(x) tm::weightSMART(x, spec = 'ntn'),
        )
    )

    if (!is.null(names(eztfidf$docs))) eztfidf$dtm$dimnames$Docs <- names(eztfidf$docs)

    eztfidf$values <- function(keys, mode = c('list','matrix')){
        if (mode[1] == 'list'){
            # Returns list, each containing named vector
            result <- lapply(keys, FUN = function(key){
                eztfidf$dtm[key, eztfidf$dtm[key,]$j] %>%
                    as.matrix() %>%
                    .[,order(., decreasing = T), drop = T]
            })
            # Named list if appropriate
            if (!is.null(names(eztfidf$docs))) names(result) <- names(eztfidf$docs[keys])

        } else if (mode[1] == 'matrix'){
            result <- eztfidf$dtm[keys, sort(unique(eztfidf$dtm[keys,]$j))] %>%
                as.matrix()
        }
        result
    }

    #CosineSimFlat <- function(A, B){
    #    slam::row_sums(A * B) / sqrt(slam::row_sums(A * A) * slam::row_sums(B * B))
    #}

    # Cosine similarities for a given key
    eztfidf$CosineSimVector <- function(key_a, keys_b, return_sorted = T, top = length(keys_b)){
        scores <- slam::tcrossprod_simple_triplet_matrix(
            eztfidf$dtm[key_a,]/slam::row_norms(eztfidf$dtm[key_a,]),
            eztfidf$dtm[keys_b,]/slam::row_norms(eztfidf$dtm[keys_b,])
        )
        if (!is.null(names(eztfidf$docs))) names(scores) <- keys_b
        if (return_sorted) {
            scores <- scores %>%
                sort(decreasing = T) %>%
                `[`(seq_len(top))
        }
        scores
    }

    # eztfidf$CosineSimMatrix <- function(keys_a = keys_a, keys_b){
    #     slam::tcrossprod_simple_triplet_matrix(
    #         eztfidf$dtm[keys_a,]/slam::row_norms(eztfidf$dtm[keys_a,]),
    #         eztfidf$dtm[keys_b,]/slam::row_norms(eztfidf$dtm[keys_b,])
    #     )
    # }

    eztfidf

}

#x <- EasyTFIDF(company_names_data)


#TODO:
#use slam package to do vector 1-length normalization
#allow for char vector names as keys
#create function that gets best matches given keys



