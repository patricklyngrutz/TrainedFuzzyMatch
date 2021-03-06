# eztfidf

#' Turn character vectors into tfidf distances easily.
#'
#' This function returns an eztfidf list containing convenient functions.
#' @param char_vector A character vector of documents. To be passed as a VectorSource (tm package). The values may be duplicated but the names may not.
#' @param replace_words A named character vector. The element names will be replaced with the elements.
#' @keywords nlp, fuzzy matching, tfidf, tm, easy
#' @export
#' @importFrom magrittr %>%
#' @examples
#' super_heroes <- c(
#'     'The Flash', 'The HULK', 'she-hulk', 'ant-man', 'Ironman', 'BATMAN',
#'     'superman', 'the green arrow', 'aqua-man', 'the silver surfer', 'green lantern'
#'     )
#' names(super_heroes) <- super_heroes
#' super_heroes <- gsub('man$', '-MAN', super_heroes, TRUE)  # custom cleaning
#' x <- eztfidf(
#'     super_heroes, replace_words = c('-' = ' ', 'silver' = 'gold')
#' )
#'
#' # Use numeric index or original names to see changes to docs
#' x$docs[1:10]
#' x$docs[c('The HULK','the silver surfer')]
#'
#' # Inspect bag-of-words tfidf values as a list or matrix
#' x$values(c('the green arrow','green lantern'))
#' x$values(c(2,3,8,11), mode = 'matrix')
#'
#' # Best matching values and cosine similarity matrix easily accessible
#' x$CosineSimVector(3, top = 3)
#' x$CosineSimVector('the green arrow', top = 3)
#' x$CosineSimMatrix(c(2,3,8,11))

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

    if (!is.null(names(eztfidf$docs))) {
        eztfidf$dtm$dimnames$Docs <- names(eztfidf$docs)
    } else {
        warning('char_vector provided does not have element names, recommend providing a named vector for convenience')
    }

    eztfidf$values <- function(keys = NULL, mode = c('list','matrix')){

        # Defaults to entire corpus
        if (is.null(keys)) keys <- seq_along(eztfidf$docs)

        if (mode[1] == 'list'){
            # Returns list, each containing named vector
            result <- lapply(keys, FUN = function(key){
                eztfidf$dtm[key, eztfidf$dtm[key,]$j] %>%
                    as.matrix() %>%  # necessary intermediate step from slam triplets
                    `[`(1, TRUE, drop = T) %>%  # coerces to vector with names
                    sort(decreasing = T)  # sorts by highest values first
            })
            # Named list if appropriate
            if (!is.null(names(eztfidf$docs))) names(result) <- names(eztfidf$docs[keys])

        } else if (mode[1] == 'matrix'){
            result <- eztfidf$dtm[keys, sort(unique(eztfidf$dtm[keys,]$j))] %>%
                as.matrix()
        }
        result
    }

    # Deprecated indefinitely - use CosineSimVector
    #CosineSimFlat <- function(A, B){
    #    slam::row_sums(A * B) / sqrt(slam::row_sums(A * A) * slam::row_sums(B * B))
    #}

    # Cosine similarities for a given key
    eztfidf$CosineSimVector <- function(key_a, keys_b = NULL, return_sorted = T, top = length(keys_b)) {

        if (length(key_a) > 1) stop('Argument key_a must be a single key, please use CosineSimMatrix instead')

        if (is.null(keys_b)) keys_b <- seq_along(eztfidf$docs)
        if (top == 0) top <- length(keys_b)

        # Cosine similarities calculated - will lack names initially
        tryCatch({
        scores <- slam::tcrossprod_simple_triplet_matrix(
            eztfidf$dtm[key_a,]/slam::row_norms(eztfidf$dtm[key_a,]),
            eztfidf$dtm[keys_b,]/slam::row_norms(eztfidf$dtm[keys_b,])
        )}, error = function(e){
            if (any(! c(key_a, keys_b) %in% names(eztfidf$docs))) {
                stop('Provided a key not contained in docs, please see $docs')
            }
        })

        # scores inherits the names from the documents, whether indexed by number or name
        # tm::dtm will add names by default, which will be stripped from results
        if (!is.null(names(eztfidf$docs))) {
            names(scores) <- names(eztfidf$docs[keys_b])
        } else {
            # strip away dimnames and dimensionality
            scores <- as.numeric(scores)
        }

        # Warn user if they have sorted the results without names to tie back
        if (return_sorted & is.null(names(scores)) & length(keys_b) != 1) {
            warning('Docs were not given names and results have been sorted by value')
        }

        # Warn user if they have not sorted values but requested top
        if (!is.null(top) & !return_sorted & top != length(keys_b)){
            warning('Docs must be sorted to return top values - results may be shuffled')
            scores <- scores %>%
                sort(decreasing = T)
        }

        # Sort if requested
        if (return_sorted){
            scores <- scores %>%
                sort(decreasing = T)
        }

        # Return top values if requested
        if (top == length(keys_b) | length(keys_b) == 1) {
            # No action
        } else if (!is.null(top)) {
            scores <- scores[seq_len(top)]
        }

        scores
    }

    eztfidf$CosineSimMatrix <- function(keys_a, keys_b = keys_a){

        # Cosine similarities calculated - will lack names initially
        tryCatch({
        scores <- slam::tcrossprod_simple_triplet_matrix(
            eztfidf$dtm[keys_a,]/slam::row_norms(eztfidf$dtm[keys_a,]),
            eztfidf$dtm[keys_b,]/slam::row_norms(eztfidf$dtm[keys_b,])
        )}, error = function(e){
            if (any(! c(keys_a, keys_b) %in% names(eztfidf$docs))) {
                stop('Provided a key not contained in docs, please see $docs')
            }
        })

        if (is.null(names(eztfidf$docs))){
            dimnames(scores) <- NULL
        }

        scores

    }

    eztfidf

}

#x <- EasyTFIDF(company_names_data)


#TODO:
#use slam package to do vector 1-length normalization
#allow for char vector names as keys
#create function that gets best matches given keys



