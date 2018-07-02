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

tfidf_scores <- function(char_vector, replace_words = c('\t'=' ','llc'='limited liability company','inc'='incorporated')) {
    char_vector <- company_names_data
    replace_words = c('\t'=' ','llc'='limited liability company','inc'='incorporated')
    my_corp <- tm::Corpus(tm::VectorSource(char_vector))
    my_corp <- tm::tm_map(my_corp, stringr::str_to_lower)
    my_corp <- tm::tm_map(my_corp, function(x) stringr::str_replace_all(x, '[.,;:]', ''))
    my_corp <- tm::tm_map(my_corp, function(x) stringr::str_replace_all(x, '[-~\\n/]', ' '))

    for (i in seq_along(replace_words)){
        my_corp <- tm::tm_map(my_corp, function(x) {
            stringr::str_replace_all(x, paste0('\\b',names(replace_words[i]),'\\b'), replace_words[i])
        })
    }

    tfidf_scores <- tm::DocumentTermMatrix(
        my_corp,
        control = list(
            weighting = function(x) tm::weightTfIdf(x,
                                                    normalize = T
                                                    ),
            stopwords = F,
            removeNumbers = F,
            stopwords = F,
            stemming = F,
            wordLengths = c(1,Inf)
        )
    )

    setNames(tfidf_scores[4,]$v, colnames(tfidf_scores)[tfidf_scores[4,]$j])

    #TODO:
    #use slam package to do vector 1-length normalization
    #allow for char vector names as keys
    #create function that gets best matches given keys

}
