#library('EasyTFIDF')
context('EasyTFIDF list object')

# test_data <- company_names_data  # unnecessary?

# TFIDF objects with different named-ness
tf_noname <- EasyTFIDF::EasyTFIDF(company_names_data)  # should handle the same as intname
tf_intname <- EasyTFIDF::EasyTFIDF(setNames(company_names_data, seq_along(company_names_data)))
tf_strname <- EasyTFIDF::EasyTFIDF(setNames(company_names_data, company_names_data))


test_that('Indexing works with names and numbers', {
    expect_gt(length(company_names_data),0)
    #expect_true(T)

}
)
