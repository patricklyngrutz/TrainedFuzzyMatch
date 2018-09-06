context('EasyTFIDF$docs')

# TFIDF objects with different named-ness
replacers = c('INC' = 'incorporated', 'corp' = 'CORPORATION')
tf_noname <- EasyTFIDF::EasyTFIDF(company_names_data, replacers)  # should handle the same as intname
tf_intname <- EasyTFIDF::EasyTFIDF(setNames(company_names_data, seq_along(company_names_data)), replacers)
tf_strname <- EasyTFIDF::EasyTFIDF(setNames(company_names_data, company_names_data), replacers)


test_that('Indexing docs works with names and numbers', {
    expect_gt(length(company_names_data),0)
    expect_equivalent(tf_noname$docs[1], '1st discount brokerage incorporated')
    expect_equivalent(tf_intname$docs['1'], '1st discount brokerage incorporated')
    expect_equivalent(tf_strname$docs['1st Discount Brokerage, Inc.'], '1st discount brokerage incorporated')
}
)

tf_noname$values(1:100)

context('EasyTFIDF$')
