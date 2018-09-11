context('eztfidf$docs')

# Initialize eztfidf examples for testing
replacers = c('INC' = 'incorporated', 'corp' = 'CORPORATION')
tf_intname <- eztfidf::eztfidf(setNames(company_names_data, seq_along(company_names_data)), replacers)
tf_strname <- eztfidf::eztfidf(setNames(company_names_data, company_names_data), replacers)
suppressWarnings({ tf_noname <- eztfidf::eztfidf(company_names_data, replacers) })  # intentional warning

test_that('Providing a char_vector without names produces a warning', {
    expect_warning(eztfidf::eztfidf(c('a','b','c')))
    expect_silent(eztfidf::eztfidf(c('a'='a','b'='b')))
})

test_that('Indexing docs works with numbers and names', {
    expect_gt(length(company_names_data),0)  # make sure data is available and unchanged from tests
    expect_equal(company_names_data[1:3], c('1st Discount Brokerage, Inc.', '1st Global Capital Corp', '280 Securities LLC'))
    expect_equivalent(tf_noname$docs[1], '1st discount brokerage incorporated')
    expect_equivalent(tf_intname$docs['1'], '1st discount brokerage incorporated')
    expect_equivalent(tf_strname$docs['1st Discount Brokerage, Inc.'], '1st discount brokerage incorporated')
})

test_that('Indexed docs are returned with names that were provided', {
    expect_null(names(tf_noname$docs[1]))
    expect_equal(names(tf_intname$docs['1']), '1')
    expect_equal(names(tf_strname$docs['1st Discount Brokerage, Inc.']), '1st Discount Brokerage, Inc.')
})

test_that('Multiple docs can be returned by index', {
    valid_result <- c('1st discount brokerage incorporated', '1st global capital corporation')
    expect_equivalent(tf_noname$docs[1:2], valid_result)
    expect_equivalent(tf_intname$docs[c('1','2')], valid_result)
    expect_equivalent(tf_strname$docs[names(tf_strname$docs[1:2])], valid_result)
})

context('eztfidf$values')

test_that('Indexing works with numbers or names - list mode', {
    expect_equal(length(tf_noname$values(1:3)), 3)
    expect_equal(length(tf_intname$values(c('1','2','3'))), 3)
    expect_equal(length(tf_strname$values(names(tf_strname$docs)[1:3])), 3)
})

test_that('Indexing works with numbers or names - matrix mode', {
    expect_equal(nrow(tf_noname$values(1:3, mode = 'matrix')), 3)
    expect_equal(nrow(tf_noname$values(1, mode = 'matrix')), 1)
    expect_is(tf_noname$values(1, mode = 'matrix'), 'matrix')
    expect_equal(nrow(tf_intname$values(c('1','2','3'), mode = 'matrix')), 3)
    expect_equal(nrow(tf_strname$values(names(tf_strname$docs[1:3]), mode = 'matrix')), 3)

})

context('eztfidf$CosineSimVector')

test_that('Indexing works with numbers or names', {
    expect_identical(
        tf_strname$CosineSimVector(1,2),
        tf_strname$CosineSimVector('1st Discount Brokerage, Inc.','1st Global Capital Corp')
    )
    expect_equivalent(tf_noname$CosineSimVector(1,2), tf_intname$CosineSimVector('1','2'))
})

test_that('Names are only returned if docs have names', {
    expect_named(tf_intname$CosineSimVector(1,1:3))
    expect_named(tf_intname$CosineSimVector('1',c('1','2','3')))
    expect_null(names(tf_noname$CosineSimVector(1,1:3,return_sorted = F)))
})

test_that('Warnings are given when illogical sort/top requests are made', {

    # Non-named char_vector warnings
    expect_warning(tf_noname$CosineSimVector(1,c(2,1), return_sorted = T))
    expect_silent(tf_noname$CosineSimVector(1,2))  # 1-length no issue
    expect_silent(tf_noname$CosineSimVector(1,c(2,1), return_sorted = F))  # top = everything no issue
    expect_silent(tf_noname$CosineSimVector(1,c(2,1), return_sorted = F, top = 2))  # top = everything no issue
    expect_warning(tf_noname$CosineSimVector(1,c(2,1), return_sorted = F, top = 1))

    # Named char_vector should be safe
    expect_silent(tf_intname$CosineSimVector(1,c(2,1), return_sorted = T, top = 1))
    expect_silent(tf_intname$CosineSimVector(1,c(2,1), return_sorted = T, top = 2))
    expect_warning(tf_intname$CosineSimVector(1,c(2,1), return_sorted = F, top = 1))
    expect_silent(tf_intname$CosineSimVector(1,c(2,1), return_sorted = F, top = 2))
})

test_that('CosineSimVector returns NA if asked for top N out of N-1 keys', {
    expect_equal(tf_intname$CosineSimVector('1',c('1','2'), top = 3)[[3]], as.numeric(NA))
    suppressWarnings({  # intentional warning
    expect_equal(tf_noname$CosineSimVector(1,c(1,2), top = 3)[[3]], as.numeric(NA))
    })
})
