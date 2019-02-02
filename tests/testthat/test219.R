context("eud")

test_that("Whether eud gives a correct output",{
  expect_equal(eud(14000,777),    7)
  expect_equal(eud(100,1000), 100)
  expect_equal(eud(-100,1000),100)
  expect_error(eud("100",1000))
  expect_error(eud(100,"1000"))
  expect_error(eud(T,"1000"))
})

context("vandijk")

test_that("Whether vandijk gives a correct output",{
  wiki_graph <-
    data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
               v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
               w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))

  expect_equal(vandijk(wiki_graph,1),c(0,7,9,20,20,11))
  expect_equal(vandijk(wiki_graph,3), c(9,10,0,11,11,2))

  wiki_wrong_graph <- wiki_graph
  names(wiki_wrong_graph) <- c("v1, v3, w")
  expect_error(vandijk(wiki_wrong_graph,3))

  wiki_wrong_graph <- wiki_graph[1:2]
  expect_error(vandijk(wiki_wrong_graph,3))
  expect_error(vandijk(wiki_graph,7))
  expect_error(vandijk(as.matrix(wiki_graph),3))
})
