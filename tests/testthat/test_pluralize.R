context('pluralize')

test_that("buffalo and tomato are pluralized correctly", {
  expect_equal(pluralize('buffalo'), 'buffaloes')
  expect_equal(pluralize('tomato'), 'tomatoes')
})

test_that("bus is pluralized correctly", {
  expect_equal(pluralize('bus'), 'buses')
})

test_that("'ta' suffixes remain 'ta'", {
  expect_equal(pluralize('aorta'), 'aorta')
})

test_that("'ia' suffixes remain 'ia'", {
  expect_equal(pluralize('militia'), 'militia')
})

test_that("'tum' suffixes change to 'ta'", {
  expect_equal(pluralize('momentum'), 'momenta')
})

test_that("'ium' suffixes change to 'ia'", {
  expect_equal(pluralize('titanium'), 'titania')
})

test_that("alias and status are pluralized correctly", {
  expect_equal(pluralize('alias'), 'aliases')
  expect_equal(pluralize('status'), 'statuses')
})

test_that("octopus and virus are pluralized correctly", {
  expect_equal(pluralize('octopus'), 'octopi')
  expect_equal(pluralize('virus'), 'viri')
})

test_that("axis and testis are pluralized correctly", {
  expect_equal(pluralize('axis'), 'axes')
  expect_equal(pluralize('testis'), 'testes')
})

test_that("'sis' suffixes are changed to 'ses'", {
  expect_equal(pluralize('basis'), 'bases')
})

test_that("other words ending in s are unchanged", {
  expect_equal(pluralize('series'), 'series')
})

test_that("'lf', 'rf', and 'fe' suffixes are changed to 'ves'", {
  expect_equal(pluralize('elf'), 'elves')
  expect_equal(pluralize('scarf'), 'scarves')
  expect_equal(pluralize('wife'), 'wives')
})

test_that("'fe' suffixes folling an f are pluralized normally", {
  expect_equal(pluralize('giraffe'), 'giraffes')
})

test_that("'(consonant)y' and 'quy' suffixes end in ies", {
  expect_equal(pluralize('soliloquy'), 'soliloquies')
  expect_equal(pluralize('puppy'), 'puppies')
})

test_that("words ending with x,ch,ss or sh are appended with es", {
  expect_equal(pluralize('box'), 'boxes')
  expect_equal(pluralize('church'), 'churches')
  expect_equal(pluralize('mass'), 'masses')
  expect_equal(pluralize('fish'), 'fishes')
})

test_that("vertex, index and matrix are pluralized properly", {
  expect_equal(pluralize('vertex'), 'vertices')
  expect_equal(pluralize('index'), 'indices')
  expect_equal(pluralize('matrix'), 'matrices')
})

test_that("mouse and louse are pluralized properly", {
  expect_equal(pluralize('mouse'), 'mice')
  expect_equal(pluralize('louse'), 'lice')
})

test_that("ox are pluralized properly", {
  expect_equal(pluralize('ox'), 'oxen')
})

test_that("quiz is pluralized properly", {
  expect_equal(pluralize('quiz'), 'quizzes')
})

test_that("other unmatched words are appended with s", {
  expect_equal(pluralize('word'), 'words')
})
