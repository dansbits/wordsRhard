#' Pluralize an english noun
#'
#' Pluralize an english noun
#'
#'
#' @param word The word to be pluralized
#' @param count An optional count to determine if pluralization is necessary. The word wil always be pluralized if left blank.
#' @return The pluralized word or the original word if count is 1 or -1.
#'
#' @examples
#' pluralize("chair")    # => "chairs"
#' pluralize("box", 1)   # => "box"
#' pluralize("box", 2)   # => "boxes"
#'
#' @export
#'
pluralize = function(word, count = NA) {
  if(!is.na(count) & abs(count) == 1) {
    return(word)
  } else {
    pattern = .getPluralizationPattern(word)
    return(.applyPluralizationPattern(word, pattern))
  }
}

.applyPluralizationPattern = function(word, pattern) {
  pluralized_word = gsub(pattern$selector, pattern$substitution, word)

  return(pluralized_word)
}

#' Identify the pluralization pattern which matches the word
#'
#' Searches through .pluralizationPatterns and finds the first
#' pattern which matches the given word
#'
#' @param word The word for which the pattern must be found
#' @return The pattern from .pluralizationPatterns which matches the word
.getPluralizationPattern = function(word) {
  for(i in 1:nrow(.pluralizationPatterns)) {
    pattern = .pluralizationPatterns[i,]

    if(grepl(pattern$pattern, word)) {
      return(pattern)
    }
  }
}

.pluralizationPatterns = rbind(
  data.frame(pattern = "^(quiz)$", selector = "$", substitution = "zes"),
  data.frame(pattern = "^ox$", selector = "$", substitution = "en"),
  data.frame(pattern = "^(m|l)ouse$", selector = "ouse$", substitution = "ice"),
  data.frame(pattern = "^(matr|vert|ind)(ix|ex)$", selector = "(ix|ex)$", substitution = "ices"),
  data.frame(pattern = "^.*(x|ch|ss|sh)$", selector = "$", substitution = "es"),
  data.frame(pattern = "^.*([^aeiouy]|qu)y$", selector = "y$", substitution = "ies"),
  data.frame(pattern = "^.*([^f]fe|([lr])f)$", selector = "(f|fe)$", substitution = "ves"),
  data.frame(pattern = "^.*sis$", selector = "sis$", substitution = "ses"),
  data.frame(pattern = "^.*[ti]a$", selector = "a$", substitution = "a"),
  data.frame(pattern = "^.*[ti]um$", selector = "um$", substitution = "a"),
  data.frame(pattern = "^(buffal|tomat)o$", selector = "$", substitution = "es"),
  data.frame(pattern = "^(bu)s$", selector = "$", substitution = "es"),
  data.frame(pattern = "^(alias|status)$", selector = "$", substitution = "es"),
  data.frame(pattern = "^(octop|vir)us$", selector = "us$", substitution = "i"),
  data.frame(pattern = "^(ax|test)is$", selector = "is$", substitution = "es"),
  data.frame(pattern = "^.*s$", selector = "$", substitution = ""),
  data.frame(pattern = "$", selector = "$", substitution = "s")
)
