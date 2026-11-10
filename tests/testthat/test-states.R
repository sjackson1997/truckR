# --- Place names -----------------------------------------------------------------------------

test_that("accents, case, punctuation and hyphens are ignored when comparing names", {
  expect_equal(norm_place("Le\u00f3n"), "leon")
  expect_equal(norm_place("SAINT-LAURENT"), "saint laurent")
  expect_equal(norm_place("  Salinas   Victoria "), "salinas victoria")
  expect_equal(norm_place("O'Fallon"), "ofallon")
  expect_equal(norm_place("St. Louis"), norm_place("SAINT LOUIS"))
  expect_equal(norm_place("ST LOUIS"), "saint louis")
  expect_equal(norm_place("Ste. Anne"), "sainte anne")
  expect_equal(norm_place("San Luis Potos\u00ed"), "san luis potosi")
})

test_that("a word that merely starts with st is not changed", {
  expect_equal(norm_place("Stoney Creek"), "stoney creek")
  expect_equal(norm_place("Stanton"), "stanton")
})

test_that("Mexico City goes by several names that compare equal", {
  for (name in c("Mexico City", "MEXICO CITY", "Ciudad de M\u00e9xico", "CDMX", "Distrito Federal")) {
    expect_equal(norm_place(name), "mexico city", label = name)
  }
})

test_that("strip_accents keeps plain text and handles NA", {
  expect_equal(strip_accents("Hello"), "hello")
  expect_true(is.na(strip_accents(NA_character_)))
})

# --- Mexican states ------------------------------------------------------------------------------

test_that("there are 32 Mexican states and each key is its own code", {
  expect_equal(nrow(mx_states), 32)
  expect_equal(anyDuplicated(mx_states$key), 0)
  expect_equal(mx_state_key(mx_states$key), mx_states$key)
  expect_equal(mx_state_key(mx_states$name), mx_states$key)
})

test_that("no alias points to two different states", {
  forms <- unlist(lapply(seq_len(nrow(mx_states)), function(i) {
    aliases <- strsplit(mx_states$aliases[i], ",", fixed = TRUE)[[1]]
    stats::setNames(rep(mx_states$key[i], 2 + length(aliases)), c(mx_states$key[i], mx_states$name[i], aliases))
  }))
  tokens <- vapply(names(forms), state_token, character(1))
  by_token <- split(unname(forms), tokens)

  expect_true(all(vapply(by_token, function(k) length(unique(k)) == 1, logical(1))))
})

test_that("the codes in the sample data are recognized", {
  expect_equal(mx_state_key(c("JAL", "NL")), c("JAL", "NLE"))
})

test_that("common codes, ISO codes and names all give the same state", {
  expect_equal(mx_state_key(c("NL", "NLE", "Nuevo Le\u00f3n", "NUEVO LEON", "nuevo leon")), rep("NLE", 5))
  expect_equal(mx_state_key(c("DF", "CDMX", "CMX", "Ciudad de M\u00e9xico", "Distrito Federal")), rep("DF", 5))
  expect_equal(mx_state_key(c("QRO", "QUE", "Quer\u00e9taro")), rep("QUE", 3))
  expect_equal(mx_state_key(c("QROO", "ROO", "Quintana Roo")), rep("ROO", 3))
  expect_equal(mx_state_key(c("GTO", "GUA", "Guanajuato")), rep("GUA", 3))
  expect_equal(mx_state_key(c("COAH", "COA", "Coahuila de Zaragoza")), rep("COA", 3))
})

test_that("ambiguous or unknown codes are not guessed", {
  expect_equal(mx_state_key(c("CH", "CO", "MX", "XX", "")), rep(NA_character_, 5))
  expect_true(is.na(mx_state_key(NA_character_)))
})

test_that("a state key becomes a plain name for searching", {
  expect_equal(mx_state_name(c("NLE", "JAL", "DF")), c("Nuevo Leon", "Jalisco", "Ciudad de Mexico"))
  expect_true(is.na(mx_state_name("ZZZ")))
})

# --- Comparing states --------------------------------------------------------------------------------

test_that("United States and Canadian codes compare directly", {
  expect_equal(state_relation("TX", "TX", "USA"), "match")
  expect_equal(state_relation("tx", "TX", "USA"), "match")
  expect_equal(state_relation("TX", "OK", "USA"), "mismatch")
  expect_equal(state_relation("ON", "ON", "CAN"), "match")
  expect_equal(state_relation("ON", "QC", "CAN"), "mismatch")
})

test_that("Mexican states compare by what they mean, not by spelling", {
  expect_equal(state_relation("NL", "NLE", "MEX"), "match")
  expect_equal(state_relation("JAL", "JAL", "MEX"), "match")
  expect_equal(state_relation("DF", "DF", "MEX"), "match")
  expect_equal(state_relation("NL", "TAM", "MEX"), "mismatch")
  expect_equal(state_relation("JAL", "NLE", "MEX"), "mismatch")
})

test_that("only a proven difference counts as a mismatch", {
  expect_equal(state_relation(NA_character_, "TX", "USA"), "unknown")
  expect_equal(state_relation("TX", NA_character_, "USA"), "unknown")
  expect_equal(state_relation("XX", "TX", "USA"), "unknown")       # not a US state
  expect_equal(state_relation("TX", "XX", "USA"), "unknown")
  expect_equal(state_relation("CH", "CHP", "MEX"), "unknown")      # ambiguous code
  expect_equal(state_relation("NL", "XXX", "MEX"), "unknown")
})

test_that("a Canadian province and a Mexican state that share a code are not confused", {
  expect_equal(state_relation("NL", "NL", "CAN"), "match")
  expect_equal(state_relation("NL", "NLE", "MEX"), "match")
  expect_equal(state_relation("NL", "NS", "CAN"), "mismatch")
})

test_that("with no country, equal codes match, two different US or Canadian codes differ, and the rest is unknown", {
  expect_equal(state_relation("TX", "TX", NA_character_), "match")
  expect_equal(state_relation("TX", "OK", NA_character_), "mismatch")
  expect_equal(state_relation("NL", "NLE", NA_character_), "unknown")   # NLE is not a US or Canadian code
  expect_equal(state_relation("TX", "JAL", NA_character_), "unknown")
})

test_that("state code validity depends on the country", {
  expect_true(valid_state_code("TX", "USA"))
  expect_false(valid_state_code("TX", "CAN"))
  expect_true(valid_state_code("ON", "CAN"))
  expect_false(valid_state_code("ON", "USA"))
  expect_true(valid_state_code("NL", "MEX"))
  expect_true(valid_state_code("Jalisco", "MEX"))
  expect_false(valid_state_code("XX", "MEX"))
  expect_true(valid_state_code("TX", NA_character_))
  expect_true(valid_state_code("ON", NA_character_))
  expect_false(valid_state_code(NA_character_, "USA"))
})
