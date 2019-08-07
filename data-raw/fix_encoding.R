# There are some problems with encoding for the test data sets.
# Al characters and factor levels have to be encoded as UTF-8.

# Encoding as UTF-8
enc <- function(x) UseMethod("enc")

enc.factor <- function(x) {
  levels(x) <- enc2utf8(levels(x))
  x
}

enc.character <- function(x) enc2utf8(x)

# Get all data sets to encode
files <- dir("data-raw", ".RData")


# Encode all data sets and save in data folder
for (file in files) {
  # file <- files[1]
  load(file.path("data-raw", file))
  nm <- gsub(".RData", "", file)
  df <- get(nm)

  # Encode all factor variables as UTF-8
  fctrs     <- vapply(df, is.factor, NA)
  df[fctrs] <- lapply(df[fctrs], enc)

  chrs      <- vapply(df, function(x) is.character(x) && !is.factor(x), NA)
  df[chrs]  <- lapply(df[chrs], enc)

  # Save data to correct folder
  save(list = nm, file = file.path("data", paste0(nm, ".rda")),
       compress = "bzip2")
}
