lin <- function(z, freq) {
  z
}

chg <- function(z, freq) {
  z - dplyr::lag(z, freq)
}

ch1 <- function(z, freq) {
  chg(z, 12)
}

pch <- function(z, freq) {
  100 * ( z / dplyr::lag(z, freq) - 1 )
}

pc1 <- function(z, freq) {
  pch(z, 12)
}

pca <- function(z, freq) {
  y <- z / dplyr::lag(z, freq)
  100 * ( y ** (12 / freq) - 1 )
}

transformations <-
list(
	"lin" = lin,
	"chg" = chg,
	"ch1" = ch1,
	"pch" = pch,
	"pc1" = pc1,
	"pca" = pca
)
