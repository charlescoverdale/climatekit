# make_slide_figures.R (climatekit)
# Prepare slide-ready figures:
#  1. Copy the paper's Central Park PDF as the hero figure
#  2. Generate a QR code pointing to the paper PDF
#
# Usage:  Rscript make_slide_figures.R

suppressPackageStartupMessages({
  if (!requireNamespace("qrcode", quietly = TRUE)) {
    install.packages("qrcode", repos = "https://cloud.r-project.org")
  }
  library(qrcode)
})

fig_dir <- "figures"
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

# ------------------------------------------------------------------
# 1. Hero figure: Central Park 3-series temperature index chart
# ------------------------------------------------------------------
src <- file.path("..", "figures", "fig6_central_park.pdf")
dst <- file.path(fig_dir, "hero_figure.pdf")

if (file.exists(src)) {
  file.copy(src, dst, overwrite = TRUE)
  cat("Copied hero figure to", dst, "\n")
} else {
  stop("Source figure not found: ", src,
       ". Run the paper's make_figures.R first.")
}

# ------------------------------------------------------------------
# 2. QR code to the paper PDF on the publications page
# ------------------------------------------------------------------
paper_url <- "https://charlescoverdale.github.io/files/coverdale_climatekit_2026.pdf"

qr <- qr_code(paper_url, ecl = "M")
png(
  filename = file.path(fig_dir, "qrcode_paper.png"),
  width = 800, height = 800, res = 300, bg = "white"
)
par(mar = rep(0, 4))
plot(qr)
dev.off()

cat("QR code written to", file.path(fig_dir, "qrcode_paper.png"), "\n")
