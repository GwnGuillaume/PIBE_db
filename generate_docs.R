# Load necessary packages
library(usethis)
library(devtools)
library(pkgdown)

# Clean and update documentation
devtools::document()

# Generate pkgdown documentation without installing
options(pkgdown.install = FALSE)

# Delete content of docs/ repository
unlink("docs", recursive = TRUE)

# Specify the path to the articles directory
articles_path <- "docs/articles"

# Generate ui-doc.md file
# cat(readLines("R/ui.R"), sep = "\n", file = "ui-doc.md", append = TRUE)

# Read the R script
script_content <- readLines("R/ui.R", warn = FALSE)

# Create an output Markdown file
output_file <- "ui-doc.md"
cat("", file = output_file)

# Iterate through lines of the script
for (line in script_content) {
  # Check for specific patterns and convert to Markdown
  if (grepl("^#", line)) {
    # Heading
    cat(paste("##", substr(line, 3, nchar(line)), "\n"), file = output_file, append = TRUE)
  } else if (grepl("^dashboardPage", line)) {
    # Start of a new section
    cat("\n```{r chunk-name, echo=FALSE}\n", file = output_file, append = TRUE)
    cat(line, file = output_file, append = TRUE)
  } else if (grepl("^\\)", line)) {
    # End of a section
    cat(line, "\n```\n", file = output_file, append = TRUE)
  } else {
    # Other lines
    cat(line, "\n", file = output_file, append = TRUE)
  }
}

# Build home page
pkgdown::build_home(preview = FALSE)

# Move and rename the article ui-doc.html in the docs/articles/index.html
if (!dir.exists(articles_path)) {
  dir.create(articles_path, recursive = TRUE)
}
file.rename(from = "docs/ui-doc.html", to = "docs/articles/index.html")

# Build reference pages
pkgdown::build_reference(preview = FALSE)

# Build articles
pkgdown::build_articles(preview = FALSE)

# Path of file index.html
index_path <- file.path("docs", "index.html")

# Open file index.html in web browser
browseURL(index_path)
