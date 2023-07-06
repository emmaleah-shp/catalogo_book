--- 
title: "Seguridad"
site: bookdown::bookdown_site
output: bookdown::gitbook
---

# Seguridad

## Publishing {-}

HTML books can be published online, see: https://bookdown.org/yihui/bookdown/publishing.html

## 404 pages {-}

By default, users will be directed to a 404 page if they try to access a webpage that cannot be found. If you'd like to customize your 404 page instead of using the default, you may add either a `_404.Rmd` or `_404.md` file to your project root and use code and/or Markdown syntax.

## Metadata for sharing {-}

Bookdown HTML books will provide HTML metadata for social sharing on platforms like Twitter, Facebook, and LinkedIn, using information you provide in the `index.Rmd` YAML. To setup, set the `url` for your book and the path to your `cover-image` file. Your book's `title` and `description` are also used.
