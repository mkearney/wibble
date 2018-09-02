
mwk <- Webpage$new("https://www.espn.com")

mwk$links

mwk$data

mwk$text

cat_call(mwk$text)

mwk$tables


devtools::load_all()

u <- c("https://mikewk.com", "https://espn.com")
x <- structure(u, class = c("url", "character"))

cat_call(as.character(u))
x


