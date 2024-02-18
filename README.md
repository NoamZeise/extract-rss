# Extract RSS

A tool for turning a webpage into an rss feed.


Many websites do not have an rss feed, and there are a few paid services that
solve this problem by parsing a page and creating a feed.


This tool will generate rss feeds from a website, although you 
have to write the code to select articles and article details (examples in `sources.lisp`).

# Using

Add a shebang to start of `add-feeds.lisp` for your supported common lisp implementation
You can run `add-feeds.lisp` as a script periodically (ie chron), which takes a path to a git repo.
It will add the xml feed files to that path, and updated and push the changes.
