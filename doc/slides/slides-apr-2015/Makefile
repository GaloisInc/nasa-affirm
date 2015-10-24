pdf: ivory-to-sal.pdf
ivory-to-sal.pdf: ivory-to-sal.md
	pandoc -t beamer -f markdown \
		--highlight-style=tango \
		$< -o $@

html: index.html
index.html: ivory-to-sal.md
	pandoc -t slidy -f markdown -s $< -o $@

all: html pdf

.PHONY: all html pdf
