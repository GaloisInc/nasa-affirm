index.html: tower-to-sal.md
	pandoc -t slidy -f markdown -s $< -o $@

index-reveal.html: tower-to-sal.md
	pandoc -t revealjs -f markdown+definition_lists \
		-s $< -o $@ \
		-V revealjs-url=https://cdnjs.cloudflare.com/ajax/libs/reveal.js/3.0.0 \
		-V transition=slide \
		--highlight-style=zenburn
	gsed -i s/simple.css/black.css/ index.html
