.PHONY: fetch all

all: build

fetch:
	curl -o src/guide.csv "https://docs.google.com/spreadsheets/d/e/2PACX-1vT6a48c7DbGQxBREh3fhsK8huxsxNedhVux_xYKMl1ktfgZehvWpznhQ9h9nHOyMx7X1AnOJL1c6bTz/pub?gid=1147444412&single=true&output=csv"

build: fetch src/*elm
	elm-app build
	touch docs/.nojekyll
	/bin/echo -n guide.theborderland.se > docs/CNAME
	ln -s index.html docs/404.html
	ln -s WWW-FOMO* docs/print
	ln -s WWW-FOMO* docs/print.pdf
	git add docs

