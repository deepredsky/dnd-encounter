.PHONY: all elm_js minify clean

elm_js := static/js/elm.js
css := static/css/all.css
main := src/Main.elm

css_files := static/css/bulma.min.css static/css/style.css

all: clean elm_js minify $(css)

elm_js:
	npx elm make --output=$(elm_js) --optimize $(main)

$(css): $(css_files)
	cat $^ > $@

minify:
	npx elm-minify $(elm_js) --overwrite

clean:
	rm -f $(elm_js) $(css)
