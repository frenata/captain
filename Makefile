SRC = src
BUILD = build
MAIN = Captain
ASSETS = assets
CSS = $(SRC)/css
FONTS = $(ASSETS)/font

all: build deploy clean

build: build-directory js html #css fonts favicon

build-directory:
	mkdir -p $(BUILD)

js:
	elm-make src/$(MAIN).elm --output $(BUILD)/app.js
	uglifyjs $(BUILD)/app.js --compress --mangle \
		--output $(BUILD)/app.min.js 2> /dev/null
	mv $(BUILD)/app.min.js $(BUILD)/app.js

html:
	cp $(SRC)/index.html $(BUILD)/index.html

css:
	cat $(CSS)/*.css > \
		$(BUILD)/style.css

fonts: 
	cp -r $(FONTS) $(BUILD)

favicon:
	cp $(ASSETS)/favicon.ico $(BUILD)

deploy:
	cp CNAME $(BUILD)/CNAME
	surge $(BUILD)

clean:
	rm -rf $(BUILD)
