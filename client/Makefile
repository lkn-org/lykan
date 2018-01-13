export PATH := $(shell npm bin):${PATH}
JS_SRC := $(shell find src/ -name '*.js')

index.js: .npm ${JS_SRC}
	webpack

.npm: package.json
	npm install
	touch .npm
