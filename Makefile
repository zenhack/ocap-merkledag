

sandstorm_exe := omd-sandstorm-grain

all: webui
	go build ./...

sandstorm: cmd/$(sandstorm_exe)/$(sandstorm_exe)

cmd/$(sandstorm_exe)/$(sandstorm_exe):
	cd cmd/$(sandstorm_exe) && go build

pack: omd.spk
omd.spk: sandstorm sandstorm-files.list
	spk pack $@
dev: sandstorm
	spk dev

clean:
	cd $(webui) && rm -rf out

webui := pkg/webui
ui_deps := $(webui)/node_modules/.deps_installed
webui: $(webui)/out/bundle.min.js
ts_src := \
	  $(shell find $(webui)/src -type f -name '*.ts') \
	  $(shell find $(webui)/src -type f -name '*.tsx')
$(ui_deps): $(webui)/package.json
	cd $(webui) && npm install
	touch $@
$(webui)/out/.ts-build: $(ts_src) $(webui)/tsconfig.json $(ui_deps)
	cd $(webui) && tsc
	touch $@
$(webui)/out/bundle.js: $(webui)/out/.ts-build $(webui)/webpack.config.js
	cd $(webui) && npx webpack
$(webui)/out/bundle.min.js: $(webui)/out/bundle.js $(ui_deps)
	cd $(webui) && npx uglifyjs --compress --mangle < out/bundle.js > out/bundle.min.js

.PHONY: all webui dev sandstorm pack
