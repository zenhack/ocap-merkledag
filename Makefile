

sandstorm_exe := omd-sandstorm-grain

all: webui
	go build ./...

sandstorm: cmd/$(sandstorm_exe)/$(sandstorm_exe)
	cd cmd/$(sandstorm_exe) && go build

dev: sandstorm
	spk dev

webui := pkg/webui
webui: $(webui)/bundle.js
ts_src := $(shell find $(webui) -type f -name '*.ts')
$(webui)/bundle.js: $(ts_src) $(webui)/tsconfig.json
	cd $(webui) && tsc
# TODO: minify:
#$(webui)/bundle.min.js: $(webui)/bundle.js
#	cd $(webui) && npx uglfiyjs --compress --mangle < bundle.js > bundle.min.js

.PHONY: all webui dev sandstorm
