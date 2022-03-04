

sandstorm_exe := omd-sandstorm-grain

all:
	go build ./...

sandstorm: cmd/$(sandstorm_exe)/$(sandstorm_exe)
	cd cmd/$(sandstorm_exe) && go build

dev: sandstorm
	spk dev
