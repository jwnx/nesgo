.PHONY: all clean nesgo

TST=./tst
BIN=./bin
RES=./res
LOG=./log

TESTS=$(addprefix ${BIN}/, $(notdir $(patsubst %.s,%,$(sort $(wildcard ${TST}/*.s)))))

all: asm tooly nesgo

asm:
	git submodule update --init --recursive
	cd asm6f && $(MAKE) all

tooly: asm
	cd TOOLY && $(MAKE) all

front:
	cd nesgo/gui/frontend && npm install && npm run build

gui: front
	go run github.com/leaanthony/mewn/cmd/mewn nesgo/gui/app.go

nesgo: gui
	cd nesgo && go build -o ./build/nesgo -ldflags "-s -w -X github.com/wailsapp/wails.BuildMode=Debug" ./cmd/nesgo.go

${BIN}/%: ${TST}/%.s
	./asm6f/asm6f $^ $@

${BIN}:
	@mkdir -p ${BIN}

${LOG}:
	@mkdir -p ${LOG}

test: asm nesgo ${BIN} ${LOG} ${TESTS}
	@{  echo "************************* Tests ******************************"; \
		test_failed=0; \
		test_passed=0; \
		for test in ${TESTS}; do \
			result="${LOG}/$$(basename $$test).log"; \
			expected="${RES}/$$(basename $$test).r"; \
			printf "Running $$test: "; \
			./nesgo/build/nesgo -rom $$test > $$result 2>&1; \
			errors=`diff -y --suppress-common-lines $$expected $$result | grep '^' | wc -l`; \
			if [ "$$errors" -eq 0 ]; then \
				printf "\033[0;32mPASSED\033[0m\n"; \
				test_passed=$$((test_passed+1)); \
			else \
				printf "\033[0;31mFAILED [$$errors errors]\033[0m\n"; \
				test_failed=$$((test_failed+1)); \
			fi; \
		done; \
		echo "*********************** Summary ******************************"; \
		echo "- $$test_passed tests passed"; \
		echo "- $$test_failed tests failed"; \
		echo "**************************************************************"; \
	}

clean:
	cd asm6f && $(MAKE) clean
	cd TOOLY && $(MAKE) clean
	rm -rf ./nesgo/build
	rm -rf ./nesgo/gui/frontend/node_modules
	rm -rf ./nesgo/gui/frontend/build
	rm -rf ./nesgo/gui/gui-mewn.go
	rm -rf ${LOG}/*
	rm -rf ${BIN}/*
