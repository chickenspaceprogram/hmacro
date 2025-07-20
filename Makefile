.PHONY: rel dbg asan wipe clean make test memcheck

UNIXFLAGS=-DHMACRO_DBGFLAGS=ON -DCMAKE_EXPORT_COMPILE_COMMANDS=ON
STATIC_TOOLS=-DHMACRO_CLANG_TIDY=ON
ASAN=-DHMACRO_USE_ASAN=ON
TESTS=-DHMACRO_TESTS=ON

DEBUG=-DCMAKE_BUILD_TYPE=Debug
RELWITHDEBINFO=-DCMAKE_BUILD_TYPE=RelWithDebInfo
RELEASE=-DCMAKE_BUILD_TYPE=Release 

THREADS=-j8
BUILD_DIR=build

# this is a makefile i have to shorten the commands I have to type

make: $(BUILD_DIR)
	: 'build'
	cmake --build $(BUILD_DIR) $(THREADS)

rel: wipe
	: 'rel'
	cmake -B $(BUILD_DIR) $(RELEASE) $(UNIXFLAGS) $(TESTS) $(STATIC_TOOLS)
	cmake --build $(BUILD_DIR) $(THREADS)

dbg: wipe
	: 'dbg'
	cmake -B $(BUILD_DIR) $(DEBUG) $(UNIXFLAGS) $(TESTS) $(STATIC_TOOLS)
	cmake --build $(BUILD_DIR) $(THREADS)

reldbg: wipe
	: 'rel with debug info'
	cmake -B $(BUILD_DIR) $(RELWITHDEBINFO) $(UNIXFLAGS) $(TESTS) $(STATIC_TOOLS)
	cmake --build $(BUILD_DIR) $(THREADS)

asan: wipe
	: 'asan'
	cmake -B $(BUILD_DIR) $(DEBUG) $(UNIXFLAGS) $(TESTS) $(STATIC_TOOLS) $(ASAN)
	cmake --build $(BUILD_DIR) $(THREADS)

test: make
	ctest --test-dir $(BUILD_DIR) --output-on-failure $(THREADS)

memcheck: make
	ctest --test-dir $(BUILD_DIR) -T memcheck $(THREADS)

wipe:
	: 'wipe'
	rm -rf build

clean:
	: 'clean'
	cmake --build $(BUILD_DIR) --target clean

$(BUILD_DIR):
	: '$(BUILD_DIR)'
	cmake -B $(BUILD_DIR) $(DEBUG) $(UNIXFLAGS) $(TESTS) $(STATIC_TOOLS)


