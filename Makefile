EXE := tc
BUILD_DIR := build
CFLAGS := -Mtp -O2

all: $(EXE)

$(EXE): $(BUILD_DIR)
	fpc $(CFLAGS) -o$(BUILD_DIR)/$(EXE) src/Main.pas
	ln -sf ./$(BUILD_DIR)/$(EXE) ./$(EXE)

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

clean:
	rm -rf $(BUILD_DIR)

.PHONY: all clean
