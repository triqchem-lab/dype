SRC = ../../src/github/workflows
DIR = .

# Source files (e.g., stack.yml)
WORKFLOWS := $(notdir $(wildcard $(SRC)/*.yml))

.PHONY: all
all: $(WORKFLOWS)

# Copy source to generated location
$(DIR)/%: $(SRC)/%
	cp $< $@

.PHONY: clean
clean:
	rm -f $(addprefix $(DIR)/, $(WORKFLOWS))
