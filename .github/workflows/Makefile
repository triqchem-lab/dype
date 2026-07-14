SRC = ../../src/github/workflows
DIR = .
HEADER = $(SRC)/header.txt

# Source files (e.g., stack.yml)
WORKFLOWS := $(notdir $(wildcard $(SRC)/*.yml))

.PHONY: all
all: $(WORKFLOWS)

# Prepend header and copy to generated location
$(DIR)/%: $(SRC)/% $(HEADER)
	cp $(HEADER) $@
	cat $< >> $@

.PHONY: clean
clean:
	rm -f $(addprefix $(DIR)/, $(WORKFLOWS))
