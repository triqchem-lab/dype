# All makefiles must define TOP, corresponding to the dype root directory.
# This is so that they can be imported from a Makefile in a subdirectory.
ifeq ($(TOP),)
  $(error "Makefiles must define the TOP variable to correspond with the dype source root")
endif

# Andreas, 2025-03-05, STACK might be set in the environment
# (e.g. in workflow test.yml); in this case, don't override.
STACK ?= stack

# Andreas, 2022-03-10: suppress chatty announcements like
# "Stack has not been tested with GHC versions above 9.0, and using 9.2.2, this may fail".
# These might get in the way of interaction testing.
STACK_SILENT=$(STACK) --silent

# HAS_STACK detection:
# If STACK is already set in environment, the workflow has configured stack explicitly.
# Otherwise, check if stack.yaml exists in project root.
ifneq ($(STACK),stack)
  HAS_STACK := 1
else ifneq ($(wildcard $(TOP)/stack.yaml),)
  HAS_STACK := 1
else
  HAS_STACK :=
endif
