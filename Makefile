PLANEMO ?= planemo
GALAXY_BRANCH ?= release_25.1
TOOL_DIRS ?= $(shell find tools data_managers packages -name .shed.yml -exec dirname {} \; | sort)
TEST_DIRS ?= $(shell find tools data_managers -name .shed.yml -exec dirname {} \; | sort)
PLANEMO_LINT_ARGS ?= --biocontainers --skip version_bumped
PLANEMO_TEST_ARGS ?= --galaxy_branch $(GALAXY_BRANCH)

.PHONY: help list lint test

help:
	@echo "Targets:"
	@echo "  make list              Show tool repositories discovered from .shed.yml files"
	@echo "  make lint              Run planemo lint for all tool repositories"
	@echo "  make test              Run planemo test for all runnable tool repositories"
	@echo "Variables:"
	@echo "  TOOL_DIRS='tools/name' Limit lint to selected directories"
	@echo "  TEST_DIRS='tools/name' Limit test to selected directories"
	@echo "  GALAXY_BRANCH=release_25.1 Override Galaxy branch used by planemo test"

list:
	@printf '%s\n' $(TOOL_DIRS)

lint:
	$(PLANEMO) lint --fail_level warn $(PLANEMO_LINT_ARGS) $(TOOL_DIRS)

test:
	$(PLANEMO) test $(PLANEMO_TEST_ARGS) $(TEST_DIRS)
