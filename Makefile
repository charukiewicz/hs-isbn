.PHONY: help test ghcid-test ghcid-cabal docs clean

# 'help' target originally taken from: https://github.com/parsonsmatt/servant-persistent
help: ## Print help documentation
	@grep -E '^[.a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "$(COLOR_GREEN)%-30s$(COLOR_DEFAULT) %s\n", $$1, $$2}'

test: ## Run the tests with cabal
	@cabal test

docs: ## Generate the Haddock documentation
	@cabal haddock

ghcid-test: $(OBJECT_DIR) ## Run the tests with ghcid
	ghcid \
		--command="ghci -fobject-code -odir $(OBJECT_DIR) -hidir $(OBJECT_DIR) -O0 -ilib -itest test/Spec.hs" \
		--reload="lib" \
		--test="Main.main"

ghcid-cabal: $(OBJECT_DIR) ## Run the tests with ghcid
	ghcid \
		--command="cabal repl --repl-options=-ilib --repl-options=-itest isbn:isbn-test" \
		--reload="lib" \
		--test="Main.main"

clean: ## Clear all build artifacts
	@rm -rf dist-newstyle
	@rm -rf dist
	@rm -rf $(OBJECT_DIR)


OBJECT_DIR=.ghci-build-artifacts

$(OBJECT_DIR): 
	@mkdir $@

## Shell color codes

COLOR_DEFAULT=\033[0m
COLOR_BOLD=\033[1m
COLOR_GREEN=\033[36m

