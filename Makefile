.PHONY: help tests tests-watch docs docs-watch clean

# 'help' target originally taken from: https://github.com/parsonsmatt/servant-persistent
help: ## Print help documentation
	@grep -E '^[.a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "$(COLOR_GREEN)%-30s$(COLOR_DEFAULT) %s\n", $$1, $$2}'

tests: ## Run the tests with cabal
	@cabal test

tests-watch: ## Run the tests with ghcid, re-running each time the source files change
	ghcid \
		--command="cabal repl --repl-options=-ilib --repl-options=-itest isbn:isbn-test" \
		--reload="lib" \
		--test="Main.main"

docs: ## Generate the Haddock documentation
	@cabal haddock

docs-watch: ## Regenerate the docs each time the source files change
	@find lib/ -name "*.hs" | entr cabal haddock

clean: ## Clear all build artifacts
	@rm -rf dist-newstyle
	@rm -rf dist


## Shell color codes

COLOR_DEFAULT=\033[0m
COLOR_BOLD=\033[1m
COLOR_GREEN=\033[36m

