.PHONY: help tests tests-watch docs docs-watch clean hackage-build hackage-upload hackage-upload-publish

# 'help' target originally taken from: https://github.com/parsonsmatt/servant-persistent
help: ## Print help documentation
	@grep -E '^[.a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "$(COLOR_GREEN)%-30s$(COLOR_DEFAULT) %s\n", $$1, $$2}'

tests: ## Run the tests with cabal
	@cabal test --flags=dev

tests-watch: ## Run the tests with ghcid, re-running each time the source files change
	ghcid \
		--command="cabal repl --flags=dev --repl-options=-ilib --repl-options=-itest isbn:isbn-test" \
		--reload="lib" \
		--test="Main.main"

docs: ## Generate the Haddock documentation
	@cabal haddock

docs-watch: ## Regenerate the docs each time the source files change
	@find lib/ -name "*.hs" | entr cabal haddock

clean: ## Clear all build artifacts
	@rm -rf dist-newstyle
	@rm -rf dist
	@rm -rf build

BUILD_TMP_DIR=build-tmp
BUILD_DIR=build
hackage-build: ## Build the package and documentation for Hackage
	@mkdir -p $(BUILD_TMP_DIR)/{sdist,docs}
	@rm -rf $(BUILD_DIR)
	@mkdir -p $(BUILD_DIR)/{sdist,docs}
	@cabal sdist --builddir=$(BUILD_TMP_DIR)
	@cabal haddock --builddir=$(BUILD_TMP_DIR)/docs --haddock-for-hackage --enable-documentation
	@mv $(BUILD_TMP_DIR)/sdist/*.tar.gz build/sdist/
	@mv $(BUILD_TMP_DIR)/docs/*.tar.gz build/docs/
	@rm -rf $(BUILD_TMP_DIR)
	@echo "Done building package and documentation!"


hackage-upload: hackage-build ## Upload package candidate to hackage
	@cabal upload $(BUILD_DIR)/sdist/isbn-*.tar.gz
	@cabal upload -d $(BUILD_DIR)/docs/isbn-*-docs.tar.gz
	@echo "Done!"

hackage-upload-publish: $(BUILD_DIR) ## Upload package candidate to hackage
	@cabal upload --publish $(BUILD_DIR)/sdist/isbn-*.tar.gz
	@cabal upload --publish -d $(BUILD_DIR)/docs/isbn-*-docs.tar.gz
	@echo "Done!"

## Shell color codes

COLOR_DEFAULT=\033[0m
COLOR_BOLD=\033[1m
COLOR_GREEN=\033[36m

