.PHONY: help
help: ## Print documentation
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: ghcid
ghcid: ## Run ghcid with the wallet-new project
	ghcid \
	    --command "stack ghci pboy --ghci-options=-fdiagnostics-color=always"

