# Makefile to install dotfiles. Requires GNU Stow.

.DEFAULT_GOAL := help
PACKAGES      := bash emacs

.PHONY: install
install: ## Install the dotfile 'packages' into your '$HOME' dir
	@echo "Installing dotfiles into $(HOME)"
	@stow -t $(HOME) $(PACKAGES)

.PHONY: clean
clean: ## Remove dotfile symlinks
	@echo "Uninstalling dotfiles"
	@stow -D -t $(HOME) $(PACKAGES)

.PHONY: help
help:
	@echo "Targets:"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-10s\033[0m %s\n", $$1, $$2}'
