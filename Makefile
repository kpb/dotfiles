# Makefile to install dotfiles. Requires GNU Stow.

.DEFAULT_GOAL := help

.PHONY: install
install: ## Install the dotfile 'packages' into your '$HOME' dir
	@echo "Installing dotfiles into $(HOME)"
	@stow -t $(HOME) bash emacs

.PHONY: clean
clean: ## Remove dotfile symlinks using Stow
	@echo "Uninstalling dotfiles"
	@stow -t $(HOME) bash emacs

.PHONY: help
help:
	@echo "Targets:"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-10s\033[0m %s\n", $$1, $$2}'
