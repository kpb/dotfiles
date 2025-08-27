# Kenneth Does Dotfiles

I/Kenneth decided to get more serious about my dotfiles. I've started organizing them following [GNU Stow][stow] naming
conventions and created a Makefile to install/uninstall them easily.

## Use

The default Make target will output a useful help message describing the _make targets_.

```bash
$ make
Targets:
install    Install the dotfile 'packages' into your '$HOME' dir
clean      Remove dotfile symlinks using Stow
```

Enjoy!

[stow]: https://www.gnu.org/software/stow/ "GNU Stow - the symlink farm manager"
