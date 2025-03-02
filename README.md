# Dotfiles

This is more a note for future me, but feel free to use anything you
like.

## Requirements

Following is a list of a few things I always install before linking any
of the dotfiles. Everything mentioned here is installable via
`homebrew`, once that's set up.

- [homebrew](https://brew.sh/) for mac and linux
- [fish](https://fishshell.com/) shell
- [asdf](https://asdf-vm.com/) for versions of various runtimes (languages like ruby, node, sbcl etc.)
- [tmux](https://github.com/tmux/tmux/wiki) mainly for tabbing multiple shell sessions, but detach/attach does come in handy sometimes
  - Current config depends on [tmux plugin manager](https://github.com/tmux-plugins/tpm)
- [emacs](https://www.gnu.org/software/emacs/) and [neovim](https://neovim.io/) are the two editors I use the most.
  - I use [emacs-plus](https://github.com/d12frosted/homebrew-emacs-plus) flavor because I find it works best for my on a mac.
- [homebrew fonts](https://github.com/Homebrew/homebrew-cask-fonts) is a repository for all the nerd fonts
  - I use Jetbrains Mono Nerd Font in my terminal, editor etc. The homebrew package is called `font-jetbrains-mono-nerd-font`
- [iTerm2](https://iterm2.com) is my primary terminal emulator


## Installation

Clone the repo and symlink the files and directories to respective
locations in home. For example:

```fish
git clone git@github.com:akagr/dotfiles.git ~/dotfiles

ln -s ~/dotfiles/dot-gitconfig ~/.gitconfig
ln -s ~/dotfiles/dot-tmux-dot-conf ~/.tmux.conf
ln -s ~/dotfiles/dot-emacs-dot-d ~/.emacs.d
ln -s ~/dotfiles/nvim ~/.config/nvim
mkdir -p ~/.config/fish
ln -s ~/dotfiles/config.fish ~/.config/fish/config.fish
```

### Emacs setup

After linking the dotfiles and starting emacs, all the package
installation happens automatically. Once it settles, we need to run two
lisp expressions. `Note`{.verbatim}: These can also be run interactively
using `M-x`.

```lisp
(all-the-icons-install-fonts)
(nerd-icons-install-fonts)
```
The lsp package `eglot` uses a helper package `eglot-lsp-booster` to offload the json processing from
lsp server to rust plugin. It needs to be set up as such:

1. Install [rust toolchain](https://www.rust-lang.org/tools/install).
2. Run `cargo install emacs-lsp-booster`.

### Neovim setup

We need a few binaries that some functionality I use depends on but isn't automatically installed.

```fish
brew install fd fzf ripgrep gnu-sed
fzf install
```

### Git setup

Linking `.gitconfig` brings most of my customisations, but we need to
install `gnupg` and `pinentry-mac` to sign commits. Once both of these
are install, inform `git` of `pinentry-mac` with:

```fish
echo "pinentry-program $(which pinentry-mac)" >> ~/.gnupg/gpg-agent.conf
gpgconf --kill gpg-agent
```

Additionally, import an existing GPG key with:

```fish
gpg --import private.key
```

Or, to generate a new key and add it to Github, follow [github
documentation for generating
keys](https://docs.github.com/en/authentication/managing-commit-signature-verification/generating-a-new-gpg-key).

## Why name the files `dot-tmux-dot-conf` instead of `.tmux.conf`

Any file starting with a period (.) is hidden on most \*nix systems. All
the subject files which I use this repo to backup and share are named
so, so that they are always visible. While linking, I use correct names
for the symlinks.
