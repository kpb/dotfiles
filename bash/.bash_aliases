# ~/.bash_aliases
# Git shortcuts as functions (for bash completion) + portable completion loading.

# Only run in interactive shells
case $- in *i*) ;; *) return 0 2>/dev/null || exit 0 ;; esac

# Load completion framework (Mac: Homebrew bash-completion@2, Linux: Debian/Ubuntu)
[ -r /opt/homebrew/etc/profile.d/bash_completion.sh ] && source /opt/homebrew/etc/profile.d/bash_completion.sh
[ -r /usr/share/bash-completion/bash_completion ] && source /usr/share/bash-completion/bash_completion

# ---- git shortcuts (functions) -------------------------------------------
ga()  { git add "$@"; }
gb()  { git branch "$@"; }
gba() { git branch -a "$@"; }
gc()  { git commit "$@"; }
gca() { git commit -a "$@"; }
gco() { git checkout "$@"; }
gd()  { git diff "$@"; }
gl()  { git pull --prune "$@"; }
glog(){ git log --graph --pretty=format:'%Cred%h%Creset %an: %s - %Creset %C(yellow)%d%Creset %Cgreen(%cr)%Creset' \
          --abbrev-commit --date=relative "$@"; }

# Push: if upstream exists, normal push; otherwise set upstream to origin/HEAD
gp() {
  git rev-parse --abbrev-ref --symbolic-full-name @{u} >/dev/null 2>&1 \
    && git push "$@" \
    || git push -u origin HEAD "$@"
}

# Force push: same behavior for first push (sets upstream), then force
gpf() {
  git rev-parse --abbrev-ref --symbolic-full-name @{u} >/dev/null 2>&1 \
    && git push -f "$@" \
    || git push -u origin HEAD "$@"
}

gs()  { git status -sb "$@"; }

# completion wiring
# Attach git completion safely (no-op if git completion isn't loaded)
__gitwrap_complete() {
  # $1 = wrapper name, $2 = completion target (function name or "git")
  type __git_complete >/dev/null 2>&1 || return 0
  __git_complete "$1" "$2"
}

# Most wrappers can just behave like `git` completion
__gitwrap_complete ga   git
__gitwrap_complete gc   git
__gitwrap_complete gca  git
__gitwrap_complete gd   git
__gitwrap_complete gl   git
__gitwrap_complete glog git
__gitwrap_complete gp   git
__gitwrap_complete gpf  git
__gitwrap_complete gs   git

# Better UX when these complete like their specific subcommands (branches/refs)
if type _git_branch >/dev/null 2>&1; then
  __gitwrap_complete gb  _git_branch
  __gitwrap_complete gba _git_branch
else
  __gitwrap_complete gb  git
  __gitwrap_complete gba git
fi

if type _git_checkout >/dev/null 2>&1; then
  __gitwrap_complete gco _git_checkout
else
  __gitwrap_complete gco git
fi
