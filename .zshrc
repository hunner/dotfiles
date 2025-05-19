## Profiling options
PROFILE_STARTUP=false
if [[ "$PROFILE_STARTUP" == true ]]; then
  PS4=$'%D{%M%S%.} %N:%i> '
  exec 3>&2 2>/tmp/zsh-startlog.$$
  setopt xtrace prompt_subst
fi

typeset -ga preexec_functions
typeset -ga precmd_functions
typeset -ga chpwd_functions
fpath=(/usr/local/share/zsh-completions /opt/homebrew/share/zsh/site-functions $fpath)
#typeset -u fpath

# Options
setopt prompt_subst append_history hist_ignore_space hist_ignore_all_dups hist_expire_dups_first hist_find_no_dups hist_save_no_dups hist_reduce_blanks extendedglob nomatch notify dvorak interactive_comments # correct
unsetopt beep
bindkey -e

zstyle ':completion:*' completer _complete
zstyle ':completion:*' matcher-list '' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' '+l:|=* r:|=*'
autoload -Uz compinit colors vcs_info select-word-style promptinit && colors && promptinit
compinit -u

zstyle :compinstall filename '~/.zshenv'
zstyle ':vcs_info:*' actionformats '%F{6}(%f%s%F{5})%F{3}-%F{5}[%F{2}%b%F{3}|%F{1}%a%F{5}]%f '
zstyle ':vcs_info:*' formats "%{$fg_bold[black]%}[%{$fg_no_bold[blue]%}%b%{$fg_bold[black]%}]%{$reset_color%}"
zstyle ':vcs_info:*' enable git

# Don't have / as wordchar so ^w erases path parts
local WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'
#bindkey '^L' push-line
bindkey "^I" expand-or-complete-prefix
#select-word-style bash
umask 022

# Fix $TERM
if [ -f /etc/termcap ] ; then
    export TERMCAP=/etc/termcap
fi
case "$TERM" in
    rxvt-unicode) export TERM=rxvt;;
    rxvt-256color) export TERM=rxvt;;
esac

# Go lang stuff
#export GOPATH=$HOME/Documents/work/git/go
#if whence go > /dev/null ; then
#   export GOROOT=$(go env GOROOT)
#fi
#export GO111MODULE=on

# Paths
#export LD_LIBRARY_PATH=/opt/csw/lib
#zsh's path
if whence systemctl > /dev/null ; then
  systemctl --user import-environment PATH
fi
export PATH=/usr/sbin:/usr/bin:/sbin:/bin:$PATH
#export MANPATH=~/local/share/man:/usr/man:/usr/share/man:/usr/local/share/man
#paths=(/cat/bin)
prepaths=(/opt/puppetlabs/pdk/bin ~/.config/emacs/bin /opt/homebrew/bin /usr/local/bin /usr/local/sbin /usr/local/opt/node@8/bin ~/.local/bin ~/.rbenv/bin ~/local/talon ~/local/bin ~/local/sbin)
#for dir in $paths ; do
#    if [ -d $dir ] ; then
#        export PATH=$PATH:$dir
#    fi
#    if [ -d ${dir:a:h}/man ] ; then
#        export MANPATH=$MANPATH:${dir:a:h}/man
#    fi
#done
#for dir in $prepaths ; do
#    if [ -d $dir ] ; then
#        export PATH=$dir:$PATH
#    fi
#    if [ -d ${dir:a:h}/man ] ; then
#        export MANPATH=${dir:a:h}/man:$MANPATH
#    fi
#done
# Load profiles from /etc/profile.d
if test -d /etc/profile.d/; then
    for profile in /etc/profile.d/*.sh; do
        test -x $profile && . $profile
    done
    unset profile
fi
# bison for beancount 3
if [ -d /usr/local/opt/bison/bin ] ; then
    export PATH=/usr/local/opt/bison/bin:$PATH
    export LDFLAGS="-L/usr/local/opt/bison/lib"
fi
# for latex
#eval "$(/usr/libexec/path_helper)"

#gem's path
if [ -d ~/.gems/bin ] ; then
    export PATH="$HOME/.gems/bin:$PATH"
fi
if [ -d ~/.gem/ruby/1.9.1/bin ] ; then
    export PATH="$PATH:$HOME/.gem/ruby/1.9.1/bin"
fi
if [ -d ~/.gem/ruby/1.8/bin ] ; then
    export PATH="$PATH:$HOME/.gem/ruby/1.8/bin"
fi
#if [ -d ~/.rvm/bin ] ; then
#    export PATH="$HOME/.rvm/bin:$PATH"
#fi
#for dir in `find /opt/*/bin|grep /bin$` `find /opt/csw/*/bin|grep /bin$` ; do
#    export PATH=$PATH:$dir
#done
export XDG_DATA_DIRS="$HOME/.local/share:$XDG_DATA_DIRS"
export CM_DIR="$HOME/.config/clipmenu"

# Setting vars
#TERM=rxvt
#export GEM_HOME="$HOME/.gems"
#export GEM_HOME="/Library/Ruby/Gems/1.8"
#export GEM_PATH="$GEM_HOME:/usr/lib/ruby/gems/1.8"
#export GEM_PATH="/System/Library/Frameworks/Ruby.framework/Versions/1.8/usr/lib/ruby/gems/1.8"
zshhosts=(serenity.cat.pdx.edu hunner@mint.cic.pdx.edu drkatz.cat.pdx.edu walt.ece.pdx.edu bunny.cat.pdx.edu spof.cat.pdx.edu fops.cat.pdx.edu narsil.cat.pdx.edu hunner@odin.pdx.edu hunnur@alcmaeonllc.com mir.cat.pdx.edu geppetto.cat.pdx.edu)
HISTSIZE=100000000
SAVEHIST=100000000
HISTFILE=~/.history
export GPGKEY="0x1CED67750173FC1C"
if [ -f ~/.zsh/private ] ; then
  source ~/.zsh/private
fi
export NETHACKOPTIONS='autopickup,color,!cmdassist,!number_pad,hilite_pet,boulder:0,pickup_types:$"=/!?+,menustyle:partial,!legacy,suppress_alert:3.3.1'
export DEV_ROOT="$HOME/Documents/work/git/"
# This causes messages on every start without java installed
#[ -e /usr/libexec/java_home ] && export JAVA_HOME=$(/usr/libexec/java_home)
export OVFTOOL='/Applications/VMware OVF Tool/ovftool'
#export DOCKER_HOST='tcp://192.168.99.100:2375'
if whence rg > /dev/null ; then
  export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --glob "!.git/*"'
  export FZF_DEFAULT_OPTS="--history=$HOME/.fzf_history"
fi

# Prompt
#prompt_precmd() {
#    gitcolor=""
#    if zgit_isgit ; then
#        if ! zgit_isindexclean ; then
#            #PROMPT="[%F{$usercolor}%n%F{white}@%F{$hostcolor}%m%F{white}:%F{blue}%~%f](%F{cyan}$(zgit_branch)%f)>"
#            gitcolor=$fg[blue]
#        elif ! zgit_isworktreeclean ; then
#            #PROMPT="[%F{$usercolor}%n%F{white}@%F{$hostcolor}%m%F{white}:%F{blue}%~%f]>"
#            gitcolor=$fg[green]
#        fi
#    fi
#    color="%(?.$gitcolor.$fg[red])"
#    PROMPT="%m$color%#%{$reset_color%} "
#}
#precmd_functions+=prompt_precmd
PROMPT="%m%{$fg_bold[blue]%}%#%{$reset_color%} "
PTIME="%{$fg_bold[grey]%}[%{$fg_no_bold[blue]%}%T%{$fg_bold[grey]%}]%{$reset_color%}"
vcs_info_wrapper() {
  vcs_info
  if [ -n "$vcs_info_msg_0_" ]; then
    echo "%{$fg_bold[grey]%}${vcs_info_msg_0_}%{$reset_color%}$del"
  fi
}
terraform_wrapper() {
  if [[ -d .terraform ]]; then
    local tf_workspace=$(terraform workspace show)
    if [ -n "$tf_workspace" ]; then
      echo "%{$fg_bold[grey]%}[%{$fg_no_bold[blue]%}${tf_workspace}%{$fg_bold[grey]%}]%{$reset_color%}$del"
    fi
  fi
}
kubectl_wrapper() {
  if type -p kubectl > /dev/null ; then
    local k8s_context=$(kubectl config current-context)
    local k8s_namespace=$(kubectl config view --minify -o jsonpath='{.contexts[0].context.namespace}')
    if [ -n "$k8s_context" ]; then
      echo "%{$fg_bold[grey]%}[%{$fg_no_bold[blue]%}${k8s_context}:${k8s_namespace}%{$fg_bold[grey]%}]%{$reset_color%}$del"
    fi
  fi
}
RPROMPT=$'$(kubectl_wrapper)$(terraform_wrapper)$(vcs_info_wrapper)${PTIME}'

if [ `uname -s` = "SunOS" ] ; then
    export LANG="C"
else
    export LANG="en_US.UTF-8"
fi
#export LC_CTYPE="en_US.UTF-8"
export LC_COLLATE="C" #order files in ls
#export LC_TIME="en_US.UTF-8"
#export LC_NUMERIC="en_US.UTF-8"
#export LC_MONETARY="en_US.UTF-8"
#export LC_MESSAGES="en_US.UTF-8"
#export LC_ALL='en_US.utf8'
export DIALOGOPTS='--visit-items'
export MAIL=~/mail
export MAILCHECK=0
#export AWT_TOOLKIT=MToolkit
#export AWT_TOOLKIT=XToolkit
export _JAVA_AWT_WM_NONREPARENTING=1
if [ -x `whence less` ] ; then
    export PAGER='less -R'
else
    export PAGER=more
fi
if [ -z "$PERL5LIB" ] ; then
        # If PERL5LIB wasn't previously defined, set it...
        export PERL5LIB=~/local/lib/perl5:~/local/lib/perl5/site_perl
else
        # ...otherwise, extend it.
        export PERL5LIB=$PERL5LIB:~/local/lib/perl5:~/local/lib/perl5/site_perl
fi

if [ -f ~/.zsh/aliases ] ; then
  source ~/.zsh/aliases
fi

# GPG 2.1.x SSH support
# See : http://incenp.org/notes/2015/gnupg-for-ssh-authentication.html
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)

## Add extra fonts
#xset fp+ /usr/APL2/fonts/X11
#xset fp  rehash

## Set up ruby gems for $RUBYLIB
#if [ -d $HOME/.gems/gems ] ; then
#    for rlib in $HOME/.gems/gems/*/lib ; do
#        export RUBYLIB=$RUBYLIB:$rlib
#    done
#fi
#for rubylib in /usr/lib/ruby/gems/1.8/gems/*/lib ; do
#    export RUBYLIB=$RUBYLIB:$rubylib
#done

function init_coalsack_vars() {
  export COALSACK_CURRENT_CONTEXT=$(kubectl config current-context)
  if [[ 'docker-for-desktop' = $COALSACK_CURRENT_CONTEXT ]] ; then
    export COALSACK_TEST_K8S_ENDPOINT=$dfd_K8S_ENDPOINT
    export COALSACK_TEST_CA_DATA=$dfd_CA_DATA
    export COALSACK_TEST_BEARER_TOKEN=$dfd_BEARER_TOKEN
    export COALSACK_TEST_PASSWORD=$dfd_PASSWORD
  elif [[ 'minikube' = $COALSACK_CURRENT_CONTEXT ]] ; then
    export COALSACK_TEST_K8S_ENDPOINT=$minikube_K8S_ENDPOINT
    export COALSACK_TEST_CA_DATA=$minikube_CA_DATA
    export COALSACK_TEST_BEARER_TOKEN=$minikube_BEARER_TOKEN
    export COALSACK_TEST_PASSWORD=$minikube_PASSWORD
  fi
}

# Functions
#VMHOST=vmpooler.delivery.puppetlabs.net
VMHOST=vmpooler-prod.k8s.infracore.puppet.net
function listtoken() { curl -u hunter --url $VMHOST/api/v1/token ; }
function gettoken() { curl -X POST -u hunter --url $VMHOST/api/v1/token ; }
function rmtoken() { curl -X DELETE -u hunter --url $VMHOST/api/v1/token/$1 ; }
function listmyvm() { curl --url $VMHOST/api/v1/token/$(grep vmpooler_token ~/.fog | cut -d ' ' -f 4); }

function listvm() { curl -H "X-AUTH-TOKEN: $(grep vmpooler_token ~/.fog | cut -d ' ' -f 4)" -s --url $VMHOST/api/v1/vm/ ; }
function getvm() { curl -H "X-AUTH-TOKEN: $(grep vmpooler_token ~/.fog | cut -d ' ' -f 4)" -d --url $VMHOST/api/v1/vm/$1 ; }
function sshvm() { ssh -i ~/.ssh/id_rsa-acceptance root@$1 ; }
function rmvm() { curl -H "X-AUTH-TOKEN: $(grep vmpooler_token ~/.fog | cut -d ' ' -f 4)" -X DELETE --url $VMHOST/api/v1/vm/$1 ; }

function migratetoken() { curl -X POST -d '' -u hunter --url "https://nspooler-service-prod-1.delivery.puppetlabs.net/api/v1/token?token=$(grep vmpooler_token ~/.fog | cut -d ' ' -f 4)" }
function listns() { curl -H "X-AUTH-TOKEN: $(grep vmpooler_token ~/.fog | cut -d ' ' -f 4)" -s -X GET --url https://nspooler-service-prod-1.delivery.puppetlabs.net/api/v1/status/ ; }
function getns() { curl -H "X-AUTH-TOKEN: $(grep vmpooler_token ~/.fog | cut -d ' ' -f 4)" -d '' -X POST --url https://nspooler-service-prod-1.delivery.puppetlabs.net/api/v1/host/$1 ; }
function sshns() { ssh -i ~/.ssh/id_rsa-acceptance root@$1 ; }
function rmns() { curl -H "X-AUTH-TOKEN: $(grep vmpooler_token ~/.fog | cut -d ' ' -f 4)" -X DELETE --url https://nspooler-service-prod-1.delivery.puppetlabs.net/api/v1/host/$1 ; }

args() { echo $#; }
title() { WINTITLE="$*"; print -Pn "\e]0;$WINTITLE\a" }
hl() { pbpaste | highlight --syntax=$1 -O rtf | pbcopy }
#if [ x$WINDOW != x ]; then
#    # Running under screen(1)
#    precmd()  { [ -z "$WINTITLE" ] && print -Pn "\e]0;%m [W$WINDOW] [%~]\a" || : }
#    preexec() { [ -z "$WINTITLE" ] && print -Pn "\e]0;%m [W$WINDOW] [$1]\a" || : }
#else
#    precmd()  { [ -z "$WINTITLE" ] && print -Pn "\e]0;%m [%~]\a" || : }
#    preexec() { [ -z "$WINTITLE" ] && print -Pn "\e]0;%m [$1]\a" || : }
#fi
if [ x$EMACS = x -a x$DISPLAY != x ] ; then
    precmd()  { [ -z "$WINTITLE" ] && print -Pn "\e]0;%m [%~]\a" || : }
    preexec() { [ -z "$WINTITLE" ] && print -Pn "\e]0;%m [$1]\a" || : }
fi

if [ -f $HOME/.termcap ] ; then
    TERMCAP=$(< $HOME/.termcap)
    export TERMCAP
fi
make_termcap() {
    cat > $HOME/.termcap << EOF
st+24bit|st+24bit terminal (X Window System):\
    :Co#16777216:\
    :tc=xterm-unicode:\
    :tc=rxvt:
    :setrgbf=\E[38;2;#1%d;#2%d;#3%dm:\
    :setrgbb=\E[48;2;#1%d;#2%d;#3%dm:\
    :use=st+24bit:
EOF
}

# replaced by asdf
#if [ -d ~/.rbenv ] ; then
#  eval "$(rbenv init -)"
#fi
#if [ -d ~/.pyenv ] ; then
#  export PYENV_ROOT="$HOME/.pyenv"
#  export PATH="$PYENV_ROOT/bin:$PATH"
#  eval "$(pyenv init --path)"
#  eval "$(command pyenv init - zsh)" # --no-rehash)"
#fi
#[ -f ~/.zsh-fuzzy-match/fuzzy-match.zsh ] && source ~/.zsh-fuzzy-match/fuzzy-match.zsh
export PATH="${ASDF_DATA_DIR:-$HOME/.asdf}/shims:$PATH"
enabledotnet() {
  if [ -d ~/.dotnet/tools ] ; then
    # Add .NET Core SDK tools
    export PATH="$PATH:/Users/haugenh1/.dotnet/tools"
    . ~/.asdf/plugins/dotnet/set-dotnet-env.zsh
  fi
}

#export PATH=$(go env GOPATH)/bin:$PATH

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
# and for nixos
if [ -n "${commands[fzf-share]}" ]; then
  source "$(fzf-share)/key-bindings.zsh"
  source "$(fzf-share)/completion.zsh"
fi

getstageadsktoken() {
  TEMP_TOKEN=$(echo -n ${ADSK_HUNNER_STG_CLI_CLIENT_ID}:${ADSK_HUNNER_STG_CLI_CLIENT_SECRET} | base64)
    curl -s 'https://developer-stg.api.autodesk.com/authentication/v2/token' \
    -H 'Content-Type: application/x-www-form-urlencoded' \
    -H 'Accept: application/json' \
    -H "Authorization: Basic ${TEMP_TOKEN}" \
    -d 'grant_type=client_credentials' \
    -d 'scope=data:read' \
    | jq -r '.access_token'
}

getdevadsktoken() {
  TEMP_TOKEN=$(echo -n ${ADSK_HUNNER_DEV_CLI_CLIENT_ID}:${ADSK_HUNNER_DEV_CLI_CLIENT_SECRET} | base64)
    curl -s 'https://developer-dev.api.autodesk.com/authentication/v2/token' \
    -H 'Content-Type: application/x-www-form-urlencoded' \
    -H 'Accept: application/json' \
    -H "Authorization: Basic ${TEMP_TOKEN}" \
    -d 'grant_type=client_credentials' \
    -d 'scope=data:read data:write bucket:create bucket:read bucket:update bucket:delete' \
    | jq -r '.access_token'
}

getprodadsktoken() {
  TEMP_TOKEN=$(echo -n ${ADSK_HUNNER_PROD_CLI_CLIENT_ID}:${ADSK_HUNNER_PROD_CLI_CLIENT_SECRET} | base64)
    curl -s 'https://developer.api.autodesk.com/authentication/v2/token' \
    -H 'Content-Type: application/x-www-form-urlencoded' \
    -H 'Accept: application/json' \
    -H "Authorization: Basic ${TEMP_TOKEN}" \
    -d 'grant_type=client_credentials' \
    -d 'scope=data:read' \
    | jq -r '.access_token'
}

devidlookup() {
  curl -s -H "Authorization: Bearer ${$(getdevadsktoken)}" -H 'Content-Type: application/json' "https://developer-dev.api.autodesk.com/userprofile/v1/users/${1}" | jq -r '"\(.userId) \(.emailId)"'
}

stageidlookup() {
  curl -s -H "Authorization: Bearer ${$(getstageadsktoken)}" -H 'Content-Type: application/json' "https://developer-stg.api.autodesk.com/userprofile/v1/users/${1}" | jq -r '"\(.userId) \(.emailId)"'
}

prodidlookup() {
  curl -s -H "Authorization: Bearer ${$(getprodadsktoken)}" -H 'Content-Type: application/json' "https://developer.api.autodesk.com/userprofile/v1/users/${1}" | jq -r '"\(.userId) \(.emailId)"'
}

# load-nvm() {
#   export NVM_DIR="$HOME/.nvm"
#   [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
#   [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
# }
# alias nvm-lts="load-nvm && nvm install lts/* --latest-npm ----reinstall-packages-from=node && nvm alias default lts/*"
# alias nvm-latest="load-nvm && nvm install node --latest-npm ----reinstall-packages-from=node && nvm alias default node"

# added by travis gem
[ -f /Users/hunner/.travis/travis.sh ] && source /Users/hunner/.travis/travis.sh

## Profiling options
if [[ "$PROFILE_STARTUP" == true ]]; then
  unsetopt xtrace
  exec 2>&3 3>&-
fi

#test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"


# added by travis gem
[ ! -s /home/hunner/.travis/travis.sh ] || source /home/hunner/.travis/travis.sh

#eval "$(starship init zsh)"
eval "$(direnv hook zsh)"

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/home/hunner/Downloads/google-cloud-sdk/path.zsh.inc' ]; then . '/home/hunner/Downloads/google-cloud-sdk/path.zsh.inc'; fi

#eval "$(/opt/homebrew/bin/brew shellenv)"

# For 1.25 gke auth update
export USE_GKE_GCLOUD_AUTH_PLUGIN=True
# The next line enables shell command completion for gcloud.
if [ -f '/home/hunner/Downloads/google-cloud-sdk/completion.zsh.inc' ]; then . '/home/hunner/Downloads/google-cloud-sdk/completion.zsh.inc'; fi
