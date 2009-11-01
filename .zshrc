# Options
setopt appendhistory autocd extendedglob nomatch notify dvorak # correct
unsetopt beep
bindkey -e
zstyle :compinstall filename '~/.zshrc'
autoload -Uz compinit
compinit -u
bindkey '^L' push-line
bindkey "^I" expand-or-complete-prefix
umask 022

# Paths
export LD_LIBRARY_PATH=/opt/csw/lib
#zsh's path
export PATH=/usr/sbin:/usr/bin:/sbin:/bin
export MANPATH=/usr/man:/usr/share/man
paths=(~/.cabal/bin /cat/bin /cat/games/bin /opt/csw/sbin /opt/csw/bin
/pkgs/ghc/current/bin /usr/sfw/sbin /usr/sfw/bin /opt/SUNWut/sbin
/opt/SUNWut/bin /usr/ccs/bin /usr/local/bin /usr/openwin/bin
/usr/bin/X11 /usr/local/bin/X11 /usr/openwin/bin/xview /opt/java/bin
/opt/java5/bin /opt/java/jre/bin /opt/openoffice/program)
for dir in $paths ; do
    if [ -d $dir ] ; then
        export PATH=$PATH:$dir
        if [ -d `dirname $dir` ] ; then
            export MANPATH=$MANPATH:`dirname $dir`/man
        fi
    fi
done
if [ -d ~/local/bin ] ; then
    export PATH=~/local/bin:~/local/sbin:$PATH
    export MANPATH=~/local/man:$MANPATH
    export MANPATH=~/local/share/man:$MANPATH
fi
#gem's path
if [ -d ~/.gems/bin ] ; then
    export PATH="$HOME/.gems/bin:$PATH"
fi
#for dir in `find /opt/*/bin|grep /bin$` `find /opt/csw/*/bin|grep /bin$` ; do
#    export PATH=$PATH:$dir
#done

# Setting vars
#TERM=rxvt
export GEM_HOME="$HOME/.gems"
export GEM_PATH="$GEM_HOME:/usr/lib/ruby/gems/1.8"
zshhosts=(firefly.cat.pdx.edu hunner@mint.cic.pdx.edu aragog.cat.pdx.edu
zabava.cat.pdx.edu verne.hunnur.com drkatz.cat.pdx.edu walt.ece.pdx.edu
bunny.cat.pdx.edu spof.cat.pdx.edu fops.cat.pdx.edu narsil.cat.pdx.edu
serenity.cat.pdx.edu hunner@odin.pdx.edu hunnur@alcmaeonllc.com
mir.cat.pdx.edu geppetto.cat.pdx.edu 131.252.134.134)
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.history
PS1="%m%# "
export LANG="en_US.UTF-8"
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
export AWT_TOOLKIT=MToolkit
if [ -x `which less` ] ; then
    export PAGER==less
else
    export PAGER==more
fi
if [ -z "$PERL5LIB" ] ; then
        # If PERL5LIB wasn't previously defined, set it...
        export PERL5LIB=~/local/lib/perl5:~/local/lib/perl5/site_perl
else
        # ...otherwise, extend it.
        export PERL5LIB=$PERL5LIB:~/local/lib/perl5:~/local/lib/perl5/site_perl
fi

if [ -x `which vim` ] ; then
    export VISUAL=vim
    export EDITOR=vim
    if [ -n "$DISPLAY" ] ; then
        alias vi="gvim -font 'APL385 Unicode 8' -c 'set keymap=uniapl385'"
    else
        alias vi=vim
    fi
else
    export VISUAL=vi
    export EDITOR=vi
fi
#if [ -d $HOME/.gems/gems ] ; then
#    for rlib in $HOME/.gems/gems/*/lib ; do
#        export RUBYLIB=$RUBYLIB:$rlib
#    done
#fi
#for rubylib in /usr/lib/ruby/gems/1.8/gems/*/lib ; do
#    export RUBYLIB=$RUBYLIB:$rubylib
#done

# Aliases
alias ls="ls -F"
alias l="ls -F"
alias ll="l -Fl"
alias la="l -Fa"
alias lla="ll -Fa"
alias c="cd"
#alias s="TERM=xterm;ssh serenity.cat.pdx.edu"
alias s="TERM=rxvt;ssh hunner@serenity.cat.pdx.edu"
alias f="TERM=rxvt;ssh hunner@firefly.cat.pdx.edu"
alias m="TERM=rxvt;ssh hunner@mint.cic.pdx.edu"
alias odin="TERM=xterm;ssh hunner@odin.pdx.edu"
alias clancy="ssh hunnur@clancy.dreamhost.com"
alias kvar="ssh hunner@131.252.134.134"
alias kvin="ssh hunner@131.252.135.22"
alias x="exit"
alias gpg-add="/usr/libexec/gpg-preset-passphrase"
alias rsync="rsync -azPHe ssh" #-a equals -rlptgoD
alias mang="cd ~/zips/mangband ; DISPLAY=\"\" ./mangclient"
alias cl="co -l"
alias cu="ci -u"
alias sl="screen -ls"
alias sr="screen -r"
alias sx="screen -x"
alias srd="screen -rd"
alias bc="bc -q"
alias fm="fmstatus.sh&;shell-fm"
alias apl="gvim -font 'APL385 Unicode 14' -c 'set keymap=uniapl385'"
alias d="dtach -a /tmp/dtach"
alias eo="xmodmap ~/keymaps/eo_dv_hunner.pke"
alias vt="export TERM=vt220"
alias rm=rm; unalias rm #hack
alias gem="nice -n19 gem"
alias uzbl="uzbl-browser"

# Functions
args() { echo $#; }
title() { printf '\33]2;%s\007' $* }
resize() { printf '\33]50;%s%d\007' "xft:Terminus:pixelsize=" $1 ",xft:IPAGothic:antialias=true" }
asdf() {
    if [ `uname -s` = "SunOS" ] ; then
        if [ x`hostname` = x"chandra.cs.pdx.edu" ] ; then
            xmodmap ~/keymaps/eo_dv_hunner_type7.pke
        else
            xmodmap ~/keymaps/eo_dv_hunner_type7.pke
        fi
    else
        if [ x`hostname` = x"ni" ] ; then
            xmodmap ~/keymaps/nu_x61.pke
        else
            xmodmap ~/keymaps/nu_std.pke
        fi
    fi
}
aoeu() {
    if [ `uname -s` = "SunOS" ] ; then
        xmodmap ~/keymaps/original-type7-sol.pke
    else
        if [ x`hostname` = x"ni" ] ; then
            xmodmap ~/keymaps/qwerty_x61.pke
        else
            xmodmap ~/keymaps/kvar.pke
        fi
    fi
}
type7() {
    if [ `uname -s` = "SunOS" ] ; then
        xmodmap ~/keymaps/eo_dv_hunner_type7_sol.pke
    else
        xmodmap ~/keymaps/nu_type7.pke
    fi
}
zpush() {
    for host in $zshhosts ; do
        files=(.zshrc .vim .vimrc)
        if [ x$(hostname) = x$host ] ; then
            continue
        fi
        if [ $(uname) = "Linux" ] ; then
            ping -c1 -W1 ${host#*@} > /dev/null
        else
            ping -c1 -t1 ${host#*@} > /dev/null
        fi
        if [ $status -eq 0 ] ; then
            echo -n "$host: copying"
            for file in $files ; do
                echo -n .
                if ! rsync -azPH $file $host:~ > /dev/null ; then continue 2 ; fi
            done
            echo "done"
            #rsync .zshenv $host:~ > /dev/null
        else
            echo "$host: unpings"
        fi
    done;
}

ex () {
    if which gtar > /dev/null ; then
        TAR=gtar
    else
        TAR=tar
    fi
    if [ -f $1 ] ; then
        case $1 in
            *.tar.bz2)   $TAR xvjf $1    ;;
            *.tar.gz)    $TAR xvzf $1    ;;
            *.bz2)       bunzip2 $1     ;;
            *.rar)       unrar x $1     ;;
            *.gz)        gunzip $1      ;;
            *.tar)       $TAR xvf $1     ;;
            *.tbz2)      $TAR xvjf $1    ;;
            *.tgz)       $TAR xvzf $1    ;;
            *.zip)       unzip $1       ;;
            *.Z)         uncompress $1  ;;
            *.7z)        7z x $1        ;;
            *)           echo "don't know how to extract '$1'..." ;;
        esac
    else
        echo "'$1' is not a valid file!"
    fi
}
