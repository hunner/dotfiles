" load all of the HTML info
source $VIMRUNTIME/syntax/html.vim
unlet b:current_syntax
syntax include @Ruby runtime! syntax/ruby.vim
syntax region rhtmlRuby start=+<%+ end=+%>+ contains=@Ruby,rhtmlRuby
let b:current_syntax="rhtml"
