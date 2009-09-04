" detect puppet filetypes

if exists("did_load_filetypes")
    finish
endif
augroup filetypedetect
    au! BufRead,BufNewFile *.pp      setfiletype puppet
    "au! BufNewFile,BufRead *.rhtml   set syn=eruby
    "au! BufNewFile,BufRead *.haml    setfiletype haml 
    "au! BufNewFile,BufRead *.rhtml    setfiletype rhtml
    au! BufNewFile,BufRead *.rhtml    setfiletype eruby
    au! BufNewFile,BufRead *.s        setfiletype asmx86

augroup END
