#!/bin/sh

if [ x"$HOST" = "xcz.puppetlabs.lan" ] ; then
    echo 'set imap_user="hunter@puppetlabs.com"'
    echo 'set from="hunter@puppetlabs.com"'
    echo 'set realname="Hunter Haugen"'
    #set imap_pass="password"

    #set smtp_url="smtp://hunter.haugen@smtp.gmail.com:587/"

    echo 'set folder="imaps://imap.gmail.com:993"'
    echo 'set spoolfile="+INBOX"'
    set postponed="+[Gmail]/Drafts"
    set record="+[Gmail]/Sent Mail"
else
    # IMAP
    echo 'set imap_user="hunner"'
    #set imap_pass="password"

    # SMTP
    echo 'set smtp_url="smtp://mailhost.cecs.pdx.edu:587/"'
    #set ssl_min_dh_prime_bits=512
    #set smtp_pass="password"
    echo 'set from="hunner@cat.pdx.edu"'
    echo 'set realname="Hunter Haugen"'

    # Folders
    echo 'set folder="imaps://mailhost.cecs.pdx.edu:993"'
    echo 'set spoolfile="+INBOX"'
fi
