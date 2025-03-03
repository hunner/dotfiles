#!/bin/sh
if [ -n "$PINENTRY_USER_DATA" ]; then
  case "$PINENTRY_USER_DATA" in
    IJ_PINENTRY=*)
      "/Users/haugenh1/Applications/WebStorm.app/Contents/jbr/Contents/Home/bin/java" -cp "/Users/haugenh1/Applications/WebStorm.app/Contents/plugins/vcs-git/lib/git4idea-rt.jar:/Users/haugenh1/Applications/WebStorm.app/Contents/lib/externalProcess-rt.jar" git4idea.gpg.PinentryApp
      exit $?
    ;;
  esac
fi
exec /opt/homebrew/bin/pinentry-mac "$@"