#!/bin/sh

url=$1

module_user=$(echo $url | cut -d '/' -f 4)
module_name=$(echo $url | cut -d '/' -f 5)
pr_number=$(  echo $url | cut -d '/' -f 7)

if [ x = x$module_user -o x = x$module_name -o x = x$pr_number ] ; then
  echo "something wasn't set: $module_user / $module_name / $pr_number"
  exit 1
fi
if [ ! -d ~/Documents/work/git ] ; then
  echo "No ~/Documents/work/git"
  exit 1
fi
cd ~/Documents/work/git
if [ ! -d $module_name ] ; then
  clonepuppetmodule.sh $module_name
fi
cd $module_name || exit 1

payload=$(   curl -s https://api.github.com/repos/$module_user/$module_name/pulls/$pr_number || exit 1)
to_branch=$(/bin/echo "$payload" | jq -r '.base.ref')
from_ref=$( /bin/echo "$payload" | jq -r '.head.ref')
from_sha=$( /bin/echo "$payload" | jq -r '.head.sha')
from_user=$(/bin/echo "$payload" | jq -r '.head.user.login')
from_desc=$(/bin/echo "$payload" | jq -r '.body')
ssh_url=$(  /bin/echo "$payload" | jq -r '.head.repo.ssh_url')
if [ "x" = "x$payload" -o "x" = "x$to_branch" -o "x" = "x$from_ref" -o "x" = "x$from_sha" -o "x" = "x$from_user" -o "x" = "x$ssh_url" ] ; then
  echo "Something is missing"
  exit 1
fi
if ! git diff-index --quiet HEAD -- ; then
  echo "Clone isn't clean"
  exit 1
fi
git checkout $to_branch || exit 1
git pull || exit 1
echo "Fetching..."
git fetch $ssh_url $from_ref || exit 1
echo "Committing merge..."
git merge --no-ff -m "Merge pull request #${pr_number} from ${from_user}/${from_ref}

${from_desc}" $from_sha
echo "Done"
