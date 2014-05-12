#!/usr/bin/env sh

# Update the GitHub .emacs.d repository.

git add --all init.el conf vendor snippets update-github.sh
git add -u

git commit -m "Update"

git push origin master
