#!/usr/bin/env sh

# Update the GitHub .emacs.d repository.

git add -u init.el conf vendor update-github.sh
git commit -m "Update"

git push origin master
