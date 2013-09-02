#!/usr/bin/env sh

# Update the GitHub .emacs.d repository.

git add init.el conf vendor update-github.sh
git add -u

git commit -m "Update"

git push origin master
