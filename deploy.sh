#!/bin/sh
git push origin `git subtree split --prefix public source`:master --force
