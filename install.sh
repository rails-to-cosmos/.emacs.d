#!/bin/bash

apt-add-repository ppa:ubuntu-elisp/ppa
apt-get update
apt-get install emacs-snapshot emacs-snapshot-el
add-apt-repository ppa:git-core/ppa
apt-get update
apt-get install git
pip install virtualenv
