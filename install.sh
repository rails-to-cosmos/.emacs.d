#!/bin/bash

apt-add-repository ppa:ubuntu-elisp/ppa
apt-get install python-pip python-dev build-essential
pip install --upgrade pip
pip install --upgrade virtualenv
apt-get install emacs-snapshot emacs-snapshot-el
add-apt-repository ppa:git-core/ppa
apt-get update
apt-get install git
pip install virtualenv
