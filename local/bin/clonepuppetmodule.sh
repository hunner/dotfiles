#!/bin/sh

git clone git@github.com:puppetlabs/$1.git && \
cd $1 && \
git remote rename origin puppetlabs && \
git remote add origin git@github.com:hunner/$1.git && \
git fetch --all && \
cd ..
