#!/usr/bin/env bash

coq_makefile -f _CoqProject -o Makefile
cat Makefile.conf
