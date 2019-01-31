#!/bin/bash

emacs --batch \
			-l init.el \
			-l .travis-tests.el \
			-f ert-run-tests-batch-and-exit

