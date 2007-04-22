# Makefile
# Author: mfh
# Date: 31 Dec 2006
# Last modified: 31 Dec 2006
#
# Makefile for wrapper to libdl shared library functions.
# Produces a shared library called "libshared".
# 
# FIXME: Adjust this Makefile to fit your own system!
#

libshared.so: shared.c
	gcc -shared -ldl -o $@ $<
