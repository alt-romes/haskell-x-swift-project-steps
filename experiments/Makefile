.PHONY: all Memory.cmm Rect.S

all: Memory.cmm Rect.S

Memory.cmm: Memory.hs
	ghc -fforce-recomp -ddump-cmm $< > $@

Rect.S: Rect.swift
	swiftc $< -emit-ir -o - | swift demangle > $@
