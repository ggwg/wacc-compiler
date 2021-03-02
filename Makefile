# Sample Makefile for the WACC Compiler lab: edit this to build your own comiler
# Locations
# The make rules

all: rules

# runs the antlr build script then attempts to compile all scala files
rules:
	sbt assembly

.PHONY: all rules clean


