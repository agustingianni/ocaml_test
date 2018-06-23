.PHONY: default build install uninstall test clean

default: build

build:
	jbuilder build main.exe

test:
	jbuilder runtest -f

install:
	jbuilder install

uninstall:
	jbuilder uninstall

clean:
	jbuilder clean
	
run:
	./_build/default/main.exe