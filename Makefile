.PHONY: default build install uninstall test clean

default: build

build:
	jbuilder build --root src main.exe

test:
	jbuilder runtest -f --root src

install:
	jbuilder install --root src

uninstall:
	jbuilder uninstall --root src

clean:
	jbuilder clean --root src

run:
	./src/_build/default/main.exe
