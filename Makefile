watch:
	stack build --file-watch

install:
	stack build lines --copy-bins

install-packages:
	stack install