# lines

Blazing fast CLI tool for counting lines of any local or remote project.

## Install

Currently you need the haskell build tool stack installed to get this working.

### Build from sources

```
git clone https://github.com/japiirainen/lines.git && make install
```

This will try to build the project and copy the executable to your local path.

## Usage

Run on a local directory.
```.sh
lines --dir some/local/dir
```

Run on remote git repository
```.sh
lines --repo https://github.com/japiirainen/lines

Ignore some file paths. For example node_modules
```.sh
lines --repo https://github.com/japiirainen/lines --ignore node_modules
```
```

## Supported languages

- Support is currently very limited. Feel free to open an issue about missing language support.

## TODO
- Support more languages.
- Try to use conduit for all file processing.
- Need to somehow ignore folders line node_modules.