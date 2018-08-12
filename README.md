# platypussearch

Simple code search and code browsing tool. Assumes, you have installed following applications:
* grep
* bat
* ansi2html

## Notes

```
stack build --haddock
stack hoogle -- generate --local
stack build hlint hscope haskell-tools-daemon haskell-tools-cli --copy-compiler-tool
# this is sad:-(
# How to get ghc-mod working: 
# https://github.com/DanielG/ghc-mod/pull/922#issuecomment-353896120
# TODO this is not scalable for more than 1 project
stack hoogle -- server --local --port=8080
```
