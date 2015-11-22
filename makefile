all:
	ghc -o spider --make src/Main.hs -isrc -fwarn-unused-imports -XTemplateHaskell -threaded -rtsopts -with-rtsopts=-N
