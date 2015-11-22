all:
	ghc -o spider --make src/Main.hs -isrc -fwarn-unused-imports -XTemplateHaskell
