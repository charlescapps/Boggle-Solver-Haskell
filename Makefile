default: src/*.hs
	ghc -O5 -o main src/*.hs -outputdir bin
