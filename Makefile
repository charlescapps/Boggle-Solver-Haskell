solve_random: src/*.hs
	ghc -O5 -o solve_random src/*hs src/mains/SolveRandom.hs -outputdir bin

hashtest: src/BogHash.hs testhash/TestHash.hs 
	ghc -O5 -v -o hashtest src/BogHash.hs testhash/TestHash.hs -outputdir bin

clean: 
	rm -f solve_random
	rm -f hashtest
