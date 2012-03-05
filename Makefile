solve_random: src/*.hs src/mains/SolveRandom.hs
	ghc -O5 -o solve_random src/*hs src/mains/SolveRandom.hs -outputdir bin

get_runtime: src/*.hs src/mains/GetRuntime.hs
	ghc -O5 -o get_runtime src/*hs src/mains/GetRuntime.hs -outputdir bin

solve_file: src/*.hs src/mains/SolveFile.hs
	ghc -O5 -o solve_file src/*hs src/mains/SolveFile.hs -outputdir bin

hashtest: src/BogHash.hs testhash/TestHash.hs 
	ghc -O5 -v -o hashtest src/BogHash.hs testhash/TestHash.hs -outputdir bin

clean: 
	rm -f solve_random
	rm -f solve_file
	rm -f hashtest
	rm -fr bin/*
