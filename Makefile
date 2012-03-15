solve_random: src/*.hs src/mains/SolveRandom.hs
	ghc -O5 -o solve_random -main-is SolveRandom.main src/*hs src/mains/SolveRandom.hs -outputdir bin

get_runtime: src/*.hs src/mains/GetRuntime.hs
	ghc -O5 -o get_runtime -main-is GetRuntime.main src/*hs src/mains/GetRuntime.hs -outputdir bin

get_memory_used: src/*.hs src/mains/GetMemoryUsed.hs
	ghc -O5 -o get_memory_used -main-is GetMemoryUsed.main src/*hs src/mains/GetMemoryUsed.hs -outputdir bin

solve_file: src/*.hs src/mains/SolveFile.hs
	ghc -O5 -o solve_file -main-is SolveFile.main src/*hs src/mains/SolveFile.hs -outputdir bin

hashtest: src/BogHash.hs testhash/TestHash.hs 
	ghc -O5 -v -o hashtest -main-is TestHash.main src/BogHash.hs testhash/TestHash.hs -outputdir bin

clean: 
	rm -f solve_random
	rm -f get_runtime
	rm -f solve_file
	rm -f hashtest
	rm -fr bin/*
