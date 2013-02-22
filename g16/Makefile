all: clean pokesim

pokesim: PokeSim.hs Pokemon.hs PokeParse.hs PokeBattle.hs
	ghc PokeSim.hs -o pokesim

clean:
	-rm pokesim *.o *.hi
