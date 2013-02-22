all: clean pokesim

pokesim: PokeSim.hs Pokemon.hs PokeParse.hs PokeBattle.hs
	ghc PokeSim.hs

clean:
	-rm pokesim *.o *.hi
