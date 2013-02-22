all: clean pokesim

pokesim: Pokesim.hs Pokemon.hs PokeParse.hs PokeBattle.hs
	ghc Pokesim.hs

clean:
	-rm pokesim *.o *.hi
