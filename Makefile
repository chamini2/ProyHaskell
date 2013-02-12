all: clean pokesim

pokesim: pokesim.hs pokemon.hs
	ghc pokesim.hs

clean:
	-rm pokesim *.o *.hi
