install:
	ghc Sabio
	haddock -h -o docs Sabio.hs Laberinto.hs

clean:
	rm *.o *.hi Sabio
	rm -rf docs