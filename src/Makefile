poly:
	ghc -O3 -rtsopts --make Strappy/Poly.hs
ugm:
	ghc -O3 -rtsopts --make Strappy/UGM.hs
profpoly:
	ghc -O3 -fbreak-on-exception -prof -auto-all -caf-all -fforce-recomp -rtsopts --make Strappy/Poly.hs
tower:
	ghc -O3 -rtsopts --make Strappy/Tower.hs
proftower:
	ghc -O3 -fbreak-on-exception -prof -auto-all -caf-all -fforce-recomp -rtsopts --make Strappy/Tower.hs
clean:
	rm Strappy/*.o Strappy/*.hi
