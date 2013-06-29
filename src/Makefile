em:
	ghc -O3 -rtsopts --make Strappy/EM.hs
plan:
	ghc -O3 -rtsopts --make Strappy/PlanTest.hs
profem:
	ghc -O3 -prof -auto-all -caf-all -fforce-recomp -rtsopts --make Strappy/EM.hs
profplan:
	ghc -O3 -prof -auto-all -caf-all -fforce-recomp -rtsopts --make Strappy/PlanTest.hs
clean:
	rm Strappy/*.o Strappy/*.hi