adc = gnatmake -f -gnatwa -gnata -g

main: # clean puis compile projet
	gnatclean pagerank
	$(adc) pagerank

exemple: # compile, lance l'exemple et affiche les résultats
	clear
	make main
	@echo ""

	@echo "          -----| Exemple Sujet - Matrice Creuse |-----"
	./pagerank exemple-fichier.txt
	@echo "output.pr"
	@cat output.pr
	@echo "output.prw"
	@cat output.prw
	@echo ""

	@echo "          -----| Exemple Sujet - Matrice Pleine |-----"
	./pagerank -P exemple-fichier.txt
	@echo "output.pr"
	@cat output.pr
	@echo "output.prw"
	@cat output.prw

worm: # compile, lance l'exemple et affiche les résultats
	clear
	make main
	@echo ""

	@echo "          -----| Worm - Matrice Creuse |-----"
	./pagerank worm.net
	@echo "output.pr"
	@head output.pr
	@echo "..."
	@echo "output.prw"
	@head output.prw
	@echo "..."
	@echo ""

	@echo "          -----| Worm - Matrice Pleine |-----"
	./pagerank -P worm.net
	@echo "output.pr"
	@head output.pr
	@echo "..."
	@echo "output.prw"
	@head output.prw
	@echo "..."

brainlinks: # compile, lance l'exemple et affiche les résultats
	clear
	make main
	@echo ""

	@echo "          -----| Brainlinks - Matrice Creuse |-----"
	./pagerank brainlinks.net
	@echo "output.pr"
	@head output.pr
	@echo "output.prw"
	@head output.prw
	@echo ""

make robustesse:
	clear
	make main
	@echo ""
	@echo "--------------------------------------------------------------------"
	-./pagerank -E -3 exemple-fichier.txt
	@echo ""
	@echo "--------------------------------------------------------------------"
	-./pagerank -A 2 exemple-fichier.txt
	@echo ""
	@echo "--------------------------------------------------------------------"
	-./pagerank foo.net
	@echo ""
	@echo "--------------------------------------------------------------------"
	-./pagerank -Z 4566 exemple-fichier.txt
	@echo ""
	@echo "--------------------------------------------------------------------"
	-./pagerank -P -R sortie exemple-fichier.txt

test: # tests modules de calcul
	$(adc) test_creuse
	$(adc) test_pleine
	clear
	./test_creuse
	./test_pleine

testC: # tests unitaires matrice creuse
	$(adc) test_creuse
	./test_creuse

testP: # tests unitaires matrice pleine
	$(adc) test_pleine
	./test_pleine

clean: # nettoie tout (lojik)
	gnatclean pagerank
	gnatclean matrice_creuse
	gnatclean matrice_pleine
	gnatclean pagerank_doc
	gnatclean pagerank_exceptions
	gnatclean test_creuse
	gnatclean test_pleine

	rm -f *.pr
	rm -f *.prw
