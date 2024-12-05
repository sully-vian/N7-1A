#!/bin/bash

JSON_PATH="./lib/json-20160810.jar"
CHECKSTYLE_PATH="./lib/checkstyle.jar"
JUNIT_PATH="./lib/junit4.jar"

# vérifier qu'on est bien à la racine du projet
check_directory() {
	if [[ ! -d "./.git" ]]; then
		echo "Ce script doit être exécuté à la racine du projet."
		exit 1
	fi
}

# compiler les fichiers .java dans le dossier bin
compile() {
	check_directory
	javac --release 7 -cp ./src:"$JSON_PATH" -d ./bin ./src/Main.java
}

# lancer le projet
run() {
	check_directory
	java -cp ./bin:"$JSON_PATH" Main
}

# construire le jar comme il faut
build() {
	check_directory
	echo "compilation..."
	compile
	echo "création du jar..."
	jar cvfm ./simulation2D.jar ./Manifest.txt -C ./bin . > ./build.log
	echo "Terminé."
}

# lancer l'application via le jar
launch() {
	check_directory
	java -cp simulation2D.jar:"$JSON_PATH" Main 2> ./error.log
}

# compiler et lancer les tests avec JUNIT
run_tests() {
	check_directory

	echo "-------------"
	echo "ParticuleTest"
	echo "-------------"
	javac --release 7 -cp ./src:"$JUNIT_PATH" -d ./bin ./src/simulation2D/tests/ParticuleTest.java
	java -cp ./bin:"$JUNIT_PATH" org.junit.runner.JUnitCore simulation2D.tests.ParticuleTest

	echo "-------------"
	echo "Vecteur2DTest"
	echo "-------------"
	javac --release 7 -cp ./src:"$JUNIT_PATH": -d ./bin ./src/simulation2D/tests/Vecteur2DTest.java
	java -cp ./bin:"$JUNIT_PATH" org.junit.runner.JUnitCore simulation2D.tests.Vecteur2DTest
}

# supprimer les fichiers java compilés et la documentation
clean() {
	check_directory
	echo "suppression des fichiers .class..."
	find . -name *.class | xargs rm -f # supprimer tous les .class du git
	echo "vidange du dossier bin..."
	rm -rf ./bin/*
	touch ./bin/.gitkeep # pour que le dossier bin disparaisse pas du git
	echo "vidange du dossier doc..."
	rm -rf ./doc/*
	touch ./doc/.gitkeep # pour que le dossier doc disparaisse pas du git
}

# vérifier le style du code avec checkstyle
checkstyle() {
	check_directory
	echo "vérification du style de code..."
	/usr/lib/jvm/default-java/bin/java -jar $CHECKSTYLE_PATH -c ./lib/checkstyle.xml src/

}

# générer la documentation javadoc dans le dossier doc
doc() {
	check_directory
	echo "génération de la documentation..."
	javadoc -d ./doc -author -sourcepath ./src -subpackages simulation2D
}
