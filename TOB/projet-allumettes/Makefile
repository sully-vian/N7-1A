main:
	# cd src && javac -d ../bin/ allumettes/Jouer.java


checkstyle:
	@java -jar /mnt/n7fs/ens/tp_cregut/checkstyle.jar -c checkstyle.xml $(shell find src/allumettes/ -name "*.java" -not -name "*Test*")

clean:
	rm -f ./tests/*.diff ./tests/*.computed
	rm -f ./*/*/*.class
	rm -r -f ./bin

test:
	./testeur.sh tests/*.run
	cd src && javac -d ../bin/ allumettes/*Test.java
	cd bin && java org.junit.runner.JUnitCore allumettes.StrategieRapideTest