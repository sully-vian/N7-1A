main:
	javac *.java
	java org.junit.runner.JUnitCore SujetCercleTest
	java org.junit.runner.JUnitCore FormeCercleTest
	java -ea org.junit.runner.JUnitCore RobustesseCercleTest
	
	java org.junit.runner.JUnitCore CercleTest
	java org.junit.runner.JUnitCore ComplementsCercleTest

checkstyle:
	java -jar /mnt/n7fs/ens/tp_cregut/checkstyle.jar -c ../checkstyle.xml Cercle.java

clean:
	rm *.class