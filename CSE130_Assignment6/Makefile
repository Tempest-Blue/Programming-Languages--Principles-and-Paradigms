FSC = fsc -Djava.io.tmpdir=.scala-devel -classpath scalacheck.jar -feature -language:implicitConversions -deprecation -Xfatal-warnings
SCALA = scala -cp .:scalacheck.jar


all: Test.class

scalacheck.jar:
        curl -L "http://search.maven.org/remotecontent?filepath=org/scalacheck/scalacheck_2.11/1.11.4/scalacheck_2.11-1.11.4.jar" -o scalacheck.jar

Bst.class: Bst.scala scalacheck.jar
	$(FSC) $<

Decorators.class: Decorators.scala scalacheck.jar
	$(FSC) $<

Json.class: Json.scala scalacheck.jar
	$(FSC) $<

Test.class: Test.scala Bst.class Decorators.class Json.class scalacheck.jar
	$(FSC) $<

test:  Test.class
	$(SCALA) Test

clean:
	rm -rf *.class *.tmp .scala-devel
	$(FSC) -reset
	rm -rf .scala-devel

distclean: clean
	$(FSC) -shutdown
	rm -rf .scala-devel

turnin: 
	zip hw6.zip Json.scala Bst.scala Decorators.scala
	turnin -c cs130f -p hw6 hw6.zip 

zip: 
	zip hw6.zip *.scala *.out Makefile
