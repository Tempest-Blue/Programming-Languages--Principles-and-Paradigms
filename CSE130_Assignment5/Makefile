FSC = fsc -Djava.io.tmpdir=.scala-devel
SCALA = scala -cp .

all: misc words crack test

misc: 
	$(FSC) Misc.scala

words: misc
	$(FSC) Words.scala

crack: words
	javac Crypt.java
	$(FSC) Crypt.java
	$(FSC) Crack.scala

test: misc words crack
	$(FSC) Test.scala
	$(SCALA) Test

clean:
	rm -f *.class
	$(FSC) -reset

distclean: clean
	$(FSC) -shutdown

turnin: clean
	zip pa5.zip Words.scala Crack.scala
	turnin -c cs130s -p pa5 pa5.zip

zip: clean
	zip pa5.zip *scala *java Makefile words passwd
