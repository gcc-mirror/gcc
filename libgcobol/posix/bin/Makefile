#
# Demonstrate how to generate a new COBOL binding from a man page. 
#

posix-mkdir.cbl:
	man 2 mkdir | ./scrape.awk | \
	../udf-gen -D mode_t=unsigned\ long  > $@~
	@mv $@~ $@

# ... or 

posix-stat-many.scr:
	man 2 stat | col -b | ./scrape.awk > $@~
	@mv $@~ $@

.scr.cbl:
	./udf-gen -D mode_t=unsigned\ long  $^ > $@~
	@mv $@~ $@
