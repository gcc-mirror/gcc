
MAKEINFO=makeinfo
INC=../../../gcc/doc/include

all: faq/index.txt 17_intro/porting.html


faq/index.txt: faq/index.html
	lynx -dump $< | sed "s%file://localhost`pwd`%..%" > $@

17_intro/porting.html: 17_intro/porting.texi
	${MAKEINFO} -I ${INC} --html --no-split $< -o $@

