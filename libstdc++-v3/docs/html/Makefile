
MAKEINFO=makeinfo
INC=../../../gcc/doc/include

all: faq/index.txt 17_intro/porting.html 17_intro/porting-howto.html


faq/index.txt: faq/index.html
	lynx -dump $< | sed "s%file://localhost`pwd`%..%" > $@

17_intro/porting.html: 17_intro/porting.texi
	${MAKEINFO} -I ${INC} --html --no-split $< -o $@

# known to work under RH; this can be cleaned up later if needed
17_intro/porting-howto.html: 17_intro/porting-howto.xml
	xltproc -o $@ /usr/share/xml/docbook/xsl-stylesheets-1.48-2/html/docbook.xsl $<

