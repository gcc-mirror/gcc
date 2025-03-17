$!
$! This file configures the libiberty library for use with openVMS.
$!
$! We do not use the configure script, since we do not have /bin/sh
$! to execute it.
$!
$! Written by Tristan Gingold (gingold@adacore.com)
$!
$!
$!
$ copy config.h-vms config.h
$!
$ write sys$output "Generate libiberty build.com"
$!
$ create build.com
$DECK
$ FILES="getopt,obstack,xexit,xmalloc,hex,getopt1,cplus-dem,cp-demangle,"+-
    "cp-demint,asprintf,vasprintf,mkstemps,concat,getruntime,getpagesize,"+-
    "getpwd,xstrerror,xmemdup,xstrdup,xatexit,choose-temp,fnmatch,objalloc,"+-
    "safe-ctype,hashtab,lbasename,ldirname,argv,lrealpath,make-temp-file,"+-
    "stpcpy,unlink-if-ordinary"
$ OPT="/noopt/debug/warnings=disable=(missingreturn)"
$ CFLAGS=OPT + "/include=([],[-.include])/name=(as_is,shortened)" +-
 "/define=(HAVE_CONFIG_H=1)" +-
 "/prefix=(all,exc=(""getopt"",""optarg"",""optopt"",""optind"",""opterr""))"
$ write sys$output "CFLAGS=",CFLAGS
$ NUM = 0
$ LOOP:
$   F = F$ELEMENT(NUM,",",FILES)
$   IF F.EQS."," THEN GOTO END
$   write sys$output "Compiling ", F, ".c"
$   cc 'CFLAGS 'F.c
$   NUM = NUM + 1
$   GOTO LOOP
$ END:
$ purge
$ lib/create libiberty 'FILES
$EOD
