$ !
$ !	Set up to compile GCC on VMS
$ !
$ echo = "write sys$output"
$ !
$ if f$search("config.h") .nes. "" then delete config.h.*
$ copy [.config]xm-vms.h []config.h
$ echo "Linked `config.h' to `[.config]xm-vms.h'.
$ !
$ if f$search("tm.h") .nes. "" then delete tm.h.*
$ copy [.config]vms.h []tm.h
$ echo "Linked `tm.h' to `[.config]vms.h'.
$ !
$ if f$search("md.") .nes. "" then delete md..*
$ copy [.config]vax.md []md.
$ echo "Linked `md' to `[.config]vax.md'.
$ !
$ if f$search("aux-output.c") .nes. "" then delete aux-output.c.*
$ copy [.config]vax.c []aux-output.c
$ echo "Linked `aux-output.c' to `[.config]vax.c'.
$ !
$!
$!
$! Create the file version.opt, which helps identify the executable.
$!
$search version.c version_string,"="/match=and/output=t.tmp
$open ifile$ t.tmp
$read ifile$ line
$close ifile$
$delete/nolog t.tmp;
$ijk=f$locate("""",line)+1
$line=f$extract(ijk,f$length(line)-ijk,line)
$ijk=f$locate("""",line)
$line=f$extract(0,ijk,line)
$ijk=f$locate("\n",line)
$line=f$extract(0,ijk,line)
$!
$i=0
$loop:
$elm=f$element(i," ",line)
$if elm.eqs."" then goto no_ident
$if (elm.les."9").and.(elm.ges."0") then goto write_ident
$i=i+1
$goto loop
$!
$no_ident:
$elm="?.??"
$!
$!
$write_ident:
$open ifile$ version.opt/write
$write ifile$ "ident="+""""+elm+""""
$close ifile$
$pur version.opt/nolog
$!
$!
$ if f$search("config.status") .nes. "" then delete config.status.*
$ open/write file config.status
$ write file "Links are now set up for use with a vax running VMS."
$ close file
$ type config.status
