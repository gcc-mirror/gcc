$ !
$ !	Set up to compile GCC on VAX/VMS
$ !
$! Set the def dir to proper place for use in batch. Works for interactive too.
$flnm = f$enviroment("PROCEDURE")     ! get current procedure name
$set default 'f$parse(flnm,,,"DEVICE")''f$parse(flnm,,,"DIRECTORY")'
$ !
$set symbol/scope=(nolocal,noglobal)
$if f$trnlnm("IFILE$").nes."" then close/noLog ifile$
$ !
$ echo = "write sys$output"
$ !
$ if f$search("config.h") .nes. "" then delete config.h.*
$ copy [.config.vax]xm-vms.h []config.h
$ echo "Linked `config.h' to `[.config.vax]xm-vms.h'.
$ !
$ if f$search("tconfig.h") .nes. "" then delete tconfig.h.*
$ create []tconfig.h
$DECK
/* tconfig.h == config.h :: target and host configurations are the same */
#include "config.h"
$EOD
$ echo "Created `tconfig.h'.
$ !
$ if f$search("hconfig.h") .nes. "" then delete hconfig.h.*
$ create []hconfig.h
$DECK
/* hconfig.h == config.h :: host and target configurations are the same */
#include "config.h"
$EOD
$ echo "Created `hconfig.h'.
$ !
$ if f$search("tm.h") .nes. "" then delete tm.h.*
$!! copy [.config.vax]vms.h []tm.h
$ edit/tpu/nojournal/nosection/nodisplay/command=sys$input -
        [.config.vax]vms.h /output=[]tm.h
$DECK
!
!  Copy file, changing lines of the form
!	#include "vax/*"
!  into
!	#include "config-*"
!
   file := CREATE_BUFFER("file", GET_INFO(COMMAND_LINE, "file_name"));
   targ := LINE_BEGIN & '#include' & SPAN(ASCII(32)+ASCII(9)) & '"vax/';
   rang := CREATE_RANGE(BEGINNING_OF(file), END_OF(file));
   LOOP
      incl := SEARCH_QUIETLY(targ, FORWARD, EXACT, rang);
      EXITIF incl = 0;
      POSITION(BEGINNING_OF(incl));
      ERASE(incl);
      COPY_TEXT('#include "config-');
      rang := CREATE_RANGE(END_OF(incl), END_OF(file));
   ENDLOOP;
   WRITE_FILE(file, GET_INFO(COMMAND_LINE, "output_file"));
   QUIT
$EOD
$ echo "Generated `tm.h' from `[.config.vax]vms.h'.
$ !
$	!crude hack to allow compiling from [.cp] subdirectory
$ if f$search("config-vax.h") .nes. "" then delete config-vax.h;*
$ copy [.config.vax]vax.h []config-vax.h
$ echo "Linked `config-vax.h' to `[.config.vax]vax.h' for `tm.h'."
$ !
$ call make_lang_incl "options.h"
$ !
$ call make_lang_incl "specs.h"
$ !
$ if f$search("vax.md") .nes. "" then delete vax.md;*
$ copy [.config.vax]vax.md []vax.md
$ echo "Copied `vax.md' from `[.config.vax]vax.md'."
$ !
$ if f$search("aux-output.c") .nes. "" then delete aux-output.c.*
$ copy [.config.vax]vax.c []aux-output.c
$ echo "Linked `aux-output.c' to `[.config.vax]vax.c'.
$ !
$!
$!
$! Create the file version.opt, which helps identify the executable.
$!
$search version.c version_string,"="/match=and/output=t.tmp
$open ifile$ t.tmp
$read ifile$ line
$close ifile$
$delete t.tmp;
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
$purge version.opt
$!
$!
$! create linker options files that lists all of the components for all
$! possible compilers.  We do this by editing the file Makefile.in, and 
$! generating the relevant files from it.
$!
$!
$! Make a copy of the makefile if the sources are on a disk that is NFS 
$!    mounted on a unix machine.
$if f$search("Makefile.in").eqs."" .and. f$search("$M$akefile.in").nes."" -
	then copy $M$akefile.in Makefile.in
$! This should be automated across all front-end subdirectories.
$!    For now, it's hardcoded.
$if f$search("[.cp]Makefile.in").eqs."" .and. f$search("[.cp]$M$akefile.in").nes."" -
	then copy [.cp]$M$akefile.in [.cp]Makefile.in
$!
$!
$echo "Now processing Makefile.in to generate linker option files."
$edit/TPU/noJournal/noSection/noDisplay/Command=sys$input: Makefile.in
!!
VARIABLE makefile_buf, opt_file_buf, complist_buf, extra_compilers; ! Globals.

!!
PROCEDURE process_makefile( )
  !
  ! Interpret Makefile.in and subsidiary Make-lang.in templates.
  !
  LOCAL range1, cmark, makefilename;

  makefilename	  := GET_INFO (COMMAND_LINE, 'FILE_NAME'); ! "Makefile.in"
  makefile_buf	  := CREATE_BUFFER ("makefile", makefilename);
  opt_file_buf	  := CREATE_BUFFER ("opt_file");
  complist_buf	  := CREATE_BUFFER ("complist");
  extra_compilers := CREATE_ARRAY;
  !
  SET (NO_WRITE, makefile_buf, ON);	! Used as workspace; don't save it.
  SET (OUTPUT_FILE, complist_buf, "compilers.list");
  !
  ! Make some textual substitutions.
  !
  configure_makefile ();
  !
  ! Collect a list of supported compilers (``COMPILERS=xxx'' macro).
  !
  identify_compilers ();
  !
  ! Plus other known compilers described by Make-lang.in makefile fragments.
  ! Add new entries as needed; args are (target name, subdirectory name).
  !
  additional_compiler ("cc1plus", "cp");
  !
  WRITE_FILE (complist_buf);		! Now save "compilers.list".
  !
  ! Add to this list, as required.  The file "Makefile.in" is searched for
  ! a tag that looks like "LINE_BEGIN + 'tag + (optional space) + "="".
  ! The contents are assumed to be a list of object files, and from this
  ! list a VMS linker options file is generated.
  !
  generate_option_file ("OBJS",      "=", "independent.opt");
  generate_option_file ("LIB2FUNCS", "=", "libgcc2.list");
  generate_option_file ("BC_ALL",    "=", "bc_all.list");
  generate_option_file ("BI_OBJ",    "=", "bi_all.opt");
  !
  ! Now change OBJS in the Makefile, so each language specific options file
  ! does not pick up all of the language independent files.
  !
  POSITION (BEGINNING_OF (makefile_buf));
  COPY_TEXT ("OBJS=");	! New copy with empty value, seen before real OBJS.
  SPLIT_LINE;
  !
  ! Lastly, process each compiler-specific object dependency list.
  !
  POSITION (BEGINNING_OF (complist_buf));
  LOOP
    cmark := MARK (NONE);
    EXITIF (cmark = END_OF (complist_buf));
    ! The current line contains the name of a compiler target, such as "cc1".
    MESSAGE (CURRENT_LINE);	! Give some interactive feedback.
    generate_option_file (CURRENT_LINE, ":", CURRENT_LINE + "-objs.opt");
    POSITION (cmark);
    MOVE_VERTICAL (1);		! Go to the next line.
  ENDLOOP;
ENDPROCEDURE; !process_makefile
!!

PROCEDURE process_objc_lib( )
  !
  ! Interpret objc/Makefile, after finishing the top makefile.
  !
  ON_ERROR
    [TPU$_OPENIN]:
      MESSAGE ("Cannot load objc/Makefile for ""ObjClib""; skipping it.");
      RETURN;
  ENDON_ERROR;

  ERASE (makefile_buf);			!discard top Makefile
  POSITION (END_OF (makefile_buf));
  READ_FILE ("[.objc]Makefile");	!load objc one
  MESSAGE ("objclib");
  pat_replace (ASCII(9), " ");		!change any <tab> to <space>
  generate_option_file ("OBJC_O", "=", "objc-objs.opt");
  POSITION (BEGINNING_OF (makefile_buf));
  ! Join any continuation lines; we want the header list to be one line.
  pat_replace ("\" & LINE_END, );
  generate_option_file ("OBJC_H", "=", "objc-hdrs.list");
ENDPROCEDURE; !process_objc_lib
!!

PROCEDURE configure_makefile( )
  !
  ! Plug in some values normally handled by `configure'.  Rather than
  ! replacing the dummy entries, insert the real entries before them.
  !
  POSITION (BEGINNING_OF (makefile_buf));
  COPY_TEXT ("target=vax-vms");			SPLIT_LINE;
  COPY_TEXT ("out_file=aux-output.c");		SPLIT_LINE;	! vax/vax.c
  COPY_TEXT ("out_object_file=aux-output.o");	SPLIT_LINE;	! aux-output.obj
  COPY_TEXT ("md_file=vax.md");			SPLIT_LINE;	! vax/vax.md
  COPY_TEXT ("tm_file=tm.h");			SPLIT_LINE;	! vax/tm-vms.h
ENDPROCEDURE; !configure_makefile
!!

PROCEDURE identify_compilers( )
  !
  ! Retrieve the list of supported compilers from Makefile.in, and put them
  ! into file "compilers.list", one per line, for subsequent access from DCL.
  !
  LOCAL range1;

  ! Strip most comments from the makefile, to speed up subsequent processing.
  POSITION (BEGINNING_OF (makefile_buf));
  pat_replace (LINE_BEGIN & "#" & REMAIN & LINE_END, );
!#  ! Convert directory references to VMS syntax (actually, just strip it).
!#  pat_replace (" $(srcdir)/", " ");
  ! Look up the ``COMPILERS=cc1 xyzzy'' Makefile macro and put
  ! its ``cc1 xyzzy'' value into the compilers buffer.
  POSITION (BEGINNING_OF (complist_buf));
!#--at some point we may want to add this--
!#  recursive_fetch_tag ("CCCP", "=");	  ! Include the preprocessor.
!#  POSITION (END_OF (complist_buf));
  recursive_fetch_tag ("COMPILERS", "=");
  ! Convert all spaces into newlines, then remove any blank lines.
  pat_replace (SPAN(" "), LINE_END);
  pat_replace (LINE_BEGIN & LINE_END, );
ENDPROCEDURE; !identify_compilers
!!

PROCEDURE additional_compiler( cname, subdir )
  !
  ! Load Make-lang.in for compiler CNAME from SUBDIR and append it to the
  ! end of Makefile.in's buffer.  Add CNAME to the "compilers.list" buffer.
  !
  ON_ERROR
    ! Don't abort if user removes the supporting subdirectory for a
    ! language she's not interested in.
    [TPU$_OPENIN]:
      MESSAGE ("Cannot load " + subdir + "/Make-lang.in for "
	       + '"' + cname + '"' + "; skipping it.");
      RETURN;
  ENDON_ERROR;

  POSITION (END_OF (makefile_buf));
  SPLIT_LINE;	! Separate with a blank line.
  READ_FILE ("[." + subdir + "]Make-lang.in");	! Load Makefile fragment.
  ! Make sure that $(xxx_OTH_SRCS) expands to empty string by renaming $(it)
  pat_replace ("_OTH_SRCS)", "_OTH_SRCS_dummy_)");
  ! Convert subdirectory references into VMS syntax.
  pat_replace ("$(srcdir)/" + subdir + "/", "[." + subdir + "]");
  ! Add this name to compilers.list.
  POSITION (END_OF (complist_buf));
  COPY_TEXT (cname);
  ! Make array entry indexed by compiler's file name; its value is arbitrary.
  extra_compilers{cname} := subdir;
ENDPROCEDURE; !additional_compiler
!!

PROCEDURE generate_option_file( tag_name, punct, outfile_name )
  !
  ! Produce a file listing the names of particular object files, for use
  ! as input to the linker and also for use in finding source names by
  ! make-cc1.com.  Generally, any name suffix will be suppressed.
  !
  LOCAL range1, range2;

  POSITION (BEGINNING_OF (opt_file_buf));
  recursive_fetch_tag (tag_name, punct);
  ! First fix up for subdirectory/Make-lang.in.
  IF (pat_replace ("stamp-objlist" & (SPAN(" ")|LINE_END), " ") > 0) THEN
    recursive_fetch_tag ("stamp-objlist", ":");
  ENDIF;
  ! Now fix up a few things in the output buffer.
  pat_replace (("bytecode"|"Makefile") & (SPAN(" ")|LINE_END), " ");
!#  FILL (CURRENT_BUFFER, " ", 1, 80, 0);	! Condense things a bit.
  pat_replace ("." & ("o"|"c"|"y") & ((SPAN(" ")&LINE_END)|LINE_END), LINE_END);
  pat_replace ("." & ("o"|"c"|"y") & SPAN(" "), ",");
  pat_replace (".h" & (SPAN(" ")|LINE_END), ".h,");
  ! Remove trailing commas, if present.
  pat_replace ("," & ((SPAN(" ")&LINE_END)|LINE_END), LINE_END);
  ! Get rid of spaces and blank lines.
  pat_replace (SPAN(" "), LINE_END);
  pat_replace (LINE_BEGIN & LINE_END, );
  ! Second fix up for subdirectory/Make-lang.in;
  ! avoid "sticky defaults" when linker processes the resulting options file.
  IF (extra_compilers{outfile_name - "-objs.opt"} <> TPU$K_UNSPECIFIED) THEN
    POSITION (BEGINNING_OF (opt_file_buf));
    range1 := CREATE_RANGE (MARK (NONE), END_OF (CURRENT_BUFFER), NONE);
    LOOP
      range2 := SEARCH_QUIETLY (LINE_BEGIN | ",", FORWARD, EXACT, range1);
      EXITIF (range2 = 0);
      POSITION (BEGINNING_OF (range2));
      IF (CURRENT_CHARACTER = ",") THEN  MOVE_HORIZONTAL (1); ENDIF;
      ! If it's not already "[.subdir]name", explicitly make it "[]name".
      IF (CURRENT_CHARACTER <> "[") THEN  COPY_TEXT ("[]"); ENDIF;
      MOVE_HORIZONTAL (1);
      MODIFY_RANGE (range1, MARK (NONE), END_OF (range1));
    ENDLOOP;
  ENDIF;
  ! Now write the output file.
  SET (OUTPUT_FILE, opt_file_buf, outfile_name);
  WRITE_FILE (opt_file_buf);
  ERASE (opt_file_buf);		! Clear buffer out for next opt_file pass.
ENDPROCEDURE; !generate_option_file
!!

PROCEDURE recursive_fetch_tag( tag_n, punct )
  !
  ! Look up TAG_N, copy it to OPT_FILE_BUF, and then translate any $(...)
  ! definitions that appear.  The translation is put at the current point.
  !
  LOCAL mark1, mark2, range1, tag_range, tag_string;

  fetch_tag (tag_n, punct);
  ! Substitute any makefile symbols $(...).
  POSITION (BEGINNING_OF (CURRENT_BUFFER));
  LOOP
    range1 := SEARCH_QUIETLY ("$(" &
      SPAN("abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ#~0123456789")
				   & ")", FORWARD, EXACT);
    EXITIF (range1 = 0);
    POSITION (BEGINNING_OF (range1));
    MOVE_HORIZONTAL (2);	! Past opening "$(".
    mark1 := MARK (NONE);
    POSITION (END_OF (range1));
    MOVE_HORIZONTAL (-1);	! In front of closing ")".
    mark2 := MARK (NONE);
    tag_range := CREATE_RANGE (mark1, mark2, NONE);
    POSITION (END_OF (range1));
    tag_string := STR (tag_range);
    ERASE (range1);
    fetch_tag (tag_string, "=");
    POSITION (BEGINNING_OF (CURRENT_BUFFER));
  ENDLOOP;
ENDPROCEDURE; !recursive_fetch_tag
!!

PROCEDURE fetch_tag( tag_n, punct )
  !
  ! Looks up the translation of a tag, and inserts it at the current location
  ! in the buffer.
  !
  LOCAL mark0, mark1, mark2, range2;

  mark0 := MARK (NONE);     ! Remember where we started; restore before return.
  POSITION (BEGINNING_OF (makefile_buf));
  ! The tag definition always starts in the first column, and might have
  ! optional space(es) before "=" or ":" punctuation.
  range2 := SEARCH_QUIETLY (LINE_BEGIN & tag_n & ((SPAN(" ") & punct) | punct),
			    FORWARD, EXACT);
  IF (range2 = 0) THEN
    POSITION (mark0);
    RETURN;
  ENDIF;
  POSITION (END_OF (range2));
  MOVE_HORIZONTAL (1);		! Move beyond "TAG=".
  mark1 := MARK (NONE);
  POSITION (BEGINNING_OF (range2));
  LOOP
    MOVE_VERTICAL (1);
    MOVE_HORIZONTAL (-2);
    EXITIF (CURRENT_CHARACTER <> "\");
    ERASE_CHARACTER (1);
    MOVE_HORIZONTAL (1);
  ENDLOOP;
  MOVE_HORIZONTAL (1);
  mark2 := MARK (NONE);
  range2 := CREATE_RANGE (mark1, mark2, NONE);
  POSITION (mark0);
  IF (LENGTH (range2) <> 0) THEN
    COPY_TEXT (range2);
  ENDIF;
ENDPROCEDURE; !fetch_tag
!!

PROCEDURE pat_replace( oldstring, newstring )
  !
  ! Replace all occurrences of a pattern.
  !
  LOCAL range1, range2, kill_it, count;

  count := 0;
  kill_it := (GET_INFO (newstring, 'TYPE') = UNSPECIFIED);	! Omitted arg.
  range1 := CREATE_RANGE (BEGINNING_OF (CURRENT_BUFFER),
			  END_OF (CURRENT_BUFFER), NONE);
  LOOP
    range2 := SEARCH_QUIETLY (oldstring, FORWARD, EXACT, range1);
    EXITIF (range2 = 0);
    count := count + 1;
    POSITION (BEGINNING_OF (range2));
    ERASE (range2);
    IF (newstring = LINE_END) THEN
      SPLIT_LINE;
    ELSE IF (NOT kill_it) THEN
      COPY_TEXT (newstring);
    ENDIF; ENDIF;
    MODIFY_RANGE (range1, MARK (NONE), END_OF (range1));
  ENDLOOP;
  RETURN count;
ENDPROCEDURE; !pat_replace
!!

!
! This is the main routine.
!
process_makefile ();
process_objc_lib ();	!this uses a different makefile
QUIT;	! All done; don't write any modified buffers.
!!
$ echo ""
$!
$! Remove excessive versions of the option files...
$!
$ purge *.opt,*.list
$!
$!
$!
$ if f$search("config.status") .nes. "" then delete config.status.*
$ create config.status
Links are now set up for use with a vax running VMS.
$ type config.status
$ echo ""
$!
$ exit
$
$!
$! Construct a header file based on subdirectory contents
$!
$make_lang_incl: subroutine
$  if f$search(p1).nes."" then delete 'p1';*
$  create 'p1'	!empty file with ordinary text-file attributes
$  open/Append ifile$ 'p1'
$  write ifile$ "/* ''p1' */"
$  hfile = f$search("[]''p1'")
$  topdir = f$parse(hfile,,,"DIRECTORY") - "]"
$lang_incl_loop:
$  hfile = f$search("[.*]lang-''p1'")
$  if hfile.eqs."" then goto lang_incl_done
$  dir = f$parse(hfile,,,"DIRECTORY") - "]"
$! convert absolute path to relative one, yielding "[.subdir]"
$  dir = "[" + f$edit(dir - topdir,"LOWERCASE") + "]"
$  write ifile$ "#include ""''dir'lang-''p1'"""
$  goto lang_incl_loop
$lang_incl_done:
$  close ifile$
$  echo "Created `''p1''."
$ endsubroutine !make_lang_incl
