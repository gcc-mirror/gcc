$ !
$ !	Set up to compile GCC on VAX/VMS
$ !
$! Set the def dir to proper place for use in batch. Works for interactive too.
$flnm = f$enviroment("PROCEDURE")     ! get current procedure name
$set default 'f$parse(flnm,,,"DEVICE")''f$parse(flnm,,,"DIRECTORY")'
$ !
$set symbol/scope=(nolocal,noglobal)
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
$ if f$search("md.") .nes. "" then delete md..*
$ copy [.config.vax]vax.md []md.
$ echo "Linked `md' to `[.config.vax]vax.md'.
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
$edit/tpu/nojournal/nosection/nodisplay/command=sys$input
   PROCEDURE generate_option_file (TAG_NAME, outfile)
        position (beginning_of (newbuffer));
        recursive_fetch_tag (TAG_NAME);
!
! Now fix up a few things in the output buffer
!
	pat_replace ("bytecode "," ");
	pat_replace (".o ",",");
	pat_replace (".o","");  !appear at end of lines.
!
! Remove trailing commas, if present.
!
	position (beginning_of (newbuffer));
	LOOP
	  range1 := search_quietly("," & ((SPAN(" ") & LINE_END) | LINE_END),
				   FORWARD, EXACT);
	  exitif range1 = 0;
	  position (beginning_of (range1));
	  erase(range1);
	  split_line;		
	  ENDLOOP;
! get rid of leading spaces on lines.
        position (beginning_of (current_buffer)) ;
	LOOP
          range1 := search_quietly ( LINE_BEGIN & " ", FORWARD, EXACT) ;
	  EXITIF range1 = 0;
	  position (end_of (range1));
	  erase_character(1);
	ENDLOOP;       
!
! Now write the output file.
!
	SET(OUTPUT_FILE, newbuffer, outfile);
      write_file (newbuffer);
      erase (newbuffer);
   ENDPROCEDURE;

!
! Looks up a tag, copies it to newbuffer, and then translates any $(...)
! definitions that appear.  The translation is put at the current point.
!
   PROCEDURE recursive_fetch_tag (TAG_N);
   fetch_tag (TAG_N);
!
! substitute any  makefile symbols $(...)
!
        position (beginning_of (current_buffer)) ;
	LOOP
	  range1 := search_quietly ("$(" &  
      SPAN("abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ#~0123456789")
				    & ")", FORWARD, EXACT) ;
	  EXITIF range1 = 0;
	  position (beginning_of (range1));
	  move_horizontal(2);
	  mark_1 := MARK (NONE);
	  position (end_of (range1));
	  move_horizontal(-1);
	  mark_2 := MARK (NONE);
	  tag_range := CREATE_RANGE(MARK_1, MARK_2, NONE);
	  position (end_of (range1));
	  tag_string := STR (tag_range);
	  erase (range1);
	  fetch_tag (LINE_BEGIN & tag_string & ((SPAN(" ") & "=") | "="));
          position (beginning_of (current_buffer)) ;
	ENDLOOP;       
   ENDPROCEDURE;

!
! Looks up the translation of a tag, and inserts it at the current location
! in the buffer
!
   PROCEDURE fetch_tag (TAG_N);
      LOCAL mark1, mark2, mark3, range2;
      mark3 := MARK(NONE) ;
      position (beginning_of (mainbuffer)) ;
      range2 := search_quietly (TAG_N, FORWARD, EXACT) ;
      IF (range2 = 0) then 
	position (mark3);
	return;
	endif;
      position (end_of (range2)) ;
      MOVE_HORIZONTAL(1);
      mark1 := MARK(NONE) ;
      position (beginning_of (range2)) ;
      MOVE_VERTICAL(1);
      MOVE_HORIZONTAL(-2);
      LOOP
	EXITIF CURRENT_CHARACTER <> "\" ;
	ERASE_CHARACTER(1);
	MOVE_HORIZONTAL(1);
	MOVE_VERTICAL(1);
	MOVE_HORIZONTAL(-2);
	ENDLOOP;
      MOVE_HORIZONTAL(1);
      mark2 := MARK(NONE) ;
      range2 := CREATE_RANGE(mark1, mark2, NONE) ;
      position (mark3);
      if (length(range2) = 0) then return; endif;
      copy_text(range2);
   ENDPROCEDURE;

   PROCEDURE pat_replace (
      oldstring, !
      newstring)  !
      LOCAL range2;
      position (beginning_of (current_buffer)) ;
      LOOP
         range2 := search_quietly (oldstring, FORWARD, EXACT) ;
         EXITIF range2 = 0 ;
         position (beginning_of (range2)) ;
         erase (range2) ;
         copy_text (newstring) ;
         ENDLOOP ;
   ENDPROCEDURE ;

!                !
! ...fix this... !
!                !
 procedure temporary_cplusplus_hack()
  position(end_of(compiler_list));
  copy_text("cc1plus");
  position(end_of(mainbuffer));
  copy_text("cc1plus: [.cp]call,[.cp]decl,[.cp]errfn,[.cp]expr,[.cp]pt,[.cp]sig\");  split_line;
  copy_text(" [.cp]typeck2,[.cp]class,[.cp]decl2,[.cp]error,[.cp]gc,[.cp]lex\");  split_line;
  copy_text(" [.cp]parse,[.cp]ptree,[.cp]spew,[.cp]typeck,[.cp]cvt,[.cp]edsel\");  split_line;
  copy_text(" [.cp]except,[.cp]init,[.cp]method,[.cp]search,[.cp]tree,[.cp]xref\");  split_line;
  copy_text(" []c-common\");  split_line;
  copy_text(" bc-emit,bc-optab\");  split_line;
  copy_text(" obstack");  split_line;
 endprocedure;


!
! this is the start of the main procedure
   filename := GET_INFO (COMMAND_LINE, 'file_name') ;
   mainbuffer := CREATE_BUFFER ("Makefile.in", "Makefile.in") ;
   newbuffer := CREATE_BUFFER("outfile");
   compiler_list := CREATE_BUFFER("compilers");
!
! Add to this list, as required.  The file "Makefile.in" is searched for a
! tag that looks like "LINE_BEGIN + 'tag + (optional space) + "="".  The
! contents are assumed to be a list of object files, and from this list a
! VMS linker options file is generated.
!
   position (beginning_of (compiler_list));
   recursive_fetch_tag(LINE_BEGIN & "COMPILERS" & ((SPAN(" ") & "=") | "="));
   position (beginning_of (compiler_list));
   LOOP ! kill leading spaces.
	exitif current_character <> " ";
	erase_character(1);
	ENDLOOP;
   position (beginning_of (compiler_list));
      LOOP ! remove any double spaces.
         range1 := search_quietly ("  ", FORWARD, EXACT) ; EXITIF range1 = 0 ;
         position (beginning_of (range1)) ;
         erase_character(1);
         ENDLOOP ;
   position (end_of (compiler_list));
   move_horizontal(-1);
   LOOP ! kill trailing spaces.
	exitif current_character <> " ";
	erase_character(1);
	move_horizontal(-1);
	ENDLOOP;
   position (beginning_of (compiler_list));
      LOOP
         range1 := search_quietly (" ", FORWARD, EXACT) ;
         EXITIF range1 = 0 ;
         position (beginning_of (range1)) ;
         erase (range1) ;
	 split_line;
         ENDLOOP ;
!                         !
! This needs to be fixed. !
!                         !
	temporary_cplusplus_hack();
!
! We now have a list of supported compilers.  Now write it, and use it.
!
	SET(OUTPUT_FILE, compiler_list, "compilers.list");
      write_file (compiler_list);
   generate_option_file(LINE_BEGIN & "OBJS" & ((SPAN(" ") & "=") | "="),
			"independent.opt");
   generate_option_file(LINE_BEGIN & "LIB2FUNCS" & ((SPAN(" ") & "=") | "="),
			"libgcc2.list");
   generate_option_file(LINE_BEGIN & "BC_ALL" & ((SPAN(" ") & "=") | "="),
			"bc_all.opt");
   generate_option_file(LINE_BEGIN & "BI_OBJ" & ((SPAN(" ") & "=") | "="),
			"bi_all.opt");
!
! Now change OBJS in the Makefile, so each language specific options file 
! does not pick up all of the language independent files.
!
   position (beginning_of (mainbuffer));
   range1 := search_quietly (LINE_BEGIN & "OBJS" & ((SPAN(" ") & "=") | "="),
			     FORWARD, EXACT) ;
   position (end_of (range1));
   split_line;
   position (beginning_of (compiler_list));
   LOOP
     cmark := mark(NONE);
     exitif cmark = end_of(compiler_list);
     message(current_line);
     generate_option_file(LINE_BEGIN & current_line & ((SPAN(" ") & ":") | ":"),
			  current_line+"-objs.opt");
     position (cmark);
     move_vertical(1);
   ENDLOOP ;
   quit ;
$ echo ""
$!
$! Remove excessive versions of the option files...
$!
$ purge *.opt
$ purge compilers.list,libgcc2.list
$!
$!
$!
$ if f$search("config.status") .nes. "" then delete config.status.*
$ open/write file config.status
$ write file "Links are now set up for use with a vax running VMS."
$ close file
$ type config.status
$ echo ""
