#!/usr/bin/tclsh

# Helper to enforce array-ness.
proc makearray {name} {
  upvar $name ary
  set ary(_) 1
  unset ary(_)
}

global is_verbose
set is_verbose 0

# Verbose printer.
proc verbose {text} {
  global is_verbose
  if {$is_verbose} {
    puts stderr $text
  }
}

# This maps a name to its style:
# * bc    objects in this package and all its sub-packages
#         are to be compiled with the BC ABI.  It is an error
#         for sub-packages to also appear in the map.
# * bcheaders 
#         as bc, but generate header files and compile with CNI.
# * package
#         objects in this package (and possibly sub-packages,
#         if they do not appear in the map) will be compiled en masse
#         from source into a single object, using the C++ ABI.
# * ordinary
#         objects in this package (and possibly sub-packages
#         if they do not appear in the map) will be compiled one at
#         a time into separate .o files.
# * ignore
#         objects in this package are not used.  Note however that
#         most ignored files are actually handled by listing them in
#         'standard.omit'
# * interpreter
#         objects in this package (and possibly sub-packages,
#         if they do not appear in the map) are only compiled if
#         the interpreter is enabled.  They are compiled as with the
#         'package' specifier.
#
# If a package does not appear in the map, the default is 'package'.
global package_map
set package_map(.) package

# These are ignored in Classpath.
set package_map(gnu/test) ignore
set package_map(gnu/javax/swing/plaf/gtk) ignore
set package_map(gnu/gcj/tools/gc_analyze) ignore

set package_map(gnu/java/awt/peer/swing) bc

set package_map(gnu/xml/aelfred2) bc
set package_map(gnu/xml/dom) bc
set package_map(gnu/xml/libxmlj) bc
set package_map(gnu/xml/pipeline) bc
set package_map(gnu/xml/stream) bc
set package_map(gnu/xml/transform) bc
set package_map(gnu/xml/util) bc
set package_map(gnu/xml/validation) bc
set package_map(gnu/xml/xpath) bc
set package_map(javax/imageio) bc
set package_map(javax/xml) bc
set package_map(gnu/java/beans) bc
set package_map(gnu/java/awt/dnd/peer/gtk) bc
set package_map(gnu/java/util/prefs/gconf) bc
set package_map(gnu/java/awt/peer/gtk) bc
set package_map(gnu/java/awt/dnd/peer/gtk) bc
set package_map(gnu/java/awt/peer/qt) bc
set package_map(gnu/java/awt/peer/x) bc
set package_map(gnu/java/util/prefs/gconf) bc
set package_map(gnu/javax/sound/midi) bc
set package_map(gnu/javax/sound/sampled/gstreamer) ignore
set package_map(org/xml) bc
set package_map(org/w3c) bc
set package_map(org/relaxng) bc
set package_map(javax/rmi) bc
set package_map(org/omg) bc
set package_map(gnu/CORBA) bc
set package_map(gnu/javax/rmi) bc
set package_map(gnu/java/lang/management) bcheaders
set package_map(java/lang/management) bc
set package_map(gnu/classpath/management) bc
set package_map(gnu/javax/management) bc

# parser/HTML_401F.class is really big, and there have been complaints
# about this package requiring too much memory to build.  So, we
# compile it as separate objects.  But, we're careful to compile the
# sub-packages as packages.
set package_map(gnu/javax/swing/text/html/parser) ordinary
set package_map(gnu/javax/swing/text/html/parser/models) package
set package_map(gnu/javax/swing/text/html/parser/support) package

# More special cases.  These end up in their own library.
# Note that if we BC-compile AWT we must update these as well.
set package_map(gnu/gcj/xlib) package
set package_map(gnu/awt/xlib) package

# These packages should only be included if the interpreter is
# enabled.
set package_map(gnu/classpath/jdwp) interpreter
set package_map(gnu/classpath/jdwp/event) interpreter
set package_map(gnu/classpath/jdwp/event/filters) interpreter
set package_map(gnu/classpath/jdwp/exception) interpreter
set package_map(gnu/classpath/jdwp/id) interpreter
set package_map(gnu/classpath/jdwp/processor) interpreter
set package_map(gnu/classpath/jdwp/transport) interpreter
set package_map(gnu/classpath/jdwp/util) interpreter
set package_map(gnu/classpath/jdwp/value) interpreter
set package_map(gnu/gcj/jvmti) interpreter

# Some BC ABI packages have classes which must not be compiled BC.
# This maps such packages to a grep expression for excluding such
# classes.
global exclusion_map
makearray exclusion_map
# set exclusion_map(java/awt) AWTPermission

# This maps a package name to a list of corresponding .java file base
# names.  The package name will either appear as a key in package_map,
# or it will be '.' for the default.
global name_map
makearray name_map

# This maps a java file base name, like 'java/lang/Object.java', to
# the source directory in which it resides.  We keep a layer of
# indirection here so that we can override sources in Classpath with
# our own sources.
global dir_map
makearray dir_map

# An entry in this map means that all .properties files in the
# corresponding directory should be ignored.
global properties_map
makearray properties_map

# logging.properties is installed and is editable.
set properties_map(java/util/logging) _
# We haven't merged locale resources yet.
set properties_map(gnu/java/locale) _

# We want to be able to load xerces if it is on the class path.  So,
# we have to avoid compiling in the XML-related service files.
set properties_map(META-INF/services/javax.xml.parsers.DocumentBuilderFactory) _
set properties_map(META-INF/services/javax.xml.parsers.SAXParserFactory) _
set properties_map(META-INF/services/javax.xml.parsers.TransformerFactory) _
set properties_map(META-INF/services/org.relaxng.datatype.DatatypeLibraryFactory) _
set properties_map(META-INF/services/org.w3c.dom.DOMImplementationSourceList) _
set properties_map(META-INF/services/org.xml.sax.driver) _
set properties_map(META-INF/services/javax.sound.sampled.spi.AudioFileReader.in) ignore
set properties_map(META-INF/services/javax.sound.sampled.spi.MixerProvider) ignore
set properties_map(META-INF/services/javax.sound.sampled.spi.MixerProvider.in) ignore

# List of all properties files.
set properties_files {}

# List of all '@' files that we are going to compile.
set package_files {}

# List of all '@' files that we are going to compile if the
# interpreter is enabled.
set interpreter_package_files {}

# List of all header file variables.
set header_vars {}

# List of all header file variables for interpreter packages.
set interpreter_header_vars {}

# List of all BC object files.
set bc_objects {}

# List of regexps for matching ignored files.
set ignore_rx_list {}


# Return true if a given file should be ignored.
# The argument is the path name including the package part.
proc ignore_file_p {file} {
  global ignore_rx_list
  foreach rx $ignore_rx_list {
    if {[regexp -- $rx $file]} {
      verbose "ignoring $file for $rx"
      return 1
    }
  }
  return 0
}

# Read a '.omit' file and update the internal data structures.
proc read_omit_file {name} {
  global ignore_rx_list
  set fd [open $name r]
  while {! [eof $fd]} {
    set line [gets $fd]

    # Classpath's entries bogusly start with "../".
    if {[string match ../* $line]} {
      set line [string range $line 3 end]
    }

    if {$line != ""} {
      lappend ignore_rx_list $line
    }
  }
  close $fd
}

# Classify a single source file.
proc classify_source_file {basedir file} {
  global package_map name_map dir_map

  if {[ignore_file_p $file]} {
    return
  }

  set seen [info exists dir_map($file)]
  set dir_map($file) $basedir
  set pkg $file
  while {1} {
    if {[info exists package_map($pkg)]} {
      # If the entry is 'package', then set up a new entry for the
      # file's package.
      if {$package_map($pkg) == "package"} {
	set pkg [file dirname $file]
	set package_map($pkg) package
      }
      verbose "classify succeeded: $file -> $pkg"
      if {! $seen} {
	lappend name_map($pkg) $file
      }
      return
    }
    set pkg [file dirname $pkg]
  }
  error "can't happen"
}

# Scan a directory and its subdirectories for .java source files or
# .properties files.  Note that we keep basedir and subdir separate so
# we can properly update our global data structures.
proc scan_directory {basedir subdir} {
  global dir_map properties_map properties_files

  set subdirs {}
  set files {}
  set here [pwd]
  cd $basedir/$subdir
  foreach file [lsort [glob -nocomplain *]] {
    if {[string match *.java $file]} {
      lappend files $subdir/$file
    } elseif {[string match *.properties $file]} {
      if {! [info exists properties_map($subdir)]} {
	# We assume there aren't any overrides.
	lappend properties_files $basedir/$subdir/$file
      }
    } elseif {[string match *.css $file]} {
	# Special case for default.css needed by javax.swing.text.html.
	lappend properties_files $basedir/$subdir/$file
    } elseif {[file isdirectory $file]} {
      lappend subdirs $subdir/$file
    } elseif {$subdir == "META-INF/services"} {
      # Service files are generally included as properties.
      if {! [info exists properties_map($subdir/$file)]} {
	lappend properties_files $basedir/$subdir/$file
      }
    }
  }
  cd $here

  # Recurse first, so that we don't create new packages too eagerly.
  foreach dir $subdirs {
    scan_directory $basedir $dir
  }

  foreach file $files {
    classify_source_file $basedir $file
  }
}

# Scan known packages beneath the base directory for .java source
# files.
proc scan_packages {basedir} {
  foreach subdir {gnu java javax org sun com META-INF} {
    if {[file exists $basedir/$subdir]} {
      scan_directory $basedir $subdir
    }
  }
}

# Emit a rule for a 'bc' package.
proc emit_bc_rule {package} {
  global package_map exclusion_map bc_objects

  if {$package == "."} {
    set pkgname ordinary
  } else {
    set pkgname $package
  }
  set varname [join [split $pkgname /] _]_source_files
  set loname [join [split $pkgname /] -].lo
  set tname [join [split $pkgname /] -].list

  puts "$loname: \$($varname)"
  # Create a temporary list file and then compile it.  This works
  # around the libtool problem mentioned in PR 21058.  classpath was
  # built first, so the class files are to be found there.
  set omit ""
  if {[info exists exclusion_map($package)]} {
    set omit "| grep -v $exclusion_map($package)"
  }
  puts  "\t@find \$(srcdir)/classpath/lib/$package -name '*.class'${omit} > $tname"
  puts -nonewline "\t\$(LTGCJCOMPILE) -fsource-filename=\$(here)/classpath/lib/classes "
  if {$package_map($package) == "bc"} {
    puts -nonewline "-fjni "
  }
  # Unless bc is disabled with --disable-libgcj-bc, $(LIBGCJ_BC_FLAGS) is:
  #   -findirect-dispatch -fno-indirect-classes
  puts "\$(LIBGCJ_BC_FLAGS) -c -o $loname @$tname"
  puts "\t@rm -f $tname"
  puts ""

  # We skip these because they are built into their own libraries and
  # are handled specially in Makefile.am.
  if {$loname != "gnu-java-awt-peer-qt.lo" && $loname != "gnu-java-awt-peer-x.lo"} {
    lappend bc_objects $loname
  }
}

# Emit a rule for a 'package' package.
proc emit_package_rule_to_list {package package_files_list} {
  global package_map exclusion_map $package_files_list

  if {$package == "."} {
    set pkgname ordinary
  } else {
    set pkgname $package
  }
  set varname [join [split $pkgname /] _]_source_files
  set base $pkgname
  set lname $base.list
  set dname $base.deps

  if {$pkgname == "java/lang"} {
    # Object and Class are special cases due to an apparent compiler
    # bug.  Process is a special case because we don't build all
    # concrete implementations of Process on all platforms.
    set omit "| tr ' ' '\\n' | fgrep -v Object.class | fgrep -v Class.class | egrep -v '\(Ecos\|Posix\|Win32\)Process' "
  } else {
    set omit ""
  }

  # A rule to make the phony file we are going to compile.
  puts "$lname: \$($varname)"
  puts "\t@\$(mkinstalldirs) \$(dir \$@)"
  puts "\techo \$(srcdir)/classpath/lib/$package/*.class $omit> $lname"
  puts ""
  puts "-include $dname"
  puts ""
  puts ""

  if {$pkgname != "gnu/gcj/xlib" && $pkgname != "gnu/awt/xlib"
      && $pkgname != "gnu/gcj/tools/gcj_dbtool"} {
    lappend  $package_files_list $lname
  }
}

proc emit_package_rule {package} {
  global package_files
  emit_package_rule_to_list $package package_files
}

proc emit_interpreter_rule {package} {
  global interpreter_package_files
  emit_package_rule_to_list $package interpreter_package_files
}

# Emit a rule to build a package full of 'ordinary' files, that is,
# one .o for each .java.
proc emit_ordinary_rule {package} {
  global name_map package_files

  foreach file $name_map($package) {
    # Strip off the '.java'.
    set root [file rootname $file]

    # Look for all included .class files.  Assumes that we don't have
    # multiple top-level classes in a .java file.
    set lname $root.list
    set dname $root.deps

    puts "$lname: classpath/$file"
    puts "\t@\$(mkinstalldirs) \$(dir \$@)"
    puts "\techo \$(srcdir)/classpath/lib/${root}*.class> $lname"
    puts ""
    puts "-include $dname"
    puts ""
    puts ""

    lappend package_files $lname
  }
}

# Emit a package-like rule for a platform-specific Process
# implementation.
proc emit_process_package_rule {platform} {
  set base "java/process-$platform"
  set lname $base.list
  set dname $base.deps

  puts "$lname: java/lang/${platform}Process.java"
  puts "\t@\$(mkinstalldirs) \$(dir \$@)"
  puts "\techo \$(srcdir)/classpath/lib/java/lang/${platform}Process*.class > $lname"
  puts ""
  puts "-include $dname"
  puts ""
  puts ""
}

# Emit a source file variable for a package, and corresponding header
# file variable, if needed.
proc emit_source_var {package} {
  global package_map name_map dir_map header_vars interpreter_header_vars

  if {$package == "."} {
    set pkgname ordinary
  } else {
    set pkgname $package
  }
  set uname [join [split $pkgname /] _]
  set varname ${uname}_source_files
  puts -nonewline "$varname ="

  makearray dirs
  foreach base [lsort $name_map($package)] {
    # Terminate previous line.
    puts " \\"
    # Having files start with './' is ugly and confuses the automake
    # "dirstamp" code; see automake PR 461.
    set ndir $dir_map($base)/
    if {$ndir == "./"} {
      set ndir ""
    }
    puts -nonewline "${ndir}${base}"
    set dirs($dir_map($base)) 1
  }
  puts ""
  puts ""

  if {$package_map($package) != "bc"} {
    # Ugly code to build up the appropriate patsubst.
    set result "\$(patsubst %.java,%.h,\$($varname))"
    # We use -decreasing so that classpath/external will be stripped
    # before classpath.
    foreach dir [lsort -decreasing [array names dirs]] {
      if {$dir != "."} {
	set result "\$(patsubst $dir/%,%,$result)"
      }
    }

    if {$package == "." || $package == "java/lang"} {
      # Ugly hack.
      set result "\$(filter-out java/lang/Object.h java/lang/Class.h,$result)"
    }

    puts "${uname}_header_files = $result"
    puts ""
    if {$pkgname != "gnu/gcj/xlib" && $pkgname != "gnu/awt/xlib"} {
	if {$package_map($package) == "interpreter"} {
          lappend interpreter_header_vars "${uname}_header_files"
	} else {
          lappend header_vars "${uname}_header_files"
	}
    }
  }
}

# Pretty-print a Makefile variable.
proc pp_var {name valueList {pre ""} {post ""}} {
  puts ""
  puts -nonewline "$name ="
  foreach val $valueList {
    puts " \\"
    puts -nonewline "  ${pre}${val}${post}"
  }
  puts ""
}

global argv
if {[llength $argv] > 0 && [lindex $argv 0] == "-verbose"} {
  set is_verbose 1
}

# Read the proper .omit files.
read_omit_file standard.omit.in
read_omit_file classpath/lib/standard.omit.in

# Scan classpath first.
scan_packages classpath
scan_packages classpath/external/sax
scan_packages classpath/external/w3c_dom
scan_packages classpath/external/relaxngDatatype
scan_packages classpath/external/jsr166
# Resource files.
scan_packages classpath/resource
# Now scan our own files; this will correctly override decisions made
# when scanning classpath.
scan_packages .
# Files created by the build.
classify_source_file classpath gnu/java/locale/LocaleData.java
classify_source_file classpath gnu/java/security/Configuration.java

puts "## This file was automatically generated by scripts/makemake.tcl"
puts "## Do not edit!"
puts ""

foreach package [lsort [array names package_map]] {
  if {$package_map($package) == "ignore"} {
    continue
  }
  if {! [info exists name_map($package)]} {
    continue
  }

  emit_source_var $package

  if {$package_map($package) == "bc"} {
    emit_bc_rule $package
  } elseif {$package_map($package) == "bcheaders"} {
    emit_bc_rule $package
  } elseif {$package_map($package) == "ordinary"} {
    emit_ordinary_rule $package
  } elseif {$package_map($package) == "package"} {
    emit_package_rule $package
  } elseif {$package_map($package) == "interpreter"} {
    emit_interpreter_rule $package
  } else {
    error "unrecognized type: $package_map($package)"
  }
}

emit_process_package_rule Ecos
emit_process_package_rule Win32
emit_process_package_rule Posix

puts "if INTERPRETER"
pp_var interpreter_packages_source_files $interpreter_package_files
pp_var interpreter_header_files $interpreter_header_vars "\$(" ")"
puts ""
puts "else"
puts ""
puts "interpreter_packages_source_files="
puts ""
puts "interpreter_header_files="
puts ""
puts "endif"

lappend package_files {$(interpreter_packages_source_files)}
lappend header_vars interpreter_header_files

pp_var all_packages_source_files $package_files
pp_var ordinary_header_files $header_vars "\$(" ")"
pp_var bc_objects $bc_objects
pp_var property_files $properties_files
