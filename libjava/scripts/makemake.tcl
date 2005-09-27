#!/usr/bin/tclsh

# Helper to enforce array-ness.
proc makearray {name} {
  upvar $name ary
  set ary(_) 1
  unset ary(_)
}

# Verbose printer.
proc verbose {text} {
# puts stderr $text
}

# This maps a name to its style:
# * bc    objects in this package and all its sub-packages
#         are to be compiled with the BC ABI.  It is an error
#         for sub-packages to also appear in the map.
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
#
# If a package does not appear in the map, the default is 'package'.
global package_map
set package_map(.) package

# These are ignored in Classpath.
set package_map(gnu/test) ignore
set package_map(gnu/javax/swing/plaf/gtk) ignore

set package_map(gnu/xml) bc
set package_map(javax/imageio) bc
set package_map(javax/xml) bc
set package_map(gnu/java/beans) bc
set package_map(gnu/java/awt/peer/gtk) bc
set package_map(gnu/java/awt/peer/qt) bc
set package_map(org/xml) bc
set package_map(org/w3c) bc

# This is handled specially by the Makefile.
# We still want it byte-compiled so it isn't in the .omit file.
set package_map(gnu/gcj/tools/gcj_dbtool/Main.java) ignore

# These are handled specially.  If we list Class.java with other files
# in java.lang, we hit a compiler bug.
set package_map(java/lang/Class.java) ignore
set package_map(java/lang/Object.java) ignore

# More special cases.  These end up in their own library.
# Note that if we BC-compile AWT we must update these as well.
set package_map(gnu/gcj/xlib) package
set package_map(gnu/awt/xlib) package

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

# List of all '@' files that we are going to compile.
set package_files {}

# List of all header file variables.
set header_vars {}

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
      # If the entry for '.' is 'package', then set up a new entry for
      # the file's package.
      if {$pkg == "." && $package_map($pkg) == "package"} {
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

# Scan a directory and its subdirectories for .java source files.
# Note that we keep basedir and subdir separate so we can properly
# update our global data structures.
proc scan_directory {basedir subdir} {
  global dir_map

  set subdirs {}
  set files {}
  set here [pwd]
  cd $basedir/$subdir
  foreach file [lsort [glob *]] {
    if {[string match *.java $file]} {
      lappend files $subdir/$file
    } elseif {[file isdirectory $file]} {
      lappend subdirs $subdir/$file
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
  foreach subdir {gnu java javax org} {
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
  puts  "\t@find classpath/lib/$package -name '*.class'${omit} > $tname"
  puts "\t\$(LTGCJCOMPILE) -fjni -findirect-dispatch -c -o $loname @$tname"
  puts "\t@rm -f $tname"
  puts ""

  # We skip these because they are built into their own libraries and
  # are handled specially in Makefile.am.
  if {$loname != "gnu-java-awt-peer-gtk.lo"
      && $loname != "gnu-java-awt-peer-qt.lo"} {
    lappend bc_objects $loname
  }
}

# Emit a rule for a 'package' package.
proc emit_package_rule {package} {
  global package_map exclusion_map package_files

  if {$package == "."} {
    set pkgname ordinary
  } else {
    set pkgname $package
  }
  set varname [join [split $pkgname /] _]_source_files
  set base $pkgname
  set lname $base.list
  set dname $base.deps

  # A rule to make the phony file we are going to compile.
  puts "$lname: \$($varname)"
  puts "\t@\$(mkinstalldirs) \$(dir \$@)"
  puts "\t@for file in \$($varname); do \\"
  puts "\t  if test -f \$(srcdir)/\$\$file; then \\"
  puts "\t    echo \$(srcdir)/\$\$file; \\"
  puts "\t  else echo \$\$file; fi; \\"
  puts "\tdone > $lname"
  puts ""
  puts "-include $dname"
  puts ""
  puts ""

  if {$pkgname != "gnu/gcj/xlib" && $pkgname != "gnu/awt/xlib"} {
    lappend package_files $lname
  }
}

# Emit a source file variable for a package, and corresponding header
# file variable, if needed.
proc emit_source_var {package} {
  global package_map name_map dir_map header_vars

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
    foreach dir [lsort [array names dirs]] {
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
      lappend header_vars "${uname}_header_files"
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

# Read the proper .omit files.
read_omit_file standard.omit.in
read_omit_file classpath/lib/standard.omit

# Scan classpath first.
scan_packages classpath
scan_packages classpath/external/sax
scan_packages classpath/external/w3c_dom
# Now scan our own files; this will correctly override decisions made
# when scanning classpath.
scan_packages .
# Files created by the build.
classify_source_file . java/lang/ConcreteProcess.java
classify_source_file classpath java/util/LocaleData.java
classify_source_file classpath gnu/classpath/Configuration.java

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
  } elseif {$package_map($package) == "ordinary"} {
    # Nothing in particular to do here.
  } elseif {$package_map($package) == "package"} {
    emit_package_rule $package
  } else {
    error "unrecognized type: $package_map($package)"
  }
}

pp_var all_packages_source_files $package_files
pp_var ordinary_header_files $header_vars "\$(" ")"
pp_var bc_objects $bc_objects
