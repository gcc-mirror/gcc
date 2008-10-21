/* gnu.classpath.tools.gjdoc.ErrorReporter
   Copyright (C) 2001 Free Software Foundation, Inc.

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.
 
GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA. */

package gnu.classpath.tools.gjdoc;

import com.sun.javadoc.*;
import java.io.*;
import java.util.*;
import java.lang.reflect.*;

/**
 *  Simple implementation of a <code>DocErrorReporter</code>: writes
 *  to <code>System.err</code>.
 */
public class ErrorReporter implements DocErrorReporter {

   private PrintStream out;

   /**
    *  Keeps track of the number of errors occured
    *  during generation.
    */
   private int errorCount=0;

   /**
    *  Keeps track of the number of warnings occured
    *  during generation.
    */
   private int warningCount=0;

   /*
    *  When <code>true</code>, no notices will be emitted.
    */
   private boolean quiet = false;
   
   /*
    *  When <code>true</code>, no warnings will be emitted.
    */
   private boolean noWarn = false;

   public ErrorReporter()
   {
      this.out = System.err;
   }

   // Print error message, increment error count. 
   public void printError(java.lang.String msg) {
      out.println("ERROR: "+msg);
      ++errorCount;
   }

   // Print error message, increment error count. 
   public void printFatal(java.lang.String msg) {
      out.println("FATAL: "+msg);
      System.exit(10);
   }

   // Print a message. 
   public void printNotice(java.lang.String msg) {
      if (!quiet) {
	 out.println(msg);
      }
   }
   
   // Print warning message, increment warning count. 
   public void printWarning(java.lang.String msg) {
      if (!noWarn) {
	 out.println("WARNING: "+msg);
	 ++warningCount;;
      }
   }

   public int getErrorCount() {
      return errorCount;
   }

   public int getWarningCount() {
      return warningCount;
   }

   /**
    *  Specify whether notices should be printed.
    */
   public void setQuiet(boolean quiet) {
      this.quiet = quiet;
   }
}
