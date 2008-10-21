/* gnu.classpath.tools.gjdoc.Debug
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

public final class Debug {

   //private static int logLevel = 7;
   private static int logLevel = 0;

   static {
      String llProp = System.getProperty("gnu.classpath.tools.gjdoc.LogLevel");
      if (null!=llProp) logLevel = Integer.parseInt(llProp);
   }

   public static final void log(String msg) {
      System.err.println(msg);
   }

   public static final void log(int level, String msg) {
      if (level<=logLevel) {
	 System.err.println(msg);
      }
   }

   public static final void dumpArray(int level, Object[] array) {
      if (level<=logLevel) {
	 for (int i=0; i<array.length; ++i) {
	    System.err.println("  #"+i+": "+array[i]);
	 }
      }
   }
}
