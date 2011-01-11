/* gnu.classpath.tools.gjdoc.TimerDoclet
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
02111-1307 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */

package gnu.classpath.tools.gjdoc;

import com.sun.javadoc.*;

public class TimerDoclet {

   private static Object doclet = null;

   private static long maximumHeap = -1;

   private static Thread memThread;

   private static boolean runMemThread = true;

   private static void init() throws Exception {
      if (doclet==null) {
         doclet=Class.forName("com.sun.tools.doclets.standard.Standard").newInstance();
         memThread=new Thread() {

               public void run() {
                  while (runMemThread) {
                     synchronized (TimerDoclet.class) {
                        TimerDoclet.maximumHeap=Math.max(TimerDoclet.maximumHeap,
                                                         Runtime.getRuntime().totalMemory()-Runtime.getRuntime().freeMemory());
                     }
                     try { Thread.sleep(50); } catch (Exception e) {}
                  }
               }
            };
         //memThread.start();
      }
   }

   public static boolean validOptions(String[][] options, DocErrorReporter reporter)
      throws Exception {

      init();
      return ((Boolean)doclet.getClass().getMethod("validOptions", new Class[]{String[][].class, DocErrorReporter.class}).invoke(null, new Object[]{options, reporter})).booleanValue();
      //return false; //doclet.validOptions(options, reporter);
   }

   public static int optionLength(String option) throws Exception {
      init();
      return ((Integer)doclet.getClass().getMethod("optionLength", new Class[]{String.class}).invoke(null, new Object[]{option})).intValue();
   }

   public static boolean start(RootDoc root) throws Exception {
      Timer.setBeforeDocletTime();
      synchronized (TimerDoclet.class) {
         Timer.setMaxDriverHeap(maximumHeap);
         maximumHeap=-1;
      }
      //new com.sun.tools.doclets.standard.Standard().validOptions(root.options(), root);
      //new com.sun.tools.doclets.standard.Standard().start(root);

      if (validOptions(root.options(), root)) {
         doclet.getClass().getMethod("start", new Class[]{RootDoc.class}).invoke(null, new Object[]{root});
      }
      runMemThread=false;
      Timer.setStopTime();
      synchronized (TimerDoclet.class) {
         Timer.setMaxDocletHeap(maximumHeap);
      }
      Timer.shutdown();
      return true;
   }
}
