/* gnu.classpath.tools.gjdoc.Timer
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

import java.io.*;
import java.text.*;

public class Timer {

   private static long startTime, beforeDocletTime, stopTime, memoryUsed, maxDriverHeap=-1, maxDocletHeap=-1;

   static void shutdown() {
      try{
	 if (stopTime==0) return;
	 PrintWriter pw=new PrintWriter(new FileWriter("timer.out"));
	 pw.println("Preparation (driver) took "+(((double)(beforeDocletTime-startTime))/1000.)+" s");
	 pw.println("Generation (doclet) took "+(((double)(stopTime-beforeDocletTime))/1000.)+" s");
	 pw.println("");
	 pw.println("Memory used for documentation tree: "+(memoryUsed/(1024*1024))+" MB");
	 pw.println("Max. heap used for driver: "+((maxDriverHeap<0)?"N/A":((maxDriverHeap/(1024*1024))+" MB")));
	 pw.println("Max. heap used for doclet: "+((maxDocletHeap<0)?"N/A":((maxDocletHeap/(1024*1024))+" MB")));
	 pw.println("");
	 pw.println("TOTAL TIME: "+(((double)(stopTime-startTime))/1000.)+" s");
	 pw.println("TOTAL HEAP: "+((maxDocletHeap<0)?"N/A":(Math.max(maxDocletHeap,maxDriverHeap)/(1024*1024))+" MB"));
	 pw.close();
      }
      catch (IOException e) {
	 e.printStackTrace();
      }
   }

   public static void setStartTime() {
      Timer.startTime=System.currentTimeMillis();
   }

   public static void setStopTime() {
      Timer.stopTime=System.currentTimeMillis();
   }

   public static void setBeforeDocletTime() {
      Timer.beforeDocletTime=System.currentTimeMillis();
      Timer.memoryUsed=Runtime.getRuntime().totalMemory()-Runtime.getRuntime().freeMemory();
   }

   public static void setMaxDocletHeap(long maximumHeap) {
      Timer.maxDocletHeap=maximumHeap;
   }

   public static void setMaxDriverHeap(long maximumHeap) {
      Timer.maxDriverHeap=maximumHeap;
   }

}
