/* gnu.classpath.tools.doclets.StandardTaglet
   Copyright (C) 2004 Free Software Foundation, Inc.

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

package gnu.classpath.tools.doclets;

import com.sun.javadoc.Tag;
import com.sun.tools.doclets.Taglet;

/**
 *  Represents one of the built-in taglets. Used for specifying taglet
 *  order.
 */
public class StandardTaglet
   implements Taglet
{
   private String name;

   /**
    *  Initialize with one of the built-in taglet names.
    */
   public StandardTaglet(String name) {
      this.name = name;
   }

   public boolean inField() {
      return true;
   }

   public boolean inConstructor() {
      return true;
   }
    
   public boolean inMethod() {
      return true;
   }
   
   public boolean inOverview() {
      return true;
   }

   public boolean inPackage() {
      return true;
   }

   public boolean inType() {
      return true;
   }
   
   public boolean isInlineTag() {
      return false;
   }

   public String getName() {
      return this.name;
   }   

   public String toString(Tag tag) {
      //assert(false);
      return null;
   }

   public String toString(Tag[] tags) {
      //assert(false);
      return null;
   }
}
