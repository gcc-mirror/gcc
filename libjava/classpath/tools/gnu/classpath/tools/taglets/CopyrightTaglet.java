/* gnu.classpath.tools.taglets.CopyrightTaglet
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

package gnu.classpath.tools.taglets;

import java.util.Map;

import com.sun.tools.doclets.Taglet;

import com.sun.javadoc.Tag;

/**
 *  A simple Taglet which handles Copyright information.
 */
public class CopyrightTaglet implements Taglet {

   private static final String NAME = "copyright";
   private static final String HEADER = "Copyright:";
    
   public String getName() {
      return NAME;
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

   public static void register(Map tagletMap) {
      CopyrightTaglet copyrightTaglet = new CopyrightTaglet();
      tagletMap.put(copyrightTaglet.getName(), copyrightTaglet);
   }

   public String toString(Tag tag) {
      return toString(new Tag[] { tag });
   }

   public String toString(Tag[] tags) {
      if (tags.length == 0) {
         return null;
      }
      else {
         boolean haveValidTag = false;
         for (int i = 0; i < tags.length && !haveValidTag; ++i) {
            if (tags[i].text().length() > 0) {
               haveValidTag = true;
            }
         }
         
         if (haveValidTag) {
            StringBuffer result = new StringBuffer();
            result.append("<dl>");
            for (int i = 0; i < tags.length; i++) {
               if (tags[i].text().length() > 0) {
                  result.append("<dt><i>Copyright &#169; " + tags[i].text() + "</i></dt>");
               }
            }
            result.append("</dl>");
            return result.toString();
         }
         else {
            return null;
         }
      }
   }
}
