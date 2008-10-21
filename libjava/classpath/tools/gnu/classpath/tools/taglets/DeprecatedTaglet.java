/* gnu.classpath.tools.taglets.DeprecatedTaglet
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
 *  The default Taglet which handles deprecated information.
 *
 *  @author Julian Scheid (julian@sektor37.de)
 */
public class DeprecatedTaglet implements Taglet {
   
   private static final String NAME = "deprecated";
   private static final String HEADER = "Deprecated:";

   private static boolean enabled = true;
   
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
      DeprecatedTaglet deprecatedTaglet = new DeprecatedTaglet();
      tagletMap.put(deprecatedTaglet.getName(), deprecatedTaglet);
   }

   public String toString(Tag tag) {
      if (enabled) {
         return toString(new Tag[] { tag });
      }
      else {
         return null;
      }
   }

   public String toString(Tag[] tags) {
      if (!enabled || tags.length == 0) {
         return null;
      }
      else {
         
         StringBuffer result = new StringBuffer();
         result.append("<div class=\"classdoc-tag-section-header\">");
         result.append(HEADER);
         result.append("</div>");
         result.append("<dl class=\"classdoc-list\">");
         for (int i = 0; i < tags.length; i++) {
            result.append("<dt>");
            result.append(tags[i].text());
            result.append("</dt>");
         }
         result.append("</dl>");
         return result.toString();
      }
   }

   /**
    *  Enables/disables this taglet.
    */
   public static void setTagletEnabled(boolean enabled)
   {
      DeprecatedTaglet.enabled = enabled;
   }
}
