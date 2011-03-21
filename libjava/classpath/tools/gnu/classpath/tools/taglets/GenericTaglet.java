/* gnu.classpath.tools.taglets.GenericTaglet
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

package gnu.classpath.tools.taglets;

import java.util.Map;

import com.sun.tools.doclets.Taglet;

import com.sun.javadoc.Tag;

/**
 *  A taglet which can be configured at runtime.
 *
 *  @author Julian Scheid (julian@sektor37.de)
 */
public class GenericTaglet implements Taglet {

   private String name = "since";
   private String header = "Since:";

   private boolean scopeOverview;
   private boolean scopePackage;
   private boolean scopeType;
   private boolean scopeConstructor;
   private boolean scopeMethod;
   private boolean scopeField;

   private boolean enabled = true;

   public GenericTaglet(String name,
                        String header,
                        boolean scopeOverview,
                        boolean scopePackage,
                        boolean scopeType,
                        boolean scopeConstructor,
                        boolean scopeMethod,
                        boolean scopeField)
   {
      this.name = name;
      this.header = header;
      this.scopeOverview = scopeOverview;
      this.scopePackage = scopePackage;
      this.scopeType = scopeType;
      this.scopeConstructor = scopeConstructor;
      this.scopeMethod = scopeMethod;
      this.scopeField = scopeField;
   }

   public String getName() {
      return name;
   }

   public boolean inField() {
      return scopeField;
   }

   public boolean inConstructor() {
      return scopeConstructor;
   }

   public boolean inMethod() {
      return scopeMethod;
   }

   public boolean inOverview() {
      return scopeOverview;
   }

   public boolean inPackage() {
      return scopePackage;
   }

   public boolean inType() {
      return scopeType;
   }

   public boolean isInlineTag() {
      return false;
   }

   public void register(Map tagletMap) {
      tagletMap.put(getName(), this);
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
         result.append(header);
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
   public void setTagletEnabled(boolean enabled)
   {
      this.enabled = enabled;
   }
}
