/* gnu.classpath.tools.taglets.ValueTaglet
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

package gnu.classpath.tools.taglets;

import java.util.Map;

import com.sun.tools.doclets.Taglet;

import com.sun.javadoc.Doc;
import com.sun.javadoc.Tag;
import com.sun.javadoc.FieldDoc;
import com.sun.javadoc.MemberDoc;
import com.sun.javadoc.SeeTag;

/**
 *  The default Taglet which shows final static field values.
 *
 *  @author Julian Scheid (julian@sektor37.de)
 */
public class ValueTaglet 
   implements GnuExtendedTaglet
{
   private static final String NAME = "value";

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
      return true;
   }    

   public static void register(Map tagletMap) {
      ValueTaglet valueTaglet = new ValueTaglet();
      tagletMap.put(valueTaglet.getName(), valueTaglet);
   }

   public String toString(Tag tag) {
      return null;
   }

   public String toString(Tag tag, TagletContext context) {
      if (0 == tag.inlineTags().length) {
         if (context.getDoc().isField()) {
            FieldDoc fieldDoc = (FieldDoc)context.getDoc();
            if (fieldDoc.isStatic() && fieldDoc.isFinal()) {
               return fieldDoc.constantValueExpression();
            }
         }
      }
      else {
         MemberDoc referencedMember = ((SeeTag)tag).referencedMember();
         if (null != referencedMember && referencedMember.isField()) {
            FieldDoc fieldDoc = (FieldDoc)referencedMember;
            if (fieldDoc.isStatic() && fieldDoc.isFinal()) {
               return fieldDoc.constantValueExpression();
            }
         }
      }
      return "";
   }

   public String toString(Tag[] tags) {
      return null;
   }

   public String toString(Tag[] tags, TagletContext context) {
      return null;
   }

}
