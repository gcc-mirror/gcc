/* gnu.classpath.tools.taglets.CodeTaglet
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

import com.sun.javadoc.Doc;
import com.sun.javadoc.Tag;
import com.sun.javadoc.FieldDoc;
import com.sun.javadoc.MemberDoc;
import com.sun.javadoc.SeeTag;

/**
 *  The default Taglet which shows its contents enclosed in a
 *  <code>code</code> tag.
 *
 *  @author Julian Scheid (julian@sektor37.de)
 */
public class CodeTaglet 
   implements Taglet
{
   private static final String NAME = "code";

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

   public String toString(Tag tag) {
      return "<code>" + tag.text() + "</code>";
   }

   public String toString(Tag[] tag) {
      return null;
   }

}
