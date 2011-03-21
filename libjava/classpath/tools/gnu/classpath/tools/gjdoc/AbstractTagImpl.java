/* gnu.classpath.tools.gjdoc.AbstractTagImpl
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
import java.util.*;

public abstract class AbstractTagImpl
   implements Tag, TagContainer {

   private static final Tag[] emptyTags = new Tag[0];

   protected String text;
   protected Map tagMap;

   protected AbstractTagImpl(String text) {
      this.text=text;
   }

   public void resolve() {
      Tag[] allTags=inlineTags();
      if (null != allTags) {
         for (int i=0; i<allTags.length; ++i) {
            if (allTags[i]!=this) ((AbstractTagImpl)allTags[i]).resolve();
         }
      }
      else {
         System.err.println("Null tags for " + this);
      }
   }

   protected void setBody(String body, ClassDocImpl contextClass, MemberDocImpl contextMember) {
      this.tagMap=DocImpl.parseCommentTags(body.toCharArray(),
                                           0,
                                           body.length(),
                                           contextClass,
                                           contextMember,
                                           this,
                                           null);
   }

   public Tag[] firstSentenceTags() {
      return (tagMap!=null)? (Tag[])tagMap.get("first") : emptyTags;
   }
   public Tag[] inlineTags() {
      return (tagMap!=null)? (Tag[])tagMap.get("inline") : emptyTags;
   }

   public String name() {
      return kind();
   }

   public String text() {
      return text;
   }

   public Map getTagMap() {
      return tagMap;
   }

   /**
    * Debug string containing class, name, text and tagMap.
    */
   public String toString()
   {
     return (this.getClass().getName()
             + "[name=" + name()
             + ", text=" + text()
             + ", tagMap=" + getTagMap()
             + "]");
   }
}
