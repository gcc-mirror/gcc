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
02111-1307 USA. */

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
