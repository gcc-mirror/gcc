/* gnu.classpath.tools.gjdoc.ThrowsTagImpl
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
import java.text.*;

public class ThrowsTagImpl extends AbstractTagImpl implements ThrowsTag {

   private ClassDoc exception;
   private String exceptionName;
   private String exceptionComment;

   public ThrowsTagImpl(String text, 
                        ClassDocImpl contextClass,
                        MemberDocImpl contextMember) {
      super(text);

      char[] textarr=text.toCharArray();
      int i=0;
      for (; i<textarr.length; ++i) {
	 if (!Parser.isWhitespace(textarr[i])) break;
      }
      for (; i<textarr.length; ++i) {
	 if (Parser.isWhitespace(textarr[i])) {
	    this.exceptionName=new String(textarr,0,i).trim();
	    this.exceptionComment=new String(textarr,i,textarr.length-i).trim();
	    break;
	 }
      }
      if (null != exceptionName) {
         if (contextClass==null) {
            this.exception=Main.getRootDoc().classNamed(exceptionName);
         }
         else {
            this.exception=contextClass.findClass(exceptionName);
         }
         if (exception!=null)
            this.exceptionName=exception.qualifiedName();
         else {
            if (text.trim().startsWith("<")) {
               Main.getRootDoc().printWarning("Expected exception name but got '"+text+"' in class "+contextClass.getClassName());
            }
         }
      }
      else {
         Main.getRootDoc().printWarning("@throws tag in comment for " + contextClass.qualifiedName() + "." + contextMember.name() + " doesn't specify an exception.");
      }
      if (this.exceptionComment!=null) {
         setBody(this.exceptionComment, contextClass, contextMember);
      }
   }

   public ClassDoc exception() {
      return exception;
   }

   public String exceptionName() {
      return exceptionName;
   }

   public String exceptionComment() {
      return exceptionComment;
   }

   public String kind() {
      return "@throws";
   }
}
