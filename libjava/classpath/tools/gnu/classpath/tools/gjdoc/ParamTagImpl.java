/* gnu.classpath.tools.gjdoc.ParamTagImpl
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

public class ParamTagImpl extends AbstractTagImpl implements ParamTag {

   private String parameterName;
   private String parameterComment;
   
   public ParamTagImpl(String text,
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
	    parameterName=new String(textarr,0,i).trim();
	    parameterComment=new String(textarr,i,textarr.length-i).trim();
	    break;
	 }
      }
      if (parameterComment!=null) {
         setBody(parameterComment, contextClass, contextMember);
      }
   }

   public String parameterComment() {
      return parameterComment;
   }

   public String parameterName() {
      return parameterName;
   }

   public String kind() {
      return "@param";
   }
}
