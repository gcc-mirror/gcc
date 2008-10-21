/* gnu.classpath.tools.gjdoc.SerialFieldTagImpl
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

public class SerialFieldTagImpl extends AbstractTagImpl implements SerialFieldTag {

   private String       fieldName;
   private String       fieldType;
   private String       description;
   private ClassDoc     fieldTypeDoc;
   private ClassDocImpl contextClass;

   public SerialFieldTagImpl(String text, 
                             ClassDocImpl contextClass, 
                             MemberDocImpl contextMember) {
      super(text);
      this.contextClass=contextClass;

      if (fieldName==null)
        fieldName="";
      if (fieldType==null)
        fieldType="";
      if (description==null)
        description="";

      int state=1;
      char[] textArr=text.toCharArray();
      for (int i=0; i<textArr.length; ++i) {
	 char c=textArr[i];
	 switch (state) {
	 case 1:
	    if (Parser.isWhitespace(c)) state=2;
	    else fieldName+=c;
	    break;
	 case 2:
	    if (Parser.isWhitespace(c)) state=3;
	    else fieldType+=c;
	    break;
	 case 3:
	    description+=c;
	    break;
	 }
      }
      
      setBody(description, contextClass, contextMember);

   }

   public void resolve() {

      super.resolve();
      try {
	  Type type=contextClass.typeForString(fieldType);
	  this.fieldTypeDoc=type.asClassDoc();
      } catch (ParseException e) {
	  System.err.println("FIXME: add try-catch to force compilation"
			     + e);
      }
   }

   public ClassDoc fieldTypeDoc() {
      return fieldTypeDoc;
   }

   public String fieldName() {
      return fieldName;
   }

   public String fieldType() {
      return fieldType;
   }

   public String description() {
      return description;
   }

   public String kind() {
      return "@serialField";
   }

   public int compareTo(Object o) {
      if (o!=null && o instanceof SerialFieldTagImpl) {
	 return fieldName().compareTo(((SerialFieldTagImpl)o).fieldName());
      }
      else {
	 return 0;
      }
   }
}
