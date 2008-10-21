/* gnu.classpath.tools.gjdoc.TagImpl
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

public class TagImpl extends AbstractTagImpl implements Tag {

   private String kind;
   private String name;

   TagImpl(String name, String text, ClassDocImpl contextClass, MemberDocImpl contextMember) {
      super(text);
      this.kind=name;
      this.name=name;

      setBody(text, contextClass, contextMember);
   }

   public String kind() { return kind; }
   public String name() { return name; }
   public String toString() { return kind()+":"+text(); }

}
