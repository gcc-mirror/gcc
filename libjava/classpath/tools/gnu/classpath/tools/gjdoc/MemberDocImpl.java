/* gnu.classpath.tools.gjdoc.MemberDocImpl
   Copyright (C) 2001, 2012 Free Software Foundation, Inc.

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

import java.util.*;
import com.sun.javadoc.*;

public abstract class MemberDocImpl extends ProgramElementDocImpl implements MemberDoc {

   protected String typeName;
   protected Type   type;

   public MemberDocImpl(ClassDoc containingClass,
                        PackageDoc containingPackage,
                        SourcePosition position) {

      super(containingClass,
            containingPackage,
            position);
   }

   public String qualifiedName() {
      return containingClass().qualifiedName()+"."+name();
   }

   public boolean isSynthetic() {
      return false;
   }

   int parseModifiers(char[] source, int startIndex, int endIndex) {

      Debug.log(9,"parseModifiers '"+new String(source,startIndex,endIndex-startIndex)+"'");

      final int STATE_NORMAL = 1;
      final int STATE_STARC  = 2;
      final int STATE_SLASHC = 3;

      int state = STATE_NORMAL;

      StringBuffer word = new StringBuffer();
      StringBuffer typeNameBuf = new StringBuffer();
      int lastWordStart = startIndex;
      int firstChar = 0;
      int lastChar = 0;
      for (; startIndex<endIndex; ++startIndex) {
         if (state==STATE_STARC) {
            if (startIndex<endIndex-1 && source[startIndex]=='*' && source[startIndex+1]=='/') {
               ++startIndex;
               state=STATE_NORMAL;
            }
         }
         else if (state==STATE_SLASHC) {
            if (source[startIndex]=='\n') {
               state=STATE_NORMAL;
            }
         }
         else if (startIndex<endIndex-1 && source[startIndex]=='/' && source[startIndex+1]=='*') {
            ++startIndex;
            state=STATE_STARC;
         }
         else if (source[startIndex]=='=' || source[startIndex]=='(' || source[startIndex]==';') {
            typeName = typeNameBuf.toString();
            return lastWordStart;
         }
         else if (Parser.WHITESPACE.indexOf(source[startIndex])>=0
                  || (startIndex > 0 && source[startIndex-1] == ']' && source[startIndex] != '[')) {
            if (word.length()>0 && lastChar != '.') {
               if (processModifier(word.toString())) {
               }
               else if (typeNameBuf.length()==0 && !isConstructor()) {
                  typeNameBuf.setLength(0);
                  typeNameBuf.append(word);
               }
               else if ((firstChar=='[' || firstChar==']') && !isConstructor()) {
                  typeNameBuf.append(word);
               }
               else {
                  typeName = typeNameBuf.toString();
                  return lastWordStart;
               }
               word.setLength(0);
               lastWordStart=startIndex;
            }
         }
         else {
            if (lastWordStart<0) lastWordStart=startIndex;
            lastChar = source[startIndex];
            if (0 == word.length()) {
               firstChar = lastChar;
            }
            word.append((char)lastChar);
         }
      }

      typeName = typeNameBuf.toString();
      return startIndex;
   }

    public Type type() {
        //public Type type() throws ParseException {
        Debug.log(9,"type() called on "+containingClass()+"."+this);
        if (type==null) {
            try {
                type=((ClassDocImpl)containingClass()).typeForString(typeName);
            } catch (ParseException e) {
               System.err.println("FIXME: add try-catch to force compilation");
               e.printStackTrace();
            }
        }
        return type;
    }


   protected void setName(String name) {
      this.name=name;
   }
   private String name;


   public String name() {
      return name;
   }

   public void setTypeName(String typeName) {
      this.typeName=typeName;
      this.type=null;
   }

   public String getTypeName() {
      return typeName;
   }

   // return true if this Doc is include in the active set.
   public boolean isIncluded() {
      return Main.getInstance().includeAccessLevel(accessLevel);
   }

   public int compareTo(Doc d) {
      if (d instanceof MemberDocImpl) {
         int rc=name().compareTo(((MemberDocImpl)d).name());
         if (rc==0)
            rc=containingClass().qualifiedName().compareTo(((MemberDocImpl)d).containingClass().qualifiedName());
         return rc;
      }
      else {
         return super.compareTo(d);
      }
   }

   void resolve() {

      if (type==null && typeName!=null) {
         Debug.log(1, "MemberDocImpl.resolve(), looking up type named "+typeName);
         try {
            type=((ClassDocImpl)containingClass()).typeForString(typeName);
         } catch (ParseException e) {
            //System.err.println("FIXME: add try-catch to force compilation");
            //e.printStackTrace();
            Debug.log(1, "INTERNAL WARNING: Couldn't find type for name '"+typeName+"'");
         }
      }

      if (type instanceof ClassDocProxy) {
         String className=type.qualifiedTypeName();
         ClassDoc realClassDoc=((ClassDocImpl)containingClass()).findClass(className, type.dimension());
         if (realClassDoc!=null) {
            type=realClassDoc;
         }
         else {
            //throw new Error("Class not found: "+className);
            /*** This is not an error, the class was not included
             * on the command line. Perhaps emit a notice here.
             *

            Main.getRootDoc().printError("Class not found '"
                                         + className
                                         + "' in class '"
                                         + containingClass().qualifiedName()
                                         + "' member '"
                                         + name()
                                         + "'");
            */
         }
      }
   }

   public void resolveComments()
   {
      super.resolveComments();

      if (tagMap.isEmpty()) {
         TagContainer inheritedTagMap = ClassDocImpl.findInheritedDoc(containingClass(),
                                                                      this,
                                                                      null);
         if (null != inheritedTagMap) {
            this.tagMap = inheritedTagMap.getTagMap();
         }
      }
   }
}
