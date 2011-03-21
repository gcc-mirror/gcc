/* gnu.classpath.tools.gjdoc.ExecutableMemberDocImpl
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

import java.util.*;
import java.io.*;
import com.sun.javadoc.*;

public class ExecutableMemberDocImpl extends MemberDocImpl implements ExecutableMemberDoc {

   protected ExecutableMemberDocImpl(ClassDoc containingClass,
                                     PackageDoc containingPackage,
                                     SourcePosition position) {

      super(containingClass,
            containingPackage,
            position);
   }

   protected boolean processModifier(String word) {
      if (super.processModifier(word)) {
         return true;
      }
      else if (word.equals("synchronized")) {
         isSynchronized=true;
         return true;
      }
      else if (word.equals("native")) {
         isNative=true;
         return true;
      }
      else if (word.equals("abstract")) {
         isAbstract=true;
         return true;
      }
      else {
         return false;
      }
   }

   private boolean isAbstract=false;
   private boolean isNative=false;
   private boolean isSynchronized=false;

   public boolean isAbstract() { return isAbstract; }

   public boolean isNative() { return isNative; }

   public boolean isSynchronized() { return isSynchronized; }

   public ClassDoc[] thrownExceptions() { return thrownExceptions; }

   public Parameter[] parameters() { return parameters; }

   public ThrowsTag[] throwsTags() {
      return (ThrowsTag[])getTagArr("throws", throwsTagEmptyArr);
   }

   public ParamTag[] paramTags() {
      return (ParamTag[])getTagArr("param", paramTagEmptyArr);
   }

   public String signature() { return signature; }
   public String flatSignature() { return flatSignature; }

   public ClassDoc overriddenClass() {
      for (ClassDoc cdi=(ClassDoc)containingClass().superclass(); cdi!=null; cdi=(ClassDoc)cdi.superclass()) {
         if (null!=ClassDocImpl.findMethod(cdi, name(), signature()))
            return cdi;
      }
      return null;
   }

   public static ExecutableMemberDocImpl createFromSource(ClassDoc containingClass,
                                                          PackageDoc containingPackage,
                                                          char[] source, int startIndex, int endIndex) throws IOException, ParseException {

      int lastchar=32;
      StringBuffer methodName=new StringBuffer();
      for (int i=startIndex; i<endIndex && source[i]!='('; ++i) {
         if ((Parser.WHITESPACE.indexOf(lastchar)>=0 && Parser.WHITESPACE.indexOf(source[i])<0)
             || (lastchar == ']' && Parser.WHITESPACE.indexOf(source[i])<0 && '[' != source[i])) {
            methodName.setLength(0);
            methodName.append(source[i]);
         }
         else if (Parser.WHITESPACE.indexOf(source[i])<0) {
            methodName.append(source[i]);
         }
         lastchar=source[i];
      }

      ExecutableMemberDocImpl rc;

      SourcePosition position = DocImpl.getPosition(containingClass, source, startIndex);

      if (methodName.toString().equals(((ClassDocImpl)containingClass).getClassName())) {

         // Constructor

         rc=new ConstructorDocImpl(containingClass,
                                   containingPackage,
                                   position);
      }
      else {

         // Normal method

         rc=new MethodDocImpl(containingClass,
                              containingPackage,
                              position);
      }

      if (containingClass.isInterface())
         rc.accessLevel=ACCESS_PUBLIC;

      int ndx=rc.parseModifiers(source, startIndex, endIndex);
      StringBuffer name = new StringBuffer();

      final int STATE_NORMAL=1;
      final int STATE_STARC=2;
      final int STATE_SLASHC=3;

      int state=STATE_NORMAL;

      while (source[ndx]!='(' && ndx<endIndex) {
         if (state==STATE_NORMAL) {
            if (ndx<endIndex-1 && source[ndx]=='/' && source[ndx+1]=='/') {
               ++ndx;
               state=STATE_SLASHC;
            }
            else if (ndx<endIndex-1 && source[ndx]=='/' && source[ndx+1]=='*') {
               ++ndx;
               state=STATE_STARC;
            }
            else {
               name.append(source[ndx]);
            }
         }
         else if (state==STATE_SLASHC) {
            if (source[ndx]=='\n')
               state=STATE_NORMAL;
         }
         else if (state==STATE_STARC) {
            if (ndx<endIndex-1 && source[ndx]=='*' && source[ndx+1]=='/') {
               ++ndx;
               state=STATE_NORMAL;
            }
         }
         ++ndx;
      }
      rc.setName(name.toString().trim());

      state=STATE_NORMAL;

      ++ndx;
      int endx;
      String param="";
      List parameterList=new ArrayList();
      for (endx=ndx; endx<endIndex; ++endx) {
         if (state==STATE_SLASHC) {
            if (source[endx]=='\n') {
               state=STATE_NORMAL;
            }
         }
         else if (state==STATE_STARC) {
            if (source[endx]=='*' && source[endx+1]=='/') {
               state=STATE_NORMAL;
               ++endx;
            }
         }
         else if (source[endx]=='/' && source[endx+1]=='*') {
            state=STATE_STARC;
            ++endx;
         }
         else if (source[endx]=='/' && source[endx+1]=='/') {
            state=STATE_SLASHC;
            ++endx;
         }
         else if (source[endx]==',' || source[endx]==')') {
            param=param.trim();
            if (param.length()>0) {
               int n = param.length()-1;
               int paramNameStart = 0;
               while (n >= 0) {
                  char c = param.charAt(n);
                  if ('[' == c || ']' == c || Parser.WHITESPACE.indexOf(c)>=0) {
                     paramNameStart = n + 1;
                     break;
                  }
                  else {
                     -- n;
                  }
               }
               while (n >= 0 && ('[' == param.charAt(n)
                                 || ']' == param.charAt(n)
                                 || Parser.WHITESPACE.indexOf(param.charAt(n))>=0)) {
                  -- n;
               }
               int paramTypeEnd = n + 1;
               int paramTypeStart = 0;
               while (n >= 0) {
                  char c = param.charAt(n);
                  if ('[' == c || ']' == c || Parser.WHITESPACE.indexOf(c)>=0) {
                     paramTypeStart = n + 1;
                     break;
                  }
                  else {
                     -- n;
                  }
               }

               String paramType;
               String paramName;
               if (0 != paramNameStart) {
                  paramType=param.substring(paramTypeStart, paramTypeEnd);
                  paramName=param.substring(paramNameStart);
               }
               else {
                  paramName = "";
                  StringBuffer paramTypeBuffer = new StringBuffer();
                  for (int i=0; i<param.length(); ++i) {
                     char c = param.charAt(i);
                     if ('[' != c && ']' != c && Parser.WHITESPACE.indexOf(c)<0) {
                        paramTypeBuffer.append(c);
                     }
                  }
                  paramType = paramTypeBuffer.toString();
               }
               String dimSuffix="";

               for (int i=0; i<param.length(); ++i) {
                  if ('[' == param.charAt(i)) {
                     dimSuffix += "[]";
                  }
               }
               paramType+=dimSuffix;

               if (paramType.startsWith("[")) {
                  System.err.println("broken param type in " + rc + " in " +containingClass);
               }

               parameterList.add(new ParameterImpl(paramName, paramType,
                                                   ((ClassDocImpl)containingClass).typeForString(paramType)));

               param="";
            }
         }
         else
            param+=source[endx];

         if (source[endx]==')' && state==STATE_NORMAL)
            break;
      }

      rc.setParameters((Parameter[])parameterList.toArray(new Parameter[0]));

      ++endx;
      String word="";
      String dimSuffix="";
      boolean haveThrowsKeyword=false;
      List thrownExceptionsList=new ArrayList();

      state=STATE_NORMAL;
      for (; endx<endIndex; ++endx) {
         if (state==STATE_SLASHC) {
            if (source[endx]=='\n') state=STATE_NORMAL;
         }
         else if (state==STATE_STARC) {
            if (source[endx]=='*' && source[endx+1]=='/') {
               state=STATE_NORMAL;
               ++endx;
            }
         }
         else if (source[endx]=='/' && source[endx+1]=='*') {
            state=STATE_STARC;
            ++endx;
         }
         else if (source[endx]=='/' && source[endx+1]=='/') {
            state=STATE_SLASHC;
            ++endx;
         }
         else if (Parser.WHITESPACE.indexOf(source[endx])>=0) {
            word=word.trim();
            if (!haveThrowsKeyword && word.length()>0) {
               if (word.equals("throws")) haveThrowsKeyword=true;
               else System.err.println("ARGH! "+word);
               word="";
            }
         }
         else if (source[endx]=='[' || source[endx]==']') {
            dimSuffix += source[endx];
         }
         else if (source[endx]==',' || source[endx]=='{' || source[endx]==';') {
            word=word.trim();
            if (word.length()>0) {
               ClassDoc exceptionType=rc.containingClass().findClass(word);
               if (exceptionType==null) {
                  exceptionType=new ClassDocProxy(word,
                                                  rc.containingClass());
               }
               thrownExceptionsList.add(exceptionType);
            }
            if (source[endx]=='{') {
               break;
            }
            else {
               word="";
            }
         }
         else {
            word+=source[endx];
         }
      }

      if (dimSuffix.length()>0) {
         rc.setTypeName(rc.getTypeName()+dimSuffix);
      }

      rc.setThrownExceptions((ClassDoc[])thrownExceptionsList.toArray(new ClassDoc[0]));

      return rc;
   }

   private ClassDoc[] thrownExceptions;
   private Parameter[] parameters;
   private String signature;
   private String flatSignature;

   void setParameters(Parameter[] parameters) {
      this.parameters=parameters;
   }

   void setThrownExceptions(ClassDoc[] thrownExceptions) {
      this.thrownExceptions=thrownExceptions;
   }

   void resolve() {

      for (int i=0; i<thrownExceptions.length; ++i) {
         if (thrownExceptions[i] instanceof ClassDocProxy) {
            String className=thrownExceptions[i].qualifiedName();
            ClassDoc realClassDoc=containingClass().findClass(className);
            if (realClassDoc!=null)
               thrownExceptions[i]=realClassDoc;
         }
      }

      StringBuffer signatureBuf=new StringBuffer();
      StringBuffer flatSignatureBuf=new StringBuffer();

      for (int i=0; i<parameters.length; ++i) {
         ((ParameterImpl)parameters[i]).resolve(containingClass());

         if (signatureBuf.length()>0) {
            signatureBuf.append(",");
            flatSignatureBuf.append(",");
         }
         signatureBuf.append(parameters[i].type().qualifiedTypeName());
         flatSignatureBuf.append(parameters[i].type().typeName());
         signatureBuf.append(parameters[i].type().dimension());
         flatSignatureBuf.append(parameters[i].type().dimension());
      }
      this.signature="("+signatureBuf.toString()+")";
      this.flatSignature="("+flatSignatureBuf.toString()+")";

      super.resolve();

   }

   public int compareTo(Object other) {
      int rc;
      if (other instanceof MemberDocImpl) {
         MemberDocImpl otherMember = (MemberDocImpl)other;
         rc = name().compareTo(otherMember.name());
         if (0 == rc) {
            if (other instanceof ExecutableMemberDocImpl) {
               rc = signature().compareTo(((ExecutableMemberDocImpl)other).signature());
               if (0 == rc) {
                  return containingClass().compareTo(otherMember.containingClass());
               }
            }
            else {
               rc = 1;
            }
         }
      }
      else {
         rc = 1;
      }
      return rc;
   }
}
