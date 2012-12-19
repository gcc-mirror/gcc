/* gnu.classpath.tools.gjdoc.FieldDocImpl
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
import java.lang.reflect.Modifier;

import gnu.classpath.tools.gjdoc.expr.Evaluator;
import gnu.classpath.tools.gjdoc.expr.CircularExpressionException;
import gnu.classpath.tools.gjdoc.expr.IllegalExpressionException;

public class FieldDocImpl
   extends MemberDocImpl
   implements FieldDoc, Cloneable
{

   private boolean isTransient;
   private boolean isVolatile;
   private String valueLiteral;
   private Object constantValue;
   private boolean constantValueEvaluated;

   private FieldDocImpl(ClassDoc containingClass,
                        PackageDoc containingPackage,
                        SourcePosition position) {

      super(containingClass,
            containingPackage,
            position);
   }

   private static FieldDocImpl createFieldDoc(FieldDocImpl prototype,
                                              String fieldDef,
                                              String fieldValueLiteral)
   {
      if (null != fieldValueLiteral && fieldValueLiteral.length() == 0) {
         fieldValueLiteral = null;
      }

      try {
         FieldDocImpl fieldDoc=(FieldDocImpl)prototype.clone();
         String dimSuffix="";
         while (fieldDef.trim().endsWith("[")
                || fieldDef.trim().endsWith("]")) {
            fieldDef=fieldDef.trim();
            dimSuffix=fieldDef.charAt(fieldDef.length()-1)+dimSuffix;
            fieldDef=fieldDef.substring(0,fieldDef.length()-1);
         }

         fieldDoc.setTypeName(fieldDoc.getTypeName()+dimSuffix);
         fieldDoc.setName(fieldDef.trim());
         fieldDoc.setValueLiteral(fieldValueLiteral);
         return fieldDoc;
      }
      catch (CloneNotSupportedException e) {
         // should not happen
         e.printStackTrace();
         return null;
      }
   }

   public static Collection<FieldDoc> createFromSource(ClassDoc containingClass,
                                                       PackageDoc containingPackage,
                                                       char[] source, int startIndex, int endIndex) {

      List<FieldDoc> rcList=new ArrayList<FieldDoc>();

      FieldDocImpl fd=new FieldDocImpl(containingClass,
                                       containingPackage,
                                       DocImpl.getPosition(containingClass, source, startIndex));

      int ndx=fd.parseModifiers(source, startIndex, endIndex);

      if (containingClass.isInterface()) {
         fd.accessLevel = ACCESS_PUBLIC;
      }

      final int STATE_FIELDNAME   = 1;
      final int STATE_FIELDVALUE  = 2;
      final int STATE_QUOTE       = 3;
      final int STATE_QUOTEBS     = 4;
      final int STATE_SQUOTE      = 5;
      final int STATE_SQUOTEBS    = 6;
      final int STATE_COMMENT     = 7;
      final int STATE_LINECOMMENT = 8;

      int state = STATE_FIELDNAME;
      int prevState = state;

      int bracketCount = 0;

      StringBuffer fieldNameBuf = new StringBuffer();
      StringBuffer fieldValueLiteralBuf = new StringBuffer();

      for (int i=ndx; i<endIndex; ++i) {

         char c = source[i];
         char nextChar = '\0';
         if (i + 1 < endIndex) {
            nextChar = source[i + 1];
         }
         switch (state) {
         case STATE_FIELDNAME:
            if ('/' == c && '/' == nextChar) {
               prevState = state;
               state = STATE_LINECOMMENT;
            }
            else if ('/' == c && '*' == nextChar) {
               prevState = state;
               state = STATE_COMMENT;
            }
            else if (',' == c || ';' == c) {
               rcList.add(createFieldDoc(fd, fieldNameBuf.toString(), null));
               fieldNameBuf.setLength(0);
            }
            else if ('=' == c) {
               state = STATE_FIELDVALUE;
            }
            else if (!(' ' == c || '\n' == c || '\r' == c || '\t' == c)) {
               fieldNameBuf.append(c);
            }
            break;

         case STATE_FIELDVALUE:
            if ('/' == c && '/' == nextChar) {
               prevState = state;
               state = STATE_LINECOMMENT;
            }
            else if ('/' == c && '*' == nextChar) {
               prevState = state;
               state = STATE_COMMENT;
            }
            else if ('\"' == c) {
               prevState = state;
               state = STATE_QUOTE;
               fieldValueLiteralBuf.append(c);
            }
            else if ('\'' == c) {
               prevState = state;
               state = STATE_SQUOTE;
               fieldValueLiteralBuf.append(c);
            }
            else if ('{' == c || '(' == c) {
               ++ bracketCount;
               fieldValueLiteralBuf.append(c);
            }
            else if ('}' == c || ')' == c) {
               -- bracketCount;
               fieldValueLiteralBuf.append(c);
            }
            else if (0 == bracketCount && (',' == c || ';' == c)) {
               rcList.add(createFieldDoc(fd, fieldNameBuf.toString(),
                                         fieldValueLiteralBuf.toString()));
               fieldNameBuf.setLength(0);
               fieldValueLiteralBuf.setLength(0);
               state = STATE_FIELDNAME;
            }
            else {
               fieldValueLiteralBuf.append(c);
            }
            break;

         case STATE_QUOTE:
            fieldValueLiteralBuf.append(c);
            if ('\\' == c) {
               state = STATE_QUOTEBS;
            }
            else if ('\"' == c) {
               state = prevState;
            }
            break;

         case STATE_SQUOTE:
            fieldValueLiteralBuf.append(c);
            if ('\\' == c) {
               state = STATE_SQUOTEBS;
            }
            else if ('\'' == c) {
               state = prevState;
            }
            break;

         case STATE_QUOTEBS:
            fieldValueLiteralBuf.append(c);
            state = STATE_QUOTE;
            break;

         case STATE_SQUOTEBS:
            fieldValueLiteralBuf.append(c);
            state = STATE_SQUOTE;
            break;

         case STATE_LINECOMMENT:
            if ('\n' == c) {
               state = prevState;
            }
            break;

         case STATE_COMMENT:
            if ('*' == c && '/' == nextChar) {
               ++ i;
               state = prevState;
            }
            break;
         }
      }

      if (fieldNameBuf.length() > 0) {
         rcList.add(createFieldDoc(fd, fieldNameBuf.toString(),
                                   fieldValueLiteralBuf.toString()));
      }

      return rcList;
   }

   public boolean isField() {
      return true;
   }

   public boolean isTransient() { return isTransient; }

   public boolean isVolatile() { return isVolatile; }

   public SerialFieldTag[] serialFieldTags() { return new SerialFieldTag[0]; }

   public int modifierSpecifier() {
      return super.modifierSpecifier()
         | (isVolatile()?Modifier.VOLATILE:0)
         | (isTransient()?Modifier.TRANSIENT:0)
         ;
   }

   protected boolean processModifier(String word) {
      if (super.processModifier(word)) {
         return true;
      }
      else if (word.equals("transient")) {
         isTransient=true;
         return true;
      }
      else if (word.equals("volatile")) {
         isVolatile=true;
         return true;
      }
      else {
         return false;
      }
   }

   void resolve() {
      resolveTags();
   }

   public boolean hasSerialTag() {
      return true; //tagMap.get("serial")!=null;
   }

   public String toString() { return name(); }

   public Object constantValue() {
      return constantValue(new HashSet());
   }

   public Object constantValue(Set<FieldDoc> visitedFields) {
      if (!isStatic()
          || !isFinal()
          || (!type().isPrimitive() && !"java.lang.String".equals(type().qualifiedTypeName()))
          || type.dimension().length()>0
          || null == valueLiteral) {

         return null;

      }
      else {
         if (!constantValueEvaluated) {

            visitedFields.add(this);

            String expression = "(" + type().typeName() + ")(" + valueLiteral + ")";
            try {
               this.constantValue = Evaluator.evaluate(expression,
                                                       visitedFields,
                                                       (ClassDocImpl)containingClass());
            }
            catch (CircularExpressionException e) {
               // FIXME: This should use the error reporter
               System.err.println("WARNING: Cannot resolve expression for field " + containingClass.qualifiedTypeName() + "." + name() + ": " + e.getMessage());
            }
            catch (IllegalExpressionException ignore) {
            }
            constantValueEvaluated = true;
         }
         return this.constantValue;
      }
   }

   private static void appendCharString(StringBuffer result, char c, boolean inSingleCuotes)
   {
      switch (c) {
      case '\b': result.append("\\b"); break;
      case '\t': result.append("\\t"); break;
      case '\n': result.append("\\n"); break;
      case '\f': result.append("\\f"); break;
      case '\r': result.append("\\r"); break;
      case '\"': result.append("\\\""); break;
      case '\'': result.append(inSingleCuotes ? "\\'" : "'"); break;
      default:
         if (c >= 32 && c <= 127) {
            result.append(c);
         }
         else {
            result.append("\\u");
            String hexValue = Integer.toString((int)c, 16);
            int zeroCount = 4 - hexValue.length();
            for (int i=0; i<zeroCount; ++i) {
               result.append('0');
            }
            result.append(hexValue);
         }
      }
   }

   public String constantValueExpression() {
      Object value = constantValue();

      if (null == value) {
         return "null";
      }
      else if (value instanceof String) {
         StringBuffer result = new StringBuffer("\"");
         char[] chars = ((String)value).toCharArray();
         for (int i=0; i<chars.length; ++i) {
            appendCharString(result, chars[i], false);
         }
         result.append("\"");
         return result.toString();
      }
      else if (value instanceof Float) {
         return value.toString() + "f";
      }
      else if (value instanceof Long) {
         return value.toString() + "L";
      }
      else if (value instanceof Character) {
         StringBuffer result = new StringBuffer("'");
         appendCharString(result, ((Character)value).charValue(), false);
         result.append("'");
         return result.toString();
      }
      else /* if (value instanceof Double
               || value instanceof Integer
               || value instanceof Short
               || value instanceof Byte) */ {
         return value.toString();
      }
   }

   void setValueLiteral(String valueLiteral)
   {
      this.valueLiteral = valueLiteral;
   }

   public boolean isStatic()
   {
      return super.isStatic() || containingClass().isInterface();
   }

   public boolean isFinal()
   {
      return super.isFinal() || containingClass().isInterface();
   }
}
