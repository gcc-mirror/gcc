/* Keywords.java - List of C++ keywords
 Copyright (C) 2006 Free Software Foundation, Inc.

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
 Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 02110-1301 USA.

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


package gnu.classpath.tools.javah;

import java.util.HashSet;

public class Keywords
{
/* A sorted list of all C++ keywords.  This is identical to the list
   in gcc/java/mangle.c.  */
  private static final String[] words =
    {
      "_Complex",
      "__alignof",
      "__alignof__",
      "__asm",
      "__asm__",
      "__attribute",
      "__attribute__",
      "__builtin_va_arg",
      "__complex",
      "__complex__",
      "__const",
      "__const__",
      "__extension__",
      "__imag",
      "__imag__",
      "__inline",
      "__inline__",
      "__label__",
      "__null",
      "__real",
      "__real__",
      "__restrict",
      "__restrict__",
      "__signed",
      "__signed__",
      "__typeof",
      "__typeof__",
      "__volatile",
      "__volatile__",
      "and",
      "and_eq",
      "asm",
      "auto",
      "bitand",
      "bitor",
      "bool",
      "break",
      "case",
      "catch",
      "char",
      "class",
      "compl",
      "const",
      "const_cast",
      "continue",
      "default",
      "delete",
      "do",
      "double",
      "dynamic_cast",
      "else",
      "enum",
      "explicit",
      "export",
      "extern",
      "false",
      "float",
      "for",
      "friend",
      "goto",
      "if",
      "inline",
      "int",
      "long",
      "mutable",
      "namespace",
      "new",
      "not",
      "not_eq",
      "operator",
      "or",
      "or_eq",
      "private",
      "protected",
      "public",
      "register",
      "reinterpret_cast",
      "return",
      "short",
      "signed",
      "sizeof",
      "static",
      "static_cast",
      "struct",
      "switch",
      "template",
      "this",
      "throw",
      "true",
      "try",
      "typedef",
      "typeid",
      "typename",
      "typeof",
      "union",
      "unsigned",
      "using",
      "virtual",
      "void",
      "volatile",
      "wchar_t",
      "while",
      "xor",
      "xor_eq"
    };

  private static final HashSet<String> keywords;
  static
    {
      keywords = new HashSet<String>();
      for (int i = 0; i < words.length; ++i)
        keywords.add(words[i]);
    }

  public static String getCxxName(String name)
  {
    int i;
    for (i = name.length() - 1; i >= 0 && name.charAt(i) == '$'; --i)
      ;
    if (keywords.contains(name.substring(0, i + 1)))
      return name + "$";
    return name;
  }
}
