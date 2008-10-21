/* gnu.classpath.tools.doclets.DocletOptionFlag
   Copyright (C) 2004 Free Software Foundation, Inc.

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

package gnu.classpath.tools.doclets;

/**
 *  Processes a doclet option without a value whose existance denotes
 *  that a specific feature should be enabled or disabled.
 */
public class DocletOptionFlag
   extends DocletOption
{
   private boolean value = false;

   public DocletOptionFlag(String optionName)
   {
      super(optionName);
   }

   public boolean getValue()
   {
      return this.value;
   }

   public int getLength()
   {
      return 1;
   }

   public boolean set(String[] optionArr)
   {
      value = true;
      return true;
   }
}
