/* java.lang.Character -- Wrapper class for char, and Unicode subsets
   Copyright (C) 1998, 1999, 2001, 2002 Free Software Foundation, Inc.

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

#include <config.h>

#include <gcj/cni.h>
#include <jvm.h>
#include <java/lang/Character.h>

#include <java-chartables.h>



jchar
java::lang::Character::readChar(jchar ch)
{
  // Perform 16-bit addition to find the correct entry in data.
  return data[(jchar) (blocks[ch >> SHIFT] + ch)];
}

jint
java::lang::Character::getType(jchar ch)
{
  // Perform 16-bit addition to find the correct entry in data.
  return (jint) (data[(jchar) (blocks[ch >> SHIFT] + ch)] & TYPE_MASK);
}

jchar
java::lang::Character::toLowerCase(jchar ch)
{
  return (jchar) (ch + lower[readChar(ch) >> 7]);
}

jchar
java::lang::Character::toUpperCase(jchar ch)
{
  return (jchar) (ch + upper[readChar(ch) >> 7]);
}

jchar
java::lang::Character::toTitleCase(jchar ch)
{
  // As title is short, it doesn't hurt to exhaustively iterate over it.
  for (int i = title_length - 2; i >= 0; i -= 2)
    if (title[i] == ch)
      return title[i + 1];
  return toUpperCase(ch);
}

jint
java::lang::Character::digit(jchar ch, jint radix)
{
  if (radix < MIN_RADIX || radix > MAX_RADIX)
    return (jint) -1;
  jchar attr = readChar(ch);
  if (((1 << (attr & TYPE_MASK))
       & ((1 << UPPERCASE_LETTER)
          | (1 << LOWERCASE_LETTER)
          | (1 << DECIMAL_DIGIT_NUMBER))))
    {
      // Signedness doesn't matter; 0xffff vs. -1 are both rejected.
      jint digit = (jint) numValue[attr >> 7];
      return (digit >= 0 && digit < radix) ? digit : (jint) -1;
    }
  return (jint) -1;
}

jint
java::lang::Character::getNumericValue(jchar ch)
{
  // numValue is stored as an array of jshort, since 10000 is the maximum.
  return (jint) numValue[readChar(ch) >> 7];
}

jbyte
java::lang::Character::getDirectionality(jchar ch)
{
  return direction[readChar(ch) >> 7];
}
