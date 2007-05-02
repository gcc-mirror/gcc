/* java.lang.Character -- Wrapper class for char, and Unicode subsets
   Copyright (C) 1998, 1999, 2001, 2002, 2007 Free Software Foundation, Inc.

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

#include <config.h>

#include <gcj/cni.h>
#include <jvm.h>
#include <java/lang/Character.h>

#include <java-chartables.h>



// These constants define the return values for characters that are unassigned
// or reserved for private use.
#define UNASSIGNED_TYPE 0
#define UNASSIGNED_DIGIT -1
#define UNASSIGNED_DIRECTION -1
#define UNASSIGNED_NUMERIC_VALUE -1

#define PRIVATE_TYPE 18
#define PRIVATE_DIRECTION 0

// The methods that take a char as an argument all have counterparts that 
// take ints.  The ones that take chars only work for the BMP or plane 0 of the
// Unicode standard but the ones that take ints work for all Unicode code
// points.  However, the ones that take chars don't simply redirect the calls
// because the BMP is by far the most used plane so saving a little time on
// each call makes sense.

jchar
java::lang::Character::readChar(jchar ch)
{
  // Perform 16-bit addition to find the correct entry in data.
  return data[0][(jchar) (blocks[0][ch >> shift[0]] + ch)];
}

jchar
java::lang::Character::readCodePoint(jint codePoint)
{
  jint plane = codePoint >> 16;
  jchar offset = (jchar)(codePoint & 0xffff);
  // Be careful not to call this method with an unassigned character.  The only
  // characters assigned as of Unicode 4.0.0 belong to planes 0, 1, 2, and 14.
  return data[plane][(jchar) (blocks[plane][offset >> shift[plane]] + offset)];
}

jint
java::lang::Character::getType(jchar ch)
{
  // Perform 16-bit addition to find the correct entry in data.
  return (jint) (data[0][(jchar) (blocks[0][ch >> shift[0]] + ch)] & TYPE_MASK);
}

jint
java::lang::Character::getType(jint codePoint)
{
  jint plane = codePoint >> 16;
  if (plane < 0 || (plane > 2 && plane != 14))
    {
      if (plane > 14 && ((codePoint & 0xffff) < 0xfffe))
        return (jint) PRIVATE_TYPE;
      return (jint) UNASSIGNED_TYPE;
    }
  jint offset = codePoint & 0xffff;
  return (jint) 
    (data[plane]
     [(jchar) (blocks[plane][offset >> shift[plane]] + offset)] & TYPE_MASK);
}

jchar
java::lang::Character::toLowerCase(jchar ch)
{
  return (jchar) (ch + lower[0][readChar(ch) >> 7]);
}

jint
java::lang::Character::toLowerCase(jint codePoint)
{
  jint plane = codePoint >> 16;
  if (plane < 0 || (plane > 2 && plane != 14))
    return codePoint;
  return (lower[plane][readCodePoint(codePoint) >> 7]) + codePoint;
}

jchar
java::lang::Character::toUpperCase(jchar ch)
{
  return (jchar) (ch + upper[0][readChar(ch) >> 7]);
}

jint
java::lang::Character::toUpperCase(jint codePoint)
{
  jint plane = codePoint >> 16;
  if (plane < 0 || (plane > 2 && plane != 14))
    return codePoint;
  return (upper[plane][readCodePoint(codePoint) >> 7]) + codePoint;
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
java::lang::Character::toTitleCase(jint codePoint)
{
  // As of Unicode 4.0.0 no characters outside of plane 0 have titlecase
  // mappings that are different from their uppercase mapping.
  if (codePoint >= 0 && codePoint < 0x10000)
    return toTitleCase((jchar)codePoint);
  return toUpperCase(codePoint);
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
      jint digit = (jint) numValue[0][attr >> 7];
      return (digit >= 0 && digit < radix) ? digit : (jint) -1;
    }
  return (jint) -1;
}

jint
java::lang::Character::digit(jint codePoint, jint radix)
{
  if (radix < MIN_RADIX || radix > MAX_RADIX)
    return (jint) -1;

  jint plane = codePoint >> 16;
  if (plane < 0 || (plane > 2 && plane != 14))
    return UNASSIGNED_DIGIT;

  jchar attr = readCodePoint(codePoint);
  if (((1 << (attr & TYPE_MASK))
       & ((1 << UPPERCASE_LETTER)
          | (1 << LOWERCASE_LETTER)
          | (1 << DECIMAL_DIGIT_NUMBER))))
    {
      // Signedness doesn't matter; 0xffff vs. -1 are both rejected.
      jint digit = (jint) numValue[plane][attr >> 7];
      if (digit <= -3)
        digit = largenums[-digit -3];
      return (digit >= 0 && digit < radix) ? digit : (jint) -1;
    }
  return (jint) -1;

}

jint
java::lang::Character::getNumericValue(jchar ch)
{
  // numValue is stored as an array of jshort, since 10000 is the maximum.
  return (jint) numValue[0][readChar(ch) >> 7];
}

jint
java::lang::Character::getNumericValue(jint codePoint)
{
  jint plane = codePoint >> 16;
  if (plane < 0 || (plane > 2 && plane != 14))
    return UNASSIGNED_NUMERIC_VALUE;
  jshort num = numValue[plane][readCodePoint(codePoint) >> 7];
  if (num <= -3)
    return largenums[-num - 3];
  return num;
}

jbyte
java::lang::Character::getDirectionality(jchar ch)
{
  return direction[0][readChar(ch) >> 7];
}

jbyte
java::lang::Character::getDirectionality(jint codePoint)
{
  jint plane = codePoint >> 16;
  if (plane < 0 || (plane > 2 && plane != 14))
    {
      if (plane > 14 && ((codePoint & 0xffff) < 0xfffe))
        return (jint) PRIVATE_DIRECTION;
      return (jint) UNASSIGNED_DIRECTION;
    }
  return direction[plane][readCodePoint(codePoint) >> 7];
}
