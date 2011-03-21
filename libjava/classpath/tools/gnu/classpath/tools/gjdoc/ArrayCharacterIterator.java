/* gnu.classpath.tools.gjdoc.ArrayCharacterIterator
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

import java.text.CharacterIterator;

public final class ArrayCharacterIterator implements CharacterIterator {

   private char[] data;
   private int beginIndex;
   private int endIndex;
   private int currentIndex;

   public ArrayCharacterIterator(char[] data,
                                 int beginIndex) {
      this(data,beginIndex,data.length,beginIndex);
   }

   public ArrayCharacterIterator(char[] data,
                                 int beginIndex,
                                 int endIndex) {
      this(data,beginIndex,endIndex,beginIndex);
   }

   public ArrayCharacterIterator(char[] data,
                                 int beginIndex,
                                 int endIndex,
                                 int currentIndex) {
      this.data=data;
      this.beginIndex=beginIndex;
      this.endIndex=endIndex;
      this.currentIndex=currentIndex;
   }

   // Create a copy of this iterator
   public Object clone() {
      return new ArrayCharacterIterator(data,beginIndex,endIndex,currentIndex);
   }

   // Gets the character at the current position (as returned by getIndex()).
   public char current() {
      return (currentIndex>=beginIndex && currentIndex<endIndex) ? data[currentIndex] : DONE;
   }

   // Sets the position to getBeginIndex() and returns the character at that position.
   public char first() {
      return data[currentIndex=beginIndex];
   }

   // Returns the start index of the text.
   public int getBeginIndex() {
      return beginIndex;
   }

   // Returns the end index of the text.
   public int getEndIndex() {
      return endIndex;
   }

   // Returns the current index.
   public int getIndex() {
      return currentIndex;
   }

   // Sets the position to getEndIndex()-1 (getEndIndex() if the text is empty) and returns the character at that position.
   public char last() {
      return data[currentIndex=((endIndex>beginIndex)?endIndex-1:endIndex)];
   }

   // Increments the iterator's index by one and returns the character at the new index.
   public char next() {
      return (++currentIndex<endIndex)?data[currentIndex]:DONE;
   }

   // Decrements the iterator's index by one and returns the character at the new index.
   public char previous() {
      return (--currentIndex>=beginIndex)?data[currentIndex]:DONE;
   }

   // Sets the position to the specified position in the text and returns that character.
   public char setIndex(int position) {
      this.currentIndex=position;
      return current();
   }

}
