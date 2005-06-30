/* gnu/regexp/RETokenChar.java
   Copyright (C) 1998-2001, 2004 Free Software Foundation, Inc.

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


package gnu.regexp;

final class RETokenChar extends REToken {
  private char[] ch;
  private boolean insens;

  RETokenChar(int subIndex, char c, boolean ins) {
    super(subIndex);
    ch = new char [1];
    ch[0] = (insens = ins) ? Character.toLowerCase(c) : c;
  }

  int getMinimumLength() {
    return ch.length;
  }
  
    boolean match(CharIndexed input, REMatch mymatch) {
	int z = ch.length;
	char c;
	for (int i=0; i<z; i++) {
	    c = input.charAt(mymatch.index+i);
	    if (( (insens) ? Character.toLowerCase(c) : c ) != ch[i]) {
		return false;
	    }
	}
	mymatch.index += z;

	return next(input, mymatch);
    }

  // Overrides REToken.chain() to optimize for strings
  boolean chain(REToken next) {
    if (next instanceof RETokenChar) {
      RETokenChar cnext = (RETokenChar) next;
      // assume for now that next can only be one character
      int newsize = ch.length + cnext.ch.length;
      
      char[] chTemp = new char [newsize];
      
      System.arraycopy(ch,0,chTemp,0,ch.length);
      System.arraycopy(cnext.ch,0,chTemp,ch.length,cnext.ch.length);
      
      ch = chTemp;
      return false;
    } else return super.chain(next);
  }

  void dump(StringBuffer os) {
    os.append(ch);
  }
}


