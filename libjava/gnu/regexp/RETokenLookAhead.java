/*
 *  gnu/regexp/RETokenOneOf.java
 *  Copyright (C) 1998-2001 Wes Biggs
 *
 *  This library is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published
 *  by the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */
package gnu.regexp;

/**
 * @since gnu.regexp 1.1.3
 * @author Shashank Bapat
 */
final class RETokenLookAhead extends REToken
{
  REToken re;
  boolean negative;

  RETokenLookAhead(REToken re, boolean negative) throws REException {
    super(0);
    this.re = re;
    this.negative = negative;
  }

  boolean match(CharIndexed input, REMatch mymatch)
  {
    REMatch trymatch = (REMatch)mymatch.clone();
    REMatch trymatch1 = (REMatch)mymatch.clone();
    REMatch newMatch = null;
    if (re.match(input, trymatch)) {
      if (negative) return false;
      if (next(input, trymatch1))
        newMatch = trymatch1;
    }

    if (newMatch != null) {
      if (negative) return false;
      //else
      mymatch.assignFrom(newMatch);
      return true;
    }
    else { // no match
      if (negative)
        return next(input, mymatch);
      //else
      return false;
    }
  }

    void dump(StringBuffer os) {
	os.append("(?");
	os.append(negative ? '!' : '=');
	re.dumpAll(os);
	os.append(')');
    }
}

