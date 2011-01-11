/* HashFinder.java -- finds the hash character.
   Copyright (C) 2006 Free Software Foundation

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


package gnu.classpath.tools.rmic;

import java.util.HashSet;

/**
 * This class finds the hash character (the most different character in
 * the passed array of strings). This character is used to accelerate the
 * method invocation by name.
 *
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class HashFinder
{
   /**
   * Find the hash char position in the given collection of strings.
   *
   * @param strings the string collection
   *
   * @return the optimal hash character position, always less then the
   * length of the shortest string.
   */
  public int findHashCharPosition(String[] strings)
  {
    // Find the length of the shortest string:

    int l = strings[0].length();
    for (int i = 1; i < strings.length; i++)
      {
        if (strings[i].length() < l)
          l = strings[i].length();
      }

    // Find the position with the smallest number of the matching characters:
    HashSet[] charLists = new HashSet[l];

    for (int i = 0; i < charLists.length; i++)
      {
        charLists[i] = new HashSet(strings.length);
      }

    for (int i = 0; i < strings.length; i++)
      for (int p = 0; p < l; p++)
        {
          charLists[p].add(new Integer(strings[i].charAt(p)));
        }

    int m = 0;
    int v = charLists[0].size();

    for (int i = 1; i < charLists.length; i++)
      {
        // Replace on equality also, seeking the hash char closer to the end
        // of line.
        if (charLists[i].size()>=v)
          {
            m = i;
            v = charLists[i].size();
          }
      }
    return m;
  }
}
