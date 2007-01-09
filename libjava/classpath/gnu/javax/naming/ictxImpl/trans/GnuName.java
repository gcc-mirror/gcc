/* GnuName.java -- implementation of the javax.naming.Name
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

package gnu.javax.naming.ictxImpl.trans;

import java.util.Enumeration;
import java.util.NoSuchElementException;

import javax.naming.InvalidNameException;
import javax.naming.Name;

/**
 * The implementation of the {@link Name}.
 * 
 * @author Audrius Meskauskas
 */
public class GnuName
    implements Name
{
  /**
   * The enumeration to traverse over name components.
   */
  class GnuNameEnum
      implements Enumeration
  {
    /**
     * Get the new enumeration that enumerates from the given position forward
     * 
     * @param position the position of the first name component to enumerate (0
     *          means first element)
     */
    GnuNameEnum(int position)
    {
      nxt = from + position;
    }

    /**
     * The position of the next enumeration component to be returned or -1 if
     * the end has been reached.
     */
    int nxt;

    /**
     * Check if there are more elements in this enumeration.
     */
    public boolean hasMoreElements()
    {
      return nxt >= 0;
    }

    /**
     * Return the next element or throw a NoSuchElementException if there is no
     * any.
     */
    public Object nextElement()
    {
      if (nxt < 0)
        throw new NoSuchElementException();
      Object r = content[nxt++];

      if (nxt - from == length)
        nxt = - 1;

      return r;
    }
  }

  private static final long serialVersionUID = - 3617482732056931635L;

  /**
   * The hashcode
   */
  int hash;

  /**
   * The content buffer of the name. This buffer may be shared, so the array
   * member content should never be modified.
   */
  String[] content;

  /**
   * The place, inclusive, where the name content starts in the content buffer.
   */
  int from;

  /**
   * The length of the name.
   */
  int length;

  /**
   * Creates the unitialised name.
   */
  protected GnuName()
  {

  }

  /**
   * Creates the name, containing from the given chain of the atomic components.
   * 
   * @param name the array, containing the name components.
   */
  public GnuName(String[] name)
  {
    this(name, 0, name.length);
  }

  /**
   * Creates the name that uses the given portion of the array for its
   * components.
   */
  public GnuName(String[] buffer, int useFrom, int useLength)
  {
    content = buffer;
    from = useFrom;
    length = useLength;
  }

  /**
   * Inserts the given <code>String</code> component to this <code>Name</code>
   * at the given index. The method modifies the current <code>Name</code> and
   * then returns it.
   * 
   * @exception ArrayIndexOutOfBoundsException if the given index is smaller
   *              then zero or greater then or equal to <code>size()</code>.
   * @exception InvalidNameException if the given <code>String</code> is not a
   *              valid component for this <code>Name</code>.
   */
  public Name add(int posn, String comp) throws InvalidNameException
  {
    String[] nc = new String[content.length + 1];
    System.arraycopy(content, from, nc, 0, posn);
    nc[posn] = comp;
    System.arraycopy(content, from + posn, nc, posn + 1, length - posn);

    content = nc;
    from = 0;
    length = content.length;
    hash = 0;
    return this;
  }

  /**
   * Adds the given <code>String</code> component to the end of this
   * <code>Name</code>. The method modifies the current <code>Name</code>
   * and then returns it.
   * 
   * @exception InvalidNameException if the given <code>String</code> is not a
   *              valid component for this <code>Name</code>.
   */
  public Name add(String comp) throws InvalidNameException
  {
    String[] nc = new String[content.length + 1];
    System.arraycopy(content, from, nc, 0, length);
    nc[nc.length - 1] = comp;

    content = nc;
    from = 0;
    length = content.length;
    hash = 0;
    return this;
  }

  /**
   * Inserts all the components of the given <code>Name</code> to this
   * <code>Name</code> at the given index. Components after this index (if
   * any) are shifted up. The method modifies the current <code>Name</code>
   * and then returns it.
   * 
   * @exception ArrayIndexOutOfBoundsException if the given index is smaller
   *              then zero or greater then or equal to <code>size()</code>.
   * @exception InvalidNameException if any of the given components is not a
   *              valid component for this <code>Name</code>.
   */
  public Name addAll(int posn, Name n) throws InvalidNameException
  {
    String[] nc = new String[length + n.size()];
    System.arraycopy(content, from, nc, 0, posn);

    int i = posn;
    for (int p = 0; p < n.size(); i++, p++)
      nc[i] = n.get(p);

    System.arraycopy(content, from + posn, nc, i, length - posn);

    length = length + n.size();
    hash = 0;
    content = nc;
    return this;
  }

  /**
   * Adds all the components of the given <code>Name</code> to the end of this
   * <code>Name</code>. The method modifies the current <code>Name</code>
   * and then returns it.
   * 
   * @exception InvalidNameException if any of the given components is not a
   *              valid component for this <code>Name</code>.
   */
  public Name addAll(Name suffix) throws InvalidNameException
  {
    String[] nc = new String[length + suffix.size()];
    System.arraycopy(content, from, nc, 0, length);

    for (int i = length, p = 0; i < nc.length; i++, p++)
      nc[i] = suffix.get(p);

    length = length + suffix.size();
    hash = 0;
    content = nc;
    return this;
  }

  /**
   * Compares the given object to this <code>Name</code>. Returns a negative
   * value if the given <code>Object</code> is smaller then this
   * <code>Name</code>, a positive value if the <code>Object</code> is
   * bigger, and zero if the are equal. If the <code>Object</code> is not of a
   * class that can be compared to the class of this <code>Name</code> then a
   * <code>ClassCastException</code> is thrown. Note that it is not guaranteed
   * that <code>Name</code>s implemented in different classes can be
   * compared. The definition of smaller, bigger and equal is up to the actual
   * implementing class.
   */
  public int compareTo(Object obj)
  {
    Name n = (Name) obj;

    int l = Math.min(length, n.size());
    int c;

    for (int i = 0; i < l; i++)
      {
        c = content[from + i].compareTo(n.get(i));
        if (c != 0)
          return c;
      }
    return length - n.size();
  }

  /**
   * Returns <code>true</code> if this <code>Name</code> ends with the
   * components of the given <code>Name</code>, <code>false</code>
   * otherwise.
   */
  public boolean endsWith(Name n)
  {
    if (n.size() > length)
      return false;

    int ofs = length - n.size() + from;

    for (int i = 0; i < n.size(); i++, ofs++)
      if (! content[ofs].equals(n.get(i)))
        return false;

    return true;
  }

  /**
   * Gets the component at the given index.
   * 
   * @exception ArrayIndexOutOfBoundsException if the given index is smaller
   *              then zero or greater then or equal to <code>size()</code>.
   */
  public String get(int posn)
  {
    return content[from + posn];
  }

  /**
   * Returns a non-null (but possibly empty) <code>Enumeration</code> of the
   * components of the <code>Name</code> as <code>String</code>s.
   */
  public Enumeration getAll()
  {
    return new GnuNameEnum(0);
  }

  /**
   * Returns the components till the given index as a <code>Name</code>. The
   * returned <code>Name</code> can be modified without changing the original.
   * 
   * @param posn the ending position, exclusive
   * @exception ArrayIndexOutOfBoundsException if the given index is smaller
   *              then zero or greater then or equal to <code>size()</code>.
   */
  public Name getPrefix(int posn)
  {
    return new GnuName(content, from, posn);
  }

  /**
   * Returns the components from the given index till the end as a
   * <code>Name</code>. The returned <code>Name</code> can be modified
   * without changing the original.
   * 
   * @param posn the starting position, inclusive. If it is equal to the size of
   *          the name, the empty name is returned.
   * @exception ArrayIndexOutOfBoundsException if the given index is smaller
   *              then zero or greater then or equal to <code>size()</code>.
   */
  public Name getSuffix(int posn)
  {
    return new GnuName(content, from + posn, length - posn);
  }

  /**
   * Returns <code>true</code> if the number of components of this
   * <code>Name</code> is zero, <code>false</code> otherwise.
   */
  public boolean isEmpty()
  {
    return length == 0;
  }

  /**
   * Removes the component at the given index from this <code>Name</code>.
   * The method modifies the current <code>Name</code> and then returns it.
   * 
   * @exception InvalidNameException if the name size reduces below zero.
   */
  public Object remove(int posn) throws InvalidNameException
  {
    if (length == 0)
      throw new InvalidNameException("negative size");
    else
      {
        length--;
        if (posn == 0)
          from++;
        else if (posn < length)
          {
            String[] nc = new String[length];
            System.arraycopy(content, from, nc, 0, posn);
            System.arraycopy(content, from + posn + 1, nc, posn, length - posn);
            content = nc;
            from = 0;
          }
      }
    hash = 0;
    return this;
  }

  /**
   * Returns the number of components of this <code>Name</code>. The returned
   * number can be zero.
   */
  public int size()
  {
    return length;
  }

  /**
   * Returns <code>true</code> if this <code>Name</code> starts with the
   * components of the given <code>Name</code>, <code>false</code>
   * otherwise.
   */
  public boolean startsWith(Name n)
  {
    if (n.size() > length)
      return false;

    for (int i = 0; i < n.size(); i++)
      if (! content[from + i].equals(n.get(i)))
        return false;

    return true;
  }

  /**
   * Returns a clone of this <code>Name</code>. It will be a deep copy of all
   * the components of the <code>Name</code> so that changes to components of
   * the components does not change the component in this <code>Name</code>.
   */
  public Object clone()
  {
    return new GnuName(content, from, length);
  }

  /**
   * The name is equal to other name if they contents are equal.
   */
  public boolean equals(Object arg0)
  {
    if (this == arg0)
      return true;
    else if (arg0 instanceof Name)
      {
        Name n = (Name) arg0;
        if (length != n.size())
          return false;

        for (int i = 0; i < length; i++)
          if (! content[from + i].equals(n.get(i)))
            return false;
        return true;
      }
    else
      return false;
  }

  /**
   * Overridden to make consistent with equals.
   */
  public int hashCode()
  {
    if (hash == 0 && length > 0)
      {
        int s = 0;
        for (int i = from; i < from + length; i++)
          s ^= content[i].hashCode();
        hash = s;
      }
    return hash;
  }

  /**
   * Get the string representation, separating the name components by slashes
   */
  public String toString()
  {
    StringBuffer b = new StringBuffer();
    for (int i = 0; i < length; i++)
      {
        b.append(get(i));
        if (i < length - 1)
          b.append('/');
      }
    return b.toString();
  }
}
