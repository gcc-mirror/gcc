/* PrimitiveEntry.java -- 
   Copyright (C) 2003, 2006 Free Software Foundation, Inc.

This file is a part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
USA

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
exception statement from your version.  */


package gnu.javax.crypto.keyring;

import java.util.Date;

/**
 * A primitive entry is an entry that contains a single cryptographic entity.
 */
public abstract class PrimitiveEntry
    extends Entry
{
  /** The creation date. */
  protected Date creationDate;

  protected PrimitiveEntry(int type, Date creationDate, Properties properties)
  {
    super(type, properties);
    if (creationDate == null)
      this.creationDate = new Date();
    else
      this.creationDate = (Date) creationDate.clone();
    if (! this.properties.containsKey("alias")
        || this.properties.get("alias").length() == 0)
      throw new IllegalArgumentException("primitive entries MUST have an alias");
    this.properties.put("creation-date",
                        String.valueOf(this.creationDate.getTime()));
  }

  protected PrimitiveEntry(int type)
  {
    super(type);
  }

  /**
   * Returns the alias of this primitive entry.
   * 
   * @return The alias.
   */
  public String getAlias()
  {
    return properties.get("alias");
  }

  /**
   * Returns the creation date of this primitive entry.
   * 
   * @return The creation date.
   */
  public Date getCreationDate()
  {
    return (Date) creationDate.clone();
  }

  public boolean equals(Object object)
  {
    if (! getClass().equals(object.getClass()))
      return false;
    return getAlias().equals(((PrimitiveEntry) object).getAlias());
  }

  protected final void makeCreationDate() throws MalformedKeyringException
  {
    String s = properties.get("creation-date");
    if (s == null)
      throw new MalformedKeyringException("no creation date");
    try
      {
        creationDate = new Date(Long.parseLong(s));
      }
    catch (NumberFormatException nfe)
      {
        throw new MalformedKeyringException("invalid creation date");
      }
  }
}
