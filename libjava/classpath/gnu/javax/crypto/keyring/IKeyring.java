/* IKeyring.java --
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

import java.io.IOException;
import java.util.Enumeration;
import java.util.List;
import java.util.Map;

/**
 * The top-level interface to a <i>keyring:</i> a file that is used to store
 * and protect public and private cryptographic keys.
 * <p>
 * A <i>keyring</i> is modelled as a mapping of one <i>alias</i> to one or
 * more <i>entries</i> (optionally of different types).
 * <p>
 * See also the sub-interfaces {@link IPublicKeyring} and
 * {@link IPrivateKeyring} for special types of <i>keyrings</i> --the
 * difference being in the type of entries they contain.
 */
public interface IKeyring
{
  /**
   * Property name for the source of data to load the keyring from. The value
   * mapped must be a {@link java.io.InputStream}.
   */
  public static final String KEYRING_DATA_IN = "gnu.crypto.keyring.data.in";

  /**
   * Property name for the data sink to store the keyring to. The value mapped
   * must be a {@link java.io.OutputStream}.
   */
  public static final String KEYRING_DATA_OUT = "gun.crypto.keyring.data.out";

  /**
   * Property name for the keyring's top-level password, used to authenticate
   * and/or transform the store itself. The mapped value must be a char array.
   */
  public static final String KEYRING_PASSWORD = "gnu.crypto.keyring.password";

  /**
   * Loads a keyring into memory.
   * <p>
   * What happens to the current contents of this keyring? are the new ones
   * merged with the current ones or do they simply replace them?
   *
   * @param attributes The attributes that designate the source where the store
   *          is to be loaded from. What happens
   * @throws IllegalArgumentException If the attributes are inappropriate.
   * @throws IOException If the keyring file cannot be read.
   * @throws SecurityException If the given password is incorrect, or if the
   *           top-level authentication or decryption fails.
   */
  void load(Map attributes) throws IOException;

  /**
   * Stores the contents of this keyring to persistent storage as specified by
   * the designated <code>attributes</code>.
   *
   * @param attributes the attributes that define where the contents of this
   *          keyring will be stored.
   * @throws IOException if an exception occurs during the process.
   */
  void store(Map attributes) throws IOException;

  /**
   * Resets this keyring, clearing all sensitive data. This method always
   * suceeds.
   */
  void reset();

  /**
   * Returns the number of entries in this keyring.
   *
   * @return The number of current entries in this keyring.
   */
  int size();

  /**
   * Returns an {@link Enumeration} of all aliases (instances of {@link String})
   * in this keyring.
   *
   * @return The enumeration of {@link String}s each representing an <i>alias</i>
   *         found in this keyring.
   */
  Enumeration aliases();

  /**
   * Tests whether or not this keyring contains the given alias.
   *
   * @param alias The alias to check.
   * @return true if this keyring contains the alias.
   */
  boolean containsAlias(String alias);

  /**
   * Returns a {@link List} of entries (instances of {@link Entry}) for the
   * given <code>alias</code>, or <code>null</code> if there no such entry
   * exists.
   *
   * @param alias The alias of the entry(ies) to return.
   * @return A list of all entries (instances of {@link Entry} that have the
   *         given <code>alias</code>, or <code>null</code> if no one
   *         {@link Entry} can be found with the designated <code>alias</code>.
   */
  List get(String alias);

  /**
   * Adds a designated {@link Entry} to this keyring.
   * <p>
   * What happens if there is already an entry with the same alias?
   *
   * @param entry The entry to put in this keyring.
   */
  void add(Entry entry);

  /**
   * Removes an entry with the designated <code>alias</code> from this
   * keyring. Does nothing if there was no such entry.
   * <p>
   * What happens if there are more than one?
   *
   * @param alias The alias of the entry to remove.
   */
  void remove(String alias);
}
