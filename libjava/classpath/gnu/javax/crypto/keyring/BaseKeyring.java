/* BaseKeyring.java -- 
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

import gnu.java.security.Registry;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Enumeration;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;

public abstract class BaseKeyring
    implements IKeyring
{
  /** The top-level keyring data. */
  protected PasswordAuthenticatedEntry keyring;
  protected CompressedEntry keyring2;

  public BaseKeyring()
  {
  }

  public void load(Map attributes) throws IOException
  {
    InputStream in = (InputStream) attributes.get(KEYRING_DATA_IN);
    if (in == null)
      throw new IllegalArgumentException("no input stream");
    char[] password = (char[]) attributes.get(KEYRING_PASSWORD);
    if (password == null)
      password = new char[0];

    if (in.read() != Registry.GKR_MAGIC[0]
        || in.read() != Registry.GKR_MAGIC[1]
        || in.read() != Registry.GKR_MAGIC[2]
        || in.read() != Registry.GKR_MAGIC[3])
      throw new MalformedKeyringException("magic");

    load(in, password);
    List l = keyring.getEntries();
    if (l.size() == 1 && (l.get(0) instanceof CompressedEntry))
      keyring2 = (CompressedEntry) l.get(0);
  }

  public void store(Map attributes) throws IOException
  {
    OutputStream out = (OutputStream) attributes.get(KEYRING_DATA_OUT);
    if (out == null)
      throw new IllegalArgumentException("no output stream");
    char[] password = (char[]) attributes.get(KEYRING_PASSWORD);
    if (password == null)
      password = new char[0];
    if (keyring == null)
      throw new IllegalStateException("empty keyring");

    out.write(Registry.GKR_MAGIC);
    store(out, password);
  }

  public void reset()
  {
    keyring = null;
  }

  public int size()
  {
    if (keyring == null)
      throw new IllegalStateException("keyring not loaded");
    return ((StringTokenizer) aliases()).countTokens();
  }

  public Enumeration aliases()
  {
    if (keyring == null)
      throw new IllegalStateException("keyring not loaded");
    return new StringTokenizer(keyring.getAliasList(), ";");
  }

  public boolean containsAlias(String alias)
  {
    if (keyring == null)
      throw new IllegalStateException("keyring not loaded");
    return keyring.containsAlias(alias);
  }

  public List get(String alias)
  {
    if (keyring == null)
      throw new IllegalStateException("keyring not loaded");
    return keyring.get(alias);
  }

  public void add(Entry entry)
  {
    if (keyring == null)
      throw new IllegalStateException("keyring not loaded");
    if (keyring2 != null)
      keyring2.add(entry);
    else
      keyring.add(entry);
  }

  public void remove(String alias)
  {
    if (keyring == null)
      throw new IllegalStateException("keyring not loaded");
    keyring.remove(alias);
  }

  protected String fixAlias(String alias)
  {
    return alias.replace(';', '_');
  }

  protected abstract void load(InputStream in, char[] password)
      throws IOException;

  protected abstract void store(OutputStream out, char[] password)
      throws IOException;
}
