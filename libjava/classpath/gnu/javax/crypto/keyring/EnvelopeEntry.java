/* EnvelopeEntry.java -- 
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

import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.StringTokenizer;

/**
 * An envelope entry is a generic container for some number of primitive
 * and other envelope entries.
 */
public abstract class EnvelopeEntry extends Entry
{

  // Fields.
  // ------------------------------------------------------------------------

  /** The envelope that contains this one (if any). */
  protected EnvelopeEntry containingEnvelope;

  /** The contained entries. */
  protected List entries;

  // Constructor.
  // ------------------------------------------------------------------------

  public EnvelopeEntry(int type, Properties properties)
  {
    super(type, properties);
    entries = new LinkedList();
    if (this.properties.get("alias-list") != null)
      {
        this.properties.remove("alias-list");
      }
  }

  protected EnvelopeEntry(int type)
  {
    super(type);
    entries = new LinkedList();
  }

  // Instance methods.
  // ------------------------------------------------------------------------

  /**
   * Adds an entry to this envelope.
   *
   * @param entry The entry to add.
   */
  public void add(Entry entry)
  {
    if (!containsEntry(entry))
      {
        if (entry instanceof EnvelopeEntry)
          {
            ((EnvelopeEntry) entry).setContainingEnvelope(this);
          }
        entries.add(entry);
        payload = null;
        makeAliasList();
      }
  }

  /**
   * Tests if this envelope contains a primitive entry with the
   * given alias.
   *
   * @param alias The alias to test.
   * @return True if this envelope (or one of the contained envelopes)
   *   contains a primitive entry with the given alias.
   */
  public boolean containsAlias(String alias)
  {
    String aliases = getAliasList();
    if (aliases == null)
      {
        return false;
      }
    StringTokenizer tok = new StringTokenizer(aliases, ";");
    while (tok.hasMoreTokens())
      {
        if (tok.nextToken().equals(alias))
          {
            return true;
          }
      }
    return false;
  }

  /**
   * Tests if this envelope contains the given entry.
   *
   * @param entry The entry to test.
   * @return True if this envelope contains the given entry.
   */
  public boolean containsEntry(Entry entry)
  {
    if (entry instanceof EnvelopeEntry)
      {
        return entries.contains(entry);
      }
    else if (entry instanceof PrimitiveEntry)
      {
        for (Iterator it = entries.iterator(); it.hasNext();)
          {
            Entry e = (Entry) it.next();
            if (e.equals(entry))
              return true;
            if ((e instanceof EnvelopeEntry)
                && ((EnvelopeEntry) e).containsEntry(entry))
              return true;
          }
      }
    return false;
  }

  /**
   * Returns a copy of all entries this envelope contains.
   *
   * @return All contained entries.
   */
  public List getEntries()
  {
    return new ArrayList(entries);
  }

  /**
   * Gets all primitive entries that have the given alias. If there
   * are any masked entries that contain the given alias, they will
   * be returned as well.
   *
   * @param alias The alias of the entries to get.
   * @return A list of all primitive entries that have the given alias.
   */
  public List get(String alias)
  {
    List result = new LinkedList();
    for (Iterator it = entries.iterator(); it.hasNext();)
      {
        Entry e = (Entry) it.next();
        if (e instanceof EnvelopeEntry)
          {
            if (!((EnvelopeEntry) e).containsAlias(alias))
              {
                continue;
              }
            if (e instanceof MaskableEnvelopeEntry)
              {
                if (((MaskableEnvelopeEntry) e).isMasked())
                  {
                    result.add(e);
                    continue;
                  }
              }
            result.addAll(((EnvelopeEntry) e).get(alias));
          }
        else if (e instanceof PrimitiveEntry)
          {
            if (((PrimitiveEntry) e).getAlias().equals(alias))
              {
                result.add(e);
              }
          }
      }
    return result;
  }

  /**
   * Returns the list of all aliases contained by this envelope,
   * separated by a semicolon (';').
   *
   * @return The list of aliases.
   */
  public String getAliasList()
  {
    String list = properties.get("alias-list");
    if (list == null)
      {
        return "";
      }
    else
      {
        return list;
      }
  }

  /**
   * Removes the specified entry.
   *
   * @param entry The entry.
   * @return True if an entry was removed.
   */
  public boolean remove(Entry entry)
  {
    boolean ret = false;
    for (Iterator it = entries.iterator(); it.hasNext();)
      {
        Entry e = (Entry) it.next();
        if (e instanceof EnvelopeEntry)
          {
            if (e == entry)
              {
                it.remove();
                ret = true;
                break;
              }
            if (((EnvelopeEntry) e).remove(entry))
              {
                ret = true;
                break;
              }
          }
        else if (e instanceof PrimitiveEntry)
          {
            if (((PrimitiveEntry) e).equals(entry))
              {
                it.remove();
                ret = true;
                break;
              }
          }
      }
    if (ret)
      {
        payload = null;
        makeAliasList();
      }
    return ret;
  }

  /**
   * Removes all primitive entries that have the specified alias.
   *
   * @param alias The alias of the entries to remove.
   */
  public void remove(String alias)
  {
    for (Iterator it = entries.iterator(); it.hasNext();)
      {
        Entry e = (Entry) it.next();
        if (e instanceof EnvelopeEntry)
          {
            ((EnvelopeEntry) e).remove(alias);
          }
        else if (e instanceof PrimitiveEntry)
          {
            if (((PrimitiveEntry) e).getAlias().equals(alias))
              {
                it.remove();
              }
          }
      }
    payload = null;
    makeAliasList();
  }

  // Protected methods.
  // ------------------------------------------------------------------------

  protected void encodePayload() throws IOException
  {
    ByteArrayOutputStream bout = new ByteArrayOutputStream(1024);
    DataOutputStream out = new DataOutputStream(bout);
    for (Iterator it = entries.iterator(); it.hasNext();)
      {
        ((Entry) it.next()).encode(out);
      }
  }

  protected void setContainingEnvelope(EnvelopeEntry e)
  {
    if (containingEnvelope != null)
      {
        throw new IllegalArgumentException("envelopes may not be shared");
      }
    containingEnvelope = e;
  }

  protected void decodeEnvelope(DataInputStream in) throws IOException
  {
    while (true)
      {
        int type = in.read();
        switch (type)
          {
          case EncryptedEntry.TYPE:
            add(EncryptedEntry.decode(in));
            break;
          case PasswordEncryptedEntry.TYPE:
            add(PasswordEncryptedEntry.decode(in));
            break;
          case PasswordAuthenticatedEntry.TYPE:
            add(PasswordAuthenticatedEntry.decode(in));
            break;
          case AuthenticatedEntry.TYPE:
            add(AuthenticatedEntry.decode(in));
            break;
          case CompressedEntry.TYPE:
            add(CompressedEntry.decode(in));
            break;
          case CertificateEntry.TYPE:
            add(CertificateEntry.decode(in));
            break;
          case PublicKeyEntry.TYPE:
            add(PublicKeyEntry.decode(in));
            break;
          case PrivateKeyEntry.TYPE:
            add(PrivateKeyEntry.decode(in));
            break;
          case CertPathEntry.TYPE:
            add(CertPathEntry.decode(in));
            break;
          case BinaryDataEntry.TYPE:
            add(BinaryDataEntry.decode(in));
            break;
          case -1:
            return;
          default:
            throw new MalformedKeyringException("unknown type " + type);
          }
      }
  }

  // Own methods.
  // ------------------------------------------------------------------------

  private void makeAliasList()
  {
    if (entries.isEmpty())
      return;
    StringBuffer buf = new StringBuffer();
    for (Iterator it = entries.iterator(); it.hasNext();)
      {
        Entry entry = (Entry) it.next();
        if (entry instanceof EnvelopeEntry)
          {
            buf.append(((EnvelopeEntry) entry).getAliasList());
          }
        else if (entry instanceof PrimitiveEntry)
          {
            buf.append(((PrimitiveEntry) entry).getAlias());
          }
        if (it.hasNext())
          buf.append(';');
      }
    properties.put("alias-list", buf.toString());
    if (containingEnvelope != null)
      {
        containingEnvelope.makeAliasList();
      }
  }
}
