/* GnuPublicKeyring.java --
   Copyright (C) 2003, 2006, 2010 Free Software Foundation, Inc.

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

import gnu.java.security.Configuration;
import gnu.java.security.Registry;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.security.cert.Certificate;
import java.util.Date;
import java.util.Iterator;
import java.util.logging.Logger;

public class GnuPublicKeyring
    extends BaseKeyring
    implements IPublicKeyring
{
  private static final Logger log = Configuration.DEBUG ?
                Logger.getLogger(GnuPublicKeyring.class.getName()) : null;
  public static final int USAGE = Registry.GKR_CERTIFICATES;

  public GnuPublicKeyring(String mac, int macLen)
  {
    keyring = new PasswordAuthenticatedEntry(mac, macLen, new Properties());
    keyring2 = new CompressedEntry(new Properties());
    keyring.add(keyring2);
  }

  public GnuPublicKeyring()
  {
  }

  public boolean containsCertificate(String alias)
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "containsCertificate", alias);
    boolean result = false;
    if (containsAlias(alias))
      for (Iterator it = get(alias).iterator(); it.hasNext();)
        if (it.next() instanceof CertificateEntry)
          {
            result = true;
            break;
          }
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "containsCertificate",
                  Boolean.valueOf(result));
    return result;
  }

  public Certificate getCertificate(String alias)
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "getCertificate", alias);
    Certificate result = null;
    if (containsAlias(alias))
      for (Iterator it = get(alias).iterator(); it.hasNext();)
        {
          Entry e = (Entry) it.next();
          if (e instanceof CertificateEntry)
            {
              result = ((CertificateEntry) e).getCertificate();
              break;
            }
        }
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "getCertificate", result);
    return result;
  }

  public void putCertificate(String alias, Certificate cert)
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "putCertificate",
                   new Object[] { alias, cert });
    if (! containsCertificate(alias))
      {
        Properties p = new Properties();
        p.put("alias", fixAlias(alias));
        add(new CertificateEntry(cert, new Date(), p));
      }
    else if (Configuration.DEBUG)
      log.fine("Keyring already contains alias: " + alias);
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "putCertificate");
  }

  protected void load(InputStream in, char[] password) throws IOException
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "load");
    if (in.read() != USAGE)
      throw new MalformedKeyringException("incompatible keyring usage");
    if (in.read() != PasswordAuthenticatedEntry.TYPE)
      throw new MalformedKeyringException(
          "expecting password-authenticated entry tag");
    DataInputStream dis = new DataInputStream(in);
    keyring = PasswordAuthenticatedEntry.decode(dis, password);
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "load");
  }

  protected void store(OutputStream out, char[] password) throws IOException
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "store");
    out.write(USAGE);
    keyring.encode(new DataOutputStream(out), password);
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "store");
  }
}
