/* Utils.java -- Utility methods for JAR file signing/verification
   Copyright (C) 2006, 2007 Free Software Foundation, Inc.

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


package gnu.classpath.tools.jarsigner;

import gnu.classpath.Configuration;
import gnu.java.security.hash.Sha160;
import gnu.java.util.Base64;
import gnu.java.util.jar.JarUtils;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.util.logging.Logger;

/**
 * Collection of utility methods used in JAR file signing and verification.
 */
class HashUtils
{
  private static final Logger log = Logger.getLogger(HashUtils.class.getName());
  private Sha160 sha = new Sha160();

  // default 0-arguments constructor

  /**
   * @param stream the input stream to digest.
   * @return a base-64 representation of the resulting SHA-1 digest of the
   *         contents of the designated input stream.
   * @throws IOException if an I/O related exception occurs during the process.
   */
  String hashStream(InputStream stream) throws IOException
  {
    BufferedInputStream bis = new BufferedInputStream(stream, 4096);
    byte[] buffer = new byte[4096];
    int count = 0;
    int n;
    while ((n = bis.read(buffer)) != - 1)
      if (n > 0)
        {
          sha.update(buffer, 0, n);
          count += n;
        }
    byte[] hash = sha.digest();
    if (Configuration.DEBUG)
      log.finest("Hashed " + count + " byte(s)");
    String result = Base64.encode(hash);
    return result;
  }

  /**
   * @param ba the byte array to digest.
   * @return a base-64 representation of the resulting SHA-1 digest of the
   *         contents of the designated buffer.
   */
  String hashByteArray(byte[] ba) throws IOException
  {
    sha.update(ba);
    byte[] hash = sha.digest();
    if (Configuration.DEBUG)
      log.finest("Hashed " + ba.length + " byte(s)");
    String result = Base64.encode(hash);
    return result;
  }

  /**
   * @param name the JAR entry name
   * @param entryHash the hash of the entry file which appears in the
   *          manifest.
   * @return the base-64 encoded form of the hash of the corresponding Manifest
   *         JAR entry which will appear in the SF file under the entry with the
   *         same name.
   * @throws UnsupportedEncodingException If UTF-8 character encoding is not
   *           supported on this platform.
   */
  String hashManifestEntry(String name, String entryHash)
      throws UnsupportedEncodingException
  {
    sha.update((JarUtils.NAME + ": " + name).getBytes("UTF-8"));
    sha.update(JarUtils.CRLF);
    sha.update((Main.DIGEST + ": " + entryHash).getBytes("UTF-8"));
    sha.update(JarUtils.CRLF);
    sha.update(JarUtils.CRLF);
    byte[] sfHash = sha.digest();
    String result = Base64.encode(sfHash);
    return result;
  }
}
