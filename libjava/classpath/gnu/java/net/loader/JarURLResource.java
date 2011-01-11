/* JarURLResource.java -- a Resource for jar URLs
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


package gnu.java.net.loader;


import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.security.cert.Certificate;
import java.util.jar.JarEntry;

public final class JarURLResource extends Resource
{
  private final JarEntry entry;
  private final String name;

  public JarURLResource(JarURLLoader loader, String name, JarEntry entry)
  {
    super(loader);
    this.entry = entry;
    this.name = name;
  }

  public InputStream getInputStream() throws IOException
  {
    return ((JarURLLoader) loader).jarfile.getInputStream(entry);
  }

  public int getLength()
  {
    return (int) entry.getSize();
  }

  public Certificate[] getCertificates()
  {
    // We have to get the entry from the jar file again, because the
    // certificates will not be available until the entire entry has
    // been read.
    return ((JarEntry) ((JarURLLoader) loader).jarfile.getEntry(name))
      .getCertificates();
  }

  public URL getURL()
  {
    try
      {
        return new URL(((JarURLLoader) loader).baseJarURL, name,
                       loader.cache.get(loader.factory, "jar"));
      }
    catch (MalformedURLException e)
      {
        InternalError ie = new InternalError();
        ie.initCause(e);
        throw ie;
      }
  }
}
