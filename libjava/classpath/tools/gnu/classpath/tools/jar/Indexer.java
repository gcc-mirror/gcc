/* Indexer.java -- add index.list file to jar
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


package gnu.classpath.tools.jar;

import gnu.java.net.IndexListParser;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.text.MessageFormat;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.StringTokenizer;
import java.util.jar.Attributes;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.jar.Manifest;

public class Indexer
    extends Updater
{
  private void indexJarFile(StringBuffer result, File fileName,
                            boolean verbose)
    throws IOException
  {
    if (verbose)
      {
        String msg = MessageFormat.format(Messages.getString("Indexer.Indexing"), //$NON-NLS-1$
                                          new Object[] { fileName });
        System.err.println(msg);
      }
    JarFile jf = new JarFile(fileName);

    // Index the files in this jar.
    // The results look a little better if we keep them
    // in insertion order.
    LinkedHashSet<String> entries = new LinkedHashSet<String>();
    Enumeration e = jf.entries();
    while (e.hasMoreElements())
      {
        JarEntry entry = (JarEntry) e.nextElement();
        String name = entry.getName();
        if (name.startsWith("META-INF/")) //$NON-NLS-1$
          continue;
        int index = name.lastIndexOf('/');
        if (index != -1)
          name = name.substring(0, index);
        entries.add(name);
      }
    if (! entries.isEmpty())
      {
        result.append(fileName);
        // Any line ending will do.
        result.append('\n');
        Iterator i = entries.iterator();
        while (i.hasNext())
          {
            result.append(i.next());
            result.append('\n');
          }
        // Paragraph break.
        result.append('\n');
      }

    // Now read pointed-to jars.
    Manifest m = jf.getManifest();
    if (m != null)
      {
        File parent = fileName.getParentFile();
        Attributes attrs = m.getMainAttributes();
        String jars = attrs.getValue(Attributes.Name.CLASS_PATH);
        if (jars != null)
          {
            StringTokenizer st = new StringTokenizer(jars, " "); //$NON-NLS-1$
            while (st.hasMoreTokens())
              {
                String name = st.nextToken();
                indexJarFile(result, new File(parent, name), verbose);
              }
          }
      }

    jf.close();
  }

  protected void writeCommandLineEntries(Main parameters, OutputStream os)
    throws IOException
  {
    // This is a pretty lame design.  We know the super call will
    // only have side effects and won't actually write anything important.
    super.writeCommandLineEntries(parameters, os);

    // Now compute our index file and write it.
    StringBuffer contents = new StringBuffer();
    indexJarFile(contents, parameters.archiveFile, parameters.verbose);
    if (contents.length() != 0)
      {
        // Insert in reverse order to avoid computing anything.
        contents.insert(0, "1.0\n\n"); //$NON-NLS-1$
        contents.insert(0, IndexListParser.JAR_INDEX_VERSION_KEY);
        ByteArrayInputStream in
          = new ByteArrayInputStream(contents.toString().getBytes());
        writeFile(false, in, IndexListParser.JAR_INDEX_FILE, parameters.verbose);
      }
  }
}
