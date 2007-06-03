/* Creator.java - create a new jar file
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

import java.io.BufferedOutputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.jar.JarFile;
import java.util.jar.JarOutputStream;
import java.util.jar.Manifest;
import java.util.zip.CRC32;
import java.util.zip.ZipEntry;

public class Creator
    extends Action
{
  JarOutputStream outputStream;
  HashSet<String> writtenItems = new HashSet<String>();
  // The manifest to use, or null if we don't want a manifest.
  Manifest manifest;

  private long copyFile(CRC32 crc, InputStream is, OutputStream output)
      throws IOException
  {
    byte[] buffer = new byte[1024];
    long size = 0;
    while (true)
      {
        int len = is.read(buffer);
        if (len == - 1)
          break;
        size += len;
        output.write(buffer, 0, len);
        crc.update(buffer, 0, len);
      }
    output.close();
    return size;
  }

  protected void writeFile(boolean isDirectory, InputStream inputFile,
                           String filename, boolean verbose)
    throws IOException
  {
    if (writtenItems.contains(filename))
      {
        if (verbose)
          {
            String msg = MessageFormat.format(Messages.getString("Creator.Ignoring"), //$NON-NLS-1$
                                              new Object[] { filename });
            System.err.println(msg);
          }
        return;
      }

    ByteArrayOutputStream out = new ByteArrayOutputStream();
    CRC32 crc = new CRC32();
    long size;
    if (isDirectory)
      {
        size = 0;
      }
    else
      {
        size = copyFile(crc, inputFile, out);
      }

    ZipEntry entry = new ZipEntry(filename);
    entry.setCrc(crc.getValue());
    entry.setSize(size);

    outputStream.putNextEntry(entry);
    out.writeTo(outputStream);
    outputStream.closeEntry();
    writtenItems.add(filename);

    if (verbose)
      {
        long csize = entry.getCompressedSize();
        long perc;
        if (size == 0)
          perc = 0;
        else
          perc = 100 - (100 * csize) / size;
        String msg = MessageFormat.format(Messages.getString("Creator.Adding"), //$NON-NLS-1$
                                          new Object[]
                                            {
                                              filename,
                                              Long.valueOf(size),
                                              Long.valueOf(entry.getSize()),
                                              Long.valueOf(perc)
                                            });
        System.err.println(msg);
      }
  }

  protected void writeFile(File file, String filename, boolean verbose)
      throws IOException
  {
    boolean isDirectory = file.isDirectory();
    if (isDirectory)
      {
        if (filename.charAt(filename.length() - 1) != '/')
          filename += '/';
	writeFile(isDirectory, null, filename, verbose);
      }
    else
      {
        InputStream inputStream = new FileInputStream(file);
        writeFile(isDirectory, inputStream, filename, verbose);
        inputStream.close();
      }
  }

  private void addEntries(ArrayList<Entry> result, Entry entry)
  {
    if (entry.file.isDirectory())
      {
        String name = entry.name;
        if (name.charAt(name.length() - 1) != '/')
          {
            name += '/';
            entry = new Entry(entry.file, name);
          }
        result.add(entry);
        String[] files = entry.file.list();
        for (int i = 0; i < files.length; ++i)
          addEntries(result, new Entry(new File(entry.file, files[i]),
                                       entry.name + files[i]));
      }
    else
      result.add(entry);
  }

  private ArrayList<Entry> getAllEntries(Main parameters)
  {
    Iterator it = parameters.entries.iterator();
    ArrayList<Entry> allEntries = new ArrayList<Entry>();
    while (it.hasNext())
      {
        Entry entry = (Entry) it.next();
        addEntries(allEntries, entry);
      }
    return allEntries;
  }

  private void writeCommandLineEntries(Main parameters)
      throws IOException
  {
    // We've already written the manifest, make sure to mark it.
    writtenItems.add("META-INF/"); //$NON-NLS-1$
    writtenItems.add(JarFile.MANIFEST_NAME);

    ArrayList allEntries = getAllEntries(parameters);
    Iterator it = allEntries.iterator();
    while (it.hasNext())
      {
        Entry entry = (Entry) it.next();
        writeFile(entry.file, entry.name, parameters.verbose);
      }
  }

  protected Manifest createManifest(Main parameters)
    throws IOException
  {
    if (! parameters.wantManifest)
      return null;
    if (parameters.manifestFile != null)
      {
        // User specified a manifest file.
        InputStream contents = new FileInputStream(parameters.manifestFile);
        return new Manifest(contents);
      }
    return new Manifest();
  }

  protected void writeCommandLineEntries(Main parameters, OutputStream os)
    throws IOException
  {
    manifest = createManifest(parameters);
    outputStream = new JarOutputStream(os, manifest);
    // FIXME: in Classpath this sets the method too late for the
    // manifest file.
    outputStream.setMethod(parameters.storageMode);
    writeCommandLineEntries(parameters);
  }

  protected void close() throws IOException
  {
    outputStream.finish();
    outputStream.close();
  }

  public void run(Main parameters) throws IOException
  {
    if (parameters.archiveFile == null || parameters.archiveFile.equals("-")) //$NON-NLS-1$
      writeCommandLineEntries(parameters, System.out);
    else
      {
        OutputStream os
          = new BufferedOutputStream(new FileOutputStream(parameters.archiveFile));
        writeCommandLineEntries(parameters, os);
      }
    close();
  }
}
