/* JarSigner.java -- The signing handler of the gjarsigner tool
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


package gnu.classpath.tools.jarsigner;

import gnu.classpath.Configuration;
import gnu.classpath.SystemProperties;
import gnu.java.util.jar.JarUtils;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Enumeration;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.jar.JarOutputStream;
import java.util.logging.Logger;

/**
 * The JAR signing handler of the <code>gjarsigner</code> tool.
 */
public class JarSigner
{
  private static final Logger log = Logger.getLogger(JarSigner.class.getName());
  /** The owner tool of this handler. */
  private Main main;

  JarSigner(Main main)
  {
    super();

    this.main = main;
  }

  void start() throws Exception
  {
    if (Configuration.DEBUG)
      log.entering(this.getClass().getName(), "start"); //$NON-NLS-1$
    JarFile jarFile = new JarFile(main.getJarFileName());
    SFHelper sfHelper = new SFHelper(jarFile);

    sfHelper.startSigning();

    // 1. compute the digests
    for (Enumeration e = jarFile.entries(); e.hasMoreElements(); )
      {
        JarEntry je = (JarEntry) e.nextElement();
        String jeName = je.getName();
        if (jeName.equals(JarFile.MANIFEST_NAME)
            || jeName.endsWith(File.separator))
          continue;

        sfHelper.updateEntry(je);
        if (main.isVerbose())
          System.out.println(Messages.getString("JarSigner.1") + jeName); //$NON-NLS-1$
      }

    sfHelper.finishSigning(main.isSectionsOnly());
    if (main.isVerbose())
      System.out.println(Messages.getString("JarSigner.2") + JarFile.MANIFEST_NAME); //$NON-NLS-1$

    // 2. write jar entries and manifest
    File signedJarFile = File.createTempFile("gcp-", ".jar"); //$NON-NLS-1$ //$NON-NLS-2$
    FileOutputStream fos = new FileOutputStream(signedJarFile);
    JarOutputStream outSignedJarFile = new JarOutputStream(fos,
                                                           sfHelper.getManifest());
    for (Enumeration e = jarFile.entries(); e.hasMoreElements(); )
      {
        JarEntry je = (JarEntry) e.nextElement();
        String jeName = je.getName();
        if (jeName.equals(JarFile.MANIFEST_NAME)
            || jeName.endsWith(File.separator))
          continue;

        log.finest("Processing " + jeName); //$NON-NLS-1$
        JarEntry newEntry = new JarEntry(jeName);
        newEntry.setTime(je.getTime());
        outSignedJarFile.putNextEntry(newEntry);
        InputStream jeis = jarFile.getInputStream(je);
        copyFromTo(jeis, outSignedJarFile);
      }

    // 3. create the .SF file
    String signaturesFileName = main.getSigFileName();
    String sfFileName = JarUtils.META_INF + signaturesFileName
                        + JarUtils.SF_SUFFIX;
    if (Configuration.DEBUG)
      log.fine("Processing " + sfFileName); //$NON-NLS-1$
    JarEntry sfEntry = new JarEntry(sfFileName);
    sfEntry.setTime(System.currentTimeMillis());
    outSignedJarFile.putNextEntry(sfEntry);
    sfHelper.writeSF(outSignedJarFile);
    if (Configuration.DEBUG)
      log.fine("Created .SF file"); //$NON-NLS-1$
    if (main.isVerbose())
      System.out.println(Messages.getString("JarSigner.8") + sfFileName); //$NON-NLS-1$

    // 4. create the .DSA file
    String dsaFileName = JarUtils.META_INF + signaturesFileName
                         + JarUtils.DSA_SUFFIX;
    if (Configuration.DEBUG)
      log.fine("Processing " + dsaFileName); //$NON-NLS-1$
    JarEntry dsaEntry = new JarEntry(dsaFileName);
    dsaEntry.setTime(System.currentTimeMillis());
    outSignedJarFile.putNextEntry(dsaEntry);
    sfHelper.writeDSA(outSignedJarFile,
                      main.getSignerPrivateKey(),
                      main.getSignerCertificateChain(),
                      main.isInternalSF());
    if (Configuration.DEBUG)
      log.fine("Created .DSA file"); //$NON-NLS-1$
    if (main.isVerbose())
      System.out.println(Messages.getString("JarSigner.8") + dsaFileName); //$NON-NLS-1$

    // cleanup
    outSignedJarFile.close();
    fos.close();
    signedJarFile.renameTo(new File(main.getSignedJarFileName()));
    if (Configuration.DEBUG)
      log.fine("Renamed signed JAR file"); //$NON-NLS-1$
    if (main.isVerbose())
      System.out.println(SystemProperties.getProperty("line.separator") //$NON-NLS-1$
                         + Messages.getString("JarSigner.14")); //$NON-NLS-1$
    if (Configuration.DEBUG)
      log.exiting(this.getClass().getName(), "start"); //$NON-NLS-1$
  }

  private void copyFromTo(InputStream in, JarOutputStream out)
    throws IOException
  {
    byte[] buffer = new byte[8192];
    int n;
    while ((n = in.read(buffer)) != -1)
      if (n > 0)
        out.write(buffer, 0, n);
  }
}
