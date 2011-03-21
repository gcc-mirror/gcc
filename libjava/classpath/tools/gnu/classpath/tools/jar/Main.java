/* Main.java - jar program main()
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


package gnu.classpath.tools.jar;

import gnu.classpath.tools.common.ClasspathToolParser;
import gnu.classpath.tools.getopt.FileArgumentCallback;
import gnu.classpath.tools.getopt.Option;
import gnu.classpath.tools.getopt.OptionException;
import gnu.classpath.tools.getopt.OptionGroup;
import gnu.classpath.tools.getopt.Parser;

import java.io.BufferedReader;
import java.io.File;
import java.io.InputStreamReader;
import java.io.IOException;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.zip.ZipOutputStream;

public class Main
{
  /** The mode of operation. This is the class representing
   * the action; we make a new instance before using it. It
   * must be a subclass of Action. 'null' means the mode
   * has not yet been set.  */
  Class operationMode;

  /** The archive file name.  */
  File archiveFile;

  /** The zip storage mode.  */
  int storageMode = ZipOutputStream.DEFLATED;

  /** True if we should read file names from stdin.  */
  boolean readNamesFromStdin = false;

  /** True for verbose mode.  */
  boolean verbose = false;

  /** True if we want a manifest file.  */
  boolean wantManifest = true;

  /** Name of manifest file to use.  */
  File manifestFile;

  /** A list of Entry objects, each describing a file to write.  */
  ArrayList<Entry> entries = new ArrayList<Entry>();

  /** Used only while parsing, holds the first argument for -C.  */
  String changedDirectory;

  void setArchiveFile(String filename) throws OptionException
  {
    if (archiveFile != null)
      {
        String fmt = MessageFormat.format(Messages.getString("Main.ArchiveAlreadySet"), //$NON-NLS-1$
                                          new Object[] { archiveFile });
        throw new OptionException(fmt);
      }
    archiveFile = new File(filename);
  }

  class HandleFile
      extends FileArgumentCallback
  {
    public void notifyFile(String fileArgument)
    {
      Entry entry;
      if (changedDirectory != null)
        {
          entry = new Entry(new File(changedDirectory, fileArgument),
                            fileArgument);
          changedDirectory = null;
        }
      else
        entry = new Entry(new File(fileArgument));
      entries.add(entry);
    }
  }

  // An option that knows how to set the operation mode.
  private class ModeOption
      extends Option
  {
    private Class mode;

    public ModeOption(char shortName, String description, Class mode)
    {
      super(shortName, description);
      this.mode = mode;
    }

    public ModeOption(char shortName, String description, String argName,
                      Class mode)
    {
      super(shortName, description, argName);
      this.mode = mode;
    }

    public void parsed(String argument) throws OptionException
    {
      if (operationMode != null)
        throw new OptionException(Messages.getString("Main.ModeAlreaySet")); //$NON-NLS-1$
      operationMode = mode;
      // We know this is only the case for -i.
      if (argument != null)
        setArchiveFile(argument);
    }
  }

  private class JarParser extends ClasspathToolParser
  {
    public JarParser(String name)
    {
      super(name);
    }

    protected void validate() throws OptionException
    {
      if (operationMode == null)
        throw new OptionException(Messages.getString("Main.MustSpecify")); //$NON-NLS-1$
      if (changedDirectory != null)
        throw new OptionException(Messages.getString("Main.TwoArgsReqd")); //$NON-NLS-1$
      if (! wantManifest && manifestFile != null)
        throw new OptionException(Messages.getString("Main.CantHaveBoth")); //$NON-NLS-1$
      if (operationMode == Indexer.class)
        {
          // Some extra validation for -i.
          if (! entries.isEmpty())
            throw new OptionException(Messages.getString("Main.NoFilesWithi")); //$NON-NLS-1$
          if (! wantManifest)
            throw new OptionException(Messages.getString("Main.NoMAndi")); //$NON-NLS-1$
          if (manifestFile != null)
            throw new OptionException(Messages.getString("Main.AnotherNomAndi")); //$NON-NLS-1$
        }
    }
  }

  private ClasspathToolParser initializeParser()
  {
    ClasspathToolParser p = new JarParser("jar"); //$NON-NLS-1$
    p.setHeader(Messages.getString("Main.Usage")); //$NON-NLS-1$

    OptionGroup grp = new OptionGroup(Messages.getString("Main.OpMode")); //$NON-NLS-1$
    grp.add(new ModeOption('c', Messages.getString("Main.Create"), Creator.class)); //$NON-NLS-1$
    grp.add(new ModeOption('x', Messages.getString("Main.Extract"), Extractor.class)); //$NON-NLS-1$
    grp.add(new ModeOption('t', Messages.getString("Main.List"), Lister.class)); //$NON-NLS-1$
    grp.add(new ModeOption('u', Messages.getString("Main.Update"), Updater.class)); //$NON-NLS-1$
    // Note that -i works in-place and explicitly requires a file name.
    grp.add(new ModeOption('i', Messages.getString("Main.Index"), Messages.getString("Main.FileArg"), Indexer.class)); //$NON-NLS-1$ //$NON-NLS-2$
    p.add(grp);

    grp = new OptionGroup(Messages.getString("Main.OpMods")); //$NON-NLS-1$
    grp.add(new Option('f', Messages.getString("Main.ArchiveName"), Messages.getString("Main.FileArg2")) //$NON-NLS-1$ //$NON-NLS-2$
    {
      public void parsed(String argument) throws OptionException
      {
        setArchiveFile(argument);
      }
    });
    grp.add(new Option('0', Messages.getString("Main.NoZip")) //$NON-NLS-1$
    {
      public void parsed(String argument) throws OptionException
      {
        storageMode = ZipOutputStream.STORED;
      }
    });
    grp.add(new Option('v', Messages.getString("Main.Verbose")) //$NON-NLS-1$
    {
      public void parsed(String argument) throws OptionException
      {
        verbose = true;
      }
    });
    grp.add(new Option('M', Messages.getString("Main.NoManifest")) //$NON-NLS-1$
    {
      public void parsed(String argument) throws OptionException
      {
        wantManifest = false;
      }
    });
    grp.add(new Option('m', Messages.getString("Main.ManifestName"), Messages.getString("Main.ManifestArgName")) //$NON-NLS-1$ //$NON-NLS-2$
    {
      public void parsed(String argument) throws OptionException
      {
        manifestFile = new File(argument);
      }
    });
    // -@
    p.add(grp);

    grp = new OptionGroup(Messages.getString("Main.FileNameGroup")); //$NON-NLS-1$
    grp.add(new Option('C', Messages.getString("Main.ChangeDir"), //$NON-NLS-1$
                       Messages.getString("Main.ChangeDirArg")) //$NON-NLS-1$
    {
      public void parsed(String argument) throws OptionException
      {
        changedDirectory = argument;
      }
    });
    grp.add(new Option('@', Messages.getString("Main.Stdin"))
    {
      public void parsed(String argument) throws OptionException
      {
        readNamesFromStdin = true;
      }
    });
    p.add(grp);

    return p;
  }

  private void readNames()
  {
    String line;
    try
      {
        BufferedReader br
          = new BufferedReader(new InputStreamReader(System.in));
        while ((line = br.readLine()) != null)
          entries.add(new Entry(new File(line)));
      }
    catch (IOException _)
      {
        // Ignore.
      }
  }

  private void run(String[] args)
      throws InstantiationException, IllegalAccessException, IOException
  {
    ClasspathToolParser p = initializeParser();
    // Special hack to emulate old tar-style commands.
    if (args.length > 0 && args[0].charAt(0) != '-')
      args[0] = '-' + args[0];
    p.parse(args, new HandleFile(), true);
    if (readNamesFromStdin)
      readNames();
    Action t = (Action) operationMode.newInstance();
    t.run(this);
  }

  public static void main(String[] args)
  {
    Main jarprogram = new Main();
    try
      {
        jarprogram.run(args);
      }
    catch (Exception e)
      {
        System.err.println(Messages.getString("Main.InternalError")); //$NON-NLS-1$
        e.printStackTrace(System.err);
        System.exit(1);
      }
  }
}
