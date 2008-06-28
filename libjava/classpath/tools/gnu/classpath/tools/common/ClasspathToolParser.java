/* ClasspathToolParser.java -- Parser subclass for classpath tools
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


package gnu.classpath.tools.common;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.Reader;
import java.text.MessageFormat;
import java.util.ArrayList;

import gnu.classpath.Configuration;
import gnu.classpath.tools.getopt.FileArgumentCallback;
import gnu.classpath.tools.getopt.Option;
import gnu.classpath.tools.getopt.OptionException;
import gnu.classpath.tools.getopt.Parser;

/**
 * This is like the Parser class, but is specialized for use by
 * tools distributed with GNU Classpath.  In particular it automatically
 * computes the version string using the program's name.
 */
public class ClasspathToolParser
    extends Parser
{
  private static String getVersionString(String programName)
  {
    String fmt = (Messages.getString("ClasspathToolParser.VersionFormat")); //$NON-NLS-1$
    return MessageFormat.format(fmt, 
                                new Object[]
                                  {
                                    programName,
                                    Configuration.CLASSPATH_VERSION
                                  });
  }

  public ClasspathToolParser(String programName)
  {
    this(programName, false);
  }

  public ClasspathToolParser(String programName, boolean longOnly)
  {
    super(programName, getVersionString(programName), longOnly);
    addFinal(new Option('J',
                        Messages.getString("ClasspathToolParser.JArgument"),//$NON-NLS-1$
                        Messages.getString("ClasspathToolParser.JName"), //$NON-NLS-1$
                        true)
             {
               public void parsed(String argument) throws OptionException
               {
                 // -J should be handled by the wrapper binary.
                 // We add it here so that it shows up in the --help output.
               }
             });
  }

  public void parse(String[] inArgs, FileArgumentCallback files,
		    boolean handleFileLists)
  {
    FileArgumentCallback cb;
    
    if (handleFileLists)
      cb = new AtFileArgumentCallback(files);
    else
      cb = files;
    
    parse(inArgs, cb);
  }

  public String[] parse(String[] inArgs, boolean handleFileLists)
  {
    final ArrayList<String> fileResult = new ArrayList<String>();

    final FileArgumentCallback cb = new FileArgumentCallback()
      {
    	public void notifyFile(String fileArgument)
    	{
    	  fileResult.add(fileArgument);
    	}
      };
    
    if (handleFileLists)
      parse(inArgs, new AtFileArgumentCallback(cb));
    else
      parse(inArgs, cb);
    
    return fileResult.toArray(new String[fileResult.size()]);
  }


  /** 
   * Simple function that takes the given {@link Reader}, treats it like
   * a textfile and reads all the whitespace separated entries from it
   * and adds them to the @{link FileArgumentCallback} instance.
   *
   * @param reader the reader to read from.
   * @param cb the callback to post the filenames to.
   * @throws OptionException if an error occurs reading the list.
   */
  public void parseFileList(Reader reader, FileArgumentCallback cb)
	throws OptionException
  {
    BufferedReader breader = new BufferedReader(reader);
    String line = null;

    try
      {
        while ((line = breader.readLine()) != null)
          parseLine(line, cb);
          
        reader.close();
      }
    catch (IOException ioe)
      {
        throw new OptionException("I/O error while reading a file list", ioe);
      }
      
  }
  
  /** 
   * Parses whitespace separated file entries.
   *
   * Note: This is not coping with whitespace in files or quoting.
   *
   * @param line the line of the file to parse.
   * @param cb the callback to pass the parsed file to.
   * @throws IOException if an I/O error occurs.
   * @throws OptionException if an error occurs in the callback.
   */
  private void parseLine(String line, FileArgumentCallback cb)
    throws IOException, OptionException
  {
    final int length = line.length();
    int start = 0;
    int end = 0;

		// While not reached end of line ...
    while (start < length)
      {
	// Search for first non-whitespace character for the start of a word.
        while (Character.isWhitespace(line.codePointAt(start)))
          {
            start++;
	    
            if (start == length)
              return;
          }
	
        end = start + 1;
	
	// Search for first whitespace character for the end of a word.
        while (end < length && !Character.isWhitespace(line.codePointAt(end)))
          end++;
	
        cb.notifyFile(line.substring(start, end));
	
        start = end + 1;
      }
  }

  /** 
   * Implementation of {@link FileArgumentCallback} that handles
   * file arguments in {@link #notifyFile} starting with a <code>@</code>
   * through {@link ClasspathToolParser#parseFileList}.
   */
  class AtFileArgumentCallback extends FileArgumentCallback
  {
    FileArgumentCallback cb;
    
    AtFileArgumentCallback(FileArgumentCallback cb)
    {
      this.cb = cb;
    }

    @Override
    public void notifyFile(String fileArgument)
      throws OptionException
    {
      if (fileArgument.codePointAt(0) == '@')
	{
	  FileReader fr = null;
	  
	  try
	    {
	      fr = new FileReader(fileArgument.substring(1));
	    }
	  catch (FileNotFoundException fnfe)
	    {
	      throw new OptionException("File not found: " + fileArgument.substring(1),
					fnfe);
	    }
	  
	  ClasspathToolParser.this.parseFileList(fr, cb);
	}
      else
	cb.notifyFile(fileArgument);
    }
    
  }

}
