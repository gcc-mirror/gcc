/* FileHandler.java -- a class for publishing log messages to log files
   Copyright (C) 2002, 2003, 2004, 2005  Free Software Foundation, Inc.

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


package java.util.logging;

import gnu.java.lang.CPStringBuilder;

import java.io.File;
import java.io.FileOutputStream;
import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.LinkedList;
import java.util.ListIterator;

/**
 * A <code>FileHandler</code> publishes log records to a set of log
 * files.  A maximum file size can be specified; as soon as a log file
 * reaches the size limit, it is closed and the next file in the set
 * is taken.
 *
 * <p><strong>Configuration:</strong> Values of the subsequent
 * <code>LogManager</code> properties are taken into consideration
 * when a <code>FileHandler</code> is initialized.  If a property is
 * not defined, or if it has an invalid value, a default is taken
 * without an exception being thrown.
 *
 * <ul>
 *
 * <li><code>java.util.FileHandler.level</code> - specifies
 *     the initial severity level threshold. Default value:
 *     <code>Level.ALL</code>.</li>
 *
 * <li><code>java.util.FileHandler.filter</code> - specifies
 *     the name of a Filter class. Default value: No Filter.</li>
 *
 * <li><code>java.util.FileHandler.formatter</code> - specifies
 *     the name of a Formatter class. Default value:
 *     <code>java.util.logging.XMLFormatter</code>.</li>
 *
 * <li><code>java.util.FileHandler.encoding</code> - specifies
 *     the name of the character encoding. Default value:
 *     the default platform encoding.</li>
 *
 * <li><code>java.util.FileHandler.limit</code> - specifies the number
 *     of bytes a log file is approximately allowed to reach before it
 *     is closed and the handler switches to the next file in the
 *     rotating set.  A value of zero means that files can grow
 *     without limit.  Default value: 0 (unlimited growth).</li>
 *
 * <li><code>java.util.FileHandler.count</code> - specifies the number
 *     of log files through which this handler cycles.  Default value:
 *     1.</li>
 *
 * <li><code>java.util.FileHandler.pattern</code> - specifies a
 *     pattern for the location and name of the produced log files.
 *     See the section on <a href="#filePatterns">file name
 *     patterns</a> for details.  Default value:
 *     <code>"%h/java%u.log"</code>.</li>
 *
 * <li><code>java.util.FileHandler.append</code> - specifies
 *     whether the handler will append log records to existing
 *     files, or whether the handler will clear log files
 *     upon switching to them. Default value: <code>false</code>,
 *     indicating that files will be cleared.</li>
 *
 * </ul>
 *
 * <p><a name="filePatterns"><strong>File Name Patterns:</strong></a>
 * The name and location and log files are specified with pattern
 * strings. The handler will replace the following character sequences
 * when opening log files:
 *
 * <p><ul>
 * <li><code>/</code> - replaced by the platform-specific path name
 *     separator.  This value is taken from the system property
 *     <code>file.separator</code>.</li>
 *
 * <li><code>%t</code> - replaced by the platform-specific location of
 *     the directory intended for temporary files.  This value is
 *     taken from the system property <code>java.io.tmpdir</code>.</li>
 *
 * <li><code>%h</code> - replaced by the location of the home
 *     directory of the current user.  This value is taken from the
 *     system property <code>user.home</code>.</li>
 *
 * <li><code>%g</code> - replaced by a generation number for
 *     distinguisthing the individual items in the rotating set 
 *     of log files.  The generation number cycles through the
 *     sequence 0, 1, ..., <code>count</code> - 1.</li>
 *
 * <li><code>%u</code> - replaced by a unique number for
 *     distinguisthing the output files of several concurrently
 *     running processes.  The <code>FileHandler</code> starts
 *     with 0 when it tries to open a log file.  If the file
 *     cannot be opened because it is currently in use,
 *     the unique number is incremented by one and opening
 *     is tried again.  These steps are repeated until the
 *     opening operation succeeds.
 *
 *     <p>FIXME: Is the following correct? Please review.  The unique
 *     number is determined for each log file individually when it is
 *     opened upon switching to the next file.  Therefore, it is not
 *     correct to assume that all log files in a rotating set bear the
 *     same unique number.
 *
 *     <p>FIXME: The Javadoc for the Sun reference implementation
 *     says: "Note that the use of unique ids to avoid conflicts is
 *     only guaranteed to work reliably when using a local disk file
 *     system." Why? This needs to be mentioned as well, in case
 *     the reviewers decide the statement is true.  Otherwise,
 *     file a bug report with Sun.</li>
 *
 * <li><code>%%</code> - replaced by a single percent sign.</li>
 * </ul>
 *
 * <p>If the pattern string does not contain <code>%g</code> and
 * <code>count</code> is greater than one, the handler will append
 * the string <code>.%g</code> to the specified pattern.
 *
 * <p>If the handler attempts to open a log file, this log file
 * is being used at the time of the attempt, and the pattern string
 * does not contain <code>%u</code>, the handler will append
 * the string <code>.%u</code> to the specified pattern. This
 * step is performed after any generation number has been
 * appended.
 *
 * <p><em>Examples for the GNU platform:</em> 
 *
 * <p><ul>
 *
 * <li><code>%h/java%u.log</code> will lead to a single log file
 *     <code>/home/janet/java0.log</code>, assuming <code>count</code>
 *     equals 1, the user's home directory is
 *     <code>/home/janet</code>, and the attempt to open the file
 *     succeeds.</li>
 *
 * <li><code>%h/java%u.log</code> will lead to three log files
 *     <code>/home/janet/java0.log.0</code>,
 *     <code>/home/janet/java0.log.1</code>, and
 *     <code>/home/janet/java0.log.2</code>,
 *     assuming <code>count</code> equals 3, the user's home
 *     directory is <code>/home/janet</code>, and all attempts
 *     to open files succeed.</li>
 *
 * <li><code>%h/java%u.log</code> will lead to three log files
 *     <code>/home/janet/java0.log.0</code>,
 *     <code>/home/janet/java1.log.1</code>, and
 *     <code>/home/janet/java0.log.2</code>,
 *     assuming <code>count</code> equals 3, the user's home
 *     directory is <code>/home/janet</code>, and the attempt
 *     to open <code>/home/janet/java0.log.1</code> fails.</li>
 *
 * </ul>
 *
 * @author Sascha Brawer (brawer@acm.org)
 */
public class FileHandler
  extends StreamHandler
{
  /**
   * A literal that prefixes all file-handler related properties in the
   * logging.properties file.
   */
  private static final String PROPERTY_PREFIX = "java.util.logging.FileHandler";
  /**
   * The name of the property to set for specifying a file naming (incl. path)
   * pattern to use with rotating log files.
   */
  private static final String PATTERN_KEY = PROPERTY_PREFIX + ".pattern";
  /**
   * The default pattern to use when the <code>PATTERN_KEY</code> property was
   * not specified in the logging.properties file.
   */
  private static final String DEFAULT_PATTERN = "%h/java%u.log";
  /**
   * The name of the property to set for specifying an approximate maximum
   * amount, in bytes, to write to any one log output file. A value of zero
   * (which is the default) implies a no limit.
   */
  private static final String LIMIT_KEY = PROPERTY_PREFIX + ".limit";
  private static final int DEFAULT_LIMIT = 0;
  /**
   * The name of the property to set for specifying how many output files to
   * cycle through. The default value is 1.
   */
  private static final String COUNT_KEY = PROPERTY_PREFIX + ".count";
  private static final int DEFAULT_COUNT = 1;
  /**
   * The name of the property to set for specifying whether this handler should
   * append, or not, its output to existing files. The default value is
   * <code>false</code> meaning NOT to append.
   */
  private static final String APPEND_KEY = PROPERTY_PREFIX + ".append";
  private static final boolean DEFAULT_APPEND = false;

  /**
   * The number of bytes a log file is approximately allowed to reach
   * before it is closed and the handler switches to the next file in
   * the rotating set.  A value of zero means that files can grow
   * without limit.
   */
  private final int limit;


 /**
  * The number of log files through which this handler cycles.
  */
  private final int count;


  /**
   * The pattern for the location and name of the produced log files.
   * See the section on <a href="#filePatterns">file name patterns</a>
   * for details.
   */
  private final String pattern;


  /**
   * Indicates whether the handler will append log records to existing
   * files (<code>true</code>), or whether the handler will clear log files
   * upon switching to them (<code>false</code>).
   */
  private final boolean append;


  /**
   * The number of bytes that have currently been written to the stream.
   * Package private for use in inner classes.
   */
  long written;


  /**
   * A linked list of files we are, or have written to. The entries
   * are file path strings, kept in the order 
   */
  private LinkedList logFiles;


  /**
   * Constructs a <code>FileHandler</code>, taking all property values
   * from the current {@link LogManager LogManager} configuration.
   *
   * @throws java.io.IOException FIXME: The Sun Javadoc says: "if
   *         there are IO problems opening the files."  This conflicts
   *         with the general principle that configuration errors do
   *         not prohibit construction. Needs review.
   *
   * @throws SecurityException if a security manager exists and
   *         the caller is not granted the permission to control
   *         the logging infrastructure.
   */
  public FileHandler()
    throws IOException, SecurityException
  {
    this(LogManager.getLogManager().getProperty(PATTERN_KEY),
	 LogManager.getIntProperty(LIMIT_KEY, DEFAULT_LIMIT),
	 LogManager.getIntProperty(COUNT_KEY, DEFAULT_COUNT),
	 LogManager.getBooleanProperty(APPEND_KEY, DEFAULT_APPEND));
  }


  /* FIXME: Javadoc missing. */
  public FileHandler(String pattern)
    throws IOException, SecurityException
  {
    this(pattern, DEFAULT_LIMIT, DEFAULT_COUNT, DEFAULT_APPEND);
  }


  /* FIXME: Javadoc missing. */
  public FileHandler(String pattern, boolean append)
    throws IOException, SecurityException
  {
    this(pattern, DEFAULT_LIMIT, DEFAULT_COUNT, append);
  }


  /* FIXME: Javadoc missing. */
  public FileHandler(String pattern, int limit, int count)
    throws IOException, SecurityException
  {
    this(pattern, limit, count, 
	 LogManager.getBooleanProperty(APPEND_KEY, DEFAULT_APPEND));
  }


  /**
   * Constructs a <code>FileHandler</code> given the pattern for the
   * location and name of the produced log files, the size limit, the
   * number of log files thorough which the handler will rotate, and
   * the <code>append</code> property.  All other property values are
   * taken from the current {@link LogManager LogManager}
   * configuration.
   *
   * @param pattern The pattern for the location and name of the
   *        produced log files.  See the section on <a
   *        href="#filePatterns">file name patterns</a> for details.
   *        If <code>pattern</code> is <code>null</code>, the value is
   *        taken from the {@link LogManager LogManager} configuration
   *        property
   *        <code>java.util.logging.FileHandler.pattern</code>.
   *        However, this is a pecularity of the GNU implementation,
   *        and Sun's API specification does not mention what behavior
   *        is to be expected for <code>null</code>. Therefore,
   *        applications should not rely on this feature.
   *
   * @param limit specifies the number of bytes a log file is
   *        approximately allowed to reach before it is closed and the
   *        handler switches to the next file in the rotating set.  A
   *        value of zero means that files can grow without limit.
   *
   * @param count specifies the number of log files through which this
   *        handler cycles.
   *
   * @param append specifies whether the handler will append log
   *        records to existing files (<code>true</code>), or whether the
   *        handler will clear log files upon switching to them
   *        (<code>false</code>).
   *
   * @throws java.io.IOException FIXME: The Sun Javadoc says: "if
   *         there are IO problems opening the files."  This conflicts
   *         with the general principle that configuration errors do
   *         not prohibit construction. Needs review.
   *
   * @throws SecurityException if a security manager exists and
   *         the caller is not granted the permission to control
   *         the logging infrastructure.
   *         <p>FIXME: This seems in contrast to all other handler
   *         constructors -- verify this by running tests against
   *         the Sun reference implementation.
   */
  public FileHandler(String pattern,
		     int limit,
		     int count,
		     boolean append)
    throws IOException, SecurityException
  {
    super(/* output stream, created below */ null,
	  PROPERTY_PREFIX,
	  /* default level */ Level.ALL,
	  /* formatter */ null,
	  /* default formatter */ XMLFormatter.class);

    if ((limit <0) || (count < 1))
      throw new IllegalArgumentException();

    this.pattern = pattern != null ? pattern : DEFAULT_PATTERN;
    this.limit = limit;
    this.count = count;
    this.append = append;
    this.written = 0;
    this.logFiles = new LinkedList ();

    setOutputStream (createFileStream (this.pattern, limit, count, append,
                                       /* generation */ 0));
  }


  /* FIXME: Javadoc missing. */
  private OutputStream createFileStream(String pattern,
                                        int limit,
                                        int count,
                                        boolean append,
                                        int generation)
  {
    String  path;
    int     unique = 0;

    /* Throws a SecurityException if the caller does not have
     * LoggingPermission("control").
     */
    LogManager.getLogManager().checkAccess();

    /* Default value from the java.util.logging.FileHandler.pattern
     * LogManager configuration property.
     */
    if (pattern == null)
      pattern = LogManager.getLogManager().getProperty(PATTERN_KEY);
    if (pattern == null)
      pattern = DEFAULT_PATTERN;

    if (count > 1 && !has (pattern, 'g'))
      pattern = pattern + ".%g";

    do
    {
      path = replaceFileNameEscapes(pattern, generation, unique, count);

      try
      {
	File file = new File(path);
        if (!file.exists () || append)
          {
            FileOutputStream fout = new FileOutputStream (file, append);
            // FIXME we need file locks for this to work properly, but they
            // are not implemented yet in Classpath! Madness!
//             FileChannel channel = fout.getChannel ();
//             FileLock lock = channel.tryLock ();
//             if (lock != null) // We've locked the file.
//               {
                if (logFiles.isEmpty ())
                  logFiles.addFirst (path);
                return new ostr (fout);
//               }
          }
      }
      catch (Exception ex)
      {
        reportError (null, ex, ErrorManager.OPEN_FAILURE);
      }

      unique = unique + 1;
      if (!has (pattern, 'u'))
        pattern = pattern + ".%u";
    }
    while (true);
  }


  /**
   * Replaces the substrings <code>"/"</code> by the value of the
   * system property <code>"file.separator"</code>, <code>"%t"</code>
   * by the value of the system property
   * <code>"java.io.tmpdir"</code>, <code>"%h"</code> by the value of
   * the system property <code>"user.home"</code>, <code>"%g"</code>
   * by the value of <code>generation</code>, <code>"%u"</code> by the
   * value of <code>uniqueNumber</code>, and <code>"%%"</code> by a
   * single percent character.  If <code>pattern</code> does
   * <em>not</em> contain the sequence <code>"%g"</code>,
   * the value of <code>generation</code> will be appended to
   * the result.
   *
   * @throws NullPointerException if one of the system properties
   *         <code>"file.separator"</code>,
   *         <code>"java.io.tmpdir"</code>, or
   *         <code>"user.home"</code> has no value and the
   *         corresponding escape sequence appears in
   *         <code>pattern</code>.
   */
  private static String replaceFileNameEscapes(String pattern,
					       int generation,
					       int uniqueNumber,
					       int count)
  {
    CPStringBuilder buf = new CPStringBuilder(pattern);
    String       replaceWith;
    boolean      foundGeneration = false;

    int pos = 0;
    do
    {
      // Uncomment the next line for finding bugs.
      // System.out.println(buf.substring(0,pos) + '|' + buf.substring(pos));
      
      if (buf.charAt(pos) == '/')
      {
	/* The same value is also provided by java.io.File.separator. */
	replaceWith = System.getProperty("file.separator");
	buf.replace(pos, pos + 1, replaceWith);
	pos = pos + replaceWith.length() - 1;
	continue;
      }

      if (buf.charAt(pos) == '%')
      {
        switch (buf.charAt(pos + 1))
	{
	case 't':
	  replaceWith = System.getProperty("java.io.tmpdir");
	  break;

	case 'h':
	  replaceWith = System.getProperty("user.home");
	  break;

	case 'g':
	  replaceWith = Integer.toString(generation);
	  foundGeneration = true;
	  break;

	case 'u':
	  replaceWith = Integer.toString(uniqueNumber);
	  break;

	case '%':
	  replaceWith = "%";
	  break;

	default:
	  replaceWith = "??";
	  break; // FIXME: Throw exception?
	}

	buf.replace(pos, pos + 2, replaceWith);
	pos = pos + replaceWith.length() - 1;
	continue;
      }
    }
    while (++pos < buf.length() - 1);

    if (!foundGeneration && (count > 1))
    {
      buf.append('.');
      buf.append(generation);
    }

    return buf.toString();
  }


  /* FIXME: Javadoc missing. */
  public void publish(LogRecord record)
  {
    if (limit > 0 && written >= limit)
      rotate ();
    super.publish(record);
    flush ();
  }

  /**
   * Rotates the current log files, possibly removing one if we
   * exceed the file count.
   */
  private synchronized void rotate ()
  {
    if (logFiles.size () > 0)
      {
        File f1 = null;
        ListIterator lit = null;

        // If we reach the file count, ditch the oldest file.
        if (logFiles.size () == count)
          {
            f1 = new File ((String) logFiles.getLast ());
            f1.delete ();
            lit = logFiles.listIterator (logFiles.size () - 1);
          }
        // Otherwise, move the oldest to a new location.
        else
          {
            String path = replaceFileNameEscapes (pattern, logFiles.size (),
                                                  /* unique */ 0, count);
            f1 = new File (path);
            logFiles.addLast (path);
            lit = logFiles.listIterator (logFiles.size () - 1);
          }

        // Now rotate the files.
        while (lit.hasPrevious ())
          {
            String s = (String) lit.previous ();
            File f2 = new File (s);
            f2.renameTo (f1);
            f1 = f2;
          }
      }

    setOutputStream (createFileStream (pattern, limit, count, append,
                                       /* generation */ 0));

    // Reset written count.
    written = 0;
  }

  /**
   * Tell if <code>pattern</code> contains the pattern sequence
   * with character <code>escape</code>. That is, if <code>escape</code>
   * is 'g', this method returns true if the given pattern contains
   * "%g", and not just the substring "%g" (for example, in the case of
   * "%%g").
   *
   * @param pattern The pattern to test.
   * @param escape The escape character to search for.
   * @return True iff the pattern contains the escape sequence with the
   *  given character.
   */
  private static boolean has (final String pattern, final char escape)
  {
    final int len = pattern.length ();
    boolean sawPercent = false;
    for (int i = 0; i < len; i++)
      {
        char c = pattern.charAt (i);
        if (sawPercent)
          {
            if (c == escape)
              return true;
            if (c == '%') // Double percent
              {
                sawPercent = false;
                continue;
              }
          }
        sawPercent = (c == '%');
      }
    return false;
  }

  /**
   * An output stream that tracks the number of bytes written to it.
   */
  private final class ostr extends FilterOutputStream
  {
    private ostr (OutputStream out)
    {
      super (out);
    }

    public void write (final int b) throws IOException
    {
      out.write (b);
      FileHandler.this.written++; // FIXME: synchronize?
    }

    public void write (final byte[] b) throws IOException
    {
      write (b, 0, b.length);
    }

    public void write (final byte[] b, final int offset, final int length)
      throws IOException
    {
      out.write (b, offset, length);
      FileHandler.this.written += length; // FIXME: synchronize?
    }
  }
}
