/* Handler.java -- a class for publishing log messages
   Copyright (C) 2002, 2004 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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

import java.io.UnsupportedEncodingException;

/**
 * A <code>Handler</code> publishes <code>LogRecords</code> to
 * a sink, for example a file, the console or a network socket.
 * There are different subclasses of <code>Handler</code>
 * to deal with different kinds of sinks.
 *
 * <p>FIXME: Are handlers thread-safe, or is the assumption that only
 * loggers are, and a handler can belong only to one single logger? If
 * the latter, should we enforce it? (Spec not clear). In any
 * case, it needs documentation.
 *
 * @author Sascha Brawer (brawer@acm.org)
 */
public abstract class Handler
{
  Formatter     formatter;
  Filter        filter;
  Level         level;
  ErrorManager  errorManager;
  String        encoding;

  /**
   * Constructs a Handler with a logging severity level of
   * <code>Level.ALL</code>, no formatter, no filter, and
   * an instance of <code>ErrorManager</code> managing errors.
   *
   * <p><strong>Specification Note:</strong> The specification of the
   * Java<sup>TM</sup> Logging API does not mention which character
   * encoding is to be used by freshly constructed Handlers.  The GNU
   * implementation uses the default platform encoding, but other
   * Java implementations might behave differently.
   *
   * <p><strong>Specification Note:</strong> While a freshly constructed
   * Handler is required to have <em>no filter</em> according to the
   * specification, <code>null</code> is not a valid parameter for
   * <code>Handler.setFormatter</code>.  Therefore, the following
   * code will throw a <code>java.lang.NullPointerException</code>:
   *
   * <p><pre>Handler h = new MyConcreteSubclassOfHandler();
h.setFormatter(h.getFormatter());</pre>
   *
   * It seems strange that a freshly constructed Handler is not
   * supposed to provide a Formatter, but this is what the specification
   * says.
   */
  protected Handler()
  {
    level = Level.ALL;
  }


  /**
   * Publishes a <code>LogRecord</code> to an appropriate sink,
   * provided the record passes all tests for being loggable.  The
   * <code>Handler</code> will localize the message of the log
   * record and substitute any message parameters.
   *
   * <p>Most applications do not need to call this method directly.
   * Instead, they will use use a {@link Logger}, which will
   * create LogRecords and distribute them to registered handlers.
   *
   * <p>In case of an I/O failure, the <code>ErrorManager</code>
   * of this <code>Handler</code> will be informed, but the caller
   * of this method will not receive an exception.
   *
   * @param record the log event to be published.
   */
  public abstract void publish(LogRecord record);


  /**
   * Forces any data that may have been buffered to the underlying
   * output device.
   *
   * <p>In case of an I/O failure, the <code>ErrorManager</code>
   * of this <code>Handler</code> will be informed, but the caller
   * of this method will not receive an exception.
   */
  public abstract void flush();


  /**
   * Closes this <code>Handler</code> after having flushed
   * the buffers.  As soon as <code>close</code> has been called,
   * a <code>Handler</code> should not be used anymore. Attempts
   * to publish log records, to flush buffers, or to modify the
   * <code>Handler</code> in any other way may throw runtime
   * exceptions after calling <code>close</code>.
   *
   * <p>In case of an I/O failure, the <code>ErrorManager</code>
   * of this <code>Handler</code> will be informed, but the caller
   * of this method will not receive an exception.
   *
   * @throws SecurityException if a security manager exists and
   *         the caller is not granted the permission to control
   *         the logging infrastructure.
   */
  public abstract void close()
    throws SecurityException;


  /**
   * Returns the <code>Formatter</code> which will be used to
   * localize the text of log messages and to substitute
   * message parameters.  A <code>Handler</code> is encouraged,
   * but not required to actually use an assigned
   * <code>Formatter</code>.
   *
   * @return the <code>Formatter</code> being used, or
   *         <code>null</code> if this <code>Handler</code>
   *         does not use formatters and no formatter has
   *         ever been set by calling <code>setFormatter</code>.
   */
  public Formatter getFormatter()
  {
    return formatter;
  }


  /**
   * Sets the <code>Formatter</code> which will be used to
   * localize the text of log messages and to substitute
   * message parameters.  A <code>Handler</code> is encouraged,
   * but not required to actually use an assigned
   * <code>Formatter</code>.
   *
   * @param formatter the new <code>Formatter</code> to use.
   *
   * @throws SecurityException if a security manager exists and
   *         the caller is not granted the permission to control
   *         the logging infrastructure.
   *
   * @throws NullPointerException if <code>formatter</code> is
   *         <code>null</code>.
   */
  public void setFormatter(Formatter formatter)
    throws SecurityException
  {
    LogManager.getLogManager().checkAccess();
    
    /* Throws a NullPointerException if formatter is null. */
    formatter.getClass();

    this.formatter = formatter;
  }


  /**
   * Returns the character encoding which this handler uses for publishing
   * log records.
   *
   * @param encoding the name of a character encoding, or <code>null</code>
   *            for the default platform encoding.
   */
  public String getEncoding()
  {
    return encoding;
  }


  /**
   * Sets the character encoding which this handler uses for publishing
   * log records.  The encoding of a <code>Handler</code> must be
   * set before any log records have been published.
   *
   * @param encoding the name of a character encoding, or <code>null</code>
   *            for the default encoding.
   *
   * @exception SecurityException if a security manager exists and
   *            the caller is not granted the permission to control
   *            the logging infrastructure.
   *
   */
  public void setEncoding(String encoding)
    throws SecurityException, UnsupportedEncodingException
  {
    /* Should any developer ever change this implementation, they are
     * advised to have a look at StreamHandler.setEncoding(String),
     * which overrides this method without calling super.setEncoding.
     */
    LogManager.getLogManager().checkAccess();

    /* Simple check for supported encodings. This is more expensive
     * than it could be, but this method is overwritten by StreamHandler
     * anyway.
     */
    if (encoding != null)
      new String(new byte[0], encoding);

    this.encoding = encoding;
  }


  /**
   * Returns the <code>Filter</code> that currently controls which
   * log records are being published by this <code>Handler</code>.
   *
   * @return the currently active <code>Filter</code>, or
   *         <code>null</code> if no filter has been associated.
   *         In the latter case, log records are filtered purely
   *         based on their severity level.
   */
  public Filter getFilter()
  {
    return filter;
  }


  /**
   * Sets the <code>Filter</code> for controlling which
   * log records will be published by this <code>Handler</code>.
   *
   * @return the <code>Filter</code> to use, or
   *         <code>null</code> to filter log records purely based
   *         on their severity level.
   */
  public void setFilter(Filter filter)
    throws SecurityException
  {
    LogManager.getLogManager().checkAccess();
    this.filter = filter;
  }


  /**
   * Returns the <code>ErrorManager</code> that currently deals
   * with errors originating from this Handler.
   *
   * @exception SecurityException if a security manager exists and
   *            the caller is not granted the permission to control
   *            the logging infrastructure.
   */
  public ErrorManager getErrorManager()
  {
    LogManager.getLogManager().checkAccess();

    /* Developers wanting to change the subsequent code should
     * have a look at Handler.reportError -- it also can create
     * an ErrorManager, but does so without checking permissions
     * to control the logging infrastructure.
     */
    if (errorManager == null)
      errorManager = new ErrorManager();

    return errorManager;
  }


  public void setErrorManager(ErrorManager manager)
  {
    LogManager.getLogManager().checkAccess();

    /* Make sure manager is not null. */
    manager.getClass();

    this.errorManager = manager;
  }


  protected void reportError(String message, Exception ex, int code)
  {
    if (errorManager == null)
      errorManager = new ErrorManager();

    errorManager.error(message, ex, code);
  }


  /**
   * Returns the severity level threshold for this <code>Handler</code>
   * All log records with a lower severity level will be discarded;
   * a log record of the same or a higher level will be published
   * unless an installed <code>Filter</code> decides to discard it.
   *
   * @return the severity level below which all log messages
   *         will be discarded.
   */
  public Level getLevel()
  {
    return level;
  }


  /**
   * Sets the severity level threshold for this <code>Handler</code>.
   * All log records with a lower severity level will be discarded;
   * a log record of the same or a higher level will be published
   * unless an installed <code>Filter</code> decides to discard it.
   *
   * @param level the severity level below which all log messages
   *              will be discarded.
   *
   * @exception SecurityException if a security manager exists and
   *            the caller is not granted the permission to control
   *            the logging infrastructure.
   *
   * @exception NullPointerException if <code>level</code> is
   *            <code>null</code>.
   */
  public void setLevel(Level level)
  {
    LogManager.getLogManager().checkAccess();

    /* Throw NullPointerException if level is null.  */
    level.getClass();
    this.level = level;
  }


  /**
   * Checks whether a <code>LogRecord</code> would be logged
   * if it was passed to this <code>Handler</code> for publication.
   *
   * <p>The <code>Handler</code> implementation considers a record as
   * loggable if its level is greater than or equal to the severity
   * level threshold.  In a second step, if a {@link Filter} has
   * been installed, its {@link Filter#isLoggable(LogRecord) isLoggable}
   * method is invoked. Subclasses of <code>Handler</code> can override
   * this method to impose their own constraints.
   *
   * @param record the <code>LogRecord</code> to be checked.
   *
   * @return <code>true</code> if <code>record</code> would
   *         be published by {@link #publish(LogRecord) publish},
   *         <code>false</code> if it would be discarded.
   *
   * @see #setLevel(Level)
   * @see #setFilter(Filter)
   * @see Filter#isLoggable(LogRecord)
   *
   * @throws NullPointerException if <code>record</code>
   *         is <code>null</code>.
   */
  public boolean isLoggable(LogRecord record)
  {
    if (record.getLevel().intValue() < level.intValue())
      return false;
    
    if (filter != null)
      return filter.isLoggable(record);
    else
      return true;
  }
}
