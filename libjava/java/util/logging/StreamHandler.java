/* StreamHandler.java --
   A class for publishing log messages to instances of java.io.OutputStream
   Copyright (C) 2002 Free Software Foundation, Inc.

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

import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.io.Writer;

/**
 * A <code>StreamHandler</code> publishes <code>LogRecords</code> to
 * a instances of <code>java.io.OutputStream</code>.
 *
 * @author Sascha Brawer (brawer@acm.org)
 */
public class StreamHandler
  extends Handler
{
  private OutputStream  out;
  private Writer        writer;


 /**
  * Indicates the current state of this StreamHandler.  The value
  * should be one of STATE_FRESH, STATE_PUBLISHED, or STATE_CLOSED.
  */
  private int streamState = STATE_FRESH;


  /**
   * streamState having this value indicates that the StreamHandler
   * has been created, but the publish(LogRecord) method has not been
   * called yet.  If the StreamHandler has been constructed without an
   * OutputStream, writer will be null, otherwise it is set to a
   * freshly created OutputStreamWriter.
   */
  private static final int STATE_FRESH = 0;


  /**
   * streamState having this value indicates that the publish(LocRecord)
   * method has been called at least once.
   */
  private static final int STATE_PUBLISHED = 1;


  /**
   * streamState having this value indicates that the close() method
   * has been called.
   */
  private static final int STATE_CLOSED = 2;


  /**
   * Creates a <code>StreamHandler</code> without an output stream.
   * Subclasses can later use {@link
   * #setOutputStream(java.io.OutputStream)} to associate an output
   * stream with this StreamHandler.
   */
  public StreamHandler()
  {
    this(null, null);
  }


  /**
   * Creates a <code>StreamHandler</code> that formats log messages
   * with the specified Formatter and publishes them to the specified
   * output stream.
   *
   * @param out the output stream to which the formatted log messages
   *     are published.
   *
   * @param formatter the <code>Formatter</code> that will be used
   *     to format log messages.
   */
  public StreamHandler(OutputStream out, Formatter formatter)
  {
    this(out, "java.util.logging.StreamHandler", Level.INFO,
	 formatter, SimpleFormatter.class);
  }


  StreamHandler(
    OutputStream out,
    String propertyPrefix,
    Level defaultLevel,
    Formatter formatter, Class defaultFormatterClass)
  {
    this.level = LogManager.getLevelProperty(propertyPrefix + ".level",
					     defaultLevel);

    this.filter = (Filter) LogManager.getInstanceProperty(
      propertyPrefix + ".filter",
      /* must be instance of */       Filter.class,
      /* default: new instance of */  null);

    if (formatter != null)
      this.formatter = formatter;
    else
      this.formatter = (Formatter) LogManager.getInstanceProperty(
	propertyPrefix + ".formatter",
        /* must be instance of */       Formatter.class,
        /* default: new instance of */  defaultFormatterClass);

    try
    {
      String enc = LogManager.getLogManager().getProperty(propertyPrefix
							  + ".encoding");

      /* make sure enc actually is a valid encoding */
      if ((enc != null) && (enc.length() > 0))
        new String(new byte[0], enc);

      this.encoding = enc;
    }
    catch (Exception _)
    {
    }

    if (out != null)
    {
      try
      {
        changeWriter(out, getEncoding());
      }
      catch (UnsupportedEncodingException uex)
      {
	/* This should never happen, since the validity of the encoding
	 * name has been checked above.
	 */
	throw new RuntimeException(uex.getMessage());
      }
    }
  }


  private void checkOpen()
  {
    if (streamState == STATE_CLOSED)
      throw new IllegalStateException(this.toString() + " has been closed");
  }

  private void checkFresh()
  {
    checkOpen();
    if (streamState != STATE_FRESH)
      throw new IllegalStateException("some log records have been published to " + this);
  }


  private void changeWriter(OutputStream out, String encoding)
    throws UnsupportedEncodingException
  {
    OutputStreamWriter writer;

    /* The logging API says that a null encoding means the default
     * platform encoding. However, java.io.OutputStreamWriter needs
     * another constructor for the default platform encoding,
     * passing null would throw an exception.
     */
    if (encoding == null)
      writer = new OutputStreamWriter(out);
    else
      writer = new OutputStreamWriter(out, encoding);

    /* Closing the stream has side effects -- do this only after
     * creating a new writer has been successful.
     */
    if ((streamState != STATE_FRESH) || (this.writer != null))
      close();

    this.writer = writer;
    this.out = out;
    this.encoding = encoding;
    streamState = STATE_FRESH;
  }


  /**
   * Sets the character encoding which this handler uses for publishing
   * log records.  The encoding of a <code>StreamHandler</code> must be
   * set before any log records have been published.
   *
   * @param encoding the name of a character encoding, or <code>null</code>
   *            for the default encoding.
   *
   * @throws SecurityException if a security manager exists and
   *     the caller is not granted the permission to control the
   *     the logging infrastructure.
   *
   * @exception IllegalStateException if any log records have been
   *     published to this <code>StreamHandler</code> before.  Please
   *     be aware that this is a pecularity of the GNU implementation.
   *     While the API specification indicates that it is an error
   *     if the encoding is set after records have been published,
   *     it does not mandate any specific behavior for that case.
   */
  public void setEncoding(String encoding)
    throws SecurityException, UnsupportedEncodingException
  {
    /* The inherited implementation first checks whether the invoking
     * code indeed has the permission to control the logging infra-
     * structure, and throws a SecurityException if this was not the
     * case.
     *
     * Next, it verifies that the encoding is supported and throws
     * an UnsupportedEncodingExcpetion otherwise. Finally, it remembers
     * the name of the encoding.
     */
    super.setEncoding(encoding);

    checkFresh();

    /* If out is null, setEncoding is being called before an output
     * stream has been set. In that case, we need to check that the
     * encoding is valid, and remember it if this is the case.  Since
     * this is exactly what the inherited implementation of
     * Handler.setEncoding does, we can delegate.
     */
    if (out != null)
    {
      /* The logging API says that a null encoding means the default
       * platform encoding. However, java.io.OutputStreamWriter needs
       * another constructor for the default platform encoding, passing
       * null would throw an exception.
       */
      if (encoding == null)
	writer = new OutputStreamWriter(out);
      else
	writer = new OutputStreamWriter(out, encoding);
    }
  }


  /**
   * Changes the output stream to which this handler publishes
   * logging records.
   *
   * @throws SecurityException if a security manager exists and
   *         the caller is not granted the permission to control
   *         the logging infrastructure.
   *
   * @throws NullPointerException if <code>out</code>
   *         is <code>null</code>.
   */
  protected void setOutputStream(OutputStream out)
    throws SecurityException
  {
    LogManager.getLogManager().checkAccess();

    /* Throw a NullPointerException if out is null. */
    out.getClass();

    try
    {
      changeWriter(out, getEncoding());
    }
    catch (UnsupportedEncodingException ex)
    {
      /* This seems quite unlikely to happen, unless the underlying
       * implementation of java.io.OutputStreamWriter changes its
       * mind (at runtime) about the set of supported character
       * encodings.
       */
      throw new RuntimeException(ex.getMessage());
    }
  }


  /**
   * Publishes a <code>LogRecord</code> to the associated output
   * stream, provided the record passes all tests for being loggable.
   * The <code>StreamHandler</code> will localize the message of the
   * log record and substitute any message parameters.
   *
   * <p>Most applications do not need to call this method directly.
   * Instead, they will use use a {@link Logger}, which will create
   * LogRecords and distribute them to registered handlers.
   *
   * <p>In case of an I/O failure, the <code>ErrorManager</code>
   * of this <code>Handler</code> will be informed, but the caller
   * of this method will not receive an exception.
   *
   * <p>If a log record is being published to a
   * <code>StreamHandler</code> that has been closed earlier, the Sun
   * J2SE 1.4 reference can be observed to silently ignore the
   * call. The GNU implementation, however, intentionally behaves
   * differently by informing the <code>ErrorManager</code> associated
   * with this <code>StreamHandler</code>.  Since the condition
   * indicates a programming error, the programmer should be
   * informed. It also seems extremely unlikely that any application
   * would depend on the exact behavior in this rather obscure,
   * erroneous case -- especially since the API specification does not
   * prescribe what is supposed to happen.
   * 
   * @param record the log event to be published.
   */
  public void publish(LogRecord record)
  {
    String formattedMessage;

    if (!isLoggable(record))
      return;

    if (streamState == STATE_FRESH)
    {
      try
      {
        writer.write(formatter.getHead(this));
      }
      catch (java.io.IOException ex)
      {
	reportError(null, ex, ErrorManager.WRITE_FAILURE);
	return;
      }
      catch (Exception ex)
      {
	reportError(null, ex, ErrorManager.GENERIC_FAILURE);
	return;
      }

      streamState = STATE_PUBLISHED;
    }

    try
    {
      formattedMessage = formatter.format(record);
    }
    catch (Exception ex)
    {
      reportError(null, ex, ErrorManager.FORMAT_FAILURE);
      return;
    }

    try
    {
      writer.write(formattedMessage);
    }
    catch (Exception ex)
    {
      reportError(null, ex, ErrorManager.WRITE_FAILURE);
    }
  }


  /**
   * Checks whether or not a <code>LogRecord</code> would be logged
   * if it was passed to this <code>StreamHandler</code> for publication.
   *
   * <p>The <code>StreamHandler</code> implementation first checks
   * whether a writer is present and the handler's level is greater
   * than or equal to the severity level threshold.  In a second step,
   * if a {@link Filter} has been installed, its {@link
   * Filter#isLoggable(LogRecord) isLoggable} method is
   * invoked. Subclasses of <code>StreamHandler</code> can override
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
   * @throws NullPointerException if <code>record</code> is
   *         <code>null</code>.  */
  public boolean isLoggable(LogRecord record)
  {
    return (writer != null) && super.isLoggable(record);
  }


  /**
   * Forces any data that may have been buffered to the underlying
   * output device.
   *
   * <p>In case of an I/O failure, the <code>ErrorManager</code>
   * of this <code>Handler</code> will be informed, but the caller
   * of this method will not receive an exception.
   *
   * <p>If a <code>StreamHandler</code> that has been closed earlier
   * is closed a second time, the Sun J2SE 1.4 reference can be
   * observed to silently ignore the call. The GNU implementation,
   * however, intentionally behaves differently by informing the
   * <code>ErrorManager</code> associated with this
   * <code>StreamHandler</code>.  Since the condition indicates a
   * programming error, the programmer should be informed. It also
   * seems extremely unlikely that any application would depend on the
   * exact behavior in this rather obscure, erroneous case --
   * especially since the API specification does not prescribe what is
   * supposed to happen.
   */
  public void flush()
  {
    try
    {
      checkOpen();
      if (writer != null)
        writer.flush();
    }
    catch (Exception ex)
    {
      reportError(null, ex, ErrorManager.FLUSH_FAILURE);
    }
  }


  /**
   * Closes this <code>StreamHandler</code> after having forced any
   * data that may have been buffered to the underlying output
   * device. 
   *
   * <p>As soon as <code>close</code> has been called,
   * a <code>Handler</code> should not be used anymore. Attempts
   * to publish log records, to flush buffers, or to modify the
   * <code>Handler</code> in any other way may throw runtime
   * exceptions after calling <code>close</code>.</p>
   *
   * <p>In case of an I/O failure, the <code>ErrorManager</code>
   * of this <code>Handler</code> will be informed, but the caller
   * of this method will not receive an exception.</p>
   *
   * <p>If a <code>StreamHandler</code> that has been closed earlier
   * is closed a second time, the Sun J2SE 1.4 reference can be
   * observed to silently ignore the call. The GNU implementation,
   * however, intentionally behaves differently by informing the
   * <code>ErrorManager</code> associated with this
   * <code>StreamHandler</code>.  Since the condition indicates a
   * programming error, the programmer should be informed. It also
   * seems extremely unlikely that any application would depend on the
   * exact behavior in this rather obscure, erroneous case --
   * especially since the API specification does not prescribe what is
   * supposed to happen.
   *
   * @throws SecurityException if a security manager exists and
   *         the caller is not granted the permission to control
   *         the logging infrastructure.
   */
  public void close()
    throws SecurityException
  {
    LogManager.getLogManager().checkAccess();

    try
    {
      /* Although  flush also calls checkOpen, it catches
       * any exceptions and reports them to the ErrorManager
       * as flush failures.  However, we want to report
       * a closed stream as a close failure, not as a
       * flush failure here.  Therefore, we call checkOpen()
       * before flush().
       */
      checkOpen();
      flush();

      if (writer != null)
      {
	if (formatter != null)
	{
	  /* Even if the StreamHandler has never published a record,
	   * it emits head and tail upon closing. An earlier version
	   * of the GNU Classpath implementation did not emitted
	   * anything. However, this had caused XML log files to be
	   * entirely empty instead of containing no log records.
	   */
	  if (streamState == STATE_FRESH)
            writer.write(formatter.getHead(this));
	  if (streamState != STATE_CLOSED)
	    writer.write(formatter.getTail(this));
	}
	streamState = STATE_CLOSED;
        writer.close();
      }
    }
    catch (Exception ex)
    {
      reportError(null, ex, ErrorManager.CLOSE_FAILURE);
    }
  }
}
