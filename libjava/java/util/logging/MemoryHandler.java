/* MemoryHandler.java
   -- a class for buffering log messages in a memory buffer

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
exception statement from your version.

*/


package java.util.logging;

/**
 * A <code>MemoryHandler</code> maintains a circular buffer of
 * log records.
 *
 * <p><strong>Configuration:</strong> Values of the subsequent
 * <code>LogManager</code> properties are taken into consideration
 * when a <code>MemoryHandler</code> is initialized.
 * If a property is not defined, or if it has an invalid
 * value, a default is taken without an exception being thrown.
 *
 * <ul>
 *
 * <li><code>java.util.MemoryHandler.level</code> - specifies
 *     the initial severity level threshold. Default value:
 *     <code>Level.ALL</code>.</li>
 *
 * <li><code>java.util.MemoryHandler.filter</code> - specifies
 *     the name of a Filter class. Default value: No Filter.</li>
 *
 * <li><code>java.util.MemoryHandler.size</code> - specifies the
 *     maximum number of log records that are kept in the circular
 *     buffer.  Default value: 1000.</li>
 *
 * <li><code>java.util.MemoryHandler.push</code> - specifies the
 *     <code>pushLevel</code>. Default value:
 *     <code>Level.SEVERE</code>.</li>
 *
 * <li><code>java.util.MemoryHandler.target</code> - specifies the
 *     name of a subclass of {@link Handler} that will be used as the
 *     target handler.  There is no default value for this property;
 *     if it is not set, the no-argument MemoryHandler constructor
 *     will throw an exception.</li>
 *
 * </ul>
 *
 * @author Sascha Brawer (brawer@acm.org)
 */
public class MemoryHandler
  extends Handler
{
  /**
   * The storage area used for buffering the unpushed log records in
   * memory.
   */
  private final LogRecord[] buffer;


  /**
   * The current position in the circular buffer. For a new
   * MemoryHandler, or immediately after {@link #push()} was called,
   * the value of this variable is zero.  Each call to {@link
   * #publish(LogRecord)} will store the published LogRecord into
   * <code>buffer[position]</code> before position is incremented by
   * one.  If position becomes greater than the size of the buffer, it
   * is reset to zero.
   */
  private int position;


  /**
   * The number of log records which have been published, but not
   * pushed yet to the target handler.
   */
  private int numPublished;


  /**
   * The push level threshold for this <code>Handler</code>.  When a
   * record is published whose severity level is greater than or equal
   * to the <code>pushLevel</code> of this <code>MemoryHandler</code>,
   * the {@link #push()} method will be invoked for pushing the buffer
   * contents to the target <code>Handler</code>.
   */
  private Level pushLevel;


  /**
   * The Handler to which log records are forwarded for actual
   * publication.
   */
  private final Handler target;


  /**
   * Constructs a <code>MemoryHandler</code> for keeping a circular
   * buffer of LogRecords; the initial configuration is determined by
   * the <code>LogManager</code> properties described above.
   */
  public MemoryHandler()
  {
    this((Handler) LogManager.getInstanceProperty(
	   "java.util.logging.MemoryHandler.target",
	   Handler.class, /* default */ null),
	 LogManager.getIntPropertyClamped(
	   "java.util.logging.MemoryHandler.size",
	   /* default */ 1000,
	   /* minimum value */ 1,
	   /* maximum value */ Integer.MAX_VALUE),
	 LogManager.getLevelProperty(
	   "java.util.logging.MemoryHandler.push",
	   /* default push level */ Level.SEVERE));
  }

  
  /**
   * Constructs a <code>MemoryHandler</code> for keeping a circular
   * buffer of LogRecords, given some parameters. The values of the
   * other parameters are taken from LogManager properties, as
   * described above.
   *
   * @param target the target handler that will receive those
   *               log records that are passed on for publication.
   *
   * @param size the number of log records that are kept in the buffer.
   *             The value must be a at least one.
   *
   * @param pushLevel the push level threshold for this
   *     <code>MemoryHandler</code>.  When a record is published whose
   *     severity level is greater than or equal to
   *     <code>pushLevel</code>, the {@link #push()} method will be
   *     invoked in order to push the bufffer contents to
   *     <code>target</code>.
   *
   * @throws java.lang.IllegalArgumentException if <code>size</code>
   *         is negative or zero. The GNU implementation also throws
   *         an IllegalArgumentException if <code>target</code> or
   *         <code>pushLevel</code> are <code>null</code>, but the
   *         API specification does not prescribe what should happen
   *         in those cases.
   */
  public MemoryHandler(Handler target, int size, Level pushLevel)
  { 
    if ((target == null) || (size <= 0) || (pushLevel == null))
      throw new IllegalArgumentException();

    buffer = new LogRecord[size];
    this.pushLevel = pushLevel;
    this.target = target;

    setLevel(LogManager.getLevelProperty(
      "java.util.logging.MemoryHandler.level",
      /* default value */ Level.ALL));

    setFilter((Filter) LogManager.getInstanceProperty(
      "java.util.logging.MemoryHandler.filter",
      /* must be instance of */ Filter.class,
      /* default value */ null));
  }


  /**
   * Stores a <code>LogRecord</code> in a fixed-size circular buffer,
   * provided the record passes all tests for being loggable.  If the
   * buffer is full, the oldest record will be discarded.
   *
   * <p>If the record has a severity level which is greater than or
   * equal to the <code>pushLevel</code> of this
   * <code>MemoryHandler</code>, the {@link #push()} method will be
   * invoked for pushing the buffer contents to the target
   * <code>Handler</code>.
   *
   * <p>Most applications do not need to call this method directly.
   * Instead, they will use use a {@link Logger}, which will create
   * LogRecords and distribute them to registered handlers.
   *
   * @param record the log event to be published.
   */
  public void publish(LogRecord record)
  {
    if (!isLoggable(record))
      return;

    buffer[position] = record;
    position = (position + 1) % buffer.length;
    numPublished = numPublished + 1;

    if (record.getLevel().intValue() >= pushLevel.intValue())
      push();
  }


  /**
   * Pushes the contents of the memory buffer to the target
   * <code>Handler</code> and clears the buffer. Note that
   * the target handler will discard those records that do
   * not satisfy its own severity level threshold, or that are
   * not considered loggable by an installed {@link Filter}.
   *
   * <p>In case of an I/O failure, the {@link ErrorManager} of the
   * target <code>Handler</code> will be notified, but the caller of
   * this method will not receive an exception.
   */
  public void push()
  {
    int i;

    if (numPublished < buffer.length)
    {
      for (i = 0; i < position; i++)
        target.publish(buffer[i]);
    }
    else
    {
      for (i = position; i < buffer.length; i++)
	target.publish(buffer[i]);
      for (i = 0; i < position; i++)
	target.publish(buffer[i]);
    }

    numPublished = 0;
    position = 0;
  }


  /**
   * Forces any data that may have been buffered by the target
   * <code>Handler</code> to the underlying output device, but
   * does <em>not</em> push the contents of the circular memory
   * buffer to the target handler.
   *
   * <p>In case of an I/O failure, the {@link ErrorManager} of the
   * target <code>Handler</code> will be notified, but the caller of
   * this method will not receive an exception.
   *
   * @see #push()
   */
  public void flush()
  {
    target.flush();
  }


  /**
   * Closes this <code>MemoryHandler</code> and its associated target
   * handler, discarding the contents of the memory buffer.  However,
   * any data that may have been buffered by the target
   * <code>Handler</code> is forced to the underlying output device.
   *
   * <p>As soon as <code>close</code> has been called,
   * a <code>Handler</code> should not be used anymore. Attempts
   * to publish log records, to flush buffers, or to modify the
   * <code>Handler</code> in any other way may throw runtime
   * exceptions after calling <code>close</code>.</p>
   *
   * <p>In case of an I/O failure, the <code>ErrorManager</code> of
   * the associated target <code>Handler</code> will be informed, but
   * the caller of this method will not receive an exception.</p>
   *
   * @throws SecurityException if a security manager exists and
   *         the caller is not granted the permission to control
   *         the logging infrastructure.
   *
   * @see #push()
   */
  public void close()
    throws SecurityException
  {
    push();

    /* This will check for LoggingPermission("control"). If the
     * current security context does not grant this permission,
     * push() has been executed, but this does not impose a
     * security risk.
     */
    target.close();
  }

    

  /**
   * Returns the push level threshold for this <code>Handler</code>.
   * When a record is published whose severity level is greater
   * than or equal to the <code>pushLevel</code> of this
   * <code>MemoryHandler</code>, the {@link #push()} method will be
   * invoked for pushing the buffer contents to the target
   * <code>Handler</code>.
   *
   * @return the push level threshold for automatic pushing.
   */
  public Level getPushLevel()
  {
    return pushLevel;
  }


  /**
   * Sets the push level threshold for this <code>Handler</code>.
   * When a record is published whose severity level is greater
   * than or equal to the <code>pushLevel</code> of this
   * <code>MemoryHandler</code>, the {@link #push()} method will be
   * invoked for pushing the buffer contents to the target
   * <code>Handler</code>.
   *
   * @param pushLevel the push level threshold for automatic pushing.
   *
   * @exception SecurityException if a security manager exists and
   *            the caller is not granted the permission to control
   *            the logging infrastructure.
   *
   * @exception NullPointerException if <code>pushLevel</code> is
   *            <code>null</code>.
   */
  public void setPushLevel(Level pushLevel)
  {
    LogManager.getLogManager().checkAccess();

    /* Throws a NullPointerException if pushLevel is null. */
    pushLevel.getClass();

    this.pushLevel = pushLevel;
  }
}
