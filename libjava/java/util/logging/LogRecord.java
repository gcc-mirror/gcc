/* LogRecord.java
   -- a class for the state associated with individual logging events

Copyright (C) 2002, 2003 Free Software Foundation, Inc.

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

import java.util.ResourceBundle;


/**
 * A <code>LogRecord</code> contains the state for an individual
 * event to be logged.
 *
 * <p>As soon as a LogRecord instance has been handed over to the
 * logging framework, applications should not manipulate it anymore.
 *
 * @author Sascha Brawer (brawer@acm.org)
 */
public class LogRecord
  implements java.io.Serializable
{
  /**
   * The severity level of this <code>LogRecord</code>.
   */
  private Level      level;


  /**
   * The sequence number of this <code>LogRecord</code>.
   */
  private long       sequenceNumber;


  /**
   * The name of the class that issued the logging request, or
   * <code>null</code> if this information could not be obtained.
   */
  private String     sourceClassName;


  /**
   * The name of the method that issued the logging request, or
   * <code>null</code> if this information could not be obtained.
   */
  private String sourceMethodName;


  /**
   * The message for this <code>LogRecord</code> before
   * any localization or formatting.
   */
  private String message;


  /**
   * An identifier for the thread in which this <code>LogRecord</code>
   * was created.  The identifier is not necessarily related to any
   * thread identifiers used by the operating system.
   */
  private int threadID;


  /**
   * The time when this <code>LogRecord</code> was created,
   * in milliseconds since the beginning of January 1, 1970.
   */
  private long millis;


  /**
   * The Throwable associated with this <code>LogRecord</code>, or
   * <code>null</code> if the logged event is not related to an
   * exception or error.
   */
  private Throwable thrown;


  /**
   * The name of the logger where this <code>LogRecord</code> has
   * originated, or <code>null</code> if this <code>LogRecord</code>
   * does not originate from a <code>Logger</code>.
   */
  private String  loggerName;


  /**
   * The name of the resource bundle used for localizing log messages,
   * or <code>null</code> if no bundle has been specified.
   */
  private String resourceBundleName;

  private transient Object[] parameters;

  private transient ResourceBundle bundle;


  /**
   * Constructs a <code>LogRecord</code> given a severity level and
   * an unlocalized message text.  In addition, the sequence number,
   * creation time (as returned by <code>getMillis()</code>) and
   * thread ID are assigned. All other properties are set to
   * <code>null</code>.
   *
   * @param level the severity level, for example <code>Level.WARNING</code>.
   *
   * @param message the message text (which will be used as key
   *                for looking up the localized message text
   *                if a resource bundle has been associated). 
   */
  public LogRecord(Level level, String message)
  {
    this.level = level;
    this.message = message;
    this.millis = System.currentTimeMillis();

    /* A subclass of java.lang.Thread could override hashCode(),
     * in which case the result would not be guaranteed anymore
     * to be unique among all threads.  While System.identityHashCode
     * is not necessarily unique either, it at least cannot be
     * overridden by user code.  However, is might be a good idea
     * to use something better for generating thread IDs.
     */
    this.threadID = System.identityHashCode(Thread.currentThread());

    sequenceNumber = allocateSeqNum();
  }


  /**
   * Determined with the serialver tool of the Sun J2SE 1.4.
   */
  static final long serialVersionUID = 5372048053134512534L;

  private void readObject(java.io.ObjectInputStream in)
    throws java.io.IOException, java.lang.ClassNotFoundException
  {
    in.defaultReadObject();

    /* We assume that future versions will be downwards compatible,
     * so we can ignore the versions.
     */
    byte majorVersion = in.readByte();
    byte minorVersion = in.readByte();

    int numParams = in.readInt();
    if (numParams >= 0)
    {
      parameters = new Object[numParams];
      for (int i = 0; i < numParams; i++)
	parameters[i] = in.readObject();
    }
  }


  /**
   * @serialData The default fields, followed by a major byte version
   * number, followed by a minor byte version number, followed by
   * information about the log record parameters.  If
   * <code>parameters</code> is <code>null</code>, the integer -1 is
   * written, otherwise the length of the <code>parameters</code>
   * array (which can be zero), followed by the result of calling
   * {@link Object#toString() toString()} on the parameter (or
   * <code>null</code> if the parameter is <code>null</code>).
   *
   * <p><strong>Specification Note:</strong> The Javadoc for the
   * Sun reference implementation does not specify the version
   * number. FIXME: Reverse-engineer the JDK and file a bug
   * report with Sun, asking for amendment of the specification.
   */
  private void writeObject(java.io.ObjectOutputStream out)
    throws java.io.IOException
  {
    out.defaultWriteObject();

    /* Major, minor version number: The Javadoc for J2SE1.4 does not
     * specify the values.
     */
    out.writeByte(0);
    out.writeByte(0);

    if (parameters == null)
      out.writeInt(-1);
    else
    {
      out.writeInt(parameters.length);
      for (int i = 0; i < parameters.length; i++)
      {
	if (parameters[i] == null)
	  out.writeObject(null);
	else
	  out.writeObject(parameters[i].toString());
      }
    }
  }


  /**
   * Returns the name of the logger where this <code>LogRecord</code>
   * has originated.
   *
   * @return the name of the source {@link Logger}, or
   *         <code>null</code> if this <code>LogRecord</code>
   *         does not originate from a <code>Logger</code>.
   */
  public String getLoggerName()
  {
    return loggerName;
  }


  /**
   * Sets the name of the logger where this <code>LogRecord</code>
   * has originated.
   *
   * <p>As soon as a <code>LogRecord</code> has been handed over
   * to the logging framework, applications should not modify it
   * anymore.  Therefore, this method should only be called on
   * freshly constructed LogRecords.
   *
   * @param name the name of the source logger, or <code>null</code> to
   *             indicate that this <code>LogRecord</code> does not
   *             originate from a <code>Logger</code>.
   */
  public void setLoggerName(String name)
  {
    loggerName = name;
  }


  /**
   * Returns the resource bundle that is used when the message
   * of this <code>LogRecord</code> needs to be localized.
   *
   * @return the resource bundle used for localization,
   *         or <code>null</code> if this message does not need
   *         to be localized.
   */
  public ResourceBundle getResourceBundle()
  {
    return bundle;
  }


  /**
   * Sets the resource bundle that is used when the message
   * of this <code>LogRecord</code> needs to be localized.
   *
   * <p>As soon as a <code>LogRecord</code> has been handed over
   * to the logging framework, applications should not modify it
   * anymore.  Therefore, this method should only be called on
   * freshly constructed LogRecords.
   *
   * @param bundle  the resource bundle to be used, or
   *                <code>null</code> to indicate that this
   *                message does not need to be localized.
   */
  public void setResourceBundle(ResourceBundle bundle)
  {
    this.bundle = bundle;

    /* FIXME: Is there a way to infer the name
     * of a resource bundle from a ResourceBundle object?
     */
    this.resourceBundleName = null;
  }


  /**
   * Returns the name of the resource bundle that is used when the
   * message of this <code>LogRecord</code> needs to be localized.
   *
   * @return the name of the resource bundle used for localization,
   *         or <code>null</code> if this message does not need
   *         to be localized.
   */
  public String getResourceBundleName()
  {
    return resourceBundleName;
  }


  /**
   * Sets the name of the resource bundle that is used when the
   * message of this <code>LogRecord</code> needs to be localized.
   *
   * <p>As soon as a <code>LogRecord</code> has been handed over
   * to the logging framework, applications should not modify it
   * anymore.  Therefore, this method should only be called on
   * freshly constructed LogRecords.
   *
   * @param name the name of the resource bundle to be used, or
   *             <code>null</code> to indicate that this message
   *             does not need to be localized.
   */
  public void setResourceBundleName(String name)
  {
    resourceBundleName = name;
    bundle = null;
    
    try
    {
      if (resourceBundleName != null)
	bundle = ResourceBundle.getBundle(resourceBundleName);
    }
    catch (java.util.MissingResourceException _)
    {
    }
  }


  /**
   * Returns the level of the LogRecord.
   *
   * <p>Applications should be aware of the possibility that the
   *  result is not necessarily one of the standard logging levels,
   *  since the logging framework allows to create custom subclasses
   *  of <code>java.util.logging.Level</code>.  Therefore, filters
   *  should perform checks like <code>theRecord.getLevel().intValue()
   *  == Level.INFO.intValue()</code> instead of <code>theRecord.getLevel()
   *  == Level.INFO</code>.
   */
  public Level getLevel()
  {
    return level;
  }


  /**
   * Sets the severity level of this <code>LogRecord</code> to a new
   * value.
   *
   * <p>As soon as a <code>LogRecord</code> has been handed over
   * to the logging framework, applications should not modify it
   * anymore.  Therefore, this method should only be called on
   * freshly constructed LogRecords.
   *
   * @param level the new severity level, for example
   *              <code>Level.WARNING</code>.
   */
  public void setLevel(Level level)
  {
    this.level = level;
  }


  /**
   * The last used sequence number for any LogRecord.
   */
  private static long lastSeqNum = 0;


  /**
   * Allocates a sequence number for a new LogRecord.  This class
   * method is only called by the LogRecord constructor.
   */
  private synchronized static long allocateSeqNum()
  {
    lastSeqNum += 1;
    return lastSeqNum;
  }


  /**
   * Returns the sequence number of this <code>LogRecord</code>.
   */
  public long getSequenceNumber()
  {
    return sequenceNumber;
  }


  /**
   * Sets the sequence number of this <code>LogRecord</code> to a new
   * value.
   *
   * <p>As soon as a <code>LogRecord</code> has been handed over
   * to the logging framework, applications should not modify it
   * anymore.  Therefore, this method should only be called on
   * freshly constructed LogRecords.
   *
   * @param seqNum the new sequence number.
   */
  public void setSequenceNumber(long seqNum)
  {
    this.sequenceNumber = seqNum;
  }


  /**
   * Returns the name of the class where the event being logged
   * has had its origin.  This information can be passed as
   * parameter to some logging calls, and in certain cases, the
   * logging framework tries to determine an approximation
   * (which may or may not be accurate).
   * 
   * @return the name of the class that issued the logging request,
   *         or <code>null</code> if this information could not
   *         be obtained.
   */
  public String getSourceClassName()
  {
    if (sourceClassName != null)
      return sourceClassName;

    /*  FIXME: Should infer this information from the call stack. */
    return null;
  }


  /**
   * Sets the name of the class where the event being logged
   * has had its origin.
   *
   * <p>As soon as a <code>LogRecord</code> has been handed over
   * to the logging framework, applications should not modify it
   * anymore.  Therefore, this method should only be called on
   * freshly constructed LogRecords.
   * 
   * @param sourceClassName the name of the class that issued the
   *          logging request, or <code>null</code> to indicate that
   *          this information could not be obtained.
   */
  public void setSourceClassName(String sourceClassName)
  {
    this.sourceClassName = sourceClassName;
  }


  /**
   * Returns the name of the method where the event being logged
   * has had its origin.  This information can be passed as
   * parameter to some logging calls, and in certain cases, the
   * logging framework tries to determine an approximation
   * (which may or may not be accurate).
   * 
   * @return the name of the method that issued the logging request,
   *         or <code>null</code> if this information could not
   *         be obtained.
   */
  public String getSourceMethodName()
  {
    if (sourceMethodName != null)
      return sourceMethodName;

    /* FIXME: Should infer this information from the call stack. */
    return null;
  }


  /**
   * Sets the name of the method where the event being logged
   * has had its origin.
   *
   * <p>As soon as a <code>LogRecord</code> has been handed over
   * to the logging framework, applications should not modify it
   * anymore.  Therefore, this method should only be called on
   * freshly constructed LogRecords.
   * 
   * @param sourceMethodName the name of the method that issued the
   *          logging request, or <code>null</code> to indicate that
   *          this information could not be obtained.
   */
  public void setSourceMethodName(String sourceMethodName)
  {
    this.sourceMethodName = sourceMethodName;
  }


  /**
   * Returns the message for this <code>LogRecord</code> before
   * any localization or parameter substitution.
   *
   * <p>A {@link Logger} will try to localize the message
   * if a resource bundle has been associated with this
   * <code>LogRecord</code>.  In this case, the logger will call
   * <code>getMessage()</code> and use the result as the key
   * for looking up the localized message in the bundle.
   * If no bundle has been associated, or if the result of
   * <code>getMessage()</code> is not a valid key in the
   * bundle, the logger will use the raw message text as
   * returned by this method.
   *
   * @return the message text, or <code>null</code> if there
   *         is no message text.
   */
  public String getMessage()
  {
    return message;
  }


  /**
   * Sets the message for this <code>LogRecord</code>.
   *
   * <p>A <code>Logger</code> will try to localize the message
   * if a resource bundle has been associated with this
   * <code>LogRecord</code>.  In this case, the logger will call
   * <code>getMessage()</code> and use the result as the key
   * for looking up the localized message in the bundle.
   * If no bundle has been associated, or if the result of
   * <code>getMessage()</code> is not a valid key in the
   * bundle, the logger will use the raw message text as
   * returned by this method.
   *
   * <p>It is possible to set the message to either an empty String or
   * <code>null</code>, although this does not make the the message
   * very helpful to human users.
   *
   * @param message the message text (which will be used as key
   *                for looking up the localized message text
   *                if a resource bundle has been associated). 
   */
  public void setMessage(String message)
  {
    this.message = message;
  }


  /**
   * Returns the parameters to the log message.
   *
   * @return the parameters to the message, or <code>null</code> if
   *         the message has no parameters.
   */
  public Object[] getParameters()
  {
    return parameters;
  }


  /**
   * Sets the parameters to the log message.
   *
   * <p>As soon as a <code>LogRecord</code> has been handed over
   * to the logging framework, applications should not modify it
   * anymore.  Therefore, this method should only be called on
   * freshly constructed LogRecords.
   *
   * @param parameters the parameters to the message, or <code>null</code>
   *                   to indicate that the message has no parameters.
   */
  public void setParameters(Object[] parameters)
  {
    this.parameters = parameters;
  }


  /**
   * Returns an identifier for the thread in which this
   * <code>LogRecord</code> was created.  The identifier is not
   * necessarily related to any thread identifiers used by the
   * operating system.
   *
   * @return an identifier for the source thread.
   */
  public int getThreadID()
  {
    return threadID;
  }


  /**
   * Sets the identifier indicating in which thread this
   * <code>LogRecord</code> was created.  The identifier is not
   * necessarily related to any thread identifiers used by the
   * operating system.
   *
   * <p>As soon as a <code>LogRecord</code> has been handed over
   * to the logging framework, applications should not modify it
   * anymore.  Therefore, this method should only be called on
   * freshly constructed LogRecords.
   *
   * @param threadID the identifier for the source thread.
   */
  public void setThreadID(int threadID)
  {
    this.threadID = threadID;
  }


  /**
   * Returns the time when this <code>LogRecord</code> was created.
   *
   * @return the time of creation in milliseconds since the beginning
   *         of January 1, 1970.
   */
  public long getMillis()
  {
    return millis;
  }


  /**
   * Sets the time when this <code>LogRecord</code> was created.
   *
   * <p>As soon as a <code>LogRecord</code> has been handed over
   * to the logging framework, applications should not modify it
   * anymore.  Therefore, this method should only be called on
   * freshly constructed LogRecords.
   *
   * @param millis the time of creation in milliseconds since the
   *               beginning of January 1, 1970.
   */
  public void setMillis(long millis)
  {
    this.millis = millis;
  }


  /**
   * Returns the Throwable associated with this <code>LogRecord</code>,
   * or <code>null</code> if the logged event is not related to an exception
   * or error.
   */
  public Throwable getThrown()
  {
    return thrown;
  }


  /**
   * Associates this <code>LogRecord</code> with an exception or error.
   *
   * <p>As soon as a <code>LogRecord</code> has been handed over
   * to the logging framework, applications should not modify it
   * anymore.  Therefore, this method should only be called on
   * freshly constructed LogRecords.
   *
   * @param thrown the exception or error to associate with, or
   *               <code>null</code> if this <code>LogRecord</code>
   *               should be made unrelated to an exception or error.
   */
  public void setThrown(Throwable thrown)
  {
    this.thrown = thrown;
  }
}
