/* Logger.java -- a class for logging messages
   Copyright (C) 2002, 2004, 2006, 2007 Free Software Foundation, Inc.

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

import java.util.List;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.security.AccessController;
import java.security.PrivilegedAction;

/**
 * A Logger is used for logging information about events. Usually, there is a
 * seprate logger for each subsystem or component, although there is a shared
 * instance for components that make only occasional use of the logging
 * framework.
 * <p>
 * It is common to name a logger after the name of a corresponding Java package.
 * Loggers are organized into a hierarchical namespace; for example, the logger
 * <code>"org.gnu.foo"</code> is the <em>parent</em> of logger
 * <code>"org.gnu.foo.bar"</code>.
 * <p>
 * A logger for a named subsystem can be obtained through {@link
 * java.util.logging.Logger#getLogger(java.lang.String)}. However, only code
 * which has been granted the permission to control the logging infrastructure
 * will be allowed to customize that logger. Untrusted code can obtain a
 * private, anonymous logger through {@link #getAnonymousLogger()} if it wants
 * to perform any modifications to the logger.
 * <p>
 * FIXME: Write more documentation.
 *
 * @author Sascha Brawer (brawer@acm.org)
 */
public class Logger
{
  static final Logger root = new Logger("", null);

  /**
   * A logger provided to applications that make only occasional use of the
   * logging framework, typically early prototypes. Serious products are
   * supposed to create and use their own Loggers, so they can be controlled
   * individually.
   */
  public static final Logger global;

  /**
   * Use to lock methods on this class instead of calling synchronize on methods
   * to avoid deadlocks. Yeah, no kidding, we got them :)
   */
  private static final Object[] lock = new Object[0];

  static
    {
      // Our class might be initialized from an unprivileged context
      global = (Logger) AccessController.doPrivileged(new PrivilegedAction()
      {
        public Object run()
        {
          return getLogger("global");
        }
      });
    }

  /**
   * The name of the Logger, or <code>null</code> if the logger is anonymous.
   * <p>
   * A previous version of the GNU Classpath implementation granted untrusted
   * code the permission to control any logger whose name was null. However,
   * test code revealed that the Sun J2SE 1.4 reference implementation enforces
   * the security control for any logger that was not created through
   * getAnonymousLogger, even if it has a null name. Therefore, a separate flag
   * {@link Logger#anonymous} was introduced.
   */
  private final String name;

  /**
   * The name of the resource bundle used for localization.
   * <p>
   * This variable cannot be declared as <code>final</code> because its value
   * can change as a result of calling getLogger(String,String).
   */
  private String resourceBundleName;

  /**
   * The resource bundle used for localization.
   * <p>
   * This variable cannot be declared as <code>final</code> because its value
   * can change as a result of calling getLogger(String,String).
   */
  private ResourceBundle resourceBundle;

  private Filter filter;

  private final List handlerList = new java.util.ArrayList(4);

  private Handler[] handlers = new Handler[0];

  /**
   * Indicates whether or not this logger is anonymous. While a
   * LoggingPermission is required for any modifications to a normal logger,
   * untrusted code can obtain an anonymous logger and modify it according to
   * its needs.
   * <p>
   * A previous version of the GNU Classpath implementation granted access to
   * every logger whose name was null. However, test code revealed that the Sun
   * J2SE 1.4 reference implementation enforces the security control for any
   * logger that was not created through getAnonymousLogger, even if it has a
   * null name.
   */
  private boolean anonymous;

  private boolean useParentHandlers;

  private Level level;

  private Logger parent;

  /**
   * Constructs a Logger for a subsystem. Most applications do not need to
   * create new Loggers explicitly; instead, they should call the static factory
   * methods {@link #getLogger(java.lang.String,java.lang.String) getLogger}
   * (with ResourceBundle for localization) or
   * {@link #getLogger(java.lang.String) getLogger} (without ResourceBundle),
   * respectively.
   *
   * @param name the name for the logger, for example "java.awt" or
   *            "com.foo.bar". The name should be based on the name of the
   *            package issuing log records and consist of dot-separated Java
   *            identifiers.
   * @param resourceBundleName the name of a resource bundle for localizing
   *            messages, or <code>null</code> to indicate that messages do
   *            not need to be localized.
   * @throws java.util.MissingResourceException if
   *             <code>resourceBundleName</code> is not <code>null</code>
   *             and no such bundle could be located.
   */
  protected Logger(String name, String resourceBundleName)
      throws MissingResourceException
  {
    this.name = name;
    this.resourceBundleName = resourceBundleName;

    if (resourceBundleName == null)
      resourceBundle = null;
    else
      resourceBundle = ResourceBundle.getBundle(resourceBundleName);

    level = null;

    /*
     * This is null when the root logger is being constructed, and the root
     * logger afterwards.
     */
    parent = root;

    useParentHandlers = (parent != null);
  }

  /**
   * Finds a registered logger for a subsystem, or creates one in case no logger
   * has been registered yet.
   *
   * @param name the name for the logger, for example "java.awt" or
   *            "com.foo.bar". The name should be based on the name of the
   *            package issuing log records and consist of dot-separated Java
   *            identifiers.
   * @throws IllegalArgumentException if a logger for the subsystem identified
   *             by <code>name</code> has already been created, but uses a a
   *             resource bundle for localizing messages.
   * @throws NullPointerException if <code>name</code> is <code>null</code>.
   * @return a logger for the subsystem specified by <code>name</code> that
   *         does not localize messages.
   */
  public static Logger getLogger(String name)
  {
    return getLogger(name, null);
  }

  /**
   * Finds a registered logger for a subsystem, or creates one in case no logger
   * has been registered yet.
   * <p>
   * If a logger with the specified name has already been registered, the
   * behavior depends on the resource bundle that is currently associated with
   * the existing logger.
   * <ul>
   * <li>If the existing logger uses the same resource bundle as specified by
   * <code>resourceBundleName</code>, the existing logger is returned.</li>
   * <li>If the existing logger currently does not localize messages, the
   * existing logger is modified to use the bundle specified by
   * <code>resourceBundleName</code>. The existing logger is then returned.
   * Therefore, all subsystems currently using this logger will produce
   * localized messages from now on.</li>
   * <li>If the existing logger already has an associated resource bundle, but
   * a different one than specified by <code>resourceBundleName</code>, an
   * <code>IllegalArgumentException</code> is thrown.</li>
   * </ul>
   *
   * @param name the name for the logger, for example "java.awt" or
   *            "org.gnu.foo". The name should be based on the name of the
   *            package issuing log records and consist of dot-separated Java
   *            identifiers.
   * @param resourceBundleName the name of a resource bundle for localizing
   *            messages, or <code>null</code> to indicate that messages do
   *            not need to be localized.
   * @return a logger for the subsystem specified by <code>name</code>.
   * @throws java.util.MissingResourceException if
   *             <code>resourceBundleName</code> is not <code>null</code>
   *             and no such bundle could be located.
   * @throws IllegalArgumentException if a logger for the subsystem identified
   *             by <code>name</code> has already been created, but uses a
   *             different resource bundle for localizing messages.
   * @throws NullPointerException if <code>name</code> is <code>null</code>.
   */
  public static Logger getLogger(String name, String resourceBundleName)
  {
    LogManager lm = LogManager.getLogManager();
    Logger result;

    if (name == null)
      throw new NullPointerException();

    /*
     * Without synchronized(lm), it could happen that another thread would
     * create a logger between our calls to getLogger and addLogger. While
     * addLogger would indicate this by returning false, we could not be sure
     * that this other logger was still existing when we called getLogger a
     * second time in order to retrieve it -- note that LogManager is only
     * allowed to keep weak references to registered loggers, so Loggers can be
     * garbage collected at any time in general, and between our call to
     * addLogger and our second call go getLogger in particular. Of course, we
     * assume here that LogManager.addLogger etc. are synchronizing on the
     * global LogManager object. There is a comment in the implementation of
     * LogManager.addLogger referring to this comment here, so that any change
     * in the synchronization of LogManager will be reflected here.
     */
    synchronized (lock)
      {
        synchronized (lm)
          {
            result = lm.getLogger(name);
            if (result == null)
              {
                boolean couldBeAdded;

                result = new Logger(name, resourceBundleName);
                couldBeAdded = lm.addLogger(result);
                if (! couldBeAdded)
                  throw new IllegalStateException("cannot register new logger");
              }
            else
              {
                /*
                 * The logger already exists. Make sure it uses the same
                 * resource bundle for localizing messages.
                 */
                String existingBundleName = result.getResourceBundleName();

                /*
                 * The Sun J2SE 1.4 reference implementation will return the
                 * registered logger object, even if it does not have a resource
                 * bundle associated with it. However, it seems to change the
                 * resourceBundle of the registered logger to the bundle whose
                 * name was passed to getLogger.
                 */
                if ((existingBundleName == null) &&
                    (resourceBundleName != null))
                  {
                    /*
                     * If ResourceBundle.getBundle throws an exception, the
                     * existing logger will be unchanged. This would be
                     * different if the assignment to resourceBundleName came
                     * first.
                     */
                    result.resourceBundle =
                      ResourceBundle.getBundle(resourceBundleName);

                    result.resourceBundleName = resourceBundleName;
                    return result;
                  }

                if ((existingBundleName != resourceBundleName)
                    && ((existingBundleName == null)
                        || !existingBundleName.equals(resourceBundleName)))
                  {
                    throw new IllegalArgumentException();
                  }
              }
          }
      }

    return result;
  }

  /**
   * Creates a new, unnamed logger. Unnamed loggers are not registered in the
   * namespace of the LogManager, and no special security permission is required
   * for changing their state. Therefore, untrusted applets are able to modify
   * their private logger instance obtained through this method.
   * <p>
   * The parent of the newly created logger will the the root logger, from which
   * the level threshold and the handlers are inherited.
   */
  public static Logger getAnonymousLogger()
  {
    return getAnonymousLogger(null);
  }

  /**
   * Creates a new, unnamed logger. Unnamed loggers are not registered in the
   * namespace of the LogManager, and no special security permission is required
   * for changing their state. Therefore, untrusted applets are able to modify
   * their private logger instance obtained through this method.
   * <p>
   * The parent of the newly created logger will the the root logger, from which
   * the level threshold and the handlers are inherited.
   *
   * @param resourceBundleName the name of a resource bundle for localizing
   *            messages, or <code>null</code> to indicate that messages do
   *            not need to be localized.
   * @throws java.util.MissingResourceException if
   *             <code>resourceBundleName</code> is not <code>null</code>
   *             and no such bundle could be located.
   */
  public static Logger getAnonymousLogger(String resourceBundleName)
      throws MissingResourceException
  {
    Logger result;

    result = new Logger(null, resourceBundleName);
    result.anonymous = true;
    return result;
  }

  /**
   * Returns the name of the resource bundle that is being used for localizing
   * messages.
   *
   * @return the name of the resource bundle used for localizing messages, or
   *         <code>null</code> if the parent's resource bundle is used for
   *         this purpose.
   */
  public String getResourceBundleName()
  {
    synchronized (lock)
      {
        return resourceBundleName;
      }
  }

  /**
   * Returns the resource bundle that is being used for localizing messages.
   *
   * @return the resource bundle used for localizing messages, or
   *         <code>null</code> if the parent's resource bundle is used for
   *         this purpose.
   */
  public ResourceBundle getResourceBundle()
  {
    synchronized (lock)
      {
        return resourceBundle;
      }
  }

  /**
   * Returns the severity level threshold for this <code>Handler</code>. All
   * log records with a lower severity level will be discarded; a log record of
   * the same or a higher level will be published unless an installed
   * <code>Filter</code> decides to discard it.
   *
   * @return the severity level below which all log messages will be discarded,
   *         or <code>null</code> if the logger inherits the threshold from
   *         its parent.
   */
  public Level getLevel()
  {
    synchronized (lock)
      {
        return level;
      }
  }

  /**
   * Returns whether or not a message of the specified level would be logged by
   * this logger.
   *
   * @throws NullPointerException if <code>level</code> is <code>null</code>.
   */
  public boolean isLoggable(Level level)
  {
    synchronized (lock)
      {
        if (this.level != null)
          return this.level.intValue() <= level.intValue();

        if (parent != null)
          return parent.isLoggable(level);
        else
          return false;
      }
  }

  /**
   * Sets the severity level threshold for this <code>Handler</code>. All log
   * records with a lower severity level will be discarded immediately. A log
   * record of the same or a higher level will be published unless an installed
   * <code>Filter</code> decides to discard it.
   *
   * @param level the severity level below which all log messages will be
   *            discarded, or <code>null</code> to indicate that the logger
   *            should inherit the threshold from its parent.
   * @throws SecurityException if this logger is not anonymous, a security
   *             manager exists, and the caller is not granted the permission to
   *             control the logging infrastructure by having
   *             LoggingPermission("control"). Untrusted code can obtain an
   *             anonymous logger through the static factory method
   *             {@link #getAnonymousLogger(java.lang.String) getAnonymousLogger}.
   */
  public void setLevel(Level level)
  {
    synchronized (lock)
      {
        /*
         * An application is allowed to control an anonymous logger without
         * having the permission to control the logging infrastructure.
         */
        if (! anonymous)
          LogManager.getLogManager().checkAccess();

        this.level = level;
      }
  }

  public Filter getFilter()
  {
    synchronized (lock)
      {
        return filter;
      }
  }

  /**
   * @throws SecurityException if this logger is not anonymous, a security
   *             manager exists, and the caller is not granted the permission to
   *             control the logging infrastructure by having
   *             LoggingPermission("control"). Untrusted code can obtain an
   *             anonymous logger through the static factory method
   *             {@link #getAnonymousLogger(java.lang.String) getAnonymousLogger}.
   */
  public void setFilter(Filter filter) throws SecurityException
  {
    synchronized (lock)
      {
        /*
         * An application is allowed to control an anonymous logger without
         * having the permission to control the logging infrastructure.
         */
        if (! anonymous)
          LogManager.getLogManager().checkAccess();

        this.filter = filter;
      }
  }

  /**
   * Returns the name of this logger.
   *
   * @return the name of this logger, or <code>null</code> if the logger is
   *         anonymous.
   */
  public String getName()
  {
    /*
     * Note that the name of a logger cannot be changed during its lifetime, so
     * no synchronization is needed.
     */
    return name;
  }

  /**
   * Passes a record to registered handlers, provided the record is considered
   * as loggable both by {@link #isLoggable(Level)} and a possibly installed
   * custom {@link #setFilter(Filter) filter}.
   * <p>
   * If the logger has been configured to use parent handlers, the record will
   * be forwarded to the parent of this logger in addition to being processed by
   * the handlers registered with this logger.
   * <p>
   * The other logging methods in this class are convenience methods that merely
   * create a new LogRecord and pass it to this method. Therefore, subclasses
   * usually just need to override this single method for customizing the
   * logging behavior.
   *
   * @param record the log record to be inspected and possibly forwarded.
   */
  public void log(LogRecord record)
  {
    synchronized (lock)
      {
        if (!isLoggable(record.getLevel()))
          return;

        if ((filter != null) && ! filter.isLoggable(record))
          return;

        /*
         * If no logger name has been set for the log record, use the name of
         * this logger.
         */
        if (record.getLoggerName() == null)
          record.setLoggerName(name);

        /*
         * Avoid that some other thread is changing the logger hierarchy while
         * we are traversing it.
         */
        synchronized (LogManager.getLogManager())
          {
            Logger curLogger = this;

            do
              {
                /*
                 * The Sun J2SE 1.4 reference implementation seems to call the
                 * filter only for the logger whose log method is called, never
                 * for any of its parents. Also, parent loggers publish log
                 * record whatever their level might be. This is pretty weird,
                 * but GNU Classpath tries to be as compatible as possible to
                 * the reference implementation.
                 */
                for (int i = 0; i < curLogger.handlers.length; i++)
                  curLogger.handlers[i].publish(record);

                if (curLogger.getUseParentHandlers() == false)
                  break;

                curLogger = curLogger.getParent();
              }
            while (parent != null);
          }
      }
  }

  public void log(Level level, String message)
  {
    if (isLoggable(level))
      log(level, message, (Object[]) null);
  }

  public void log(Level level, String message, Object param)
  {
    synchronized (lock)
      {
        if (isLoggable(level))
          {
            StackTraceElement caller = getCallerStackFrame();
            logp(level, caller != null ? caller.getClassName() : "<unknown>",
                 caller != null ? caller.getMethodName() : "<unknown>",
                 message, param);
          }
      }
  }

  public void log(Level level, String message, Object[] params)
  {
    synchronized (lock)
      {
        if (isLoggable(level))
          {
            StackTraceElement caller = getCallerStackFrame();
            logp(level, caller != null ? caller.getClassName() : "<unknown>",
                 caller != null ? caller.getMethodName() : "<unknown>",
                 message, params);

          }
      }
  }

  public void log(Level level, String message, Throwable thrown)
  {
    synchronized (lock)
      {
        if (isLoggable(level))
          {
            StackTraceElement caller = getCallerStackFrame();
            logp(level, caller != null ? caller.getClassName() : "<unknown>",
                 caller != null ? caller.getMethodName() : "<unknown>",
                 message, thrown);
          }
      }
  }

  public void logp(Level level, String sourceClass, String sourceMethod,
                   String message)
  {
    synchronized (lock)
      {
        logp(level, sourceClass, sourceMethod, message, (Object[]) null);
      }
  }

  public void logp(Level level, String sourceClass, String sourceMethod,
                   String message, Object param)
  {
    synchronized (lock)
      {
        logp(level, sourceClass, sourceMethod, message, new Object[] { param });
      }

  }

  private ResourceBundle findResourceBundle()
  {
    synchronized (lock)
      {
        if (resourceBundle != null)
          return resourceBundle;

        if (parent != null)
          return parent.findResourceBundle();

        return null;
      }
  }

  private void logImpl(Level level, String sourceClass, String sourceMethod,
                       String message, Object[] params)
  {
    synchronized (lock)
      {
        LogRecord rec = new LogRecord(level, message);

        rec.setResourceBundle(findResourceBundle());
        rec.setSourceClassName(sourceClass);
        rec.setSourceMethodName(sourceMethod);
        rec.setParameters(params);

        log(rec);
      }
  }

  public void logp(Level level, String sourceClass, String sourceMethod,
                   String message, Object[] params)
  {
    synchronized (lock)
      {
        logImpl(level, sourceClass, sourceMethod, message, params);
      }
  }

  public void logp(Level level, String sourceClass, String sourceMethod,
                   String message, Throwable thrown)
  {
    synchronized (lock)
      {
        LogRecord rec = new LogRecord(level, message);

        rec.setResourceBundle(resourceBundle);
        rec.setSourceClassName(sourceClass);
        rec.setSourceMethodName(sourceMethod);
        rec.setThrown(thrown);

        log(rec);
      }
  }

  public void logrb(Level level, String sourceClass, String sourceMethod,
                    String bundleName, String message)
  {
    synchronized (lock)
      {
        logrb(level, sourceClass, sourceMethod, bundleName, message,
              (Object[]) null);
      }
  }

  public void logrb(Level level, String sourceClass, String sourceMethod,
                    String bundleName, String message, Object param)
  {
    synchronized (lock)
      {
        logrb(level, sourceClass, sourceMethod, bundleName, message,
              new Object[] { param });
      }
  }

  public void logrb(Level level, String sourceClass, String sourceMethod,
                    String bundleName, String message, Object[] params)
  {
    synchronized (lock)
      {
        LogRecord rec = new LogRecord(level, message);

        rec.setResourceBundleName(bundleName);
        rec.setSourceClassName(sourceClass);
        rec.setSourceMethodName(sourceMethod);
        rec.setParameters(params);

        log(rec);
      }
  }

  public void logrb(Level level, String sourceClass, String sourceMethod,
                    String bundleName, String message, Throwable thrown)
  {
    synchronized (lock)
      {
        LogRecord rec = new LogRecord(level, message);

        rec.setResourceBundleName(bundleName);
        rec.setSourceClassName(sourceClass);
        rec.setSourceMethodName(sourceMethod);
        rec.setThrown(thrown);

        log(rec);
      }
  }

  public void entering(String sourceClass, String sourceMethod)
  {
    synchronized (lock)
      {
        if (isLoggable(Level.FINER))
          logp(Level.FINER, sourceClass, sourceMethod, "ENTRY");
      }
  }

  public void entering(String sourceClass, String sourceMethod, Object param)
  {
    synchronized (lock)
      {
        if (isLoggable(Level.FINER))
          logp(Level.FINER, sourceClass, sourceMethod, "ENTRY {0}", param);
      }
  }

  public void entering(String sourceClass, String sourceMethod, Object[] params)
  {
    synchronized (lock)
      {
        if (isLoggable(Level.FINER))
          {
            CPStringBuilder buf = new CPStringBuilder(80);
            buf.append("ENTRY");
            for (int i = 0; i < params.length; i++)
              {
                buf.append(" {");
                buf.append(i);
                buf.append('}');
              }

            logp(Level.FINER, sourceClass, sourceMethod, buf.toString(), params);
          }
      }
  }

  public void exiting(String sourceClass, String sourceMethod)
  {
    synchronized (lock)
      {
        if (isLoggable(Level.FINER))
          logp(Level.FINER, sourceClass, sourceMethod, "RETURN");
      }
  }

  public void exiting(String sourceClass, String sourceMethod, Object result)
  {
    synchronized (lock)
      {
        if (isLoggable(Level.FINER))
          logp(Level.FINER, sourceClass, sourceMethod, "RETURN {0}", result);
      }
  }

  public void throwing(String sourceClass, String sourceMethod, Throwable thrown)
  {
    synchronized (lock)
      {
        if (isLoggable(Level.FINER))
          logp(Level.FINER, sourceClass, sourceMethod, "THROW", thrown);
      }
  }

  /**
   * Logs a message with severity level SEVERE, indicating a serious failure
   * that prevents normal program execution. Messages at this level should be
   * understandable to an inexperienced, non-technical end user. Ideally, they
   * explain in simple words what actions the user can take in order to resolve
   * the problem.
   *
   * @see Level#SEVERE
   * @param message the message text, also used as look-up key if the logger is
   *            localizing messages with a resource bundle. While it is possible
   *            to pass <code>null</code>, this is not recommended, since a
   *            logging message without text is unlikely to be helpful.
   */
  public void severe(String message)
  {
    synchronized (lock)
      {
        if (isLoggable(Level.SEVERE))
          log(Level.SEVERE, message);
      }
  }

  /**
   * Logs a message with severity level WARNING, indicating a potential problem
   * that does not prevent normal program execution. Messages at this level
   * should be understandable to an inexperienced, non-technical end user.
   * Ideally, they explain in simple words what actions the user can take in
   * order to resolve the problem.
   *
   * @see Level#WARNING
   * @param message the message text, also used as look-up key if the logger is
   *            localizing messages with a resource bundle. While it is possible
   *            to pass <code>null</code>, this is not recommended, since a
   *            logging message without text is unlikely to be helpful.
   */
  public void warning(String message)
  {
    synchronized (lock)
      {
        if (isLoggable(Level.WARNING))
          log(Level.WARNING, message);
      }
  }

  /**
   * Logs a message with severity level INFO. {@link Level#INFO} is intended for
   * purely informational messages that do not indicate error or warning
   * situations. In the default logging configuration, INFO messages will be
   * written to the system console. For this reason, the INFO level should be
   * used only for messages that are important to end users and system
   * administrators. Messages at this level should be understandable to an
   * inexperienced, non-technical user.
   *
   * @param message the message text, also used as look-up key if the logger is
   *            localizing messages with a resource bundle. While it is possible
   *            to pass <code>null</code>, this is not recommended, since a
   *            logging message without text is unlikely to be helpful.
   */
  public void info(String message)
  {
    synchronized (lock)
      {
        if (isLoggable(Level.INFO))
          log(Level.INFO, message);
      }
  }

  /**
   * Logs a message with severity level CONFIG. {@link Level#CONFIG} is intended
   * for static configuration messages, for example about the windowing
   * environment, the operating system version, etc.
   *
   * @param message the message text, also used as look-up key if the logger is
   *            localizing messages with a resource bundle. While it is possible
   *            to pass <code>null</code>, this is not recommended, since a
   *            logging message without text is unlikely to be helpful.
   */
  public void config(String message)
  {
    synchronized (lock)
      {
        if (isLoggable(Level.CONFIG))
          log(Level.CONFIG, message);
      }
  }

  /**
   * Logs a message with severity level FINE. {@link Level#FINE} is intended for
   * messages that are relevant for developers using the component generating
   * log messages. Examples include minor, recoverable failures, or possible
   * inefficiencies.
   *
   * @param message the message text, also used as look-up key if the logger is
   *            localizing messages with a resource bundle. While it is possible
   *            to pass <code>null</code>, this is not recommended, since a
   *            logging message without text is unlikely to be helpful.
   */
  public void fine(String message)
  {
    synchronized (lock)
      {
        if (isLoggable(Level.FINE))
          log(Level.FINE, message);
      }
  }

  /**
   * Logs a message with severity level FINER. {@link Level#FINER} is intended
   * for rather detailed tracing, for example entering a method, returning from
   * a method, or throwing an exception.
   *
   * @param message the message text, also used as look-up key if the logger is
   *            localizing messages with a resource bundle. While it is possible
   *            to pass <code>null</code>, this is not recommended, since a
   *            logging message without text is unlikely to be helpful.
   */
  public void finer(String message)
  {
    synchronized (lock)
      {
        if (isLoggable(Level.FINER))
          log(Level.FINER, message);
      }
  }

  /**
   * Logs a message with severity level FINEST. {@link Level#FINEST} is intended
   * for highly detailed tracing, for example reaching a certain point inside
   * the body of a method.
   *
   * @param message the message text, also used as look-up key if the logger is
   *            localizing messages with a resource bundle. While it is possible
   *            to pass <code>null</code>, this is not recommended, since a
   *            logging message without text is unlikely to be helpful.
   */
  public void finest(String message)
  {
    synchronized (lock)
      {
        if (isLoggable(Level.FINEST))
          log(Level.FINEST, message);
      }
  }

  /**
   * Adds a handler to the set of handlers that get notified when a log record
   * is to be published.
   *
   * @param handler the handler to be added.
   * @throws NullPointerException if <code>handler</code> is <code>null</code>.
   * @throws SecurityException if this logger is not anonymous, a security
   *             manager exists, and the caller is not granted the permission to
   *             control the logging infrastructure by having
   *             LoggingPermission("control"). Untrusted code can obtain an
   *             anonymous logger through the static factory method
   *             {@link #getAnonymousLogger(java.lang.String) getAnonymousLogger}.
   */
  public void addHandler(Handler handler) throws SecurityException
  {
    synchronized (lock)
      {
        if (handler == null)
          throw new NullPointerException();

        /*
         * An application is allowed to control an anonymous logger without
         * having the permission to control the logging infrastructure.
         */
        if (! anonymous)
          LogManager.getLogManager().checkAccess();

        if (! handlerList.contains(handler))
          {
            handlerList.add(handler);
            handlers = getHandlers();
          }
      }
  }

  /**
   * Removes a handler from the set of handlers that get notified when a log
   * record is to be published.
   *
   * @param handler the handler to be removed.
   * @throws SecurityException if this logger is not anonymous, a security
   *             manager exists, and the caller is not granted the permission to
   *             control the logging infrastructure by having
   *             LoggingPermission("control"). Untrusted code can obtain an
   *             anonymous logger through the static factory method {@link
   *             #getAnonymousLogger(java.lang.String) getAnonymousLogger}.
   * @throws NullPointerException if <code>handler</code> is <code>null</code>.
   */
  public void removeHandler(Handler handler) throws SecurityException
  {
    synchronized (lock)
      {
        /*
         * An application is allowed to control an anonymous logger without
         * having the permission to control the logging infrastructure.
         */
        if (! anonymous)
          LogManager.getLogManager().checkAccess();

        if (handler == null)
          throw new NullPointerException();

        handlerList.remove(handler);
        handlers = getHandlers();
      }
  }

  /**
   * Returns the handlers currently registered for this Logger. When a log
   * record has been deemed as being loggable, it will be passed to all
   * registered handlers for publication. In addition, if the logger uses parent
   * handlers (see {@link #getUseParentHandlers() getUseParentHandlers} and
   * {@link #setUseParentHandlers(boolean) setUseParentHandlers}, the log
   * record will be passed to the parent's handlers.
   */
  public Handler[] getHandlers()
  {
    synchronized (lock)
      {
        /*
         * We cannot return our internal handlers array because we do not have
         * any guarantee that the caller would not change the array entries.
         */
        return (Handler[]) handlerList.toArray(new Handler[handlerList.size()]);
      }
  }

  /**
   * Returns whether or not this Logger forwards log records to handlers
   * registered for its parent loggers.
   *
   * @return <code>false</code> if this Logger sends log records merely to
   *         Handlers registered with itself; <code>true</code> if this Logger
   *         sends log records not only to Handlers registered with itself, but
   *         also to those Handlers registered with parent loggers.
   */
  public boolean getUseParentHandlers()
  {
    synchronized (lock)
      {
        return useParentHandlers;
      }
  }

  /**
   * Sets whether or not this Logger forwards log records to handlers registered
   * for its parent loggers.
   *
   * @param useParentHandlers <code>false</code> to let this Logger send log
   *            records merely to Handlers registered with itself;
   *            <code>true</code> to let this Logger send log records not only
   *            to Handlers registered with itself, but also to those Handlers
   *            registered with parent loggers.
   * @throws SecurityException if this logger is not anonymous, a security
   *             manager exists, and the caller is not granted the permission to
   *             control the logging infrastructure by having
   *             LoggingPermission("control"). Untrusted code can obtain an
   *             anonymous logger through the static factory method
   *             {@link #getAnonymousLogger(java.lang.String) getAnonymousLogger}.
   */
  public void setUseParentHandlers(boolean useParentHandlers)
  {
    synchronized (lock)
      {
        /*
         * An application is allowed to control an anonymous logger without
         * having the permission to control the logging infrastructure.
         */
        if (! anonymous)
          LogManager.getLogManager().checkAccess();

        this.useParentHandlers = useParentHandlers;
      }
  }

  /**
   * Returns the parent of this logger. By default, the parent is assigned by
   * the LogManager by inspecting the logger's name.
   *
   * @return the parent of this logger (as detemined by the LogManager by
   *         inspecting logger names), the root logger if no other logger has a
   *         name which is a prefix of this logger's name, or <code>null</code>
   *         for the root logger.
   */
  public Logger getParent()
  {
    synchronized (lock)
      {
        return parent;
      }
  }

  /**
   * Sets the parent of this logger. Usually, applications do not call this
   * method directly. Instead, the LogManager will ensure that the tree of
   * loggers reflects the hierarchical logger namespace. Basically, this method
   * should not be public at all, but the GNU implementation follows the API
   * specification.
   *
   * @throws NullPointerException if <code>parent</code> is <code>null</code>.
   * @throws SecurityException if this logger is not anonymous, a security
   *             manager exists, and the caller is not granted the permission to
   *             control the logging infrastructure by having
   *             LoggingPermission("control"). Untrusted code can obtain an
   *             anonymous logger through the static factory method
   *             {@link #getAnonymousLogger(java.lang.String) getAnonymousLogger}.
   */
  public void setParent(Logger parent)
  {
    synchronized (lock)
      {
        if (parent == null)
          throw new NullPointerException();

        if (this == root)
          throw new IllegalArgumentException(
                                             "the root logger can only have a null parent");

        /*
         * An application is allowed to control an anonymous logger without
         * having the permission to control the logging infrastructure.
         */
        if (! anonymous)
          LogManager.getLogManager().checkAccess();

        this.parent = parent;
      }
  }

  /**
   * Gets the StackTraceElement of the first class that is not this class. That
   * should be the initial caller of a logging method.
   *
   * @return caller of the initial logging method or null if unknown.
   */
  private StackTraceElement getCallerStackFrame()
  {
    Throwable t = new Throwable();
    StackTraceElement[] stackTrace = t.getStackTrace();
    int index = 0;

    // skip to stackentries until this class
    while (index < stackTrace.length
           && ! stackTrace[index].getClassName().equals(getClass().getName()))
      index++;

    // skip the stackentries of this class
    while (index < stackTrace.length
           && stackTrace[index].getClassName().equals(getClass().getName()))
      index++;

    return index < stackTrace.length ? stackTrace[index] : null;
  }

  /**
   * Reset and close handlers attached to this logger. This function is package
   * private because it must only be available to the LogManager.
   */
  void resetLogger()
  {
    for (int i = 0; i < handlers.length; i++)
      {
        handlers[i].close();
        handlerList.remove(handlers[i]);
      }
    handlers = getHandlers();
  }
}
