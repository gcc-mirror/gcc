/* LogManager.java -- a class for maintaining Loggers and managing
   configuration properties
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

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.io.IOException;
import java.io.InputStream;
import java.lang.ref.WeakReference;
import java.net.URL;
import java.util.Collections;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.StringTokenizer;

/**
 * The <code>LogManager</code> maintains a hierarchical namespace
 * of Logger objects and manages properties for configuring the logging
 * framework. There exists only one single <code>LogManager</code>
 * per virtual machine. This instance can be retrieved using the
 * static method {@link #getLogManager()}.
 *
 * <p><strong>Configuration Process:</strong> The global LogManager
 * object is created and configured when the class
 * <code>java.util.logging.LogManager</code> is initialized.
 * The configuration process includes the subsequent steps:
 *
 * <ul>
 * <li>If the system property <code>java.util.logging.manager</code>
 *     is set to the name of a subclass of
 *     <code>java.util.logging.LogManager</code>, an instance of
 *     that subclass is created and becomes the global LogManager.
 *     Otherwise, a new instance of LogManager is created.</li>
 * <li>The <code>LogManager</code> constructor tries to create
 *     a new instance of the class specified by the system
 *     property <code>java.util.logging.config.class</code>.
 *     Typically, the constructor of this class will call
 *     <code>LogManager.getLogManager().readConfiguration(java.io.InputStream)</code>
 *     for configuring the logging framework.
 *     The configuration process stops at this point if
 *     the system property <code>java.util.logging.config.class</code>
 *     is set (irrespective of whether the class constructor
 *     could be called or an exception was thrown).</li>
 *
 * <li>If the system property <code>java.util.logging.config.class</code>
 *     is <em>not</em> set, the configuration parameters are read in from
 *     a file and passed to
 *     {@link #readConfiguration(java.io.InputStream)}.
 *     The name and location of this file are specified by the system
 *     property <code>java.util.logging.config.file</code>.</li>
 * <li>If the system property <code>java.util.logging.config.file</code>
 *     is not set, however, the contents of the URL
 *     "{gnu.classpath.home.url}/logging.properties" are passed to
 *     {@link #readConfiguration(java.io.InputStream)}.
 *     Here, "{gnu.classpath.home.url}" stands for the value of
 *     the system property <code>gnu.classpath.home.url</code>.</li>
 * </ul>
 *
 * @author Sascha Brawer (brawer@acm.org)
 */
public class LogManager
{
  /**
   * The singleton LogManager instance.
   */
  private static LogManager logManager;

  /**
   * The registered named loggers; maps the name of a Logger to
   * a WeakReference to it.
   */
  private Map loggers;
  final Logger rootLogger;

  /**
   * The properties for the logging framework which have been
   * read in last.
   */
  private Properties properties;

  /**
   * A delegate object that provides support for handling
   * PropertyChangeEvents.  The API specification does not
   * mention which bean should be the source in the distributed
   * PropertyChangeEvents, but Mauve test code has determined that
   * the Sun J2SE 1.4 reference implementation uses the LogManager
   * class object. This is somewhat strange, as the class object
   * is not the bean with which listeners have to register, but
   * there is no reason for the GNU Classpath implementation to
   * behave differently from the reference implementation in
   * this case.
   */
  private final PropertyChangeSupport pcs = new PropertyChangeSupport( /* source bean */
  LogManager.class);

  protected LogManager()
  {
    if (logManager != null)
      throw new IllegalStateException("there can be only one LogManager; use LogManager.getLogManager()");

    logManager = this;
    loggers = new java.util.HashMap();
    rootLogger = new Logger("", null);
    addLogger(rootLogger);

    /* Make sure that Logger.global has the rootLogger as its parent.
     *
     * Logger.global is set during class initialization of Logger,
     * which may or may not be before this code is being executed.
     * For example, on the Sun 1.3.1 and 1.4.0 JVMs, Logger.global
     * has been set before this code is being executed. In contrast,
     * Logger.global still is null on GCJ 3.2.  Since the LogManager
     * and Logger classes are mutually dependent, both behaviors are
     * correct.
     *
     * This means that we cannot depend on Logger.global to have its
     * value when this code executes, although that variable is final.
     * Since Logger.getLogger will always return the same logger for
     * the same name, the subsequent line works fine irrespective of
     * the order in which classes are initialized.
     */
    Logger.getLogger("global").setParent(rootLogger);
    Logger.getLogger("global").setUseParentHandlers(true);
  }

  /**
   * Returns the globally shared LogManager instance.
   */
  public static LogManager getLogManager()
  {
    return logManager;
  }

  static
    {
      makeLogManager();

      /* The Javadoc description of the class explains
       * what is going on here.
       */
      Object configurator = createInstance(System.getProperty("java.util.logging.config.class"),
                                           /* must be instance of */ Object.class);

      try
        {
	  if (configurator == null)
	    getLogManager().readConfiguration();
        }
      catch (IOException ex)
        {
	  /* FIXME: Is it ok to ignore exceptions here? */
        }
    }

  private static LogManager makeLogManager()
  {
    String managerClassName;
    LogManager manager;

    managerClassName = System.getProperty("java.util.logging.manager");
    manager = (LogManager) createInstance(managerClassName, LogManager.class);
    if (manager != null)
      return manager;

    if (managerClassName != null)
      System.err.println("WARNING: System property \"java.util.logging.manager\""
                         + " should be the name of a subclass of java.util.logging.LogManager");

    return new LogManager();
  }

  /**
   * Registers a listener which will be notified when the
   * logging properties are re-read.
   */
  public synchronized void addPropertyChangeListener(PropertyChangeListener listener)
  {
    /* do not register null. */
    listener.getClass();

    pcs.addPropertyChangeListener(listener);
  }

  /**
   * Unregisters a listener.
   *
   * If <code>listener</code> has not been registered previously,
   * nothing happens.  Also, no exception is thrown if
   * <code>listener</code> is <code>null</code>.
   */
  public synchronized void removePropertyChangeListener(PropertyChangeListener listener)
  {
    if (listener != null)
      pcs.removePropertyChangeListener(listener);
  }

  /**
   * Adds a named logger.  If a logger with the same name has
   * already been registered, the method returns <code>false</code>
   * without adding the logger.
   *
   * <p>The <code>LogManager</code> only keeps weak references
   * to registered loggers.  Therefore, names can become available
   * after automatic garbage collection.
   *
   * @param logger the logger to be added.
   *
   * @return <code>true</code>if <code>logger</code> was added,
   *         <code>false</code> otherwise.
   *
   * @throws NullPointerException if <code>name</code> is
   *         <code>null</code>.
   */
  public synchronized boolean addLogger(Logger logger)
  {
    /* To developers thinking about to remove the 'synchronized'
     * declaration from this method: Please read the comment
     * in java.util.logging.Logger.getLogger(String, String)
     * and make sure that whatever you change wrt. synchronization
     * does not endanger thread-safety of Logger.getLogger.
     * The current implementation of Logger.getLogger assumes
     * that LogManager does its synchronization on the globally
     * shared instance of LogManager.
     */
    String name;
    WeakReference ref;

    /* This will throw a NullPointerException if logger is null,
     * as required by the API specification.
     */
    name = logger.getName();

    ref = (WeakReference) loggers.get(name);
    if (ref != null)
      {
	if (ref.get() != null)
	  return false;

	/* There has been a logger under this name in the past,
	 * but it has been garbage collected.
	 */
	loggers.remove(ref);
      }

    /* Adding a named logger requires a security permission. */
    if ((name != null) && ! name.equals(""))
      checkAccess();

    Logger parent = findAncestor(logger);
    loggers.put(name, new WeakReference(logger));
    if (parent != logger.getParent())
      logger.setParent(parent);

    /* It can happen that existing loggers should be children of
     * the newly added logger. For example, assume that there
     * already exist loggers under the names "", "foo", and "foo.bar.baz".
     * When adding "foo.bar", the logger "foo.bar.baz" should change
     * its parent to "foo.bar".
     */
    if (parent != rootLogger)
      {
	for (Iterator iter = loggers.keySet().iterator(); iter.hasNext();)
	  {
	    Logger possChild = (Logger) ((WeakReference) loggers.get(iter.next()))
	                       .get();
	    if ((possChild == null) || (possChild == logger)
	        || (possChild.getParent() != parent))
	      continue;

	    if (! possChild.getName().startsWith(name))
	      continue;

	    if (possChild.getName().charAt(name.length()) != '.')
	      continue;

	    possChild.setParent(logger);
	  }
      }

    return true;
  }

  /**
   * Finds the closest ancestor for a logger among the currently
   * registered ones.  For example, if the currently registered
   * loggers have the names "", "foo", and "foo.bar", the result for
   * "foo.bar.baz" will be the logger whose name is "foo.bar".
   *
   * @param child a logger for whose name no logger has been
   *        registered.
   *
   * @return the closest ancestor for <code>child</code>,
   *         or <code>null</code> if <code>child</code>
   *         is the root logger.
   *
   * @throws NullPointerException if <code>child</code>
   *         is <code>null</code>.
   */
  private synchronized Logger findAncestor(Logger child)
  {
    String childName = child.getName();
    int childNameLength = childName.length();
    Logger best = rootLogger;
    int bestNameLength = 0;

    Logger cand;
    String candName;
    int candNameLength;

    if (child == rootLogger)
      return null;

    for (Iterator iter = loggers.keySet().iterator(); iter.hasNext();)
      {
	candName = (String) iter.next();
	candNameLength = candName.length();

	if (candNameLength > bestNameLength
	    && childNameLength > candNameLength
	    && childName.startsWith(candName)
	    && childName.charAt(candNameLength) == '.')
	  {
	    cand = (Logger) ((WeakReference) loggers.get(candName)).get();
	    if ((cand == null) || (cand == child))
	      continue;

	    bestNameLength = candName.length();
	    best = cand;
	  }
      }

    return best;
  }

  /**
   * Returns a Logger given its name.
   *
   * @param name the name of the logger.
   *
   * @return a named Logger, or <code>null</code> if there is no
   *     logger with that name.
   *
   * @throw java.lang.NullPointerException if <code>name</code>
   *     is <code>null</code>.
   */
  public synchronized Logger getLogger(String name)
  {
    WeakReference ref;

    /* Throw a NullPointerException if name is null. */
    name.getClass();

    ref = (WeakReference) loggers.get(name);
    if (ref != null)
      return (Logger) ref.get();
    else
      return null;
  }

  /**
   * Returns an Enumeration of currently registered Logger names.
   * Since other threads can register loggers at any time, the
   * result could be different any time this method is called.
   *
   * @return an Enumeration with the names of the currently
   *    registered Loggers.
   */
  public synchronized Enumeration getLoggerNames()
  {
    return Collections.enumeration(loggers.keySet());
  }

  /**
   * Resets the logging configuration by removing all handlers for
   * registered named loggers and setting their level to <code>null</code>.
   * The level of the root logger will be set to <code>Level.INFO</code>.
   *
   * @throws SecurityException if a security manager exists and
   *         the caller is not granted the permission to control
   *         the logging infrastructure.
   */
  public synchronized void reset() throws SecurityException
  {
    /* Throw a SecurityException if the caller does not have the
     * permission to control the logging infrastructure.
     */
    checkAccess();

    properties = new Properties();

    Iterator iter = loggers.values().iterator();
    while (iter.hasNext())
      {
	WeakReference ref;
	Logger logger;

	ref = (WeakReference) iter.next();
	if (ref != null)
	  {
	    logger = (Logger) ref.get();

	    if (logger == null)
	      iter.remove();
	    else if (logger != rootLogger)
	      logger.setLevel(null);
	  }
      }

    rootLogger.setLevel(Level.INFO);
  }

  /**
   * Configures the logging framework by reading a configuration file.
   * The name and location of this file are specified by the system
   * property <code>java.util.logging.config.file</code>.  If this
   * property is not set, the URL
   * "{gnu.classpath.home.url}/logging.properties" is taken, where
   * "{gnu.classpath.home.url}" stands for the value of the system
   * property <code>gnu.classpath.home.url</code>.
   *
   * <p>The task of configuring the framework is then delegated to
   * {@link #readConfiguration(java.io.InputStream)}, which will
   * notify registered listeners after having read the properties.
   *
   * @throws SecurityException if a security manager exists and
   *         the caller is not granted the permission to control
   *         the logging infrastructure, or if the caller is
   *         not granted the permission to read the configuration
   *         file.
   *
   * @throws IOException if there is a problem reading in the
   *         configuration file.
   */
  public synchronized void readConfiguration()
    throws IOException, SecurityException
  {
    String path;
    InputStream inputStream;

    path = System.getProperty("java.util.logging.config.file");
    if ((path == null) || (path.length() == 0))
      {
	String url = (System.getProperty("gnu.classpath.home.url")
	             + "/logging.properties");
	inputStream = new URL(url).openStream();
      }
    else
      inputStream = new java.io.FileInputStream(path);

    try
      {
	readConfiguration(inputStream);
      }
    finally
      {
	/* Close the stream in order to save
	 * resources such as file descriptors.
	 */
	inputStream.close();
      }
  }

  public synchronized void readConfiguration(InputStream inputStream)
    throws IOException, SecurityException
  {
    Properties newProperties;
    Enumeration keys;

    checkAccess();
    newProperties = new Properties();
    newProperties.load(inputStream);
    this.properties = newProperties;
    keys = newProperties.propertyNames();

    while (keys.hasMoreElements())
      {
	String key = ((String) keys.nextElement()).trim();
	String value = newProperties.getProperty(key);

	if (value == null)
	  continue;

	value = value.trim();

	if ("handlers".equals(key))
	  {
	    StringTokenizer tokenizer = new StringTokenizer(value);
	    while (tokenizer.hasMoreTokens())
	      {
		String handlerName = tokenizer.nextToken();
		try
		  {
		    Class handlerClass = Class.forName(handlerName);
		    getLogger("").addHandler((Handler) handlerClass
		                             .newInstance());
		  }
		catch (ClassCastException ex)
		  {
		    System.err.println("[LogManager] class " + handlerName
		                       + " is not subclass of java.util.logging.Handler");
		  }
		catch (Exception ex)
		  {
		    //System.out.println("[LogManager.readConfiguration]"+ex);
		  }
	      }
	  }

	if (key.endsWith(".level"))
	  {
	    String loggerName = key.substring(0, key.length() - 6);
	    Logger logger = getLogger(loggerName);

	    if (logger == null)
	      {
		logger = Logger.getLogger(loggerName);
		addLogger(logger);
	      }
	    try
	      {
		logger.setLevel(Level.parse(value));
	      }
	    catch (Exception _)
	      {
		//System.out.println("[LogManager.readConfiguration] "+_);
	      }
	    continue;
	  }
      }

    /* The API specification does not talk about the
     * property name that is distributed with the
     * PropertyChangeEvent.  With test code, it could
     * be determined that the Sun J2SE 1.4 reference
     * implementation uses null for the property name.
     */
    pcs.firePropertyChange(null, null, null);
  }

  /**
   * Returns the value of a configuration property as a String.
   */
  public synchronized String getProperty(String name)
  {
    if (properties != null)
      return properties.getProperty(name);
    else
      return null;
  }

  /**
   * Returns the value of a configuration property as an integer.
   * This function is a helper used by the Classpath implementation
   * of java.util.logging, it is <em>not</em> specified in the
   * logging API.
   *
   * @param name the name of the configuration property.
   *
   * @param defaultValue the value that will be returned if the
   *        property is not defined, or if its value is not an integer
   *        number.
   */
  static int getIntProperty(String name, int defaultValue)
  {
    try
      {
	return Integer.parseInt(getLogManager().getProperty(name));
      }
    catch (Exception ex)
      {
	return defaultValue;
      }
  }

  /**
   * Returns the value of a configuration property as an integer,
   * provided it is inside the acceptable range.
   * This function is a helper used by the Classpath implementation
   * of java.util.logging, it is <em>not</em> specified in the
   * logging API.
   *
   * @param name the name of the configuration property.
   *
   * @param minValue the lowest acceptable value.
   *
   * @param maxValue the highest acceptable value.
   *
   * @param defaultValue the value that will be returned if the
   *        property is not defined, or if its value is not an integer
   *        number, or if it is less than the minimum value,
   *        or if it is greater than the maximum value.
   */
  static int getIntPropertyClamped(String name, int defaultValue,
                                   int minValue, int maxValue)
  {
    int val = getIntProperty(name, defaultValue);
    if ((val < minValue) || (val > maxValue))
      val = defaultValue;
    return val;
  }

  /**
   * Returns the value of a configuration property as a boolean.
   * This function is a helper used by the Classpath implementation
   * of java.util.logging, it is <em>not</em> specified in the
   * logging API.
   *
   * @param name the name of the configuration property.
   *
   * @param defaultValue the value that will be returned if the
   *        property is not defined, or if its value is neither
   *        <code>"true"</code> nor <code>"false"</code>.
   */
  static boolean getBooleanProperty(String name, boolean defaultValue)
  {
    try
      {
	return (new Boolean(getLogManager().getProperty(name))).booleanValue();
      }
    catch (Exception ex)
      {
	return defaultValue;
      }
  }

  /**
   * Returns the value of a configuration property as a Level.
   * This function is a helper used by the Classpath implementation
   * of java.util.logging, it is <em>not</em> specified in the
   * logging API.
   *
   * @param propertyName the name of the configuration property.
   *
   * @param defaultValue the value that will be returned if the
   *        property is not defined, or if
   *        {@link Level.parse(java.lang.String)} does not like
   *        the property value.
   */
  static Level getLevelProperty(String propertyName, Level defaultValue)
  {
    try
      {
	return Level.parse(getLogManager().getProperty(propertyName));
      }
    catch (Exception ex)
      {
	return defaultValue;
      }
  }

  /**
   * Returns the value of a configuration property as a Class.
   * This function is a helper used by the Classpath implementation
   * of java.util.logging, it is <em>not</em> specified in the
   * logging API.
   *
   * @param propertyName the name of the configuration property.
   *
   * @param defaultValue the value that will be returned if the
   *        property is not defined, or if it does not specify
   *        the name of a loadable class.
   */
  static final Class getClassProperty(String propertyName, Class defaultValue)
  {
    Class usingClass = null;

    try
      {
	String propertyValue = logManager.getProperty(propertyName);
	if (propertyValue != null)
	  usingClass = Class.forName(propertyValue);
	if (usingClass != null)
	  return usingClass;
      }
    catch (Exception _)
      {
      }

    return defaultValue;
  }

  static final Object getInstanceProperty(String propertyName, Class ofClass,
                                          Class defaultClass)
  {
    Class klass = getClassProperty(propertyName, defaultClass);
    if (klass == null)
      return null;

    try
      {
	Object obj = klass.newInstance();
	if (ofClass.isInstance(obj))
	  return obj;
      }
    catch (Exception _)
      {
      }

    if (defaultClass == null)
      return null;

    try
      {
	return defaultClass.newInstance();
      }
    catch (java.lang.InstantiationException ex)
      {
	throw new RuntimeException(ex.getMessage());
      }
    catch (java.lang.IllegalAccessException ex)
      {
	throw new RuntimeException(ex.getMessage());
      }
  }

  /**
   * An instance of <code>LoggingPermission("control")</code>
   * that is shared between calls to <code>checkAccess()</code>.
   */
  private static final LoggingPermission controlPermission = new LoggingPermission("control",
                                                                                   null);

  /**
   * Checks whether the current security context allows changing
   * the configuration of the logging framework.  For the security
   * context to be trusted, it has to be granted
   * a LoggingPermission("control").
   *
   * @throws SecurityException if a security manager exists and
   *         the caller is not granted the permission to control
   *         the logging infrastructure.
   */
  public void checkAccess() throws SecurityException
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm != null)
      sm.checkPermission(controlPermission);
  }

  /**
   * Creates a new instance of a class specified by name.
   *
   * @param className the name of the class of which a new instance
   *        should be created.
   *
   * @param ofClass the class to which the new instance should
   *        be either an instance or an instance of a subclass.
   *        FIXME: This description is just terrible.
   *
   * @return the new instance, or <code>null</code> if
   *         <code>className</code> is <code>null</code>, if no class
   *         with that name could be found, if there was an error
   *         loading that class, or if the constructor of the class
   *         has thrown an exception.
   */
  static final Object createInstance(String className, Class ofClass)
  {
    Class klass;

    if ((className == null) || (className.length() == 0))
      return null;

    try
      {
	klass = Class.forName(className);
	if (! ofClass.isAssignableFrom(klass))
	  return null;

	return klass.newInstance();
      }
    catch (Exception _)
      {
	return null;
      }
    catch (java.lang.LinkageError _)
      {
	return null;
      }
  }
}
