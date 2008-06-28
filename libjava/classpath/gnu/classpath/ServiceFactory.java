/* ServiceFactory.java -- Factory for plug-in services.
   Copyright (C) 2004  Free Software Foundation

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


package gnu.classpath;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;
import java.security.AccessControlContext;
import java.security.AccessController;
import java.security.PrivilegedActionException;
import java.util.Collections;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.ServiceConfigurationError;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;


/**
 * A factory for plug-ins that conform to a service provider
 * interface. This is a general mechanism that gets used by a number
 * of packages in the Java API. For instance, {@link
 * java.nio.charset.spi.CharsetProvider} allows to write custom
 * encoders and decoders for character sets, {@link
 * javax.imageio.spi.ImageReaderSpi} allows to support custom image
 * formats, and {@link javax.print.PrintService} makes it possible to
 * write custom printer drivers.
 *
 * <p>The plug-ins are concrete implementations of the service
 * provider interface, which is defined as an interface or an abstract
 * class. The implementation classes must be public and have a public
 * constructor that takes no arguments.
 *
 * <p>Plug-ins are usually deployed in JAR files. A JAR that provides
 * an implementation of a service must declare this in a resource file
 * whose name is the fully qualified service name and whose location
 * is the directory <code>META-INF/services</code>. This UTF-8 encoded
 * text file lists, on separate lines, the fully qualified names of
 * the concrete implementations. Thus, one JAR file can provide an
 * arbitrary number of implementations for an arbitrary count of
 * service provider interfaces.
 *
 * <p><b>Example</b>
 *
 * <p>For example, a JAR might provide two implementations of the
 * service provider interface <code>org.foo.ThinkService</code>,
 * namely <code>com.acme.QuickThinker</code> and
 * <code>com.acme.DeepThinker</code>. The code for <code>QuickThinker</code>
 * woud look as follows:
 *
 * <pre>
 * package com.acme;
 *
 * &#x2f;**
 * * Provices a super-quick, but not very deep implementation of ThinkService.
 * *&#x2f;
 * public class QuickThinker
 *   implements org.foo.ThinkService
 * {
 *   &#x2f;**
 *   * Constructs a new QuickThinker. The service factory (which is
 *   * part of the Java environment) calls this no-argument constructor
 *   * when it looks up the available implementations of ThinkService.
 *   *
 *   * &lt;p&gt;Note that an application might query all available
 *   * ThinkService providers, but use just one of them. Therefore,
 *   * constructing an instance should be very inexpensive. For example,
 *   * large data structures should only be allocated when the service
 *   * actually gets used.
 *   *&#x2f;
 *   public QuickThinker()
 *   {
 *   }
 *
 *   &#x2f;**
 *   * Returns the speed of this ThinkService in thoughts per second.
 *   * Applications can choose among the available service providers
 *   * based on this value.
 *   *&#x2f;
 *   public double getSpeed()
 *   {
 *     return 314159.2654;
 *   }
 *
 *   &#x2f;**
 *   * Produces a thought. While the returned thoughts are not very
 *   * deep, they are generated in very short time.
 *   *&#x2f;
 *   public Thought think()
 *   {
 *     return null;
 *   }
 * }
 * </pre>
 *
 * <p>The code for <code>com.acme.DeepThinker</code> is left as an
 * exercise to the reader.
 *
 * <p>Acme&#x2019;s <code>ThinkService</code> plug-in gets deployed as
 * a JAR file. Besides the bytecode and resources for
 * <code>QuickThinker</code> and <code>DeepThinker</code>, it also
 * contains the text file
 * <code>META-INF/services/org.foo.ThinkService</code>:
 *
 * <pre>
 * # Available implementations of org.foo.ThinkService
 * com.acme.QuickThinker
 * com.acme.DeepThinker
 * </pre>
 *
 * <p><b>Thread Safety</b>
 *
 * <p>It is safe to use <code>ServiceFactory</code> from multiple
 * concurrent threads without external synchronization.
 *
 * <p><b>Note for User Applications</b>
 *
 * <p>User applications that want to load plug-ins should not directly
 * use <code>gnu.classpath.ServiceFactory</code>, because this class
 * is only available in Java environments that are based on GNU
 * Classpath. Instead, it is recommended that user applications call
 * {@link
 * javax.imageio.spi.ServiceRegistry#lookupProviders(Class)}. This API
 * is actually independent of image I/O, and it is available on every
 * environment.
 *
 * @author <a href="mailto:brawer@dandelis.ch">Sascha Brawer</a>
 */
public final class ServiceFactory
{
  /**
   * A logger that gets informed when a service gets loaded, or
   * when there is a problem with loading a service.
   *
   * <p>Because {@link java.util.logging.Logger#getLogger(String)}
   * is thread-safe, we do not need to worry about synchronization
   * here.
   */
  private static final Logger LOGGER = Logger.getLogger("gnu.classpath");

  /**
   * Declared private in order to prevent constructing instances of
   * this utility class.
   */
  private ServiceFactory()
  {
  }


  /**
   * Finds service providers that are implementing the specified
   * Service Provider Interface.
   *
   * <p><b>On-demand loading:</b> Loading and initializing service
   * providers is delayed as much as possible. The rationale is that
   * typical clients will iterate through the set of installed service
   * providers until one is found that matches some criteria (like
   * supported formats, or quality of service). In such scenarios, it
   * might make sense to install only the frequently needed service
   * providers on the local machine. More exotic providers can be put
   * onto a server; the server will only be contacted when no suitable
   * service could be found locally.
   *
   * <p><b>Security considerations:</b> Any loaded service providers
   * are loaded through the specified ClassLoader, or the system
   * ClassLoader if <code>classLoader</code> is
   * <code>null</code>. When <code>lookupProviders</code> is called,
   * the current {@link AccessControlContext} gets recorded. This
   * captured security context will determine the permissions when
   * services get loaded via the <code>next()</code> method of the
   * returned <code>Iterator</code>.
   *
   * @param spi the service provider interface which must be
   * implemented by any loaded service providers.
   *
   * @param loader the class loader that will be used to load the
   * service providers, or <code>null</code> for the system class
   * loader. For using the context class loader, see {@link
   * #lookupProviders(Class)}.
   *
   * @return an iterator over instances of <code>spi</code>.
   *
   * @throws IllegalArgumentException if <code>spi</code> is
   * <code>null</code>.
   */
  public static <P> Iterator<P> lookupProviders(Class<P> spi,
						ClassLoader loader)
  {
    return lookupProviders(spi, loader, false);
  }

  /**
   * Finds service providers that are implementing the specified
   * Service Provider Interface.
   *
   * <p><b>On-demand loading:</b> Loading and initializing service
   * providers is delayed as much as possible. The rationale is that
   * typical clients will iterate through the set of installed service
   * providers until one is found that matches some criteria (like
   * supported formats, or quality of service). In such scenarios, it
   * might make sense to install only the frequently needed service
   * providers on the local machine. More exotic providers can be put
   * onto a server; the server will only be contacted when no suitable
   * service could be found locally.
   *
   * <p><b>Security considerations:</b> Any loaded service providers
   * are loaded through the specified ClassLoader, or the system
   * ClassLoader if <code>classLoader</code> is
   * <code>null</code>. When <code>lookupProviders</code> is called,
   * the current {@link AccessControlContext} gets recorded. This
   * captured security context will determine the permissions when
   * services get loaded via the <code>next()</code> method of the
   * returned <code>Iterator</code>.
   *
   * @param spi the service provider interface which must be
   * implemented by any loaded service providers.
   *
   * @param loader the class loader that will be used to load the
   * service providers, or <code>null</code> for the system class
   * loader. For using the context class loader, see {@link
   * #lookupProviders(Class)}.
   * @param error true if a {@link ServiceConfigurationError}
   *              should be thrown when an error occurs, rather
   *              than it merely being logged.
   * @return an iterator over instances of <code>spi</code>.
   *
   * @throws IllegalArgumentException if <code>spi</code> is
   * <code>null</code>.
   */
  public static <P> Iterator<P> lookupProviders(Class<P> spi,
						ClassLoader loader,
						boolean error)
  {
    String resourceName;
    Enumeration<URL> urls;

    if (spi == null)
      throw new IllegalArgumentException();

    if (loader == null)
      loader = ClassLoader.getSystemClassLoader();

    resourceName = "META-INF/services/" + spi.getName();
    try
      {
        urls = loader.getResources(resourceName);
      }
    catch (IOException ioex)
      {
        /* If an I/O error occurs here, we cannot provide any service
         * providers. In this case, we simply return an iterator that
         * does not return anything (no providers installed).
         */
        log(Level.WARNING, "cannot access {0}", resourceName, ioex);
	if (error)
	  throw new ServiceConfigurationError("Failed to access + " +
					      resourceName, ioex);
	else
	  {
	    List<P> empty = Collections.emptyList();
	    return empty.iterator();
	  }
      }

    return new ServiceIterator<P>(spi, urls, loader, error,
				  AccessController.getContext());
  }


  /**
   * Finds service providers that are implementing the specified
   * Service Provider Interface, using the context class loader
   * for loading providers.
   *
   * @param spi the service provider interface which must be
   * implemented by any loaded service providers.
   *
   * @return an iterator over instances of <code>spi</code>.
   *
   * @throws IllegalArgumentException if <code>spi</code> is
   * <code>null</code>.
   *
   * @see #lookupProviders(Class, ClassLoader)
   */
  public static <P> Iterator<P> lookupProviders(Class<P> spi)
  {
    ClassLoader ctxLoader;

    ctxLoader = Thread.currentThread().getContextClassLoader();
    return lookupProviders(spi, ctxLoader);
  }


  /**
   * An iterator over service providers that are listed in service
   * provider configuration files, which get passed as an Enumeration
   * of URLs. This is a helper class for {@link
   * ServiceFactory#lookupProviders(Class, ClassLoader)}.
   *
   * @author <a href="mailto:brawer@dandelis.ch">Sascha Brawer</a>
   */
  private static final class ServiceIterator<P>
    implements Iterator<P>
  {
    /**
     * The service provider interface (usually an interface, sometimes
     * an abstract class) which the services must implement.
     */
    private final Class<P> spi;


    /**
     * An Enumeration<URL> over the URLs that contain a resource
     * <code>META-INF/services/&lt;org.foo.SomeService&gt;</code>,
     * as returned by {@link ClassLoader#getResources(String)}.
     */
    private final Enumeration<URL> urls;


    /**
     * The class loader used for loading service providers.
     */
    private final ClassLoader loader;


    /**
     * The security context used when loading and initializing service
     * providers. We want to load and initialize all plug-in service
     * providers under the same security context, namely the one that
     * was active when {@link #lookupProviders(Class, ClassLoader)} has
     * been called.
     */
    private final AccessControlContext securityContext;


    /**
     * A reader for the current file listing class names of service
     * implementors, or <code>null</code> when the last reader has
     * been fetched.
     */
    private BufferedReader reader;
    

    /**
     * The URL currently being processed. This is only used for
     * emitting error messages.
     */
    private URL currentURL;


    /**
     * The service provider that will be returned by the next call to
     * {@link #next()}, or <code>null</code> if the iterator has
     * already returned all service providers.
     */
    private P nextProvider;

    /**
     * True if a {@link ServiceConfigurationError} should be thrown
     * when an error occurs, instead of it merely being logged.
     */
    private boolean error;

    /**
     * Constructs an Iterator that loads and initializes services on
     * demand.
     *
     * @param spi the service provider interface which the services
     * must implement. Usually, this is a Java interface type, but it
     * might also be an abstract class or even a concrete superclass.
     *
     * @param urls an Enumeration<URL> over the URLs that contain a
     * resource
     * <code>META-INF/services/&lt;org.foo.SomeService&gt;</code>, as
     * determined by {@link ClassLoader#getResources(String)}.
     *
     * @param loader the ClassLoader that gets used for loading
     * service providers.
     *
     * @param error true if a {@link ServiceConfigurationError}
     *              should be thrown when an error occurs, rather
     *              than it merely being logged.
     *
     * @param securityContext the security context to use when loading
     * and initializing service providers.
     */
    ServiceIterator(Class<P> spi, Enumeration<URL> urls, ClassLoader loader,
		    boolean error, AccessControlContext securityContext)
    {
      this.spi = spi;
      this.urls = urls;
      this.loader = loader;
      this.securityContext = securityContext;
      this.error = error;
      this.nextProvider = loadNextServiceProvider();
    }


    /**
     * @throws NoSuchElementException if {@link #hasNext} returns
     * <code>false</code>.
     */
    public P next()
    {
      P result;

      if (!hasNext())
        throw new NoSuchElementException();

      result = nextProvider;
      nextProvider = loadNextServiceProvider();
      return result;
    }


    public boolean hasNext()
    {
      return nextProvider != null;
    }


    public void remove()
    {
      throw new UnsupportedOperationException();
    }


    private P loadNextServiceProvider()
    {
      String line;
      
      if (reader == null)
        advanceReader();

      for (;;)
        {
          /* If we have reached the last provider list, we cannot
           * retrieve any further lines.
           */
          if (reader == null)
            return null;

          try
            {
              line = reader.readLine();
            }
          catch (IOException readProblem)
            {
              log(Level.WARNING, "IOException upon reading {0}", currentURL,
                  readProblem);
              line = null;
	      if (error)
		throw new ServiceConfigurationError("Error reading " +
						    currentURL, readProblem);
            }

          /* When we are at the end of one list of services,
           * switch over to the next one.
           */
          if (line == null)
            {
              advanceReader();
              continue;
            }


          // Skip whitespace at the beginning and end of each line.
          line = line.trim();

          // Skip empty lines.
          if (line.length() == 0)
            continue;

          // Skip comment lines.
          if (line.charAt(0) == '#')
            continue;

          try
            {
              log(Level.FINE,
                  "Loading service provider \"{0}\", specified"
                  + " by \"META-INF/services/{1}\" in {2}.",
                  new Object[] { line, spi.getName(), currentURL },
                  null);

              /* Load the class in the security context that was
               * active when calling lookupProviders.
               */
              return AccessController.doPrivileged(
                new ServiceProviderLoadingAction<P>(spi, line, loader),
                securityContext);
            }
          catch (Exception ex)
            {
              String msg = "Cannot load service provider class \"{0}\","
                + " specified by \"META-INF/services/{1}\" in {2}";
              if (ex instanceof PrivilegedActionException
                  && ex.getCause() instanceof ClassCastException)
                msg = "Service provider class \"{0}\" is not an instance"
                  + " of \"{1}\". Specified"
                  + " by \"META-INF/services/{1}\" in {2}.";

              log(Level.WARNING, msg,                  
                  new Object[] { line, spi.getName(), currentURL },
                  ex);
	      if (error)
		throw new ServiceConfigurationError("Cannot load service "+
						    "provider class " +
						    line + " specified by "+
						    "\"META-INF/services/"+
						    spi.getName() + "\" in "+
						    currentURL, ex);
              continue;
            }
        }
    }


    private void advanceReader()
    {
      do
        {
          if (reader != null)
            {
              try
                {
                  reader.close();
                  log(Level.FINE, "closed {0}", currentURL, null);
                }
              catch (Exception ex)
                {
                  log(Level.WARNING, "cannot close {0}", currentURL, ex);
		  if (error)
		    throw new ServiceConfigurationError("Cannot close " +
							currentURL, ex);
                }
              reader = null;
              currentURL = null;
            }

        if (!urls.hasMoreElements())
          return;

        currentURL = urls.nextElement();
        try
          {
            reader = new BufferedReader(new InputStreamReader(
              currentURL.openStream(), "UTF-8"));
            log(Level.FINE, "opened {0}", currentURL, null);
          }
        catch (Exception ex)
          {
            log(Level.WARNING, "cannot open {0}", currentURL, ex);
	    if (error)
	      throw new ServiceConfigurationError("Cannot open " +
						  currentURL, ex);
          }
        }
      while (reader == null);
    }
  }


  // Package-private to avoid a trampoline.
  /**
   * Passes a log message to the <code>java.util.logging</code>
   * framework. This call returns very quickly if no log message will
   * be produced, so there is not much overhead in the standard case.
   *
   * @param level the severity of the message, for instance {@link
   * Level#WARNING}.
   *
   * @param msg the log message, for instance <code>&#x201c;Could not
   * load {0}.&#x201d;</code>
   *
   * @param param the parameter(s) for the log message, or
   * <code>null</code> if <code>msg</code> does not specify any
   * parameters. If <code>param</code> is not an array, an array with
   * <code>param</code> as its single element gets passed to the
   * logging framework.
   *
   * @param t a Throwable that is associated with the log record, or
   * <code>null</code> if the log message is not associated with a
   * Throwable.
   */
  static void log(Level level, String msg, Object param, Throwable t)
  {
    LogRecord rec;

    // Return quickly if no log message will be produced.
    if (!LOGGER.isLoggable(level))
      return;

    rec = new LogRecord(level, msg);
    if (param != null && param.getClass().isArray())
      rec.setParameters((Object[]) param);
    else
      rec.setParameters(new Object[] { param });

    rec.setThrown(t);

    // While java.util.logging can sometimes infer the class and
    // method of the caller, this automatic inference is not reliable
    // on highly optimizing VMs. Also, log messages make more sense to
    // developers when they display a public method in a public class;
    // otherwise, they might feel tempted to figure out the internals
    // of ServiceFactory in order to understand the problem.
    rec.setSourceClassName(ServiceFactory.class.getName());
    rec.setSourceMethodName("lookupProviders");

    LOGGER.log(rec);
  }
}
