/* Copyright (C) 1999, 2000, 2002  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.net;

import java.io.*;
import java.util.jar.*;
import java.util.Enumeration;
import java.util.Vector;
import java.security.CodeSource;
import java.security.SecureClassLoader;
import java.security.PermissionCollection;
import java.security.cert.Certificate;

/**
 * @since 1.2
 */
public class URLClassLoader extends SecureClassLoader
{
  // The URLStreamHandlerFactory
  URLStreamHandlerFactory factory = null;

  // `path' contains simply the URL's we're using for the searching.
  private Vector path; 

  // If path[n] is a zip/jar, then this holds a JarURLConnection for
  // that thing, otherwise, path[n] is null.
  private Vector info; 

  private URLStreamHandler getHandler0 (String protocol)
  {
    if (factory != null)
      return factory.createURLStreamHandler(protocol);
    else
      return null;
  }

  public URLClassLoader (URL[] urls)
  { 
    this (urls, null, null);
  }
  
  public URLClassLoader (URL[] urls, ClassLoader parent)
  { 
    this (urls, parent, null);
  }

  // A File URL may actually be a Jar URL.  Convert if possible.
  private URL jarFileize (URL url)
  {
    if (! url.getProtocol ().equals ("jar"))
      {
	String f = url.getFile ();

	// If it ends with '/' we'll take it for a directory,
	// otherwise it's a jar file.  This is how JDK 1.2 defines
	// it, so we will not try to be smart here.
	if (f.charAt (f.length ()-1) != '/')
	  {
	    try
	      {
		url = new URL ("jar", "", -1, (url.toExternalForm ())+"!/", 
			       getHandler0 ("jar"));
	      } 
	    catch (MalformedURLException x)
	      {
		/* ignore */
	      }
	  }
      }
    return url;
  }

  protected void addURL (URL url)
  {
    JarURLConnection conn = null;
    
    // Convert a Jar File URL into Jar URL if possible.
    url = jarFileize (url);

    path.addElement (url);

    if (url.getProtocol ().equals ("jar"))
      {
	try
	  {
	    conn = (JarURLConnection) url.openConnection ();
	  }
	catch (java.io.IOException x)
	  {
	    /* ignore */
	  }
      }

    info.addElement (conn);
  }

  public URLClassLoader (URL[] urls, ClassLoader parent,
			 URLStreamHandlerFactory fac)
  { 
    super (parent);

    factory = fac;

    if (urls == null || urls.length == 0)
      {
	path = new Vector (1);
	info = new Vector (1);
	return;
      }

    path = new Vector (urls.length);
    info = new Vector (urls.length);

    for (int i = 0; i < urls.length; i++)
      {
	// Convert a Jar File URL into a Jar URL is possible. 
	URL u = jarFileize(urls[i]);

	path.addElement (u);

	if (u.getProtocol ().equals ("jar"))
	  {
	    JarURLConnection conn = null;
	    try
	      {
		conn = (JarURLConnection) u.openConnection ();
	      }
	    catch (java.io.IOException x)
	      {
		/* ignore */
	      }
	    info.addElement (conn);
	  }
	else
	  {
	    info.addElement (null);
	  }
      }
  }

  public URL[] getURLs ()
  {
    URL[] urls = new URL[path.size()];
    path.copyInto (urls);
    return urls;
  }
  
  public Enumeration findResources (String name)
  {
    Vector results = new Vector ();

    for (int i = 0; i < path.size(); i++)
      {
	URL u = (URL)path.elementAt (i);
		
	try {
	  JarURLConnection conn = (JarURLConnection) info.elementAt (i);
	  
	  if (conn != null)
	    {
	      if (conn.getJarFile().getJarEntry (name) != null)
		results.addElement (new URL(u, name,
					getHandler0 (u.getProtocol())));
	    }
	  else
	    {
	      URL p = new URL (u, name, getHandler0 (u.getProtocol()));
			    
	      InputStream is = p.openStream();
	      if (is != null)
		{
		  is.close();
		  results.addElement (p);
		}
	    }
		    
	  // if we get an exception ... try the next path element
	} catch (IOException x) {
	  continue;
	}
      }
	
    return results.elements ();
  }

  public URL findResource (String name)
  {
    for (int i = 0; i < path.size(); i++)
      {
	URL u = (URL)path.elementAt (i);
	
	try {
	  JarURLConnection conn = (JarURLConnection) info.elementAt (i);
 	  
	  if (conn != null)
	    {
	      if (conn.getJarFile().getJarEntry (name) != null)
		return new URL(u, name, getHandler0 (u.getProtocol()));
	    }
	  else
	    {
	      URL p = new URL (u, name, getHandler0 (u.getProtocol()));

	      InputStream is = p.openStream();
	      if (is != null)
		{
		  is.close();
		  return p;
		}
	    }
	
	  // if we get an exception ... try the next path element
	} catch (IOException x) {
	  continue;
	}
      }

    return null;
  }

  // and finally, we can implement our class loader functionality.
  protected Class findClass (String name)
    throws ClassNotFoundException
  {
    if (name == null)
      throw new ClassNotFoundException ("null");

    try 
      {
	URL url = getResource (name.replace ('.', '/') + ".class");

	if (url == null)
	  throw new ClassNotFoundException (name);

	URLConnection connection = url.openConnection ();
	InputStream is = connection.getInputStream ();

	int len = connection.getContentLength ();
	byte[] data = new byte[len];

	int left = len;
	int off  = 0;
	while (left > 0)
	  {
	    int c = is.read (data, off, len-off);
	    if (c == -1 || c == 0)
	      throw new InternalError ("premature end of file");
	    left -= c;
	    off += c;
	  }

	// Now construct the CodeSource (if loaded from a jar file)
	CodeSource source = null;
	if (url.getProtocol().equals("jar"))
	  {
	    Certificate[] certificates =
	      ((JarURLConnection) connection).getCertificates();
	    String u = url.toExternalForm ();
	    u = u.substring (4); //skip "jar:"
	    int i = u.indexOf ('!');
	    if (i >= 0)
	      u = u.substring (0, i);
	    url = new URL("jar", "", u);

	    source = new CodeSource(url, certificates);
	  }
	else if (url.getProtocol().equals("file"))
	  {
	    try
	      {
		String u = url.toExternalForm();
		// Skip "file:" and then get canonical directory name.
		File f = new File(u.substring(5));
		f = f.getCanonicalFile();
		url = new URL("file", "", f.getParent());
		source = new CodeSource (url, null);
	      }
	    catch (IOException ignore)
	      {
	      }
	  }

	return defineClass (name, data, 0, len, source);
      } 
    catch (java.io.IOException x)
      {
	throw new ClassNotFoundException(name);
      }
  }

  /** 
   * Defines a Package based on the given name and the supplied manifest
   * information. The manifest indicates the tile, version and
   * vendor information of the specification and implementation and wheter the
   * package is sealed. If the Manifest indicates that the package is sealed
   * then the Package will be sealed with respect to the supplied URL.
   *
   * @exception IllegalArgumentException If this package name already exists
   * in this class loader
   * @param name The name of the package
   * @param manifest The manifest describing the specification,
   * implementation and sealing details of the package
   * @param url the code source url to seal the package
   * @return the defined Package
   */
  protected Package definePackage(String name, Manifest manifest, URL url) 
    throws IllegalArgumentException
  {
    Attributes attr = manifest.getMainAttributes();
    String specTitle =
      attr.getValue(Attributes.Name.SPECIFICATION_TITLE); 
    String specVersion =
      attr.getValue(Attributes.Name.SPECIFICATION_VERSION); 
    String specVendor =
      attr.getValue(Attributes.Name.SPECIFICATION_VENDOR); 
    String implTitle =
      attr.getValue(Attributes.Name.IMPLEMENTATION_TITLE); 
    String implVersion =
      attr.getValue(Attributes.Name.IMPLEMENTATION_VERSION); 
    String implVendor =
      attr.getValue(Attributes.Name.IMPLEMENTATION_VENDOR);

    // Look if the Manifest indicates that this package is sealed
    // XXX - most likely not completely correct!
    // Shouldn't we also check the sealed attribute of the complete jar?
    // http://java.sun.com/products/jdk/1.3/docs/guide/extensions/spec.html#bundled
    // But how do we get that jar manifest here?
    String sealed = attr.getValue(Attributes.Name.SEALED);
    if ("false".equals(sealed))
      {
	// Make sure that the URL is null so the package is not
	// sealed.
	url = null;
      }

    return definePackage(name, specTitle, specVersion, specVendor,
			 implTitle, implVersion, implVendor, url);
  }

  /**
   * Returns the permissions needed to access a particular code source.
   * These permissions includes those returned by
   * <CODE>SecureClassLoader.getPermissions</CODE> and the actual permissions
   * to access the objects referenced by the URL of the code source.
   * The extra permissions added depend on the protocol and file portion of
   * the URL in the code source. If the URL has the "file" protocol ends with
   * a / character then it must be a directory and a file Permission to read
   * everthing in that directory and all subdirectories is added. If the URL
   * had the "file" protocol and doesn't end with a / character then it must
   * be a normal file and a file permission to read that file is added. If the
   * URL has any other protocol then a socket permission to connect and accept
   * connections from the host portion of the URL is added.
   * @param source The codesource that needs the permissions to be accessed
   * @return the collection of permissions needed to access the code resource
   * @see SecureClassLoader.getPermissions()
   */
  protected PermissionCollection getPermissions(CodeSource source)
  {
    // XXX - This implementation does exactly as the Javadoc describes.
    // But maybe we should/could use URLConnection.getPermissions()?

    // First get the permissions that would normally be granted
    PermissionCollection permissions = super.getPermissions(source);
        
    // Now add the any extra permissions depending on the URL location
    URL url = source.getLocation();
    String protocol = url.getProtocol();
    if (protocol.equals("file"))
      {
	String file = url.getFile();
	// If the file end in / it must be an directory
	if (file.endsWith("/"))
	  {
	    // Grant permission to read everything in that directory and
	    // all subdirectories
	    permissions.add(new FilePermission(file + "-", "read"));
	  }
	else
	  {
	    // It is a 'normal' file
	    // Grant permission to access that file
	    permissions.add(new FilePermission(file, "read"));
	  }
      }
    else
      {
	// Grant permission to connect to and accept connections from host
	  String host = url.getHost();
	  permissions.add(new SocketPermission(host, "connect,accept"));
      }

    return permissions;
  }

  /**
   * Creates a new instance of a URLClassLoader that gets classes from the
   * supplied URLs. This class loader will have as parent the standard
   * system class loader.
   * @param urls the initial URLs used to resolve classes and resources
   */
  public static URLClassLoader newInstance(URL urls[]) throws
    SecurityException
  {
    return new URLClassLoader(urls);
  }

  /**
   * Creates a new instance of a URLClassLoader that gets classes from the
   * supplied URLs and with the supplied loader as parent class loader.
   * @param urls the initial URLs used to resolve classes and resources
   * @param parent the parent class loader
   */
  public static URLClassLoader newInstance(URL urls[],
					   ClassLoader parent)
    throws SecurityException
  {
    return new URLClassLoader(urls, parent);
  }
}
