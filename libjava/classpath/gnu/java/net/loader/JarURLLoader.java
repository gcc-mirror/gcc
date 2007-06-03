package gnu.java.net.loader;

import gnu.java.net.IndexListParser;

import java.io.IOException;
import java.net.JarURLConnection;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.net.URLStreamHandlerFactory;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.jar.Attributes;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.jar.Manifest;

/**
 * A <code>JarURLLoader</code> is a type of <code>URLLoader</code>
 * only loading from jar url.
 */
public final class JarURLLoader extends URLLoader
{
  // True if we've initialized -- i.e., tried open the jar file.
  boolean initialized;
  // The jar file for this url.
  JarFile jarfile;
  // Base jar: url for all resources loaded from jar.
  final URL baseJarURL;
  // The "Class-Path" attribute of this Jar's manifest.
  ArrayList<URLLoader> classPath;
  // If not null, a mapping from INDEX.LIST for this jar only.
  // This is a set of all prefixes and top-level files that
  // ought to be available in this jar.
  Set indexSet;

  // This constructor is used internally.  It purposely does not open
  // the jar file -- it defers this until later.  This allows us to
  // implement INDEX.LIST lazy-loading semantics.
  private JarURLLoader(URLClassLoader classloader, URLStreamHandlerCache cache,
                       URLStreamHandlerFactory factory,
                       URL baseURL, URL absoluteUrl,
                       Set indexSet)
  {
    super(classloader, cache, factory, baseURL, absoluteUrl);

    URL newBaseURL = null;
    try
      {
        // Cache url prefix for all resources in this jar url.
        String base = baseURL.toExternalForm() + "!/";
        newBaseURL = new URL("jar", "", -1, base, cache.get(factory, "jar"));
      }
    catch (MalformedURLException ignore)
      {
        // Ignore.
      }
    this.baseJarURL = newBaseURL;
    this.classPath = null;
    this.indexSet = indexSet;
  }

  // This constructor is used by URLClassLoader.  It will immediately
  // try to read the jar file, in case we've found an index or a class-path
  // setting.  FIXME: it would be nice to defer this as well, but URLClassLoader
  // makes this hard.
  public JarURLLoader(URLClassLoader classloader, URLStreamHandlerCache cache,
                      URLStreamHandlerFactory factory,
                      URL baseURL, URL absoluteUrl)
  {
    this(classloader, cache, factory, baseURL, absoluteUrl, null);
    initialize();
  }

  private void initialize()
  {
    JarFile jarfile = null;
    try
      {
        jarfile =
          ((JarURLConnection) baseJarURL.openConnection()).getJarFile();
        
        Manifest manifest;
        Attributes attributes;
        String classPathString;

        IndexListParser parser = new IndexListParser(jarfile, baseJarURL,
                                                     baseURL);
        LinkedHashMap<URL, Set<String>> indexMap = parser.getHeaders();
        if (indexMap != null)
          {
            // Note that the index also computes
            // the resulting Class-Path -- there are jars out there
            // where the index lists some required jars which do
            // not appear in the Class-Path attribute in the manifest.
            this.classPath = new ArrayList<URLLoader>();
            Iterator<Map.Entry<URL, Set<String>>> it = indexMap.entrySet().iterator();
            while (it.hasNext())
              {
                Map.Entry<URL, Set<String>> entry = it.next();
                URL subURL = entry.getKey();
                Set<String> prefixes = entry.getValue();
                if (subURL.equals(baseURL))
                  this.indexSet = prefixes;
                else
                  {
                    JarURLLoader subLoader = new JarURLLoader(classloader,
                                                              cache,
                                                              factory, subURL,
                                                              subURL,
                                                              prefixes);
                    // Note that we don't care if the sub-loader itself has an
                    // index or a class-path -- only the top-level jar
                    // file gets this treatment; its index should cover
                    // everything.
                    this.classPath.add(subLoader);
                  }
              }
          }
        else if ((manifest = jarfile.getManifest()) != null
                 && (attributes = manifest.getMainAttributes()) != null
                 && ((classPathString
                      = attributes.getValue(Attributes.Name.CLASS_PATH)) 
                     != null))
          {
            this.classPath = new ArrayList<URLLoader>();
            StringTokenizer st = new StringTokenizer(classPathString, " ");
            while (st.hasMoreElements ()) 
              {
                String e = st.nextToken ();
                try
                  {
                    URL subURL = new URL(baseURL, e);
                    // We've seen at least one jar file whose Class-Path
                    // attribute includes the original jar.  If we process
                    // that normally we end up with infinite recursion.
                    if (subURL.equals(baseURL))
                      continue;
                    JarURLLoader subLoader = new JarURLLoader(classloader,
                                                              cache, factory,
                                                              subURL, subURL);
                    this.classPath.add(subLoader);
                    ArrayList<URLLoader> extra = subLoader.getClassPath();
                    if (extra != null)
                      this.classPath.addAll(extra);
                  }
                catch (java.net.MalformedURLException xx)
                  {
                    // Give up
                  }
              }
          }
      }
    catch (IOException ioe)
      {
        /* ignored */
      }

    this.jarfile = jarfile;
    this.initialized = true;
  }

  /** get resource with the name "name" in the jar url */
  public Resource getResource(String name)
  {
    if (name.startsWith("/"))
      name = name.substring(1);
    if (indexSet != null)
      {
        // Trust the index.
        String basename = name;
        int offset = basename.lastIndexOf('/');
        if (offset != -1)
          basename = basename.substring(0, offset);
        if (! indexSet.contains(basename))
          return null;
        // FIXME: if the index claim to hold the resource, and jar file
        // doesn't have it, we're supposed to throw an exception.  However,
        // in our model this is tricky to implement, as another URLLoader from
        // the same top-level jar may have an overlapping index entry.
      }

    if (! initialized)
      initialize();
    if (jarfile == null)
      return null;

    JarEntry je = jarfile.getJarEntry(name);
    if (je != null)
      return new JarURLResource(this, name, je);
    else
      return null;
  }

  public Manifest getManifest()
  {
    try
      {
        return (jarfile == null) ? null : jarfile.getManifest();
      }
    catch (IOException ioe)
      {
        return null;
      }
  }

  public ArrayList<URLLoader> getClassPath()
  {
    return classPath;
  }
}
