/* gnu.classpath.tools.doclets.xmldoclet.doctranslet.DocTranslet
   Copyright (C) 2001 Free Software Foundation, Inc.

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
02111-1307 USA. */

package gnu.classpath.tools.doclets.xmldoclet.doctranslet;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;

import java.net.MalformedURLException;
import java.net.URL;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.HashMap;
import java.util.Map;

import java.util.jar.JarFile;
import java.util.jar.Manifest;
import java.util.jar.Attributes;

import javax.xml.transform.ErrorListener;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.URIResolver;

import javax.xml.transform.dom.DOMResult;

import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import org.xml.sax.SAXException;

import gnu.classpath.tools.IOToolkit;
import gnu.classpath.tools.doclets.xmldoclet.Driver;

import com.sun.javadoc.DocErrorReporter;

public class DocTranslet implements ErrorListener {

   private static class DocErrorReporterOutputStream
      extends OutputStream
   {
      private ByteArrayOutputStream out = new ByteArrayOutputStream();
      private DocErrorReporter reporter;

      public DocErrorReporterOutputStream(DocErrorReporter reporter) {
         this.reporter = reporter;
      }

      public void write(int ch) {
         out.write(ch);
         if (ch == 10) {
            reporter.printNotice(out.toString());
            out.reset();
         }
      }
   }

   private String mainResourceFilename;
   private ClassLoader classLoader;
   private Map transformerMap = new java.util.HashMap(); //WeakHashMap();
   private DocTransletOptions options;
   
   protected DocTranslet(String mainResourceFilename,
                         ClassLoader classLoader)
      throws DocTransletConfigurationException {

      if (mainResourceFilename.length() > 0 && mainResourceFilename.charAt(0) == '/') {
         mainResourceFilename = mainResourceFilename.substring(1);
      }
      this.mainResourceFilename = mainResourceFilename;
      this.classLoader = classLoader;
   }

   private static boolean equalsFile(File file1, File file2) {
      return file1.getAbsolutePath().equals(file2.getAbsolutePath());
   }

   private static File getParentFile(File file) {
      String filename = file.getAbsolutePath();
      if (filename.endsWith(File.separator)) {
         filename = filename.substring(0, filename.length() - 1);
      }
      int lastSlash = filename.lastIndexOf(File.separatorChar);
      if (lastSlash > 0) {
         filename = filename.substring(0, lastSlash);
      }
      else {
         filename = File.separator;
      }

      return new File(filename);
   }

   private static boolean cacheXSLTSheets = true;

   public void apply(File xmlSourceDirectory, File targetDirectory,
                     DocErrorReporter reporter)
      throws DocTransletException {

      PrintStream err = System.err;

      try{
         URL mainResourceURL = classLoader == null ?
	     ClassLoader.getSystemResource(mainResourceFilename):
	     classLoader.getResource(mainResourceFilename);

         if (null == mainResourceURL) {
            throw new DocTransletException("Cannot find resource '" + mainResourceFilename + "'");
         }

         
         Map parameters = new HashMap();
         parameters.put("gjdoc.xmldoclet.version", Driver.XMLDOCLET_VERSION);

         parameters.put("gjdoc.option.nonavbar", xsltBoolean(options.nonavbar));
         parameters.put("gjdoc.option.noindex", xsltBoolean(options.noindex));
         parameters.put("gjdoc.option.notree", xsltBoolean(options.notree));
         parameters.put("gjdoc.option.nocomment", xsltBoolean(options.nocomment));
         parameters.put("gjdoc.option.nohelp", xsltBoolean(options.nohelp));
         parameters.put("gjdoc.option.splitindex", xsltBoolean(options.splitindex));
         parameters.put("gjdoc.option.linksource", xsltBoolean(options.linksource));
         parameters.put("gjdoc.option.nodeprecatedlist", xsltBoolean(options.nodeprecatedlist));
         parameters.put("gjdoc.option.uses", xsltBoolean(options.uses));
         parameters.put("gjdoc.option.windowtitle", options.windowtitle);
         parameters.put("gjdoc.option.helpfile", options.helpfile);
         parameters.put("gjdoc.option.stylesheetfile", options.stylesheetfile);
         parameters.put("gjdoc.option.header", options.header);
         parameters.put("gjdoc.option.footer", options.footer);
         parameters.put("gjdoc.option.bottom", options.bottom);
         parameters.put("gjdoc.option.doctitle", options.doctitle);

         List outputFileList = getOutputFileList(mainResourceURL,
                                                 xmlSourceDirectory,
                                                 parameters);

         reporter.printNotice("Running DocTranslet...");
            
         TransformerFactory transformerFactory 
            = TransformerFactory.newInstance();

         transformerFactory.setErrorListener(this);

         boolean isLibxmlJ 
            = transformerFactory.getClass().getName().equals("gnu.xml.libxmlj.transform.TransformerFactoryImpl");

         for (Iterator it = outputFileList.iterator(); it.hasNext(); ) {

            if (isLibxmlJ) {
               System.gc();
               Runtime.getRuntime().runFinalization();
            }

            OutputFileInfo fileInfo = (OutputFileInfo)it.next();

            File targetFile = new File(targetDirectory, fileInfo.getName());
            File packageTargetDir = getParentFile(targetFile);

            if (!packageTargetDir.exists() && !packageTargetDir.mkdirs()) {
               throw new DocTransletException("Target directory " + packageTargetDir + " does not exist and cannot be created.");
            }

            if (options.linksource) {
               File sourceTargetDirectory = new File(targetDirectory, "src-html");
               File sourceTargetFile = new File(sourceTargetDirectory, fileInfo.getName());
               File sourcePackageTargetDir = getParentFile(sourceTargetFile);

               if (!sourcePackageTargetDir.exists() && !sourcePackageTargetDir.mkdirs()) {
                  throw new DocTransletException("Target directory " + packageTargetDir + " does not exist and cannot be created.");
               }
            }

            if (options.uses) {
               File usesTargetDirectory = new File(targetDirectory, "class-use");
               File usesTargetFile = new File(usesTargetDirectory, fileInfo.getName());
               File usesPackageTargetDir = getParentFile(usesTargetFile);

               if (!usesPackageTargetDir.exists() && !usesPackageTargetDir.mkdirs()) {
                  throw new DocTransletException("Target directory " + packageTargetDir + " does not exist and cannot be created.");
               }
            }

            if (null != fileInfo.getSource()) {
            
               reporter.printNotice("Copying " + fileInfo.getComment() + "...");
               InputStream in = new URL(mainResourceURL, fileInfo.getSource()).openStream();
               FileOutputStream out = new FileOutputStream(targetFile.getAbsolutePath());
               IOToolkit.copyStream(in, out);
               in.close();
               out.close();
            }
            else {
            
               reporter.printNotice("Generating " + fileInfo.getComment() + "...");

               String pathToRoot = "";
               for (File file = getParentFile(targetFile); !equalsFile(file, targetDirectory); file = getParentFile(file)) {
                  pathToRoot += "../";
               }
            
               StreamResult out = new StreamResult(targetFile.getAbsolutePath());

               StreamSource in = new StreamSource(new File(xmlSourceDirectory, "index.xml").getAbsolutePath());
               URL resource = new URL(mainResourceURL, fileInfo.getSheet());


               StreamSource xsltSource = new StreamSource(resource.toExternalForm());

               if (null != fileInfo.getInfo()) {
                  parameters.put("gjdoc.outputfile.info", fileInfo.getInfo());
               }
               parameters.put("gjdoc.pathtoroot", pathToRoot);

               Transformer transformer;
               transformer = (Transformer)transformerMap.get(xsltSource.getSystemId());
               if (null == transformer) {
                  transformer = transformerFactory.newTransformer(xsltSource);
                  if (cacheXSLTSheets) {
                     transformerMap.put(xsltSource.getSystemId(), transformer);
                  }
               }

               transformer.clearParameters();
               for (Iterator pit = parameters.keySet().iterator(); pit.hasNext(); ) {
                  String key = (String)pit.next();
                  String value = (String)parameters.get(key);
                  transformer.setParameter(key, value);
               }

               transformer.setErrorListener(this);
               DocErrorReporterOutputStream errorReporterOut
                  = new DocErrorReporterOutputStream(reporter);
               System.setErr(new PrintStream(errorReporterOut));

               transformer.transform(in, out);
               errorReporterOut.flush();
            }
         }
      }
      catch (MalformedURLException e) {
	 throw new DocTransletException(e);
      }
      catch (TransformerFactoryConfigurationError e) {
	 throw new DocTransletException(e);
      }
      catch (TransformerException e) {
	 throw new DocTransletException(e.getMessageAndLocation(), e);
      }
      catch (IOException e) {
	 throw new DocTransletException(e);
      }
      finally {
         System.setErr(err);
      }
   }

   private List getOutputFileList(URL resource, File xmlSourceDirectory, Map parameters) 
      throws DocTransletException {

      try {
	 List result;

	 OutputStream out = new ByteArrayOutputStream();

         DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();
         DocumentBuilder documentBuilder = documentBuilderFactory.newDocumentBuilder();
         Document document = documentBuilder.newDocument();
         DOMResult domResult = new DOMResult(document);         
         {
            StreamSource source = new StreamSource(resource.toExternalForm());
            
            TransformerFactory transformerFactory = TransformerFactory.newInstance();
            Transformer transformer = (Transformer)transformerFactory.newTransformer(source);
            
            transformer.clearParameters();
            for (Iterator pit = parameters.keySet().iterator(); pit.hasNext(); ) {
               String key = (String)pit.next();
               String value = (String)parameters.get(key);
               transformer.setParameter(key, value);
            }
            
            transformer.transform(new StreamSource(new File(xmlSourceDirectory, 
                                                            "index.xml").getAbsolutePath()), 
                                  domResult);
         }

         {	 
            NodeList nodeList = document.getElementsByTagName("outputfile");
            result = new ArrayList(nodeList.getLength());

            for (int i=0; i<nodeList.getLength(); ++i) {
               Element elem = (Element)nodeList.item(i);
               String name    = getTextContent(elem.getElementsByTagName("name").item(0));
               String source  
                  = (null != elem.getElementsByTagName("source").item(0))
                  ? getTextContent(elem.getElementsByTagName("source").item(0))
                  : null;
               String sheet
                  = (null != elem.getElementsByTagName("sheet").item(0))
                  ? getTextContent(elem.getElementsByTagName("sheet").item(0))
                  : null;
               String comment = getTextContent(elem.getElementsByTagName("comment").item(0));
               String info    = null;
               if (elem.getElementsByTagName("info").getLength() > 0) {
                  if (null != elem.getElementsByTagName("info").item(0).getFirstChild()) {
                     info = getTextContent(elem.getElementsByTagName("info").item(0));
                  }
                  else {
                     info = "";
                  }
               }
               result.add(new OutputFileInfo(name, source, sheet, comment, info));
            }
         }
         return result;
      }
      catch (TransformerFactoryConfigurationError e) {
	 throw new DocTransletException(e);
      }
      catch (TransformerException e) {
	 throw new DocTransletException(e.getMessageAndLocation(), e);
      }
      catch (ParserConfigurationException e) {
	 throw new DocTransletException(e);
      }
   }

   private String getTextContent(Node elem)
   {
      StringBuffer result = new StringBuffer();
      NodeList children = elem.getChildNodes();
      for (int i=0; i<children.getLength(); ++i) {
         Node item = children.item(i);
         if (null != item) {
            String value = item.getNodeValue();
            if (null != value) {
               result.append(value);
            }
         }
      }
      return result.toString();
   }

   public void setOptions(DocTransletOptions options) {
      this.options = options;
   }


   public static DocTranslet fromClasspath(String resourceName) 
      throws DocTransletConfigurationException {

      return new DocTranslet(resourceName, 
                             DocTranslet.class.getClassLoader());
   }

   public static DocTranslet fromJarFile(File jarFile) 
      throws DocTransletConfigurationException {

      try {
         JarFile inputJarFile = new JarFile(jarFile, false, JarFile.OPEN_READ);
      
         Manifest manifest = inputJarFile.getManifest();
         
         if (null == manifest) {
         
            throw new DocTransletConfigurationException("Jar file '" + jarFile + "' doesn't contain a manifest.");
         }
         
         Attributes mainAttributes = manifest.getMainAttributes();
      
         String docTransletMainEntry = mainAttributes.getValue("doctranslet-main-entry");

         if (null == docTransletMainEntry) {
            
            throw new DocTransletConfigurationException("Manifest in Jar file '" + jarFile + "' doesn't contain a doctranslet-main-entry specification.");
         }
         
         return new DocTranslet(docTransletMainEntry, 
                                new JarClassLoader(inputJarFile));
      }
      catch (IOException e) {
         throw new DocTransletConfigurationException(e);
      }
   }

   private static String xsltBoolean(boolean b) {
      return b ? "1" : "";
   }

  public void error (TransformerException exception)
    throws TransformerException {

     throw exception;
  }

  public void fatalError (TransformerException exception)
    throws TransformerException {

     throw exception;
  }

  public void warning (TransformerException exception)
    throws TransformerException {

     System.err.println("WWW: " + exception.getMessage());
  }
}
