/* xmlj_transform.c -
   Copyright (C) 2003, 2004 Free Software Foundation, Inc.

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

#include "gnu_xml_libxmlj_transform_GnomeTransformerFactory.h"
#include "gnu_xml_libxmlj_transform_GnomeTransformer.h"

#include "xmlj_dom.h"
#include "xmlj_io.h"
#include "xmlj_error.h"
#include "xmlj_node.h"
#include "xmlj_sax.h"
#include "xmlj_util.h"

#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>

#include <libxml/xmlmemory.h>
#include <libxml/debugXML.h>
#include <libxml/xmlIO.h>
#include <libxml/xinclude.h>
#include <libxml/parser.h>
#include <libxml/catalog.h>
#include <libxslt/keys.h>
#include <libxslt/xslt.h>
#include <libxslt/xsltInternals.h>
#include <libxslt/transform.h>
#include <libxslt/xsltutils.h>
#include <libxslt/functions.h>
#include <libxslt/extensions.h>
#include <libxslt/documents.h>

/* Local function prototypes */

void
xmljDocumentFunction (xmlXPathParserContextPtr ctxt, int nargs);

xsltStylesheetPtr
xmljGetStylesheetID (JNIEnv * env, jobject transformer);

jobject
xmljGetTransformerProperties (JNIEnv *env, jobject transformer);

const xmlChar *
xmljBooleanToString (int value);

void
xmljSetOutputProperties (JNIEnv *env, jobject transformer,
                         xsltStylesheetPtr stylesheet);

jobjectArray
xmljGetParameterArray (JNIEnv *env, jobject transformer);

const char **
xmljGetParameters (JNIEnv *env, jobjectArray pa);

void
xmljFreeParameters (JNIEnv *env, jobjectArray pa, const char **parameters);

xmlDocPtr
xmljTransform (JNIEnv *env, jobject transformer, xmlDocPtr source);

void
xmljTransformToSAX (JNIEnv *env, jobject transformer, xmlDocPtr source,
                    jobject callback);

xmlDocPtr
xmljDocLoader (const xmlChar *uri, xmlDictPtr dict, int options,
               void *ctxt, xsltLoadType type);

/* HACK: store stylesheet URL as context for resolving URIs in xmljDocLoader */
static jstring stylesheetURL = NULL;

/*
 * --------------------------------------------------------------------------
 * 
 * Native implementation for class
 * gnu.xml.libxmlj.transform.GnomeTransformer follows.
 */

static void
xmljSetProperty (JNIEnv * env, jobject outputProperties,
		 jmethodID setPropertyMethodID, const char *name,
		 const xmlChar * value)
{
  if (NULL != value)
    {
      jstring nameString = (*env)->NewStringUTF (env, name);
      jstring valueString = (*env)->NewStringUTF (env, (const char *) value);

      jobject prevValue = (*env)->CallObjectMethod (env, outputProperties,
						    setPropertyMethodID,
						    nameString, valueString);
      if (NULL != prevValue)
	{
	  (*env)->DeleteLocalRef (env, prevValue);
	}

      (*env)->DeleteLocalRef (env, nameString);
      (*env)->DeleteLocalRef (env, valueString);
    }
}

typedef struct CdataSectionScannerInfo_
{
  JNIEnv *env;
  jobject stringBuffer;
  jmethodID appendMethodID;
  int isFirst;
} CdataSectionScannerInfo;

static void
cdataSectionScanner (void *payload, void *data, xmlChar * name)
{
  CdataSectionScannerInfo *info = (CdataSectionScannerInfo *) data;
  JNIEnv *env = info->env;
  jstring nameString = (*env)->NewStringUTF (env, (const char *) name);
  jstring blankString = (*env)->NewStringUTF (env, " ");
  jobject stringBuffer;
  if (!info->isFirst)
    {
      stringBuffer
	= (*env)->CallObjectMethod (env,
				    info->stringBuffer,
				    info->appendMethodID, blankString);
      (*env)->DeleteLocalRef (env, stringBuffer);
    }
  info->isFirst = 0;
  stringBuffer
    = (*env)->CallObjectMethod (env,
				info->stringBuffer,
				info->appendMethodID, nameString);
  (*env)->DeleteLocalRef (env, stringBuffer);
  (*env)->DeleteLocalRef (env, blankString);
  (*env)->DeleteLocalRef (env, nameString);
}

void
xmljDocumentFunction (xmlXPathParserContextPtr ctxt, int nargs)
{
  xmlXPathObjectPtr obj, obj2 = NULL;

  if ((nargs < 1) || (nargs > 2))
    {
      xsltTransformError (xsltXPathGetTransformContext (ctxt), NULL, NULL,
			  "document() : invalid number of args %d\n", nargs);
      ctxt->error = XPATH_INVALID_ARITY;
      return;
    }
  if (ctxt->value == NULL)
    {
      xsltTransformError (xsltXPathGetTransformContext (ctxt), NULL, NULL,
			  "document() : invalid arg value\n");
      ctxt->error = XPATH_INVALID_TYPE;
      return;
    }

  if (nargs == 2)
    {
      if (ctxt->value->type != XPATH_NODESET)
	{
	  xsltTransformError (xsltXPathGetTransformContext (ctxt), NULL, NULL,
			      "document() : invalid arg expecting a nodeset\n");
	  ctxt->error = XPATH_INVALID_TYPE;
	  return;
	}

      obj2 = valuePop (ctxt);
    }

  if (ctxt->value->type == XPATH_NODESET)
    {
      int i;
      xmlXPathObjectPtr newobj, ret;

      obj = valuePop (ctxt);
      ret = xmlXPathNewNodeSet (NULL);

      if (obj->nodesetval)
	{
	  for (i = 0; i < obj->nodesetval->nodeNr; i++)
	    {
	      valuePush (ctxt,
			 xmlXPathNewNodeSet (obj->nodesetval->nodeTab[i]));
	      xmlXPathStringFunction (ctxt, 1);
	      if (nargs == 2)
		{
		  valuePush (ctxt, xmlXPathObjectCopy (obj2));
		}
	      else
		{
		  valuePush (ctxt,
			     xmlXPathNewNodeSet (obj->nodesetval->
						 nodeTab[i]));
		}
	      xsltDocumentFunction (ctxt, 2);
	      newobj = valuePop (ctxt);
	      ret->nodesetval = xmlXPathNodeSetMerge (ret->nodesetval,
						      newobj->nodesetval);
	      xmlXPathFreeObject (newobj);
	    }
	}

      xmlXPathFreeObject (obj);
      if (obj2 != NULL)
        {
          xmlXPathFreeObject (obj2);
        }
      valuePush (ctxt, ret);
      return;
    }
  /*
   * Make sure it's converted to a string
   */
  xmlXPathStringFunction (ctxt, 1);
  if (ctxt->value->type != XPATH_STRING)
    {
      xsltTransformError (xsltXPathGetTransformContext (ctxt), NULL, NULL,
			  "document() : invalid arg expecting a string\n");
      ctxt->error = XPATH_INVALID_TYPE;
      if (obj2 != NULL)
	xmlXPathFreeObject (obj2);
      return;
    }
  obj = valuePop (ctxt);
  if (obj->stringval == NULL)
    {
      valuePush (ctxt, xmlXPathNewNodeSet (NULL));
    }
  else
    {

      xsltTransformContextPtr tctxt;

      tctxt = xsltXPathGetTransformContext (ctxt);

      {
        SAXParseContext *saxContext =
          (SAXParseContext *) tctxt->style->_private;

        xmlDocPtr tree = xmljResolveURIAndOpen (saxContext,
                                                (const char*)obj->stringval, 
                                                NULL);

        xsltNewDocument (tctxt, tree);	/* FIXME - free at a later point */

        valuePush (ctxt, xmlXPathNewNodeSet ((xmlNodePtr) tree));
      }
    }
  xmlXPathFreeObject (obj);
  if (obj2 != NULL) {
    xmlXPathFreeObject (obj2);
  }
}

/*
 * Returns the stylesheet pointer for the given GnomeTransformer.
 */
xsltStylesheetPtr
xmljGetStylesheetID (JNIEnv * env, jobject transformer)
{
  jclass cls;
  jfieldID field;
  jobject id;
  xsltStylesheetPtr stylesheet;

  if (transformer == NULL)
    {
      xmljThrowException (env, "javax/xml/transform/TransformerException",
                          "Transformer is null");
      return NULL;
    }
  cls = (*env)->GetObjectClass (env, transformer);
  if (cls == NULL)
    {
      return NULL;
    }
  field = (*env)->GetFieldID (env, cls, "stylesheet", "Ljava/lang/Object;");
  if (field == NULL)
    {
      return NULL;
    }
  id = (*env)->GetObjectField (env, transformer, field);
  stylesheet = (xsltStylesheetPtr) xmljAsPointer (env, id);
  if (stylesheet == NULL)
    {
      xmljThrowException (env, "javax/xml/transform/TransformerException",
                          "Stylesheet is null");
      return NULL;
    }
  return stylesheet;
}

jobject
xmljGetTransformerProperties (JNIEnv *env, jobject transformer)
{
  jclass cls;
  jfieldID field;
  
  cls = (*env)->GetObjectClass (env, transformer);
  if (cls == NULL)
    {
      return NULL;
    }
  field = (*env)->GetFieldID (env, cls, "outputProperties",
                              "Ljava/util/Properties;");
  if (field == NULL)
    {
      return NULL;
    }
  return (*env)->GetObjectField (env, transformer, field);
}

const xmlChar *
xmljBooleanToString (int value)
{
  return value ? BAD_CAST "yes" : BAD_CAST "no";
}

/*
 * Sets the output properties for the given transformer,
 * based on its stylesheet.
 */
void
xmljSetOutputProperties (JNIEnv *env, jobject transformer,
                         xsltStylesheetPtr stylesheet)
{
  jobject outputProperties;
  jclass propertiesClass;
  jmethodID setPropertyMethod;

  outputProperties = xmljGetTransformerProperties (env, transformer);
  if (outputProperties == NULL)
    {
      return;
    }
  propertiesClass = (*env)->FindClass (env, "java/util/Properties");
  if (propertiesClass == NULL)
    {
      return;
    }
  setPropertyMethod =
    (*env)->GetMethodID (env, propertiesClass, "setProperty",
                         "(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/Object;");
  if (setPropertyMethod == NULL)
    {
      return;
    }
  
  xmljSetProperty (env, outputProperties, setPropertyMethod,
                   "encoding", stylesheet->encoding);
  
  xmljSetProperty (env, outputProperties, setPropertyMethod,
                   "media-type", stylesheet->mediaType);
  
  xmljSetProperty (env, outputProperties, setPropertyMethod,
                   "doctype-public", stylesheet->doctypePublic);
  
  xmljSetProperty (env, outputProperties, setPropertyMethod,
                   "doctype-system", stylesheet->doctypeSystem);
  
  xmljSetProperty (env, outputProperties, setPropertyMethod,
                   "indent", xmljBooleanToString (stylesheet->indent));
  
  xmljSetProperty (env, outputProperties, setPropertyMethod,
                   "method", stylesheet->method);
  
  xmljSetProperty (env, outputProperties, setPropertyMethod,
                   "standalone", xmljBooleanToString (stylesheet->standalone));
  
  xmljSetProperty (env, outputProperties, setPropertyMethod,
                   "version", stylesheet->version);
  
  xmljSetProperty (env, outputProperties, setPropertyMethod,
                   "omit-xml-declaration",
                   xmljBooleanToString (stylesheet->omitXmlDeclaration));
  
    {
      CdataSectionScannerInfo info;
      jclass stringBufferClass
        =
        (*env)->FindClass (env,
                           "java/lang/StringBuffer");
      jmethodID stringBufferConstructorID =
        (*env)->GetMethodID (env, stringBufferClass,
                             "<init>", "()V");
      jmethodID toStringMethodID =
        (*env)->GetMethodID (env, stringBufferClass,
                             "toString",
                             "()Ljava/lang/String;");
      info.env = env;
      info.isFirst = 1;
      info.stringBuffer
        = (*env)->AllocObject (env, stringBufferClass);
      (*env)->CallVoidMethod (env, info.stringBuffer,
                              stringBufferConstructorID);
      info.appendMethodID =
        (*env)->GetMethodID (env, stringBufferClass,
                             "append",
                             "(Ljava/lang/String;)Ljava/lang/StringBuffer;");
      
      xmlHashScan (stylesheet->cdataSection,
                   cdataSectionScanner, &info);
      
        {
          jstring result = (jstring)
            (*env)->CallObjectMethod (env,
                                      info.stringBuffer,
                                      toStringMethodID);
          
          jstring nameString =
            (*env)->NewStringUTF (env,
                                  "cdata-section-elements");
          
          jobject prevValue
            =
            (*env)->CallObjectMethod (env,
                                      outputProperties,
                                      setPropertyMethod,
                                      nameString, result);
          if (NULL != prevValue)
            {
              (*env)->DeleteLocalRef (env, prevValue);
            }
          (*env)->DeleteLocalRef (env, nameString);
        }
      
      (*env)->DeleteLocalRef (env, info.stringBuffer);
    }
}

/*
 * Returns the parameter array for the given GnomeTransformer.
 */
jobjectArray
xmljGetParameterArray (JNIEnv *env, jobject transformer)
{
  jclass cls;
  jmethodID method;

  cls = (*env)->GetObjectClass (env, transformer);
  if (cls == NULL)
    {
      return NULL;
    }
  method = (*env)->GetMethodID (env, cls, "getParameterArray",
                                "()[Ljava/lang/String;");
  if (method == NULL)
    {
      return NULL;
    }
  return (jobjectArray) (*env)->CallObjectMethod (env, transformer, method);
}

/* Convert parameter array to xmlChar ** */
const char **
xmljGetParameters (JNIEnv *env, jobjectArray pa)
{
  int i, len;
  const char **parameters;

  len = (*env)->GetArrayLength (env, pa);
  parameters = (const char **) malloc ((len + 2) * sizeof (const char *));
  if (parameters == NULL)
    {
      return NULL;
    }
  
  for (i = 0; i < len; i++)
    {
      jstring string = (jstring) (*env)->GetObjectArrayElement (env, pa, i);
      
      if (string != NULL)
        {
          parameters[i] = (*env)->GetStringUTFChars (env, string, NULL);
        }
      else
        {
          parameters[i] = NULL;
        }
    }
  
  parameters[len] = 0;
  parameters[len + 1] = 0;
  return parameters;
}
  
/* Release parameter strings */
void
xmljFreeParameters (JNIEnv *env, jobjectArray pa, const char **parameters)
{ 
  int i, len;
  
  len = (*env)->GetArrayLength (env, pa);
  for (i = 0; i < len; i++)
    {
      jstring string = (jstring) (*env)->GetObjectArrayElement (env, pa, i);
      if (string != NULL)
        {
          (*env)->ReleaseStringUTFChars (env, string, parameters[i]);
        }
    }
  
  free (parameters);
}

xmlDocPtr
xmljTransform (JNIEnv *env, jobject transformer, xmlDocPtr source)
{
  xsltStylesheetPtr stylesheet;
  xmlDocPtr result;
  jobjectArray pa;
  const char **parameters;

  stylesheet = xmljGetStylesheetID (env, transformer);
  pa = xmljGetParameterArray (env, transformer);
  parameters = xmljGetParameters (env, pa);
  if (parameters == NULL)
    {
      xmljThrowException (env, "javax/xml/transform/TransformerException",
                          "Couldn't allocate memory for parameters");
      return NULL;
    }
  result = xsltApplyStylesheet (stylesheet, source, parameters);
  xmljFreeParameters (env, pa, parameters);
  if (result == NULL)
    {
      xmljThrowException (env, "javax/xml/transform/TransformerException",
                          "XSLT transformation failed");
    }
  return result;
}

void
xmljTransformToSAX (JNIEnv *env, jobject transformer, xmlDocPtr source,
                    jobject callback)
{
  xsltStylesheetPtr stylesheet;
  int ret;
  jobjectArray pa;
  const char **parameters;
  xmlSAXHandlerPtr sax;

  stylesheet = xmljGetStylesheetID (env, transformer);
  pa = xmljGetParameterArray (env, transformer);
  parameters = xmljGetParameters (env, pa);
  if (parameters == NULL)
    {
      xmljThrowException (env, "javax/xml/transform/TransformerException",
                          "Couldn't allocate memory for parameters");
      return;
    }
  sax = NULL; /* TODO link up sax and callback */
  ret = xsltRunStylesheet (stylesheet, source, parameters, NULL, sax, NULL);
  xmljFreeParameters (env, pa, parameters);
  if (ret == -1)
    {
      xmljThrowException (env, "javax/xml/transform/TransformerException",
                          "XSLT transformation failed");
    }
}

xmlDocPtr
xmljDocLoader (const xmlChar *uri, xmlDictPtr dict, int options,
               void *ctxt, xsltLoadType type)
{
  JNIEnv *env;
  jclass xmljClass;
  jclass inputStreamClass;
  jmethodID getInputStream;
  jmethodID getDetectBuffer;
  jstring systemId;
  jobject inputStream;
  jbyteArray detectBuffer;

  fflush(stdout);
  env = xmljGetJNIEnv ();
  if (!env)
    {
      return NULL;
    }
  xmljClass = (*env)->FindClass (env, "gnu/xml/libxmlj/util/XMLJ");
  if (!xmljClass)
    {
      return NULL;
    }
  getInputStream =
    (*env)->GetStaticMethodID (env, xmljClass, "xmljGetInputStream",
                               "(Ljava/lang/String;Ljava/lang/String;)Lgnu/xml/libxmlj/util/NamedInputStream;");
  if (!getInputStream)
    {
      return NULL;
    }
  systemId = xmljNewString (env, uri);
  inputStream = (*env)->CallStaticObjectMethod (env, xmljClass, getInputStream,
                                                stylesheetURL, systemId);
  if (!inputStream)
    {
      return NULL;
    }
  inputStreamClass = (*env)->GetObjectClass (env, inputStream);
  if (!inputStreamClass)
    {
      return NULL;
    }
  getDetectBuffer = (*env)->GetMethodID (env, inputStreamClass,
                                         "getDetectBuffer", "()[B");
  if (!getDetectBuffer)
    {
      return NULL;
    }
  detectBuffer = (*env)->CallObjectMethod (env, inputStream, getDetectBuffer);
  if (!detectBuffer)
    {
      return NULL;
    }
  return xmljParseDocument (env, NULL, inputStream, detectBuffer,
                            NULL, systemId, stylesheetURL,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 2);
}

/* GnomeTransformer.newStylesheet */
JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_transform_GnomeTransformer_newStylesheet (JNIEnv *env,
                                                               jobject self)
{
  xsltStylesheetPtr stylesheet;
  jobject ret;

  stylesheetURL = NULL;
  xsltSetLoaderFunc (xmljDocLoader);
  stylesheet = xsltNewStylesheet ();
  xmljSetOutputProperties (env, self, stylesheet);
  ret = xmljAsField (env, stylesheet);
  if (ret == NULL)
    {
      xmljThrowException (env,
                          "javax/xml/transform/TransformerConfigurationException",
                          "Can't create Java object for stylesheet");
    }
  return ret;
}

/* GnomeTransformer.newStylesheetFromStream */
JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_transform_GnomeTransformer_newStylesheetFromStream
(JNIEnv *env, jobject self, jobject in, jbyteArray detectBuffer,
 jstring publicId, jstring systemId, jstring base,
 jboolean entityResolver, jboolean errorHandler)
{
  xmlDocPtr doc;
  xsltStylesheetPtr stylesheet;
  jobject ret;

  doc = xmljParseDocument (env, self, in, detectBuffer, publicId, systemId,
                           base, 0, 0, 0, 0, 0, 
                           entityResolver, errorHandler, 0, 0, 2);
  if (doc == NULL)
    {
      return NULL;
    }
  stylesheetURL = systemId;
  xsltSetLoaderFunc (xmljDocLoader);
  stylesheet = xsltParseStylesheetDoc (doc);
  if (stylesheet == NULL)
    {
      xmljThrowException (env,
                          "javax/xml/transform/TransformerConfigurationException",
                          "Error parsing XSLT stylesheet");
      return NULL;
    }
  xmljSetOutputProperties (env, self, stylesheet);
  ret =  xmljAsField (env, stylesheet);
  if (ret == NULL)
    {
      xmljThrowException (env,
                          "javax/xml/transform/TransformerConfigurationException",
                          "Can't create Java object for stylesheet");
    }
  return ret;
}

/* GnomeTransformer.newStylesheetFromDoc */
JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_transform_GnomeTransformer_newStylesheetFromDoc
(JNIEnv *env, jobject self, jobject in)
{
  xmlDocPtr doc;
  xsltStylesheetPtr stylesheet;
  jobject ret;

  doc = (xmlDocPtr) xmljGetNodeID (env, in);
  if (doc == NULL)
    {
      return NULL;
    }
  stylesheetURL = xmljNewString (env, doc->URL);
  xsltSetLoaderFunc (xmljDocLoader);
  stylesheet = xsltParseStylesheetDoc (doc);
  if (stylesheet == NULL)
    {
      xmljThrowException (env,
                          "javax/xml/transform/TransformerConfigurationException",
                          "Error parsing XSLT stylesheet");
    }
  xmljSetOutputProperties (env, self, stylesheet);
  ret =  xmljAsField (env, stylesheet);
  if (ret == NULL)
    {
      xmljThrowException (env,
                          "javax/xml/transform/TransformerConfigurationException",
                          "Can't create Java object for stylesheet");
    }
  return ret;
}

/* GnomeTransformer.transformStreamToStream */
JNIEXPORT void JNICALL
Java_gnu_xml_libxmlj_transform_GnomeTransformer_transformStreamToStream
(JNIEnv *env, jobject self, jobject in, jbyteArray detectBuffer,
 jstring publicId, jstring systemId, jstring base,
 jboolean entityResolver, jboolean errorHandler, jobject out)
{
  xmlDocPtr source;
  xmlDocPtr result;

  source = xmljParseDocument (env, self, in, detectBuffer, publicId, systemId,
                              base, 0, 0, 0, 0, 0, 
                              entityResolver, errorHandler, 0, 0, 2);
  result = xmljTransform (env, self, source);
  xmljSaveFileToJavaOutputStream (env, out, result,
                                  (const char*) result->encoding);
  xmlFreeDoc (result);
}

/* GnomeTransformer.transformStreamToDoc */
JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_transform_GnomeTransformer_transformStreamToDoc
(JNIEnv *env, jobject self, jobject in, jbyteArray detectBuffer,
 jstring publicId, jstring systemId, jstring base,
 jboolean entityResolver, jboolean errorHandler)
{
  xmlDocPtr source;
  xmlDocPtr result;

  source = xmljParseDocument (env, self, in, detectBuffer, publicId, systemId,
                              base, 0, 0, 0, 0, 0, 
                              entityResolver, errorHandler, 0, 0, 2);
  result = xmljTransform (env, self, source);
  return xmljGetNodeInstance (env, (xmlNodePtr) result);
}

/* GnomeTransformer.transformStreamToSAX */
JNIEXPORT void JNICALL
Java_gnu_xml_libxmlj_transform_GnomeTransformer_transformStreamToSAX
(JNIEnv *env, jobject self, jobject in, jbyteArray detectBuffer,
 jstring publicId, jstring systemId, jstring base,
 jboolean entityResolver, jboolean errorHandler, jobject callback)
{
  xmlDocPtr source;

  source = xmljParseDocument (env, self, in, detectBuffer, publicId, systemId,
                              base, 0, 0, 0, 0, 0, 
                              entityResolver, errorHandler, 0, 0, 2);
  xmljTransformToSAX (env, self, source, callback);
}

/* GnomeTransformer.transformDocToStream */
JNIEXPORT void JNICALL
Java_gnu_xml_libxmlj_transform_GnomeTransformer_transformDocToStream
(JNIEnv *env, jobject self, jobject doc, jobject out)
{
  xmlDocPtr source;
  xmlDocPtr result;

  source = (xmlDocPtr) xmljGetNodeID (env, doc);
  result = xmljTransform (env, self, source);
  xmljSaveFileToJavaOutputStream (env, out, result,
                                  (const char*) result->encoding);
  xmlFreeDoc (result);
}

/* GnomeTransformer.transformDocToDoc */
JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_transform_GnomeTransformer_transformDocToDoc
(JNIEnv *env, jobject self, jobject doc)
{
  xmlDocPtr source;
  xmlDocPtr result;

  source = (xmlDocPtr) xmljGetNodeID (env, doc);
  result = xmljTransform (env, self, source);
  return xmljGetNodeInstance (env, (xmlNodePtr) result);
}

/* GnomeTransformer.transformDocToSAX */
JNIEXPORT void JNICALL
Java_gnu_xml_libxmlj_transform_GnomeTransformer_transformDocToSAX
(JNIEnv *env, jobject self, jobject doc, jobject callback)
{
  xmlDocPtr source;

  source = (xmlDocPtr) xmljGetNodeID (env, doc);
  xmljTransformToSAX (env, self, source, callback);
}

/* GnomeTransformer.free */
JNIEXPORT void JNICALL
Java_gnu_xml_libxmlj_transform_GnomeTransformer_free (JNIEnv *env,
                                                      jobject self)
{
  xsltStylesheetPtr stylesheet;

  stylesheet = xmljGetStylesheetID (env, self);
  xsltFreeStylesheet (stylesheet);
}

/*
 * --------------------------------------------------------------------------
 * Native implementation for class
 * gnu.xml.libxmlj.transform.GnomeTransformerFactory follows.
 */

/* GnomeTransformerFactory.freeLibxsltGlobal */
JNIEXPORT void JNICALL
Java_gnu_xml_libxmlj_transform_GnomeTransformerFactory_freeLibxsltGlobal (
  JNIEnv *env __attribute__((__unused__)),
  jclass clazz __attribute__((__unused__)))
{
  xsltCleanupGlobals ();
  xmlCleanupParser ();
}

