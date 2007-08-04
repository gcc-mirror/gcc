/* xmlj_xpath.c -
   Copyright (C) 2004 Free Software Foundation, Inc.

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

#include "gnu_xml_libxmlj_dom_GnomeDocument.h"
#include "gnu_xml_libxmlj_dom_GnomeElement.h"
#include "gnu_xml_libxmlj_dom_GnomeXPathExpression.h"
#include "gnu_xml_libxmlj_dom_GnomeXPathNodeList.h"
#include "gnu_xml_libxmlj_dom_GnomeXPathResult.h"
#include "xmlj_node.h"
#include "xmlj_util.h"
#include <libxml/xpath.h>

/* Local function prototypes */

xmlXPathContextPtr
xmljCreateXPathContextPtr (xmlNodePtr node);

jobject
xmljGetXPathResult (JNIEnv *env, xmlXPathObjectPtr obj);

jobject
xmljGetXPathNodeList (JNIEnv *env, xmlXPathObjectPtr obj);

xmlXPathObjectPtr
xmljGetXPathObjectID (JNIEnv *env, jobject obj);

/**
 * Creates an XPath context for the given node.
 */
xmlXPathContextPtr
xmljCreateXPathContextPtr (xmlNodePtr node)
{
  xmlXPathContextPtr ctx;

  ctx = xmlXPathNewContext (node->doc);
  ctx->node = node;
  return ctx;
}

/**
 * Converts an xmlXPathObjectPtr to a Java XPathResult.
 */
jobject
xmljGetXPathResult (JNIEnv *env, xmlXPathObjectPtr obj)
{
  jclass cls;
  jmethodID method;
  jobject ret;
  jobject val;
  
  if (obj == NULL)
    {
      return NULL;
    }
  cls = (*env)->FindClass (env, "gnu/xml/libxmlj/dom/GnomeXPathResult");
  if (cls == NULL)
    {
      return NULL;
    }
  method = (*env)->GetMethodID (env, cls, "<init>", "(Ljava/lang/Object;)V");
  if (method == NULL)
    {
      return NULL;
    }
  val = xmljAsField (env, obj);
  ret = (*env)->NewObject (env, cls, method, val);
  
  return ret;
}

/**
 * Converts an xmlXPathObjectPtr to a Java XPathNodeList.
 */
jobject
xmljGetXPathNodeList (JNIEnv *env, xmlXPathObjectPtr obj)
{
  jclass cls;
  jmethodID method;
  jobject ret;
  jobject val;
  
  if (obj == NULL)
    {
      return NULL;
    }
  cls = (*env)->FindClass (env, "gnu/xml/libxmlj/dom/GnomeXPathNodeList");
  if (cls == NULL)
    {
      return NULL;
    }
  method = (*env)->GetMethodID (env, cls, "<init>", "(Ljava/lang/Object;)V");
  if (method == NULL)
    {
      return NULL;
    }
  val = xmljAsField (env, obj);
  ret = (*env)->NewObject (env, cls, method, val);
  
  return ret;
}

xmlXPathObjectPtr
xmljGetXPathObjectID (JNIEnv *env, jobject obj)
{
  jclass cls;
  jfieldID field;
  jobject val;
  xmlXPathObjectPtr ret;

  cls = (*env)->GetObjectClass (env, obj);
  if (cls == NULL)
    {
      return NULL;
    }
  field = (*env)->GetFieldID (env, cls, "obj", "Ljava/lang/Object;");
  if (field == NULL)
    {
      return NULL;
    }
  val = (*env)->GetObjectField (env, obj, field);
  ret = (xmlXPathObjectPtr) xmljAsPointer (env, val);

  return ret;
}

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeDocument_evaluate (JNIEnv *env,
                                                 jobject self
						 __attribute__((__unused__)),
                                                 jstring expression,
                                                 jobject contextNode,
                                                 jobject resolver,
                                                 jshort type,
                                                 jobject result)
{
  const xmlChar *str;
  xmlNodePtr node;
  xmlXPathContextPtr ctx;
  xmlXPathObjectPtr eval = NULL;
  
  str = xmljGetStringChars (env, expression);
  node = xmljGetNodeID (env, contextNode);
  if (node == NULL)
    {
      return NULL;
    }
  ctx = xmljCreateXPathContextPtr (node);
  if (ctx != NULL)
    {
      eval = xmlXPathEval (str, ctx);
      xmlXPathFreeContext (ctx);
    }
  xmlFree ((xmlChar *) str);
  return xmljGetXPathResult (env, eval);
}

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeXPathExpression_init (JNIEnv *env,
                                                    jobject self
						    __attribute__((__unused__)),
                                                    jstring expression)
{
  const xmlChar *str;
  xmlXPathCompExprPtr ptr;

  str = xmljGetStringChars (env, expression);
  ptr = xmlXPathCompile (str);
  xmlFree ((xmlChar *) str);
  return xmljAsField (env, ptr);
}

JNIEXPORT void JNICALL
Java_gnu_xml_libxmlj_dom_GnomeXPathExpression_free (JNIEnv *env,
                                                    jobject self
						    __attribute__((__unused__)),
                                                    jobject ptr)
{
  xmlXPathCompExprPtr expr;

  expr = (xmlXPathCompExprPtr) xmljAsPointer (env, ptr);
  xmlXPathFreeCompExpr (expr);
}

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeXPathExpression_doEvaluate (JNIEnv *env,
                                                          jobject self
							  __attribute__((__unused__)),
                                                          jobject ptr,
                                                          jobject contextNode,
                                                          jshort type,
                                                          jobject result)
{
  xmlXPathCompExprPtr expr;
  xmlNodePtr node;
  xmlXPathContextPtr ctx;
  xmlXPathObjectPtr eval = NULL;

  expr = (xmlXPathCompExprPtr) xmljAsPointer (env, ptr);
  node = xmljGetNodeID (env, contextNode);
  if (node == NULL)
    {
      return NULL;
    }
  ctx = xmljCreateXPathContextPtr (node);
  if (ctx != NULL)
    {
      eval = xmlXPathCompiledEval (expr, ctx);
      xmlXPathFreeContext (ctx);
    }
  return xmljGetXPathResult (env, eval);
}

JNIEXPORT void JNICALL
Java_gnu_xml_libxmlj_dom_GnomeXPathResult_free (JNIEnv *env,
                                                jobject self
						__attribute__((__unused__)),
                                                jobject obj)
{
  xmlXPathFreeObject ((xmlXPathObjectPtr) xmljAsPointer (env, obj));
}

JNIEXPORT jshort JNICALL
Java_gnu_xml_libxmlj_dom_GnomeXPathResult_getResultType (JNIEnv *env,
                                                         jobject self)
{
  xmlXPathObjectPtr obj;
  
  obj = xmljGetXPathObjectID (env, self);
  switch (obj->type)
    {
    case XPATH_UNDEFINED:
      return 0; /* ANY_TYPE */
    case XPATH_NUMBER:
      return 1; /* NUMBER_TYPE */
    case XPATH_STRING:
      return 2; /* STRING_TYPE */
    case XPATH_BOOLEAN:
      return 3; /* BOOLEAN_TYPE */
    case XPATH_NODESET:
      return 6; /* UNORDERED_NODE_SNAPSHOT_TYPE */
    case XPATH_POINT:
    case XPATH_RANGE:
    case XPATH_LOCATIONSET:
    case XPATH_USERS:
    case XPATH_XSLT_TREE:
      /* TODO */
    default:
      return -1; /* TODO */
    }
}

JNIEXPORT jdouble JNICALL
Java_gnu_xml_libxmlj_dom_GnomeXPathResult_getNumberValue (JNIEnv *env,
                                                          jobject self)
{
  xmlXPathObjectPtr obj;
  
  obj = xmljGetXPathObjectID (env, self);
  if (obj == NULL)
    {
      return 0.0;
    }
  return obj->floatval;
}

JNIEXPORT jstring JNICALL
Java_gnu_xml_libxmlj_dom_GnomeXPathResult_getStringValue (JNIEnv *env,
                                                          jobject self)
{
  xmlXPathObjectPtr obj;
  
  obj = xmljGetXPathObjectID (env, self);
  if (obj == NULL)
    {
      return NULL;
    }
  return xmljNewString (env, obj->stringval);
}

JNIEXPORT jboolean JNICALL
Java_gnu_xml_libxmlj_dom_GnomeXPathResult_getBooleanValue (JNIEnv *env,
                                                           jobject self)
{
  xmlXPathObjectPtr obj;
  
  obj = xmljGetXPathObjectID (env, self);
  return obj->boolval;
}

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeXPathResult_getSingleNodeValue (JNIEnv *env,
                                                              jobject self)
{
  xmlXPathObjectPtr obj;
  
  obj = xmljGetXPathObjectID (env, self);
  if (obj == NULL)
    {
      return NULL;
    }
  if (obj->nodesetval == NULL)
    {
      return NULL;
    }
  if (obj->nodesetval->nodeNr > 0)
    {
      return xmljGetNodeInstance (env, obj->nodesetval->nodeTab[0]);
    }
  else
    {
      return NULL;
    }
}

JNIEXPORT jboolean JNICALL
Java_gnu_xml_libxmlj_dom_GnomeXPathResult_getInvalidIteratorState (JNIEnv *env,
                                                                   jobject self)
{
  xmlXPathObjectPtr obj;
  
  obj = xmljGetXPathObjectID (env, self);
  return 0; /* TODO */
}

JNIEXPORT jint JNICALL
Java_gnu_xml_libxmlj_dom_GnomeXPathResult_getSnapshotLength (JNIEnv *env,
                                                             jobject self)
{
  xmlXPathObjectPtr obj;
  
  obj = xmljGetXPathObjectID (env, self);
  if (obj == NULL)
    {
      return -1;
    }
  if (obj->nodesetval == NULL)
    {
      return -1;
    }
  return obj->nodesetval->nodeNr;
}

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeXPathResult_iterateNext (JNIEnv *env,
                                                       jobject self)
{
  xmlXPathObjectPtr obj;
  
  obj = xmljGetXPathObjectID (env, self);
  return NULL; /* TODO */
}

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeXPathResult_snapshotItem (JNIEnv *env,
                                                        jobject self,
                                                        jint index)
{
  xmlXPathObjectPtr obj;
  
  obj = xmljGetXPathObjectID (env, self);
  if (obj == NULL)
    {
      return NULL;
    }
  if (obj->nodesetval == NULL)
    {
      return NULL;
    }
  if (obj->nodesetval->nodeNr > 0)
    {
      return xmljGetNodeInstance (env, obj->nodesetval->nodeTab[index]);
    }
  else
    {
      return NULL;
    }
}

/* -- GnomeXPathNodeList -- */

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeDocument_getElementsByTagName (JNIEnv *env,
                                                             jobject self,
                                                             jstring name)
{
  return Java_gnu_xml_libxmlj_dom_GnomeElement_getElementsByTagName (env,
                                                                     self,
                                                                     name);
}

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeElement_getElementsByTagName (JNIEnv *env,
                                                             jobject self,
                                                             jstring name)
{
  const xmlChar *s_name;
  const xmlChar *format;
  xmlChar expr[256];
  xmlNodePtr node;
  xmlXPathContextPtr ctx;
  xmlXPathObjectPtr eval = NULL;
  
  node = xmljGetNodeID (env, self);
  if (node == NULL)
    {
      return NULL;
    }
  s_name = xmljGetStringChars (env, name);
  if (xmlStrEqual (s_name, BAD_CAST "*"))
    {
      format = xmlCharStrdup ("descendant-or-self::*[node-type()=1]");
      if (xmlStrPrintf (expr, 256, format) == -1)
        {
          return NULL;
        }
    }
  else
    {
      format = xmlCharStrdup ("descendant-or-self::*[name()='%s']");
      if (xmlStrPrintf (expr, 256, format, s_name) == -1)
        {
          return NULL;
        }
    }
  xmlFree ((xmlChar *) s_name);
  ctx = xmljCreateXPathContextPtr (node);
  if (ctx != NULL)
    {
      eval = xmlXPathEval (expr, ctx);
      xmlXPathFreeContext (ctx);
    }
  return xmljGetXPathNodeList (env, eval);
}

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeDocument_getElementsByTagNameNS (JNIEnv *env,
                                                               jobject self,
                                                               jstring uri,
                                                               jstring localName)
{
  return Java_gnu_xml_libxmlj_dom_GnomeElement_getElementsByTagNameNS (env,
                                                                       self,
                                                                       uri,
                                                                       localName);
}

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeElement_getElementsByTagNameNS (JNIEnv *env,
                                                               jobject self,
                                                               jstring uri,
                                                               jstring localName)
{
  const xmlChar *s_uri;
  const xmlChar *s_localName;
  const xmlChar *format;
  xmlChar expr[256];
  xmlNodePtr node;
  xmlXPathContextPtr ctx;
  xmlXPathObjectPtr eval = NULL;
  
  node = xmljGetNodeID (env, self);
  if (node == NULL)
    {
      return NULL;
    }
  s_uri = xmljGetStringChars (env, uri);
  s_localName = xmljGetStringChars (env, localName);
  if (uri == NULL)
    {
      /* namespace URI is empty */
      if (xmlStrEqual (s_localName, BAD_CAST "*"))
        {
          format = xmlCharStrdup ("descendant-or-self::*[namespace-uri()='' and node-type()=1]");
          if (xmlStrPrintf (expr, 256, format) == -1)
            {
              return NULL;
            }
        }
      else
        {
          format = xmlCharStrdup ("descendant-or-self::*[namespace-uri()='' and local-name()='%s']");
          if (xmlStrPrintf (expr, 256, format, s_localName) == -1)
            {
              return NULL;
            }
        }
    }
  else if (xmlStrEqual (s_uri, BAD_CAST "*"))
    {
      /* matches all namespaces */
      if (xmlStrEqual (s_localName, BAD_CAST "*"))
        {
          format = xmlCharStrdup ("descendant-or-self::*[node-type()=1]");
          if (xmlStrPrintf (expr, 256, format) == -1)
            {
              return NULL;
            }
        }
      else
        {
          format = xmlCharStrdup ("descendant-or-self::*[local-name()='%s']");
          if (xmlStrPrintf (expr, 256, format, s_localName) == -1)
            {
              return NULL;
            }
        }
    }
  else
    {
      if (xmlStrEqual (s_localName, BAD_CAST "*"))
        {
          format = xmlCharStrdup ("descendant-or-self::*[namespace-uri()='%s' and node-type()=1]");
          if (xmlStrPrintf (expr, 256, format, s_uri) == -1)
            {
              return NULL;
            }
        }
      else
        {
          format = xmlCharStrdup ("descendant-or-self::*[namespace-uri()='%s' and local-name()='%s']");
          if (xmlStrPrintf (expr, 256, format, s_uri, s_localName) == -1)
            {
              return NULL;
            }
        }
    }
  xmlFree ((xmlChar *) s_uri);
  xmlFree ((xmlChar *) s_localName);
  ctx = xmljCreateXPathContextPtr (node);
  if (ctx != NULL)
    {
      eval = xmlXPathEval (expr, ctx);
      xmlXPathFreeContext (ctx);
    }
  return xmljGetXPathNodeList (env, eval);
}

JNIEXPORT void JNICALL
Java_gnu_xml_libxmlj_dom_GnomeXPathNodeList_free (JNIEnv *env,
                                                  jobject self
						  __attribute__((__unused__)),
                                                  jobject obj)
{
  xmlXPathFreeObject ((xmlXPathObjectPtr) xmljAsPointer (env, obj));
}

JNIEXPORT jint JNICALL
Java_gnu_xml_libxmlj_dom_GnomeXPathNodeList_getLength (JNIEnv *env,
                                                       jobject self)
{
  xmlXPathObjectPtr obj;
  
  obj = xmljGetXPathObjectID (env, self);
  if (obj == NULL)
    {
      return 0;
    }
  if (obj->nodesetval == NULL)
    {
      return 0;
    }
  return obj->nodesetval->nodeNr;
}

JNIEXPORT jobject JNICALL
Java_gnu_xml_libxmlj_dom_GnomeXPathNodeList_item (JNIEnv *env,
                                                  jobject self,
                                                  jint index)
{
  xmlXPathObjectPtr obj;
  
  obj = xmljGetXPathObjectID (env, self);
  if (obj == NULL)
    {
      return NULL;
    }
  if (obj->nodesetval == NULL)
    {
      return NULL;
    }
  if (obj->nodesetval->nodeNr > 0)
    {
      return xmljGetNodeInstance (env, obj->nodesetval->nodeTab[index]);
    }
  else
    {
      return NULL;
    }
}

