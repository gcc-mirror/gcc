/* xmlj_sax.c -
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

#include "xmlj_sax.h"
#include "xmlj_io.h"
#include "xmlj_util.h"
#include <unistd.h>
#include <string.h>

xmlExternalEntityLoader defaultLoader = NULL;

void
xmljDispatchError (xmlParserCtxtPtr ctx,
                   xmlSAXLocatorPtr loc,
                   JNIEnv *env,
                   jobject target,
                   jmethodID method,
                   const char *msg,
                   va_list args);

/* -- GnomeLocator -- */

JNIEXPORT jstring JNICALL
Java_gnu_xml_libxmlj_sax_GnomeLocator_publicId (JNIEnv * env,
                                                jobject self
						__attribute__((__unused__)),
                                                jobject j_ctx,
                                                jobject j_loc)
{
  xmlParserCtxtPtr ctx;
  xmlSAXLocatorPtr loc;
  SAXParseContext *sax;

  ctx = (xmlParserCtxtPtr) xmljAsPointer (env, j_ctx);
  loc = (xmlSAXLocatorPtr) xmljAsPointer (env, j_loc);
  sax = (SAXParseContext *) ctx->_private;
  
  return sax->publicId;
}

JNIEXPORT jstring JNICALL
Java_gnu_xml_libxmlj_sax_GnomeLocator_systemId (JNIEnv * env,
                                                jobject self
						__attribute__((__unused__)),
                                                jobject j_ctx,
                                                jobject j_loc)
{
  xmlParserCtxtPtr ctx;
  xmlSAXLocatorPtr loc;
  SAXParseContext *sax;

  ctx = (xmlParserCtxtPtr) xmljAsPointer (env, j_ctx);
  loc = (xmlSAXLocatorPtr) xmljAsPointer (env, j_loc);
  sax = (SAXParseContext *) ctx->_private;
  
  return sax->systemId;
}

JNIEXPORT jint JNICALL
Java_gnu_xml_libxmlj_sax_GnomeLocator_lineNumber (JNIEnv * env,
                                                  jobject self
						  __attribute__((__unused__)),
                                                  jobject j_ctx,
                                                  jobject j_loc)
{
  xmlParserCtxtPtr ctx;
  xmlSAXLocatorPtr loc;

  ctx = (xmlParserCtxtPtr) xmljAsPointer (env, j_ctx);
  loc = (xmlSAXLocatorPtr) xmljAsPointer (env, j_loc);
  if (ctx == NULL || ctx->input == NULL)
    {
      return -1;
    }
  return ctx->input->line;
}

JNIEXPORT jint JNICALL
Java_gnu_xml_libxmlj_sax_GnomeLocator_columnNumber (JNIEnv * env,
                                                    jobject self
						    __attribute__((__unused__)),
                                                    jobject j_ctx,
                                                    jobject j_loc)
{
  xmlParserCtxtPtr ctx;
  xmlSAXLocatorPtr loc;

  ctx = (xmlParserCtxtPtr) xmljAsPointer (env, j_ctx);
  loc = (xmlSAXLocatorPtr) xmljAsPointer (env, j_loc);
  if (ctx == NULL || ctx->input == NULL)
    {
      return -1;
    }
  return ctx->input->col;
}

/* -- GnomeXMLReader -- */

/*
 * Entry point for SAX parsing.
 */
JNIEXPORT void JNICALL
Java_gnu_xml_libxmlj_sax_GnomeXMLReader_parseStream (JNIEnv * env,
                                                     jobject self,
                                                     jobject in,
                                                     jbyteArray detectBuffer,
                                                     jstring publicId,
                                                     jstring systemId,
                                                     jstring base,
                                                     jboolean validate,
                                                     jboolean contentHandler,
                                                     jboolean dtdHandler,
                                                     jboolean entityResolver,
                                                     jboolean errorHandler,
                                                     jboolean
                                                     declarationHandler,
                                                     jboolean lexicalHandler)
{
  xmljParseDocument (env,
                     self,
                     in,
                     detectBuffer,
                     publicId,
                     systemId,
                     base,
                     validate,
                     0,
                     0,
                     contentHandler,
                     dtdHandler,
                     entityResolver,
                     errorHandler,
                     declarationHandler,
                     lexicalHandler,
                     0);
}

xmlParserInputPtr
xmljExternalEntityLoader (const char *url, const char *id,
                          xmlParserCtxtPtr ctx)
{
  const xmlChar *systemId;
  const xmlChar *publicId;
  xmlParserInputPtr ret;

  systemId = xmlCharStrdup (url);
  publicId = xmlCharStrdup (id);
  /* TODO convert systemId to absolute URI */
  ret = xmljSAXResolveEntity (ctx, publicId, systemId);
  if (ret == NULL)
    {
      ret = defaultLoader (url, id, ctx);
    }
  return ret;
}

/*
 * Allocates and configures a SAX handler that can report the various
 * classes of callback.
 */
xmlSAXHandlerPtr
xmljNewSAXHandler (jboolean contentHandler,
                   jboolean dtdHandler,
                   jboolean entityResolver,
                   jboolean errorHandler,
                   jboolean declarationHandler,
                   jboolean lexicalHandler)
{
  xmlSAXHandlerPtr sax;

  sax = (xmlSAXHandlerPtr) malloc (sizeof (xmlSAXHandler));
  if (sax == NULL)
    {
      return NULL;
    }
  memset (sax, 0, sizeof (xmlSAXHandler));
  xmlSAXVersion (sax, 1); /* TODO SAX2 */

  if (dtdHandler)
    {
      sax->internalSubset = &xmljSAXInternalSubset;
    }
  if (defaultLoader == NULL)
    {
      defaultLoader = xmlGetExternalEntityLoader ();
      xmlSetExternalEntityLoader (xmljExternalEntityLoader);
    }
  if (entityResolver)
    {
      sax->resolveEntity = &xmljSAXResolveEntity;
    }

  if (declarationHandler)
    {
      sax->entityDecl = &xmljSAXEntityDecl;
      sax->notationDecl = &xmljSAXNotationDecl;
      sax->attributeDecl = &xmljSAXAttributeDecl;
      sax->elementDecl = &xmljSAXElementDecl;
      sax->unparsedEntityDecl = &xmljSAXUnparsedEntityDecl;
    }

  /* We always listen for the locator callback */
  sax->setDocumentLocator = &xmljSAXSetDocumentLocator;
  if (contentHandler)
    {
      sax->startDocument = &xmljSAXStartDocument;
      sax->endDocument = &xmljSAXEndDocument;
      sax->startElement = &xmljSAXStartElement;
      sax->endElement = &xmljSAXEndElement;
      sax->characters = &xmljSAXCharacters;
      sax->ignorableWhitespace = &xmljSAXIgnorableWhitespace;
      sax->processingInstruction = &xmljSAXProcessingInstruction;
    }

  /* We always intercept getEntity */
  /* TODO this should only be if lexicalHandler */
  sax->getEntity = &xmljSAXGetEntity;
  if (lexicalHandler)
    {
      sax->getEntity = &xmljSAXGetEntity;
      sax->reference = &xmljSAXReference;
      sax->comment = &xmljSAXComment;
      sax->cdataBlock = &xmljSAXCDataBlock;
    }
  else if (contentHandler)
    {
      sax->cdataBlock = &xmljSAXCharacters;
    }

  if (errorHandler)
    {
      sax->warning = &xmljSAXWarning;
      sax->error = &xmljSAXError;
      sax->fatalError = &xmljSAXFatalError;
    }
  
  return sax;
}

/* -- Callback functions -- */

void
xmljSAXInternalSubset (void *vctx,
                       const xmlChar * name,
                       const xmlChar * publicId, const xmlChar * systemId)
{
  xmlParserCtxtPtr ctx;
  SAXParseContext *sax;
  JNIEnv *env;
  jobject target;
  jstring j_name;
  jstring j_publicId;
  jstring j_systemId;

  xmlSAX2InternalSubset (vctx, name, publicId, systemId);

  ctx = (xmlParserCtxtPtr) vctx;
  sax = (SAXParseContext *) ctx->_private;
  env = sax->env;
  target = sax->obj;

  xmljCheckWellFormed (ctx);
  if ((*env)->ExceptionOccurred (env))
    {
      return;
    }

  if (sax->startDTD == NULL)
    {
      sax->startDTD =
        xmljGetMethodID (env,
                         target,
                         "startDTD",
                         "(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)V");
      if (sax->startDTD == NULL)
        {
          return;
        }
    }

  j_name = xmljNewString (env, name);
  j_publicId = xmljNewString (env, publicId);
  j_systemId = xmljNewString (env, systemId);

  (*env)->CallVoidMethod (env,
                          target,
                          sax->startDTD,
                          j_name,
                          j_publicId,
                          j_systemId);
}

xmlParserInputPtr
xmljSAXResolveEntity (void *vctx,
                      const xmlChar * publicId, const xmlChar * systemId)
{
  xmlParserCtxtPtr ctx;
  SAXParseContext *sax;
  JNIEnv *env;
  jobject target;
  jstring j_publicId;
  jstring j_systemId;
  jobject inputStream;

  /* xmlSAX2ResolveEntity (vctx, publicId, systemId); */

  ctx = (xmlParserCtxtPtr) vctx;
  if (ctx->_private == NULL)
    {
      /* Not in Kansas */
      return NULL;
    }
  sax = (SAXParseContext *) ctx->_private;
  env = sax->env;
  target = sax->obj;

  if ((*env)->ExceptionOccurred (env))
    {
      return NULL;
    }

  if (sax->resolveEntity == NULL)
    {
      sax->resolveEntity =
        xmljGetMethodID (env,
                         target,
                         "resolveEntity",
                         "(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)Ljava/io/InputStream;");
      if (sax->resolveEntity == NULL)
        {
          return NULL;
        }
    }

  j_publicId = xmljNewString (env, publicId);
  j_systemId = xmljNewString (env, systemId);

  inputStream = (*env)->CallObjectMethod (env,
                                          target,
                                          sax->resolveEntity,
                                          j_publicId,
                                          j_systemId,
                                          sax->systemId);

  /* Return an xmlParserInputPtr corresponding to the input stream */
  if (inputStream != NULL)
    {
      jbyteArray detectBuffer;
      jmethodID getDetectBuffer;

      /* Get the detect buffer from the NamedInputStream */
      getDetectBuffer = xmljGetMethodID (env, inputStream, "getDetectBuffer",
                                         "()[B");
      if (getDetectBuffer == NULL)
        {
          return NULL;
        }
      detectBuffer = (*env)->CallObjectMethod (env, inputStream,
                                               getDetectBuffer);
      
      return xmljNewParserInput (env, inputStream, detectBuffer, ctx);
    }
  else
    {
      return NULL;
    }
}

xmlEntityPtr
xmljSAXGetEntity (void *vctx __attribute__((__unused__)), const xmlChar * name)
{
  xmlEntityPtr ret;
  
  /* TODO */
  /* ret = xmlSAX2GetEntity (vctx, name); */
  ret = NULL;
  return ret;
}

void
xmljSAXEntityDecl (void *vctx,
                   const xmlChar * name,
                   int type,
                   const xmlChar * publicId,
                   const xmlChar * systemId,
                   xmlChar * content)
{
  xmlParserCtxtPtr ctx;
  SAXParseContext *sax;
  JNIEnv *env;
  jobject target;
  jstring j_name;
  jstring j_publicId;
  jstring j_systemId;
  jstring j_value;

  xmlSAX2EntityDecl (vctx, name, type, publicId, systemId, content);

  ctx = (xmlParserCtxtPtr) vctx;
  sax = (SAXParseContext *) ctx->_private;
  env = sax->env;
  target = sax->obj;

  xmljCheckWellFormed (ctx);
  if ((*env)->ExceptionOccurred (env))
    {
      return;
    }

  j_name = xmljNewString (env, name);
  switch (type)
    {
    case XML_INTERNAL_GENERAL_ENTITY:
    case XML_INTERNAL_PARAMETER_ENTITY:
    case XML_INTERNAL_PREDEFINED_ENTITY:
      if (sax->internalEntityDecl == NULL)
        {
          sax->internalEntityDecl =
            xmljGetMethodID (env,
                             target,
                             "internalEntityDecl",
                             "(Ljava/lang/String;Ljava/lang/String;)V");
          if (sax->internalEntityDecl == NULL)
            {
              return;
            }
        }
      j_value = xmljNewString (env, content);
      (*env)->CallVoidMethod (env,
                              target,
                              sax->internalEntityDecl,
                              j_name,
                              j_value);
      break;
    default:
      if (sax->externalEntityDecl == NULL)
        {
          sax->externalEntityDecl =
            xmljGetMethodID (env,
                             target,
                             "externalEntityDecl",
                             "(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)V");
          if (sax->externalEntityDecl == NULL)
            {
              return;
            }
        }
      j_publicId = xmljNewString (env, publicId);
      j_systemId = xmljNewString (env, systemId);
      (*env)->CallVoidMethod (env,
                              target,
                              sax->externalEntityDecl,
                              j_name,
                              j_publicId,
                              j_systemId);
    }
}

void
xmljSAXNotationDecl (void *vctx,
                     const xmlChar * name,
                     const xmlChar * publicId,
                     const xmlChar * systemId)
{
  xmlParserCtxtPtr ctx;
  SAXParseContext *sax;
  JNIEnv *env;
  jobject target;
  jstring j_name;
  jstring j_publicId;
  jstring j_systemId;

  xmlSAX2NotationDecl (vctx, name, publicId, systemId);

  ctx = (xmlParserCtxtPtr) vctx;
  sax = (SAXParseContext *) ctx->_private;
  env = sax->env;
  target = sax->obj;

  xmljCheckWellFormed (ctx);
  if ((*env)->ExceptionOccurred (env))
    {
      return;
    }

  if (sax->notationDecl == NULL)
    {
      sax->notationDecl =
        xmljGetMethodID (env,
                         target,
                         "notationDecl",
                         "(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)V");
      if (sax->notationDecl == NULL)
        {
          return;
        }
    }

  j_name = xmljNewString (env, name);
  j_publicId = xmljNewString (env, publicId);
  j_systemId = xmljNewString (env, systemId);

  /* Invoke the method */
  (*env)->CallVoidMethod (env,
                          target,
                          sax->notationDecl,
                          j_name,
                          j_publicId,
                          j_systemId);
}

void
xmljSAXAttributeDecl (void *vctx,
                      const xmlChar * elem,
                      const xmlChar * fullName,
                      int type,
                      int def,
                      const xmlChar * defaultValue,
                      xmlEnumerationPtr tree)
{
  xmlParserCtxtPtr ctx;
  SAXParseContext *sax;
  JNIEnv *env;
  jobject target;
  jstring j_eName;
  jstring j_aName;
  jstring j_type;
  jstring j_mode;
  jstring j_value;

  xmlSAX2AttributeDecl (vctx, elem, fullName, type, def, defaultValue, tree);

  ctx = (xmlParserCtxtPtr) vctx;
  sax = (SAXParseContext *) ctx->_private;
  env = sax->env;
  target = sax->obj;

  xmljCheckWellFormed (ctx);
  if ((*env)->ExceptionOccurred (env))
    {
      return;
    }

  if (sax->attributeDecl == NULL)
    {
      sax->attributeDecl =
        xmljGetMethodID (env,
                         target,
                         "attributeDecl",
                         "(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)V");
      if (sax->attributeDecl == NULL)
        {
          return;
        }
    }

  j_eName = xmljNewString (env, elem);
  j_aName = xmljNewString (env, fullName);
  j_type = xmljAttributeTypeName (env, type);
  j_mode = xmljAttributeModeName (env, def);
  j_value = xmljNewString (env, defaultValue);

  (*env)->CallVoidMethod (env,
                          target,
                          sax->attributeDecl,
                          j_eName,
                          j_aName,
                          j_type,
                          j_mode,
                          j_value);
}

void
xmljSAXElementDecl (void *vctx,
                    const xmlChar * name,
                    int type,
                    xmlElementContentPtr content)
{
  xmlParserCtxtPtr ctx;
  SAXParseContext *sax;
  JNIEnv *env;
  jobject target;
  jstring j_name;
  jstring j_model;

  xmlSAX2ElementDecl (vctx, name, type, content);

  ctx = (xmlParserCtxtPtr) vctx;
  sax = (SAXParseContext *) ctx->_private;
  env = sax->env;
  target = sax->obj;

  xmljCheckWellFormed (ctx);
  if ((*env)->ExceptionOccurred (env))
    {
      return;
    }

  if (sax->elementDecl == NULL)
    {
      sax->elementDecl =
        xmljGetMethodID (env,
                         target,
                         "elementDecl",
                         "(Ljava/lang/String;Ljava/lang/String;)V");
      if (sax->elementDecl == NULL)
        {
          return;
        }
    }

  j_name = xmljNewString (env, name);
  j_model = NULL;		/* TODO */

  (*env)->CallVoidMethod (env,
                          target,
                          sax->elementDecl,
                          j_name,
                          j_model);
}

void
xmljSAXUnparsedEntityDecl (void *vctx,
                           const xmlChar * name,
                           const xmlChar * publicId,
                           const xmlChar * systemId,
                           const xmlChar * notationName)
{
  xmlParserCtxtPtr ctx;
  SAXParseContext *sax;
  JNIEnv *env;
  jobject target;
  jstring j_name;
  jstring j_publicId;
  jstring j_systemId;
  jstring j_notationName;

  xmlSAX2UnparsedEntityDecl (vctx, name, publicId, systemId, notationName);
  
  ctx = (xmlParserCtxtPtr) vctx;
  sax = (SAXParseContext *) ctx->_private;
  env = sax->env;
  target = sax->obj;

  xmljCheckWellFormed (ctx);
  if ((*env)->ExceptionOccurred (env))
    {
      return;
    }

  if (sax->unparsedEntityDecl == NULL)
    {
      sax->unparsedEntityDecl =
        xmljGetMethodID (env,
                         target,
                         "unparsedEntityDecl",
                         "(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)V");
      if (sax->unparsedEntityDecl == NULL)
        {
          return;
        }
    }

  j_name = xmljNewString (env, name);
  j_publicId = xmljNewString (env, publicId);
  j_systemId = xmljNewString (env, systemId);
  j_notationName = xmljNewString (env, notationName);

  (*env)->CallVoidMethod (env,
                          target,
                          sax->unparsedEntityDecl,
                          j_name,
                          j_publicId,
                          j_systemId,
                          j_notationName);
}

void
xmljSAXSetDocumentLocator (void *vctx, xmlSAXLocatorPtr loc)
{
  xmlParserCtxtPtr ctx;
  SAXParseContext *sax;
  JNIEnv *env;
  jobject target;

  xmlSAX2SetDocumentLocator (vctx, loc);

  ctx = (xmlParserCtxtPtr) vctx;
  sax = (SAXParseContext *) ctx->_private;
  env = sax->env;
  target = sax->obj;

  if (target == NULL)
    {
      /* No Java parse context */
      return;
    }

  /* Update locator on sax context */
  sax->loc = loc;
  if ((*env)->ExceptionOccurred (env))
    {
      return;
    }

  if (sax->setDocumentLocator == NULL)
    {
      sax->setDocumentLocator = xmljGetMethodID (env,
                                                 target,
                                                 "setDocumentLocator",
                                                 "(Ljava/lang/Object;Ljava/lang/Object;)V");
      if (sax->setDocumentLocator == NULL)
        {
          return;
        }
    }

  (*env)->CallVoidMethod (env,
                          target,
                          sax->setDocumentLocator,
                          xmljAsField (env, ctx),
                          xmljAsField (env, loc));
}

void
xmljSAXStartDocument (void *vctx)
{
  xmlParserCtxtPtr ctx;
  SAXParseContext *sax;
  JNIEnv *env;
  jobject target;

  xmlSAX2StartDocument (vctx);

  ctx = (xmlParserCtxtPtr) vctx;
  sax = (SAXParseContext *) ctx->_private;
  env = sax->env;
  target = sax->obj;

  xmljCheckWellFormed (ctx);
  if ((*env)->ExceptionOccurred (env))
    {
      return;
    }

  if (sax->startDocument == NULL)
    {
      sax->startDocument = xmljGetMethodID (env,
                                            target,
                                            "startDocument",
                                            "(Z)V");
      if (sax->startDocument == NULL)
        {
          return;
        }
    }

  (*env)->CallVoidMethod (env,
                          target,
                          sax->startDocument,
                          ctx->standalone);
}

void
xmljSAXEndDocument (void *vctx)
{
  xmlParserCtxtPtr ctx;
  SAXParseContext *sax;
  JNIEnv *env;
  jobject target;

  xmlSAX2EndDocument (vctx);

  ctx = (xmlParserCtxtPtr) vctx;
  sax = (SAXParseContext *) ctx->_private;
  env = sax->env;
  target = sax->obj;

  xmljCheckWellFormed (ctx);
  if ((*env)->ExceptionOccurred (env))
    {
      return;
    }

  if (sax->endDocument == NULL)
    {
      sax->endDocument = xmljGetMethodID (env,
                                          target,
                                          "endDocument",
                                          "()V");
      if (sax->endDocument == NULL)
        {
          return;
        }
    }

  (*env)->CallVoidMethod (env,
                          target,
                          sax->endDocument);
}

void
xmljSAXStartElement (void *vctx,
                     const xmlChar * name,
                     const xmlChar ** attrs)
{
  xmlParserCtxtPtr ctx;
  SAXParseContext *sax;
  JNIEnv *env;
  jobject target;
  jstring j_name;
  jobjectArray j_attrs;
  jstring j_attr;
  jsize len;

  xmlSAX2StartElement (vctx, name, attrs);

  ctx = (xmlParserCtxtPtr) vctx;
  sax = (SAXParseContext *) ctx->_private;
  env = sax->env;
  target = sax->obj;

  xmljCheckWellFormed (ctx);
  if ((*env)->ExceptionOccurred (env))
    {
      return;
    }

  if (sax->startElement == NULL)
    {
      sax->startElement =
        xmljGetMethodID (env,
                         target,
                         "startElement",
                         "(Ljava/lang/String;[Ljava/lang/String;)V");
      if (sax->startElement == NULL)
        {
          return;
        }
    }

  j_name = xmljNewString (env, name);
  /* build attributes array */
  len = 0;
  for (len = 0; attrs && attrs[len]; len++)
    {
    }
  if (len)
    {
      if (sax->stringClass == NULL)
        {
          sax->stringClass = (*env)->FindClass (env, "java/lang/String");
          if (sax->stringClass == NULL)
            {
              fprintf (stderr, "Can't find java.lang.String class!\n");
              return;
            }
        }
      j_attrs = (*env)->NewObjectArray (env, len, sax->stringClass, NULL);
      if (j_attrs == NULL)
        {
          fprintf (stderr, "Can't allocate attributes array!\n");
          return;
        }
      len = 0;
      for (len = 0; attrs && attrs[len]; len++)
        {
          j_attr = xmljNewString (env, attrs[len]);
          (*env)->SetObjectArrayElement (env, j_attrs, len, j_attr);
        }
      
      (*env)->CallVoidMethod (env,
                              target,
                              sax->startElement,
                              j_name,
                              j_attrs);
      (*env)->DeleteLocalRef (env, j_attrs);
    }
  else
    {
      (*env)->CallVoidMethod (env,
                              target,
                              sax->startElement,
                              j_name,
                              NULL);
      
    }
}

void
xmljSAXEndElement (void *vctx,
                   const xmlChar * name)
{
  xmlParserCtxtPtr ctx;
  SAXParseContext *sax;
  JNIEnv *env;
  jobject target;
  jstring j_name;

  xmlSAX2EndElement (vctx, name);

  ctx = (xmlParserCtxtPtr) vctx;
  sax = (SAXParseContext *) ctx->_private;
  env = sax->env;
  target = sax->obj;

  xmljCheckWellFormed (ctx);
  if ((*env)->ExceptionOccurred (env))
    {
      return;
    }

  if (sax->endElement == NULL)
    {
      sax->endElement = xmljGetMethodID (env,
                                         target,
                                         "endElement",
                                         "(Ljava/lang/String;)V");
      if (sax->endElement == NULL)
        {
          return;
        }
    }

  j_name = xmljNewString (env, name);

  (*env)->CallVoidMethod (env,
                          target,
                          sax->endElement,
                          j_name);
}

void
xmljSAXReference (void *vctx,
                  const xmlChar * name)
{
  xmlSAX2Reference (vctx, name);
}

void
xmljSAXCharacters (void *vctx,
                   const xmlChar * ch,
                   int len)
{
  xmlParserCtxtPtr ctx;
  SAXParseContext *sax;
  JNIEnv *env;
  jobject target;
  jstring j_ch;
  xmlChar *dup;

  xmlSAX2Characters (vctx, ch, len);

  ctx = (xmlParserCtxtPtr) vctx;
  sax = (SAXParseContext *) ctx->_private;
  env = sax->env;
  target = sax->obj;

  xmljCheckWellFormed (ctx);
  if ((*env)->ExceptionOccurred (env))
    {
      return;
    }

  if (sax->characters == NULL)
    {
      sax->characters = xmljGetMethodID (env,
                                         target,
                                         "characters",
                                         "(Ljava/lang/String;)V");
      if (sax->characters == NULL)
        {
          return;
        }
    }

  dup = xmlStrndup (ch, len);
  j_ch = xmljNewString (env, dup);

  (*env)->CallVoidMethod (env,
                          target,
                          sax->characters,
                          j_ch);
  xmlFree (dup);
}

void
xmljSAXIgnorableWhitespace (void *vctx,
                            const xmlChar * ch,
                            int len)
{
  xmlParserCtxtPtr ctx;
  SAXParseContext *sax;
  JNIEnv *env;
  jobject target;
  jstring j_ch;
  xmlChar *dup;

  xmlSAX2IgnorableWhitespace (vctx, ch, len);

  ctx = (xmlParserCtxtPtr) vctx;
  sax = (SAXParseContext *) ctx->_private;
  env = sax->env;
  target = sax->obj;

  xmljCheckWellFormed (ctx);
  if ((*env)->ExceptionOccurred (env))
    {
      return;
    }

  if (sax->ignorableWhitespace == NULL)
    {
      sax->ignorableWhitespace = xmljGetMethodID (env,
                                                  target,
                                                  "ignorableWhitespace",
                                                  "(Ljava/lang/String;)V");
      if (sax->ignorableWhitespace == NULL)
        {
          return;
        }
    }

  dup = xmlStrndup (ch, len);
  j_ch = xmljNewString (env, dup);

  (*env)->CallVoidMethod (env,
                          target,
                          sax->ignorableWhitespace,
                          j_ch);
  xmlFree (dup);
}

void
xmljSAXProcessingInstruction (void *vctx,
                              const xmlChar * targ,
                              const xmlChar * data)
{
  xmlParserCtxtPtr ctx;
  SAXParseContext *sax;
  JNIEnv *env;
  jobject target;
  jstring j_targ;
  jstring j_data;

  xmlSAX2ProcessingInstruction (vctx, targ, data);

  ctx = (xmlParserCtxtPtr) vctx;
  sax = (SAXParseContext *) ctx->_private;
  env = sax->env;
  target = sax->obj;

  xmljCheckWellFormed (ctx);
  if ((*env)->ExceptionOccurred (env))
    {
      return;
    }

  if (sax->processingInstruction == NULL)
    {
      sax->processingInstruction =
        xmljGetMethodID (env,
                         target,
                         "processingInstruction",
                         "(Ljava/lang/String;Ljava/lang/String;)V");
      if (sax->processingInstruction == NULL)
        {
          return;
        }
    }

  j_targ = xmljNewString (env, targ);
  j_data = xmljNewString (env, data);

  (*env)->CallVoidMethod (env,
                          target,
                          sax->processingInstruction,
                          j_targ,
                          j_data);
}

void
xmljSAXComment (void *vctx,
                const xmlChar * value)
{
  xmlParserCtxtPtr ctx;
  SAXParseContext *sax;
  JNIEnv *env;
  jobject target;
  jstring j_text;

  xmlSAX2Comment (vctx, value);

  ctx = (xmlParserCtxtPtr) vctx;
  sax = (SAXParseContext *) ctx->_private;
  env = sax->env;
  target = sax->obj;

  xmljCheckWellFormed (ctx);
  if ((*env)->ExceptionOccurred (env))
    {
      return;
    }

  if (sax->comment == NULL)
    {
      sax->comment =
        xmljGetMethodID (env,
                         target,
                         "comment",
                         "(Ljava/lang/String;)V");
      if (sax->comment == NULL)
        {
          return;
        }
    }

  j_text = xmljNewString (env, value);

  (*env)->CallVoidMethod (env,
                          target,
                          sax->comment,
                          j_text);
}

void
xmljSAXCDataBlock (void *vctx,
                   const xmlChar * ch,
                   int len)
{
  xmlParserCtxtPtr ctx;
  SAXParseContext *sax;
  JNIEnv *env;
  jobject target;
  jstring j_ch;
  xmlChar *dup;

  xmlSAX2CDataBlock (vctx, ch, len);

  ctx = (xmlParserCtxtPtr) vctx;
  sax = (SAXParseContext *) ctx->_private;
  env = sax->env;
  target = sax->obj;

  xmljCheckWellFormed (ctx);
  if ((*env)->ExceptionOccurred (env))
    {
      return;
    }

  if (sax->cdataBlock == NULL)
    {
      sax->cdataBlock =
        xmljGetMethodID (env,
                         target,
                         "cdataBlock",
                         "(Ljava/lang/String;)V");
      if (sax->cdataBlock == NULL)
        {
          return;
        }
    }

  dup = xmlStrndup (ch, len);
  j_ch = xmljNewString (env, dup);

  (*env)->CallVoidMethod (env,
                          target,
                          sax->cdataBlock,
                          j_ch);
  xmlFree (dup);
}

void
xmljDispatchError (xmlParserCtxtPtr ctx,
                   xmlSAXLocatorPtr loc,
                   JNIEnv *env,
                   jobject target,
                   jmethodID method,
                   const char *msg,
                   va_list args)
{
  jint lineNumber;
  jint columnNumber;
  jstring publicId;
  jstring systemId;
  char buffer[2048] = "";

  if (msg != NULL)
    {
      vsnprintf (buffer, sizeof buffer, msg, args);
    }
  lineNumber = loc->getLineNumber (ctx);
  columnNumber = loc->getColumnNumber (ctx);
  publicId = xmljNewString (env, loc->getPublicId (ctx));
  systemId = xmljNewString (env, loc->getSystemId (ctx));
  (*env)->CallVoidMethod (env,
                          target,
                          method,
                          (*env)->NewStringUTF (env, buffer),
                          lineNumber,
                          columnNumber,
                          publicId,
                          systemId);
}

void
xmljSAXWarning (void *vctx,
                const char *msg,
                ...)
{
  va_list args;

  xmlParserCtxtPtr ctx;
  SAXParseContext *sax;
  xmlSAXLocatorPtr loc;
  JNIEnv *env;
  jobject target;

  ctx = (xmlParserCtxtPtr) vctx;
  sax = (SAXParseContext *) ctx->_private;
  loc = (xmlSAXLocatorPtr) sax->loc;
  env = sax->env;
  target = sax->obj;

  if ((*env)->ExceptionOccurred (env))
    {
      return;
    }
  if (sax->warning == NULL)
    {
      sax->warning =
        xmljGetMethodID (env,
                         target,
                         "warning",
                         "(Ljava/lang/String;IILjava/lang/String;Ljava/lang/String;)V");
      if (sax->warning == NULL)
        {
          return;
        }
    }

  va_start (args, msg);
  /* xmlParserWarning (vctx, msg, args); */
  xmljDispatchError (ctx, loc, env, target, sax->warning, msg, args);
  va_end (args);
}

void
xmljSAXError (void *vctx,
              const char *msg,
              ...)
{
  va_list args;

  xmlParserCtxtPtr ctx;
  SAXParseContext *sax;
  xmlSAXLocatorPtr loc;
  JNIEnv *env;
  jobject target;

  ctx = (xmlParserCtxtPtr) vctx;
  sax = (SAXParseContext *) ctx->_private;
  loc = (xmlSAXLocatorPtr) sax->loc;
  env = sax->env;
  target = sax->obj;

  if ((*env)->ExceptionOccurred (env))
    {
      return;
    }
  if (sax->error == NULL)
    {
      sax->error =
        xmljGetMethodID (env,
                         target,
                         "error",
                         "(Ljava/lang/String;IILjava/lang/String;Ljava/lang/String;)V");
      if (sax->error == NULL)
        {
          return;
        }
    }

  va_start (args, msg);
  /* xmlParserError (vctx, msg, args); */
  xmljDispatchError (ctx, loc, env, target, sax->error, msg, args);
  va_end (args);
}

void
xmljSAXFatalError (void *vctx,
                   const char *msg,
                   ...)
{
  va_list args;

  xmlParserCtxtPtr ctx;
  SAXParseContext *sax;
  xmlSAXLocatorPtr loc;
  JNIEnv *env;
  jobject target;

  ctx = (xmlParserCtxtPtr) vctx;
  sax = (SAXParseContext *) ctx->_private;
  loc = (xmlSAXLocatorPtr) sax->loc;
  env = sax->env;
  target = sax->obj;

  if ((*env)->ExceptionOccurred (env))
    {
      return;
    }
  if (sax->fatalError == NULL)
    {
      sax->fatalError =
        xmljGetMethodID (env,
                         target,
                         "fatalError",
                         "(Ljava/lang/String;IILjava/lang/String;Ljava/lang/String;)V");
      if (sax->fatalError == NULL)
        {
          return;
        }
    }
  
  va_start (args, msg);
  /* xmlParserError (vctx, msg, args); */
  xmljDispatchError (ctx, loc, env, target, sax->fatalError, msg, args);
  va_end (args);
}

void
xmljCheckWellFormed (xmlParserCtxtPtr ctx)
{
  if (!ctx->wellFormed)
    {
      xmljSAXFatalError (ctx, "document is not well-formed");
    }
  if (ctx->validate && !ctx->valid)
    {
      xmljSAXFatalError (ctx, "document is not valid");
    }
}

/*
 * Convert a libxml2 attribute type to a string.
 */
jstring
xmljAttributeTypeName (JNIEnv * env, int type)
{
  const char *text;

  switch (type)
    {
    case XML_ATTRIBUTE_CDATA:
      text = "CDATA";
      break;
    case XML_ATTRIBUTE_ID:
      text = "ID";
      break;
    case XML_ATTRIBUTE_IDREF:
      text = "IDREF";
      break;
    case XML_ATTRIBUTE_IDREFS:
      text = "IDREFS";
      break;
    case XML_ATTRIBUTE_NMTOKEN:
      text = "NMTOKEN";
      break;
    case XML_ATTRIBUTE_NMTOKENS:
      text = "NMTOKENS";
      break;
    case XML_ATTRIBUTE_ENTITY:
      text = "ID";
      break;
    case XML_ATTRIBUTE_ENTITIES:
      text = "ID";
      break;
    default:
      return NULL;
    }

  return (*env)->NewStringUTF (env, text);
}

/*
 * Convert a libxml2 attribute default value type to a string.
 */
jstring
xmljAttributeModeName (JNIEnv * env, int type)
{
  const char *text;

  switch (type)
    {
    case XML_ATTRIBUTE_IMPLIED:
      text = "#IMPLIED";
      break;
    case XML_ATTRIBUTE_REQUIRED:
      text = "#REQUIRED";
      break;
    case XML_ATTRIBUTE_FIXED:
      text = "#FIXED";
      break;
    default:
      return NULL;
    }

  return (*env)->NewStringUTF (env, text);
}
