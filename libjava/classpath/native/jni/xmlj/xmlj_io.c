/* xmlj_io.c -
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

#include "xmlj_io.h"
#include "xmlj_error.h"
#include "xmlj_node.h"
#include "xmlj_sax.h"
#include "xmlj_util.h"

#include <math.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>

#include <libxml/xmlIO.h>
#include <libxml/parserInternals.h>

#include <pthread.h>

#define MIN(a, b) (((a) < (b)) ? (a) : (b))
#define UNSIGN(a) (((a) < 0) ? ((a) + 0x100) : (a))

#define DETECT_BUFFER_SIZE 50

typedef struct _OutputStreamContext
{

  JNIEnv *env;
  jobject outputStream;
  jmethodID outputStreamWriteFunc;
  jmethodID outputStreamCloseFunc;

}
OutputStreamContext;

typedef struct _InputStreamContext
{

  JNIEnv *env;
  jobject inputStream;
  jmethodID inputStreamReadFunc;
  jmethodID inputStreamCloseFunc;
  jobject bufferByteArray;
  jint bufferLength;

}
InputStreamContext;

InputStreamContext *xmljNewInputStreamContext (JNIEnv * env,
                                               jobject inputStream);

void xmljFreeInputStreamContext (InputStreamContext * inContext);

int xmljInputReadCallback (void *context, char *buffer, int len);

int xmljInputCloseCallback (void *context);

int xmljOutputWriteCallback (void *context, const char *buffer, int len);

int xmljOutputCloseCallback (void *context);

OutputStreamContext *xmljNewOutputStreamContext (JNIEnv * env,
                                                 jobject outputStream);

void
xmljFreeOutputStreamContext (OutputStreamContext * outContext);

xmlCharEncoding
xmljDetectCharEncoding (JNIEnv * env, jbyteArray buffer);

int
xmljOutputWriteCallback (void *context, const char *buffer, int len)
{
  OutputStreamContext *outContext;
  JNIEnv *env;
  jbyteArray byteArray;

  outContext = (OutputStreamContext *) context;
  env = outContext->env;
  byteArray = (*env)->NewByteArray (env, len);

  if (0 != byteArray)
    {
      (*env)->SetByteArrayRegion (env, byteArray, 0, len, (jbyte *) buffer);

      (*env)->CallVoidMethod (env,
                              outContext->outputStream,
                              outContext->outputStreamWriteFunc, byteArray);

      (*env)->DeleteLocalRef (env, byteArray);

      return (*env)->ExceptionOccurred (env) ? -1 : len;
    }
  else
    {
      /* Out of memory, signal error */
      return -1;
    }
}

int
xmljOutputCloseCallback (void *context)
{
  OutputStreamContext *outContext;
  JNIEnv *env;

  outContext = (OutputStreamContext *) context;
  env = outContext->env;
  (*env)->CallVoidMethod (env,
                          outContext->outputStream,
                          outContext->outputStreamCloseFunc);

  return (*env)->ExceptionOccurred (env) ? -1 : 0;
}

int
xmljInputReadCallback (void *context, char *buffer, int len)
{
  InputStreamContext *inContext;
  JNIEnv *env;
  jint nread;
  int offset;

  inContext = (InputStreamContext *) context;
  env = inContext->env;
  nread = 0;

  for (offset = 0; offset < len && nread >= 0;)
    {
      nread = (*env)->CallIntMethod (env,
                                     inContext->inputStream,
                                     inContext->inputStreamReadFunc,
                                     inContext->bufferByteArray,
                                     0, MIN (len - offset,
                                             inContext->bufferLength));

      if (nread > 0)
        {
          (*env)->GetByteArrayRegion (env,
                                      inContext->bufferByteArray,
                                      0, nread, ((jbyte *) buffer) + offset);

          offset += nread;
        }
    }

  return (*env)->ExceptionOccurred (env) ? -1 : offset;
}

int
xmljInputCloseCallback (void *context)
{
  InputStreamContext *inContext;
  JNIEnv *env;

  inContext = (InputStreamContext *) context;
  env = inContext->env;
  (*env)->CallVoidMethod (env, inContext->inputStream,
                          inContext->inputStreamCloseFunc);

  return (*env)->ExceptionOccurred (env) ? -1 : 0;
}

InputStreamContext *
xmljNewInputStreamContext (JNIEnv * env, jobject inputStream)
{
  jclass inputStreamClass;
  InputStreamContext *result;

  inputStreamClass = (*env)->FindClass (env, "java/io/InputStream");
  if (inputStreamClass == NULL)
    {
      return NULL;
    }
  result = (InputStreamContext *) malloc (sizeof (InputStreamContext));
  if (result == NULL)
    {
      return NULL;
    }

  result->env = env;
  result->inputStream = inputStream;
  result->inputStreamReadFunc =
    (*env)->GetMethodID (env, inputStreamClass, "read", "([BII)I");
  result->inputStreamCloseFunc =
    (*env)->GetMethodID (env, inputStreamClass, "close", "()V");
  result->bufferLength = 4096;
  result->bufferByteArray = (*env)->NewByteArray (env, result->bufferLength);
  return result;
}

void
xmljFreeInputStreamContext (InputStreamContext * inContext)
{
  JNIEnv *env;

  env = inContext->env;
  (*env)->DeleteLocalRef (env, inContext->bufferByteArray);
  free (inContext);
}

OutputStreamContext *
xmljNewOutputStreamContext (JNIEnv * env, jobject outputStream)
{
  jclass outputStreamClass;
  OutputStreamContext *result;

  outputStreamClass = (*env)->FindClass (env, "java/io/OutputStream");
  if (outputStreamClass == NULL)
    {
      return NULL;
    }
  result = (OutputStreamContext *) malloc (sizeof (OutputStreamContext));
  if (result == NULL)
    {
      return NULL;
    }

  result->env = env;
  result->outputStream = outputStream;
  result->outputStreamWriteFunc =
    (*env)->GetMethodID (env, outputStreamClass, "write", "([B)V");
  result->outputStreamCloseFunc =
    (*env)->GetMethodID (env, outputStreamClass, "close", "()V");
  return result;
}


void
xmljFreeOutputStreamContext (OutputStreamContext * outContext)
{
  free (outContext);
}

SAXParseContext *
xmljNewSAXParseContext (JNIEnv * env, jobject obj, xmlParserCtxtPtr ctx,
                        jstring publicId, jstring systemId)
{
  SAXParseContext *ret;
  
  ret = (SAXParseContext *) malloc (sizeof (SAXParseContext));
  ret->env = env;
  ret->obj = obj;
  ret->ctx = ctx;
  ret->sax = ctx->sax;
  ret->loc = NULL;
  ret->publicId = publicId;
  ret->systemId = systemId;
  
  ret->startDTD = NULL;
  ret->externalEntityDecl = NULL;
  ret->internalEntityDecl = NULL;
  ret->resolveEntity = NULL;
  ret->notationDecl = NULL;
  ret->attributeDecl = NULL;
  ret->elementDecl = NULL;
  ret->unparsedEntityDecl = NULL;
  ret->setDocumentLocator = NULL;
  ret->startDocument = NULL;
  ret->endDocument = NULL;
  ret->startElement = NULL;
  ret->endElement = NULL;
  ret->characters = NULL;
  ret->ignorableWhitespace = NULL;
  ret->processingInstruction = NULL;
  ret->comment = NULL;
  ret->cdataBlock = NULL;
  ret->warning = NULL;
  ret->error = NULL;
  ret->fatalError = NULL;

  ret->resolveURIAndOpen = NULL;
  ret->stringClass = NULL; 
  return ret;
}

void
xmljFreeSAXParseContext (SAXParseContext * saxCtx)
{
  free (saxCtx);
}

xmlCharEncoding
xmljDetectCharEncoding (JNIEnv * env, jbyteArray buffer)
{
  xmlCharEncoding ret;
  jint nread;

  if (buffer == NULL)
    {
      return XML_CHAR_ENCODING_ERROR;
    }
  nread = (*env)->GetArrayLength (env, buffer);
  if (nread >= 5)
    {
      jbyte nativeBuffer[DETECT_BUFFER_SIZE + 1];
      unsigned char converted[DETECT_BUFFER_SIZE + 1];
      int i;

      memset (nativeBuffer, 0, DETECT_BUFFER_SIZE + 1);
      (*env)->GetByteArrayRegion (env, buffer, 0, nread, nativeBuffer);
      /* Convert from signed to unsigned */
      for (i = 0; i < DETECT_BUFFER_SIZE + 1; i++)
        {
          converted[i] = UNSIGN (nativeBuffer[i]);
        }
      ret = xmlDetectCharEncoding (converted, nread);
    }
  else
    {
      ret = XML_CHAR_ENCODING_NONE;
    }
  return ret;
}

xmlParserCtxtPtr
xmljNewParserContext (JNIEnv * env,
                      jobject inputStream,
                      jbyteArray detectBuffer,
                      jstring publicId,
                      jstring systemId,
                      jstring base,
                      jboolean validate,
                      jboolean coalesce,
                      jboolean expandEntities,
                      jboolean loadEntities)
{
  InputStreamContext *inputContext;
  xmlCharEncoding encoding;
  xmlParserCtxtPtr ctx;
  int options;

  encoding = xmljDetectCharEncoding (env, detectBuffer);
  if (encoding != XML_CHAR_ENCODING_ERROR)
    {
      inputContext = xmljNewInputStreamContext (env, inputStream);
      if (NULL != inputContext)
        {
          /* NOTE: userdata must be NULL for DOM to work */
          ctx = xmlCreateIOParserCtxt (NULL,
                                       NULL,
                                       xmljInputReadCallback,
                                       xmljInputCloseCallback,
                                       inputContext,
                                       encoding);
          if (NULL != ctx)
            {
              ctx->userData = ctx;
              
              /* Set parsing options */
              options = 0;
              if (validate)
                {
                  options |= XML_PARSE_DTDVALID;
                }
              if (coalesce)
                {
                  options |= XML_PARSE_NOCDATA;
                }
              if (expandEntities)
                {
                  options |= XML_PARSE_NOENT;
                }
              if (loadEntities)
                {
                  options |= XML_PARSE_DTDLOAD;
                }
              if (xmlCtxtUseOptions (ctx, options))
                {
                  xmljThrowException (env,
                                      "java/lang/RuntimeException",
                                      "Unable to set xmlParserCtxtPtr options");
                }
              if (base != NULL)
                {
                  ctx->input->directory =
                    (*env)->GetStringUTFChars (env, base, 0);
                }
              return ctx;
            }
          xmljFreeInputStreamContext (inputContext);
        }
    }
  return NULL;
}

void
xmljFreeParserContext (xmlParserCtxtPtr ctx)
{
  InputStreamContext *inputStreamContext = NULL;

  if (ctx->input != NULL && ctx->input->buf != NULL)
    {
      inputStreamContext
        = (InputStreamContext *) ctx->input->buf->context;
      
    }
  xmlFreeParserCtxt (ctx);
  if (inputStreamContext != NULL)
    {
      xmljFreeInputStreamContext (inputStreamContext);
    }
}

xmlDocPtr
xmljParseDocument (JNIEnv * env,
                   jobject self,
                   jobject in,
                   jbyteArray detectBuffer,
                   jstring publicId,
                   jstring systemId,
                   jstring base,
                   jboolean validate,
                   jboolean coalesce,
                   jboolean expandEntities,
                   jboolean contentHandler,
                   jboolean dtdHandler,
                   jboolean entityResolver,
                   jboolean errorHandler,
                   jboolean declarationHandler,
                   jboolean lexicalHandler,
                   int mode)
{
  xmlParserCtxtPtr ctx;
  SAXParseContext *saxCtx;
  xmlSAXHandlerPtr sax;

  ctx = xmljNewParserContext (env, in, detectBuffer, publicId, systemId, base,
                              validate, coalesce, expandEntities,
                              entityResolver);
  if (ctx != NULL)
    {
      saxCtx = xmljNewSAXParseContext (env, self, ctx, publicId, systemId);
      if (saxCtx != NULL)
        {
          sax = xmljNewSAXHandler (contentHandler,
                                   dtdHandler,
                                   entityResolver,
                                   errorHandler,
                                   declarationHandler,
                                   lexicalHandler);
          if (sax != NULL)
            {
              return xmljParseDocument2 (env,
                                         ctx,
                                         saxCtx,
                                         sax,
                                         mode);
            }
          xmljFreeSAXParseContext (saxCtx);
        }
      xmljFreeParserContext (ctx);
    }
  if (!(*env)->ExceptionOccurred (env))
    {
      xmljThrowException (env, "java/io/IOException",
                          "Unable to create parser context");
    }
  return NULL;
}

xmlDocPtr
xmljParseDocument2 (JNIEnv * env,
                    xmlParserCtxtPtr ctx,
                    SAXParseContext *saxCtx,
                    xmlSAXHandlerPtr sax,
                    int mode)
{
  xmlSAXHandlerPtr orig;
  xmlDocPtr doc;
  int ret;
  
  ctx->_private = saxCtx;
  ctx->userData = ctx;
  orig = ctx->sax;
  ctx->sax = sax;
  
  xmljSetThreadContext (saxCtx);

  ret = xmlParseDocument (ctx);
  doc = ctx->myDoc;
  if (ret || !doc)
    {
      const char *msg = ctx->lastError.message;
      switch (mode)
        {
        case 0:
          xmljSAXFatalError (ctx, msg);
          break;
        case 1:
          xmljThrowDOMException (env, ret, msg);
          break;
        case 2:
          xmljThrowException (env,
                              "javax/xml/transform/TransformerException",
                              msg);
        }
    }
  
  xmljClearThreadContext ();
              
  ctx->sax = orig;
  free(sax);
  xmljFreeSAXParseContext (saxCtx);
  xmljFreeParserContext (ctx);
  xmljClearStringCache ();
  return doc;
}

xmlParserInputPtr
xmljNewParserInput (JNIEnv * env,
                    jobject inputStream,
                    jbyteArray detectBuffer,
                    xmlParserCtxtPtr parserContext)
{
  xmlParserInputPtr ret;
  xmlParserInputBufferPtr input;
  xmlCharEncoding encoding;

  encoding = xmljDetectCharEncoding (env, detectBuffer);
  if (encoding != XML_CHAR_ENCODING_ERROR)
    {
      input = xmljNewParserInputBuffer (env, inputStream, encoding);
      if (input != NULL)
        {
          ret = xmlNewIOInputStream (parserContext, input, encoding);
          return ret;
        }
      xmlFreeParserInputBuffer (input);
    }
  return NULL;
}

xmlParserInputBufferPtr
xmljNewParserInputBuffer (JNIEnv * env,
                          jobject inputStream, xmlCharEncoding encoding)
{
  xmlParserInputBufferPtr ret;
  InputStreamContext *inputContext;

  inputContext = xmljNewInputStreamContext (env, inputStream);
  if (NULL != inputContext)
    {
      ret = xmlParserInputBufferCreateIO (&xmljInputReadCallback,
                                          &xmljInputCloseCallback,
                                          inputContext, encoding);
      if (ret != NULL)
        return ret;
      xmljFreeInputStreamContext (inputContext);
    }
  return NULL;
}

void
xmljSaveFileToJavaOutputStream (JNIEnv * env, jobject outputStream,
                                xmlDocPtr tree,
                                const char *outputEncodingName)
{
  OutputStreamContext *outputContext =
    xmljNewOutputStreamContext (env, outputStream);

  xmlCharEncoding outputEncoding = xmlParseCharEncoding (outputEncodingName);

  xmlOutputBufferPtr outputBuffer =
    xmlOutputBufferCreateIO (xmljOutputWriteCallback,
                             xmljOutputCloseCallback,
                             outputContext,
                             xmlGetCharEncodingHandler (outputEncoding));

  /* Write result to output stream */

  xmlSaveFileTo (outputBuffer, tree, outputEncodingName);

  xmljFreeOutputStreamContext (outputContext);
}

/*
jobject
xmljResolveURI (SaxErrorContext * saxErrorContext,
                const char *URL, const char *ID)
{
  JNIEnv *env = saxErrorContext->env;

  jstring hrefString = (*env)->NewStringUTF (env, URL);
  jstring baseString = saxErrorContext->systemId;

  jobject sourceWrapper = (*env)->CallObjectMethod (env,
                                                    saxErrorContext->
                                                    saxErrorAdapter,
                                                    saxErrorContext->
                                                    resolveURIMethodID,
                                                    hrefString,
                                                    baseString);
  (*env)->DeleteLocalRef (env, hrefString);

  if (NULL == sourceWrapper)
    {
      return NULL;
    }
  else
    {
      jobject sourceInputStream = (*env)->CallObjectMethod (env,
                                                            sourceWrapper,
                                                            saxErrorContext->
                                                            getInputStreamMethodID);

      (*env)->DeleteLocalRef (env, sourceWrapper);

      if ((*env)->ExceptionOccurred (env))
        {
          -* Report to ErrorAdapter here? *-
          return NULL;
        }

      return sourceInputStream;
    }
}*/

xmlDocPtr
xmljResolveURIAndOpen (SAXParseContext *saxContext,
                       const char *URL,
                       const char *ID)
{
  jobject libxmlDocument;
  xmlDocPtr doc;
  JNIEnv *env = saxContext->env;

  jstring hrefString = (*env)->NewStringUTF (env, URL);
  jstring baseString = saxContext->systemId;

  if (saxContext->resolveURIAndOpen == NULL)
    {
      jclass cls = (*env)->GetObjectClass (env, saxContext->obj);
      saxContext->resolveURIAndOpen =
        (*env)->GetMethodID (env, cls, "resolveURIAndOpen",
                             "Ljava/lang/String;Ljava/lang/String)Lgnu/xml/libxmlj/transform/LibxmlDocument;");
    }
  libxmlDocument =
    (*env)->CallObjectMethod (env,
                              saxContext->obj,
                              saxContext->resolveURIAndOpen,
                              hrefString,
                              baseString);

  doc = (xmlDocPtr) xmljGetNodeID (env, libxmlDocument);

  (*env)->DeleteLocalRef (env, libxmlDocument);

  if ((*env)->ExceptionOccurred (env))
    {
      /* Report to ErrorAdapter here? */
      return NULL;
    }
  else
    {
      return doc;
    }
}

/*xmlParserInputPtr
xmljLoadExternalEntity (const char *URL, const char *ID,
                        xmlParserCtxtPtr ctxt)
{
  SaxErrorContext *saxErrorContext = xmljGetThreadContext ();

  JNIEnv *env = saxErrorContext->env;

  jstring hrefString = (*env)->NewStringUTF (env, URL);
  jstring baseString = saxErrorContext->systemId;

  jobject sourceWrapper = (*env)->CallObjectMethod (env,
                                                    saxErrorContext->
                                                    saxErrorAdapter,
                                                    saxErrorContext->
                                                    resolveURIMethodID,
                                                    hrefString,
                                                    baseString);

  (*env)->DeleteLocalRef (env, hrefString);

  if (NULL == sourceWrapper)
    {
      return NULL;
    }
  else
    {
      InputStreamContext *inputContext;
      xmlParserInputBufferPtr inputBuffer;
      xmlParserInputPtr inputStream;

      jobject sourceInputStream = (*env)->CallObjectMethod (env,
                                                            sourceWrapper,
                                                            saxErrorContext->
                                                            getInputStreamMethodID);

      (*env)->DeleteLocalRef (env, sourceWrapper);

      if ((*env)->ExceptionOccurred (env))
        {
          -* Report to ErrorAdapter *-
          return NULL;
        }

      inputContext = xmljNewInputStreamContext (env, sourceInputStream);

      inputBuffer
        = xmlParserInputBufferCreateIO (xmljInputReadCallback,
                                        xmljInputCloseCallback,
                                        inputContext, XML_CHAR_ENCODING_NONE);

      inputStream = xmlNewInputStream (ctxt);
      if (inputStream == NULL)
        {
          return (NULL);
        }

      inputStream->filename = NULL;
      inputStream->directory = NULL;
      inputStream->buf = inputBuffer;

      inputStream->base = inputStream->buf->buffer->content;
      inputStream->cur = inputStream->buf->buffer->content;
      inputStream->end = &inputStream->base[inputStream->buf->buffer->use];
      if ((ctxt->directory == NULL) && (inputStream->directory != NULL))
        ctxt->directory =
          (char *) xmlStrdup ((const xmlChar *) inputStream->directory);
      return (inputStream);
    }
}*/

/* Key for the thread-specific buffer */
static pthread_key_t thread_context_key;

/* Once-only initialisation of the key */
static pthread_once_t thread_context_once = PTHREAD_ONCE_INIT;

static void
thread_context_key_alloc (void);

/* Allocate the key */
static void
thread_context_key_alloc ()
{
  pthread_key_create (&thread_context_key, NULL);
}

void
xmljSetThreadContext (SAXParseContext * context)
{
  pthread_once (&thread_context_once, thread_context_key_alloc);
  pthread_setspecific (thread_context_key, context);
}

void
xmljClearThreadContext (void)
{
  pthread_setspecific (thread_context_key, NULL);
}

/* Return the thread-specific buffer */
SAXParseContext *
xmljGetThreadContext (void)
{
  return (SAXParseContext *) pthread_getspecific (thread_context_key);
}
