// defineclass.cc - defining a class from .class format.

/* Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007
   Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

/* 
   Author: Kresten Krab Thorup <krab@gnu.org> 

   Written using the online versions of Java Language Specification (1st
   ed.) and The Java Virtual Machine Specification (2nd ed.). 

   Future work may include reading (and handling) attributes which are
   currently being ignored ("InnerClasses", "LineNumber", etc...).  
*/

#include <config.h>

#include <java-interp.h>

#include <stdlib.h>
#include <stdio.h>
#include <java-cpool.h>
#include <gcj/cni.h>
#include <execution.h>

#include <java/lang/Class.h>
#include <java/lang/Float.h>
#include <java/lang/Double.h>
#include <java/lang/Character.h>
#include <java/lang/LinkageError.h>
#include <java/lang/InternalError.h>
#include <java/lang/ClassFormatError.h>
#include <java/lang/NoClassDefFoundError.h>
#include <java/lang/ClassCircularityError.h>
#include <java/lang/IncompatibleClassChangeError.h>
#include <java/lang/reflect/Modifier.h>
#include <java/lang/reflect/Field.h>
#include <java/lang/reflect/Method.h>
#include <java/security/ProtectionDomain.h>
#include <java/io/DataOutputStream.h>
#include <java/io/ByteArrayOutputStream.h>

using namespace gcj;

#ifdef INTERPRETER

// these go in some separate functions, to avoid having _Jv_InitClass
// inserted all over the place.
static void throw_internal_error (const char *msg)
	__attribute__ ((__noreturn__));
static void throw_no_class_def_found_error (jstring msg)
	__attribute__ ((__noreturn__));
static void throw_no_class_def_found_error (const char *msg)
	__attribute__ ((__noreturn__));
static void throw_class_format_error (jstring msg)
	__attribute__ ((__noreturn__));
static void throw_incompatible_class_change_error (jstring msg)
	__attribute__ ((__noreturn__));
static void throw_class_circularity_error (jstring msg)
	__attribute__ ((__noreturn__));

/**
 * We define class reading using a class.  It is practical, since then
 * the entire class-reader can be a friend of class Class (it needs to
 * write all it's different structures); but also because this makes it
 * easy to make class definition reentrant, and thus two threads can be
 * defining classes at the same time.   This class (_Jv_ClassReader) is
 * never exposed outside this file, so we don't have to worry about
 * public or private members here.
 */

struct _Jv_ClassReader
{

  // do verification?  Currently, there is no option to disable this.
  // This flag just controls the verificaiton done by the class loader;
  // i.e., checking the integrity of the constant pool; and it is
  // allways on.  You always want this as far as I can see, but it also
  // controls weither identifiers and type descriptors/signatures are
  // verified as legal.  This could be somewhat more expensive since it
  // will call Character.isJavaIdentifier{Start,Part} for each character
  // in any identifier (field name or method name) it comes by.  Thus,
  // it might be useful to turn off this verification for classes that
  // come from a trusted source.  However, for GCJ, trusted classes are
  // most likely to be linked in.

  bool verify;

  // original input data.
  jbyteArray input_data;
  jint input_offset;

  // input data.
  unsigned char     *bytes;
  int                len;

  // current input position
  int                pos;

  // the constant pool data
  int pool_count;
  unsigned char     *tags;
  unsigned int      *offsets;

  // the class to define (see java-interp.h)
  jclass	   def;

  // the classes associated interpreter data.
  _Jv_InterpClass  *def_interp;

  // The name we found.
  _Jv_Utf8Const **found_name;

  // True if this is a 1.5 class file.
  bool             is_15;

  // Buffer holding extra reflection data.
  ::java::io::ByteArrayOutputStream *reflection_data;
  ::java::io::DataOutputStream *data_stream;


  /* check that the given number of input bytes are available */
  inline void check (int num)
  {
    if (pos + num > len)
      throw_class_format_error ("Premature end of data");
  }

  /* skip a given number of bytes in input */
  inline void skip (int num)
  {
    check (num);
    pos += num;
  }
  
  /* read an unsigned 1-byte unit */
  inline static jint get1u (unsigned char* bytes)
  {
    return bytes[0];
  }
  
  /* read an unsigned 1-byte unit */
  inline jint read1u ()
  {
    skip (1);
    return get1u (bytes+pos-1);
  }
  
  /* read an unsigned 2-byte unit */
  inline static jint get2u (unsigned char *bytes)
  {
    return (((jint)bytes[0]) << 8) | ((jint)bytes[1]);
  }
  
  /* read an unsigned 2-byte unit */
  inline jint read2u ()
  {
    skip (2);  
    return get2u (bytes+pos-2);
  }
  
  /* read a 4-byte unit */
  static jint get4 (unsigned char *bytes)
  {
    return (((jint)bytes[0]) << 24)
         | (((jint)bytes[1]) << 16)
         | (((jint)bytes[2]) << 8)
         | (((jint)bytes[3]) << 0);
  }

  /* read a 4-byte unit, (we don't do that quite so often) */
  inline jint read4 ()
  {
    skip (4);  
    return get4 (bytes+pos-4);
  }

  /* read a 8-byte unit */
  static jlong get8 (unsigned char* bytes)
  {
    return (((jlong)bytes[0]) << 56)
         | (((jlong)bytes[1]) << 48)
         | (((jlong)bytes[2]) << 40)
         | (((jlong)bytes[3]) << 32) 
         | (((jlong)bytes[4]) << 24)
         | (((jlong)bytes[5]) << 16)
         | (((jlong)bytes[6]) << 8)
         | (((jlong)bytes[7]) << 0);
  }

  /* read a 8-byte unit */
  inline jlong read8 ()
  {
    skip (8);  
    return get8 (bytes+pos-8);
  }

  inline void check_tag (int index, char expected_tag)
  {
    if (index < 0
	|| index > pool_count
	|| tags[index] != expected_tag)
      throw_class_format_error ("erroneous constant pool tag");
  }

  inline void verify_identifier (_Jv_Utf8Const* name)
  {
    if (! _Jv_VerifyIdentifier (name))
      throw_class_format_error ("erroneous identifier");
  }

  inline void verify_classname (unsigned char* ptr, _Jv_ushort length)
  {
    if (! _Jv_VerifyClassName (ptr, length))
      throw_class_format_error ("erroneous class name");
  }

  inline void verify_classname (_Jv_Utf8Const *name)
  {
    if (! _Jv_VerifyClassName (name))
      throw_class_format_error ("erroneous class name");
  }

  inline void verify_field_signature (_Jv_Utf8Const *sig)
  {
    if (! _Jv_VerifyFieldSignature (sig))
      throw_class_format_error ("erroneous type descriptor");
  }

  inline void verify_method_signature (_Jv_Utf8Const *sig)
  {
    if (! _Jv_VerifyMethodSignature (sig))
      throw_class_format_error ("erroneous type descriptor");
  }

  ::java::io::DataOutputStream *get_reflection_stream ()
  {
    if (reflection_data == NULL)
      {
	reflection_data = new ::java::io::ByteArrayOutputStream();
	data_stream = new ::java::io::DataOutputStream(reflection_data);
      }
    return data_stream;
  }

  _Jv_ClassReader (jclass klass, jbyteArray data, jint offset, jint length,
		   java::security::ProtectionDomain *pd,
		   _Jv_Utf8Const **name_result)
  {
    if (klass == 0 || length < 0 || offset+length > data->length)
      throw_internal_error ("arguments to _Jv_DefineClass");

    verify = true;
    input_data = data;
    input_offset = offset;
    bytes  = (unsigned char*) (elements (data)+offset);
    len    = length;
    pos    = 0;
    is_15  = false;

    def    = klass;
    found_name = name_result;
    reflection_data = NULL;
    data_stream = NULL;

    def->size_in_bytes = -1;
    def->vtable_method_count = -1;
    def->engine = &_Jv_soleInterpreterEngine;
    def->protectionDomain = pd;
  }

  /** and here goes the parser members defined out-of-line */
  void parse ();
  void read_constpool ();
  void prepare_pool_entry (int index, unsigned char tag,
			   bool rewrite = true);
  void read_fields ();
  void read_methods ();
  void read_one_class_attribute ();
  void read_one_method_attribute (int method);
  void read_one_code_attribute (int method);
  void read_one_field_attribute (int field, bool *);
  void throw_class_format_error (const char *msg);

  void handleEnclosingMethod(int);
  void handleGenericSignature(jv_attr_type, unsigned short, int);
  void handleAnnotationElement();
  void handleAnnotation();
  void handleAnnotations();
  void handleMemberAnnotations(jv_attr_type, int, int);
  void handleAnnotationDefault(int, int);
  void handleParameterAnnotations(int, int);
  void finish_reflection_data ();

  /** check an utf8 entry, without creating a Utf8Const object */
  bool is_attribute_name (int index, const char *name);
  
  /** return the value of a utf8 entry in the passed array */
  int pool_Utf8_to_char_arr (int index, char **entry);

  /** here goes the class-loader members defined out-of-line */
  void handleConstantPool ();
  void handleClassBegin (int, int, int);
  void handleInterfacesBegin (int);
  void handleInterface (int, int);
  void handleFieldsBegin (int);
  void handleField (int, int, int, int, int *);
  void handleConstantValueAttribute (int, int, bool *);
  void handleMethodsBegin (int);
  void handleMethod (int, int, int, int);
  void handleMethodsEnd ();
  void handleCodeAttribute (int, int, int, int, int, int);
  void handleExceptionTableEntry (int, int, int, int, int, int);

  void checkExtends (jclass sub, jclass super);
  void checkImplements (jclass sub, jclass super);

  /*
   * FIXME: we should keep a hash table of utf8-strings, since many will
   * be the same.  It's a little tricky, however, because the hash table
   * needs to interact gracefully with the garbage collector.  Much
   * memory is to be saved by this, however!  perhaps the improvement
   * could be implemented in prims.cc (_Jv_makeUtf8Const), since it
   * computes the hash value anyway.
   */
};

// Note that *NAME_RESULT will only be set if the class is registered
// with the class loader.  This is how the caller can know whether
// unregistration is require.
void
_Jv_DefineClass (jclass klass, jbyteArray data, jint offset, jint length,
		 java::security::ProtectionDomain *pd,
		 _Jv_Utf8Const **name_result)
{
  _Jv_ClassReader reader (klass, data, offset, length, pd, name_result);
  reader.parse();

  /* that's it! */
}


/** This section defines the parsing/scanning of the class data */

// Major and minor version numbers for various releases.
#define MAJOR_1_1 45
#define MINOR_1_1  3
#define MAJOR_1_2 46
#define MINOR_1_2  0
#define MAJOR_1_3 47
#define MINOR_1_3  0
#define MAJOR_1_4 48
#define MINOR_1_4  0
#define MAJOR_1_5 49
#define MINOR_1_5  0
#define MAJOR_1_6 50
#define MINOR_1_6  0

void
_Jv_ClassReader::parse ()
{
  int magic = read4 ();
  if (magic != (int) 0xCAFEBABE)
    throw_class_format_error ("bad magic number");

  int minor_version = read2u ();
  int major_version = read2u ();
  if (major_version < MAJOR_1_1 || major_version > MAJOR_1_6
      || (major_version == MAJOR_1_6 && minor_version > MINOR_1_6))
    throw_class_format_error ("unrecognized class file version");
  is_15 = (major_version >= MAJOR_1_5);

  pool_count = read2u ();

  read_constpool ();

  int access_flags = read2u ();
  int this_class = read2u ();
  int super_class = read2u ();

  check_tag (this_class, JV_CONSTANT_Class);
  if (super_class != 0) 
    check_tag (super_class, JV_CONSTANT_Class);

  handleClassBegin (access_flags, this_class, super_class);

  // Allocate our aux_info here, after the name is set, to fulfill our
  // contract with the collector interface.
  def->aux_info = (void *) _Jv_AllocRawObj (sizeof (_Jv_InterpClass));
  def_interp = (_Jv_InterpClass *) def->aux_info;

  int interfaces_count = read2u (); 

  handleInterfacesBegin (interfaces_count);

  for (int i = 0; i < interfaces_count; i++)
    {
      int iface = read2u ();
      check_tag (iface, JV_CONSTANT_Class);
      handleInterface (i, iface);
    }
  
  read_fields ();
  read_methods ();
  
  int attributes_count = read2u ();
  
  for (int i = 0; i < attributes_count; i++)
    {
      read_one_class_attribute ();
    }

  if (pos != len)
    throw_class_format_error ("unused data before end of file");

  finish_reflection_data ();

  // Tell everyone we're done.
  def->state = JV_STATE_READ;
  if (gcj::verbose_class_flag)
    _Jv_Linker::print_class_loaded (def);
  ++gcj::loadedClasses;
  def->notifyAll ();
}

void
_Jv_ClassReader::finish_reflection_data ()
{
  if (data_stream == NULL)
    return;
  data_stream->writeByte(JV_DONE_ATTR);
  data_stream->flush();
  int nbytes = reflection_data->count;
  unsigned char *new_bytes = (unsigned char *) _Jv_AllocBytes (nbytes);
  memcpy (new_bytes, elements (reflection_data->buf), nbytes);
  def->reflection_data = new_bytes;
}

void
_Jv_ClassReader::handleEnclosingMethod (int len)
{
  if (len != 4)
    throw_class_format_error ("invalid EnclosingMethod attribute");
  // FIXME: only allow one...

  int class_index = read2u();
  check_tag (class_index, JV_CONSTANT_Class);
  prepare_pool_entry (class_index, JV_CONSTANT_Class);

  int method_index = read2u();
  // Zero is ok and means no enclosing method.
  if (method_index != 0)
    {
      check_tag (method_index, JV_CONSTANT_NameAndType);
      prepare_pool_entry (method_index, JV_CONSTANT_NameAndType);
    }

  ::java::io::DataOutputStream *stream = get_reflection_stream ();
  stream->writeByte(JV_CLASS_ATTR);
  stream->writeInt(5);
  stream->writeByte(JV_ENCLOSING_METHOD_KIND);
  stream->writeShort(class_index);
  stream->writeShort(method_index);
}

void
_Jv_ClassReader::handleGenericSignature (jv_attr_type type,
					 unsigned short index,
					 int len)
{
  if (len != 2)
    throw_class_format_error ("invalid Signature attribute");

  int cpool_idx = read2u();
  check_tag (cpool_idx, JV_CONSTANT_Utf8);
  prepare_pool_entry (cpool_idx, JV_CONSTANT_Utf8, false);

  ::java::io::DataOutputStream *stream = get_reflection_stream ();
  stream->writeByte(type);
  int attrlen = 3;
  if (type != JV_CLASS_ATTR)
    attrlen += 2;
  stream->writeInt(attrlen);
  if (type != JV_CLASS_ATTR)
    stream->writeShort(index);
  stream->writeByte(JV_SIGNATURE_KIND);
  stream->writeShort(cpool_idx);
}

void
_Jv_ClassReader::handleAnnotationElement()
{
  int tag = read1u();
  switch (tag)
    {
    case 'B':
    case 'C':
    case 'S':
    case 'Z':
    case 'I':
      {
	int index = read2u();
	check_tag (index, JV_CONSTANT_Integer);
	prepare_pool_entry (index, JV_CONSTANT_Integer);
      }
      break;
    case 'D':
      {
	int index = read2u();
	check_tag (index, JV_CONSTANT_Double);
	prepare_pool_entry (index, JV_CONSTANT_Double);
      }
      break;
    case 'F':
      {
	int index = read2u();
	check_tag (index, JV_CONSTANT_Float);
	prepare_pool_entry (index, JV_CONSTANT_Float);
      }
      break;
    case 'J':
      {
	int index = read2u();
	check_tag (index, JV_CONSTANT_Long);
	prepare_pool_entry (index, JV_CONSTANT_Long);
      }
      break;
    case 's':
      {
	int index = read2u();
	// Despite what the JVM spec says, compilers generate a Utf8
	// constant here, not a String.
	check_tag (index, JV_CONSTANT_Utf8);
	prepare_pool_entry (index, JV_CONSTANT_Utf8, false);
      }
      break;

    case 'e':
      {
	int type_name_index = read2u();
	int const_name_index = read2u ();
	check_tag (type_name_index, JV_CONSTANT_Utf8);
	prepare_pool_entry (type_name_index, JV_CONSTANT_Utf8);
	check_tag (const_name_index, JV_CONSTANT_Utf8);
	prepare_pool_entry (const_name_index, JV_CONSTANT_Utf8, false);
      }
      break;
    case 'c':
      {
	int index = read2u();
	check_tag (index, JV_CONSTANT_Utf8);
	prepare_pool_entry (index, JV_CONSTANT_Utf8);
      }
      break;
    case '@':
      handleAnnotation();
      break;
    case '[':
      {
	int n_array_elts = read2u ();
	for (int i = 0; i < n_array_elts; ++i)
	  handleAnnotationElement();
      }
      break;
    default:
      throw_class_format_error ("invalid annotation element");
    }
}

void
_Jv_ClassReader::handleAnnotation()
{
  int type_index = read2u();
  check_tag (type_index, JV_CONSTANT_Utf8);
  prepare_pool_entry (type_index, JV_CONSTANT_Utf8);

  int npairs = read2u();
  for (int i = 0; i < npairs; ++i)
    {
      int name_index = read2u();
      check_tag (name_index, JV_CONSTANT_Utf8);
      prepare_pool_entry (name_index, JV_CONSTANT_Utf8, false);
      handleAnnotationElement();
    }
}

void
_Jv_ClassReader::handleAnnotations()
{
  int num = read2u();
  while (num--)
    handleAnnotation();
}

void
_Jv_ClassReader::handleMemberAnnotations(jv_attr_type member_type,
					 int member_index,
					 int len)
{
  // We're going to copy the bytes in verbatim.  But first we want to
  // make sure the attribute is well-formed, and we want to prepare
  // the constant pool.  So, we save our starting point.
  int orig_pos = pos;

  handleAnnotations();
  // FIXME: check that we read all LEN bytes?

  ::java::io::DataOutputStream *stream = get_reflection_stream ();
  stream->writeByte(member_type);
  int newLen = len + 1;
  if (member_type != JV_CLASS_ATTR)
    newLen += 2;
  stream->writeInt(newLen);
  stream->writeByte(JV_ANNOTATIONS_KIND);
  if (member_type != JV_CLASS_ATTR)
    stream->writeShort(member_index);
  // Write the data as-is.
  stream->write(input_data, input_offset + orig_pos, len);
}

void
_Jv_ClassReader::handleAnnotationDefault(int member_index, int len)
{
  int orig_pos = pos;
  handleAnnotationElement();

  ::java::io::DataOutputStream *stream = get_reflection_stream ();
  stream->writeByte(JV_METHOD_ATTR);
  stream->writeInt(len + 3);
  stream->writeByte(JV_ANNOTATION_DEFAULT_KIND);
  stream->writeShort(member_index);
  stream->write(input_data, input_offset + orig_pos, len);
}

void
_Jv_ClassReader::handleParameterAnnotations(int member_index, int len)
{
  int orig_pos = pos;

  int n_params = read1u();
  for (int i = 0; i < n_params; ++i)
    handleAnnotations();

  ::java::io::DataOutputStream *stream = get_reflection_stream ();
  stream->writeByte(JV_METHOD_ATTR);
  stream->writeInt(len + 3);
  stream->writeByte(JV_PARAMETER_ANNOTATIONS_KIND);
  stream->writeShort(member_index);
  stream->write(input_data, input_offset + orig_pos, len);
}

void _Jv_ClassReader::read_constpool ()
{
  tags    = (unsigned char*) _Jv_AllocBytes (pool_count);
  offsets = (unsigned int *) _Jv_AllocBytes (sizeof (int) * pool_count) ;

  /** first, we scan the constant pool, collecting tags and offsets */
  tags[0]   = JV_CONSTANT_Undefined;
  offsets[0] = pos;
  for (int c = 1; c < pool_count; c++)
    {
      tags[c]    = read1u ();
      offsets[c] = pos;

      switch (tags[c])
	{
	case JV_CONSTANT_String:
	case JV_CONSTANT_Class:
	  skip (2);
	  break;

	case JV_CONSTANT_Fieldref:
	case JV_CONSTANT_Methodref:
	case JV_CONSTANT_InterfaceMethodref:
	case JV_CONSTANT_NameAndType:
	case JV_CONSTANT_Integer:
	case JV_CONSTANT_Float:
	  skip (4);
	  break;

	case JV_CONSTANT_Double:
	case JV_CONSTANT_Long:
	  skip (8);
	  tags[++c] = JV_CONSTANT_Undefined;
	  break;
	    
	case JV_CONSTANT_Utf8:
	  {		    
	    int len = read2u ();
	    skip (len);
	  }
	  break;

	case JV_CONSTANT_Unicode:
	  throw_class_format_error ("unicode not supported");
	  break;

	default:
	  throw_class_format_error ("erroneous constant pool tag");
	}
    }

  handleConstantPool ();
}


void _Jv_ClassReader::read_fields ()
{
  int fields_count = read2u ();
  handleFieldsBegin (fields_count);

  // We want to sort the fields so that static fields come first,
  // followed by instance fields.  We do this before parsing the
  // fields so that we can have the new indices available when
  // creating the annotation data structures.

  // Allocate this on the heap in case there are a large number of
  // fields.
  int *fieldmap = (int *) _Jv_AllocBytes (fields_count * sizeof (int));
  int save_pos = pos;
  int static_count = 0, instance_count = -1;
  for (int i = 0; i < fields_count; ++i)
    {
      using namespace java::lang::reflect;

      int access_flags = read2u ();
      skip (4);
      int attributes_count = read2u ();

      if ((access_flags & Modifier::STATIC) != 0) 
	fieldmap[i] = static_count++;
      else
	fieldmap[i] = instance_count--;

      for (int j = 0; j < attributes_count; ++j)
	{
	  skip (2);
	  int length = read4 ();
	  skip (length);
	}
    }
  pos = save_pos;

  // In the loop above, instance fields are represented by negative
  // numbers.  Here we rewrite these to be proper offsets.
  for (int i = 0; i < fields_count; ++i)
    {
      if (fieldmap[i] < 0)
	fieldmap[i] = static_count - 1 - fieldmap[i];
    }
  def->static_field_count = static_count;

  for (int i = 0; i < fields_count; i++)
    {
      int access_flags     = read2u ();
      int name_index       = read2u ();
      int descriptor_index = read2u ();
      int attributes_count = read2u ();

      check_tag (name_index, JV_CONSTANT_Utf8);
      prepare_pool_entry (name_index, JV_CONSTANT_Utf8);

      check_tag (descriptor_index, JV_CONSTANT_Utf8);
      prepare_pool_entry (descriptor_index, JV_CONSTANT_Utf8);

      handleField (i, access_flags, name_index, descriptor_index, fieldmap);

      bool found_value = false;
      for (int j = 0; j < attributes_count; j++)
	{
	  read_one_field_attribute (fieldmap[i], &found_value);
	}
    }
}

bool
_Jv_ClassReader::is_attribute_name (int index, const char *name)
{
  check_tag (index, JV_CONSTANT_Utf8);
  int len = get2u (bytes+offsets[index]);
  if (len != (int) strlen (name))
    return false;
  else
    return !memcmp (bytes+offsets[index]+2, name, len);
}

// Get a UTF8 value from the constant pool and turn it into a garbage
// collected char array.
int _Jv_ClassReader::pool_Utf8_to_char_arr (int index, char** entry)
{
  check_tag (index, JV_CONSTANT_Utf8);
  int len = get2u (bytes + offsets[index]);
  *entry = reinterpret_cast<char *> (_Jv_AllocBytes (len + 1));
  (*entry)[len] = '\0';
  memcpy (*entry, bytes + offsets[index] + 2, len);
  return len + 1;
}

void _Jv_ClassReader::read_one_field_attribute (int field_index,
						bool *found_value)
{
  int name = read2u ();
  int length = read4 ();

  if (is_attribute_name (name, "ConstantValue"))
    {
      int cv = read2u ();

      if (cv < pool_count 
	  && cv > 0
	  && (tags[cv] == JV_CONSTANT_Integer
	      || tags[cv] == JV_CONSTANT_Float
	      || tags[cv] == JV_CONSTANT_Long
	      || tags[cv] == JV_CONSTANT_Double
	      || tags[cv] == JV_CONSTANT_String))
	{
	  handleConstantValueAttribute (field_index, cv, found_value);
	}
      else
	{
	  throw_class_format_error ("erroneous ConstantValue attribute");
	}

      if (length != 2) 
	throw_class_format_error ("erroneous ConstantValue attribute");
    }
  else if (is_attribute_name (name, "Signature"))
    handleGenericSignature(JV_FIELD_ATTR, field_index, length);
  else if (is_attribute_name (name, "RuntimeVisibleAnnotations"))
    handleMemberAnnotations(JV_FIELD_ATTR, field_index, length);
  else
    skip (length);
}

void _Jv_ClassReader::read_methods ()
{
  int methods_count = read2u ();
  
  handleMethodsBegin (methods_count);
  
  for (int i = 0; i < methods_count; i++)
    {
      int access_flags     = read2u ();
      int name_index       = read2u ();
      int descriptor_index = read2u ();
      int attributes_count = read2u ();
      
      check_tag (name_index, JV_CONSTANT_Utf8);
      prepare_pool_entry (name_index, JV_CONSTANT_Utf8);

      check_tag (descriptor_index, JV_CONSTANT_Utf8);
      prepare_pool_entry (descriptor_index, JV_CONSTANT_Utf8);

      handleMethod (i, access_flags, name_index,
		    descriptor_index);

      for (int j = 0; j < attributes_count; j++)
	{
	  read_one_method_attribute (i);
	}
    }
  
  handleMethodsEnd ();
}

void _Jv_ClassReader::read_one_method_attribute (int method_index) 
{
  int name = read2u ();
  int length = read4 ();

  if (is_attribute_name (name, "Exceptions"))
    {
      _Jv_Method *method = reinterpret_cast<_Jv_Method *>
	(&def->methods[method_index]);
      if (method->throws != NULL)
	throw_class_format_error ("only one Exceptions attribute allowed per method");

      int num_exceptions = read2u ();
      _Jv_Utf8Const **exceptions =
	(_Jv_Utf8Const **) _Jv_AllocBytes ((num_exceptions + 1)
					   * sizeof (_Jv_Utf8Const *));

      int out = 0;
      _Jv_word *pool_data = def->constants.data;
      for (int i = 0; i < num_exceptions; ++i)
	{
	  int ndx = read2u ();
	  // JLS 2nd Ed. 4.7.5 requires that the tag not be 0.
	  if (ndx != 0)
	    {
	      check_tag (ndx, JV_CONSTANT_Class);
	      exceptions[out++] = pool_data[ndx].utf8; 
	    }
	}
      exceptions[out] = NULL;
      method->throws = exceptions;
    }

  else if (is_attribute_name (name, "Code"))
    {
      int start_off = pos;
      int max_stack = read2u ();
      int max_locals = read2u ();
      int code_length = read4 ();

      int code_start = pos;
      skip (code_length);
      int exception_table_length = read2u ();

      handleCodeAttribute (method_index, 
			   max_stack, max_locals,
			   code_start, code_length,
			   exception_table_length);
      

      for (int i = 0; i < exception_table_length; i++)
	{
	  int start_pc   = read2u ();
	  int end_pc     = read2u ();
	  int handler_pc = read2u ();
	  int catch_type = read2u ();

	  if (start_pc > end_pc
	      || start_pc < 0
	      // END_PC can be equal to CODE_LENGTH.
	      // See JVM Spec 4.7.4.
	      || end_pc > code_length
	      || handler_pc >= code_length)
	    throw_class_format_error ("erroneous exception handler info");

	  if (! (tags[catch_type] == JV_CONSTANT_Class
		 || tags[catch_type] == 0))
	    {
	      throw_class_format_error ("erroneous exception handler info");
	    }

	  handleExceptionTableEntry (method_index,
				     i,
				     start_pc,
				     end_pc,
				     handler_pc, 
				     catch_type);

	}

      int attributes_count = read2u ();

      for (int i = 0; i < attributes_count; i++)
	{
	  read_one_code_attribute (method_index);
	}

      if ((pos - start_off) != length)
	throw_class_format_error ("code attribute too short");
    }
  else if (is_attribute_name (name, "Signature"))
    handleGenericSignature(JV_METHOD_ATTR, method_index, length);
  else if (is_attribute_name (name, "RuntimeVisibleAnnotations"))
    handleMemberAnnotations(JV_METHOD_ATTR, method_index, length);
  else if (is_attribute_name (name, "RuntimeVisibleParameterAnnotations"))
    handleParameterAnnotations(method_index, length);
  else if (is_attribute_name (name, "AnnotationDefault"))
    handleAnnotationDefault(method_index, length);
  else
    {
      /* ignore unknown attributes */
      skip (length);
    }
}

void _Jv_ClassReader::read_one_code_attribute (int method_index) 
{
  int name = read2u ();
  int length = read4 ();
  if (is_attribute_name (name, "LineNumberTable"))
    {
      _Jv_InterpMethod *method = reinterpret_cast<_Jv_InterpMethod *>
	(def_interp->interpreted_methods[method_index]);
      if (method->line_table != NULL)
	throw_class_format_error ("Method already has LineNumberTable");

      int table_len = read2u ();
      _Jv_LineTableEntry* table
	= (_Jv_LineTableEntry *) _Jv_AllocBytes (table_len
						 * sizeof (_Jv_LineTableEntry));
      for (int i = 0; i < table_len; i++)
       {
	 table[i].bytecode_pc = read2u ();
	 table[i].line = read2u ();
       }
      method->line_table_len = table_len;
      method->line_table = table;
    }
  else if (is_attribute_name (name, "LocalVariableTable"))
    {
      _Jv_InterpMethod *method = reinterpret_cast<_Jv_InterpMethod *>
	                       (def_interp->interpreted_methods[method_index]);
      if (method->local_var_table != NULL)
        throw_class_format_error ("Method already has LocalVariableTable");
	
      int table_len = read2u ();
      _Jv_LocalVarTableEntry *table 
        = reinterpret_cast<_Jv_LocalVarTableEntry *>
            (_Jv_AllocRawObj (table_len * sizeof (_Jv_LocalVarTableEntry)));
                               
      for (int i = 0; i < table_len; i++)
        {
          table[i].bytecode_pc = read2u ();
          table[i].length = read2u ();
          pool_Utf8_to_char_arr (read2u (), &table[i].name);
          pool_Utf8_to_char_arr (read2u (), &table[i].descriptor);
          table[i].slot = read2u ();
          
          if (table[i].slot > method->max_locals || table[i].slot < 0)
            throw_class_format_error ("Malformed Local Variable Table: Invalid Slot");
        }
	    
      method->local_var_table_len = table_len;
      method->local_var_table = table;
    }
  else
    {
      /* ignore unknown code attributes */
      skip (length);
    }
}

void _Jv_ClassReader::read_one_class_attribute () 
{
  int name = read2u ();
  int length = read4 ();
  if (is_attribute_name (name, "SourceFile"))
    {
      int source_index = read2u ();
      check_tag (source_index, JV_CONSTANT_Utf8);
      prepare_pool_entry (source_index, JV_CONSTANT_Utf8, false);
      def_interp->source_file_name = _Jv_NewStringUtf8Const
	(def->constants.data[source_index].utf8);
    }
  else if (is_attribute_name (name, "Signature"))
    handleGenericSignature(JV_CLASS_ATTR, 0, length);
  else if (is_attribute_name (name, "EnclosingMethod"))
    handleEnclosingMethod(length);
  else if (is_attribute_name (name, "RuntimeVisibleAnnotations"))
    handleMemberAnnotations(JV_CLASS_ATTR, 0, length);
  else if (is_attribute_name (name, "InnerClasses"))
    {
      ::java::io::DataOutputStream *stream = get_reflection_stream ();
      stream->writeByte(JV_CLASS_ATTR);
      stream->writeInt(length + 1);
      stream->writeByte(JV_INNER_CLASSES_KIND);
      stream->write(input_data, input_offset + pos, length);
      skip (length);
    }
  else
    {
      /* Currently, we ignore most class attributes. */
     skip (length);
    }
}




/* this section defines the semantic actions of the parser */

void _Jv_ClassReader::handleConstantPool ()
{
  /** now, we actually define the class' constant pool */

  jbyte *pool_tags = (jbyte*) _Jv_AllocBytes (pool_count);
  _Jv_word *pool_data
    = (_Jv_word*) _Jv_AllocRawObj (pool_count * sizeof (_Jv_word));

  def->constants.tags = pool_tags;
  def->constants.data = pool_data;
  def->constants.size = pool_count;

  // Here we make a pass to collect the strings!   We do this, because
  // internally in the GCJ runtime, classes are encoded with .'s not /'s. 
  // Therefore, we first collect the strings, and then translate the rest
  // of the utf8-entries (thus not representing strings) from /-notation
  // to .-notation.
  for (int i = 1; i < pool_count; i++)
    {
      if (tags[i] == JV_CONSTANT_String)
	{
	  unsigned char* str_data = bytes + offsets [i];
	  int utf_index = get2u (str_data);
	  check_tag (utf_index, JV_CONSTANT_Utf8);
	  unsigned char *utf_data = bytes + offsets[utf_index];
	  int len = get2u (utf_data);
	  pool_data[i].utf8 = _Jv_makeUtf8Const ((char*)(utf_data+2), len);
	  pool_tags[i] = JV_CONSTANT_String;
	}
      else
	{
	  pool_tags[i] = JV_CONSTANT_Undefined;
	}
    }

  // and now, we scan everything else but strings & utf8-entries.  This
  // leaves out those utf8-entries which are not used; which will be left
  // with a tag of JV_CONSTANT_Undefined in the class definition.
  for (int index = 1; index < pool_count; index++)
    {
      switch (tags[index])
	{
	case JV_CONSTANT_Undefined:
	case JV_CONSTANT_String:
	case JV_CONSTANT_Utf8:
	  continue;
	  
	default:
	  prepare_pool_entry (index, tags[index]);
	}
    }  
  
}

/* this is a recursive procedure, which will prepare pool entries as needed.
   Which is how we avoid initializing those entries which go unused. 
   
   REWRITE is true iff this pool entry is the Utf8 representation of a
   class name or a signature.
*/

void
_Jv_ClassReader::prepare_pool_entry (int index, unsigned char this_tag,
				     bool rewrite)
{
  /* these two, pool_data and pool_tags, point into the class
     structure we are currently defining */

  unsigned char *pool_tags = (unsigned char*) def->constants.tags;
  _Jv_word      *pool_data = def->constants.data;

  /* this entry was already prepared */
  if (pool_tags[index] == this_tag)
    return;

  /* this_data points to the constant-pool information for the current
     constant-pool entry */

  unsigned char *this_data = bytes + offsets[index];

  switch (this_tag)
    {
    case JV_CONSTANT_Utf8: 
      {
	int len = get2u (this_data);
	char *s = ((char*) this_data)+2;
	pool_tags[index] = JV_CONSTANT_Utf8;

	if (! rewrite)
	  {
	    pool_data[index].utf8 = _Jv_makeUtf8Const (s, len);
	    break;
	  }

	// If REWRITE is set, it is because some other tag needs this
	// utf8-entry for type information: it is a class or a
	// signature.  Thus, we translate /'s to .'s in order to
	// accomondate gcj's internal representation.
	char *buffer = (char*) __builtin_alloca (len);
	for (int i = 0; i < len; i++)
	  {
	    if (s[i] == '/')
	      buffer[i] = '.';
	    else
	      buffer[i] = s[i];
	  }
	pool_data[index].utf8 = _Jv_makeUtf8Const (buffer, len);
      }
      break;
	    
    case JV_CONSTANT_Class:      
      {
	int utf_index = get2u (this_data);
	check_tag (utf_index, JV_CONSTANT_Utf8);
	prepare_pool_entry (utf_index, JV_CONSTANT_Utf8);

	if (verify)
	  verify_classname (pool_data[utf_index].utf8);
		
	pool_data[index].utf8 = pool_data[utf_index].utf8;
	pool_tags[index] = JV_CONSTANT_Class;
      }
      break;
	    
    case JV_CONSTANT_String:
      // already handled before... 
      break;
	    
    case JV_CONSTANT_Fieldref:
    case JV_CONSTANT_Methodref:
    case JV_CONSTANT_InterfaceMethodref:
      {
	int class_index = get2u (this_data);
	int nat_index = get2u (this_data+2);

	check_tag (class_index, JV_CONSTANT_Class);
	prepare_pool_entry (class_index, JV_CONSTANT_Class);	    

	check_tag (nat_index, JV_CONSTANT_NameAndType);
	prepare_pool_entry (nat_index, JV_CONSTANT_NameAndType);

	// here, verify the signature and identifier name
	if (verify)
	{
	  _Jv_ushort name_index, type_index;
	  _Jv_loadIndexes (&pool_data[nat_index],
			   name_index, type_index);

	  if (this_tag == JV_CONSTANT_Fieldref)
	    verify_field_signature (pool_data[type_index].utf8);
	  else
	    verify_method_signature (pool_data[type_index].utf8);

	  _Jv_Utf8Const* name = pool_data[name_index].utf8;

	  if (this_tag != JV_CONSTANT_Fieldref
	      && (   _Jv_equalUtf8Consts (name, clinit_name)
		  || _Jv_equalUtf8Consts (name, init_name)))
	    /* ignore */;
	  else
	    verify_identifier (pool_data[name_index].utf8);
	}
	    
	_Jv_storeIndexes (&pool_data[index], class_index, nat_index);
	pool_tags[index] = this_tag;
      }
      break;
	    
    case JV_CONSTANT_NameAndType:
      {
	_Jv_ushort name_index = get2u (this_data);
	_Jv_ushort type_index = get2u (this_data+2);

	check_tag (name_index, JV_CONSTANT_Utf8);
	prepare_pool_entry (name_index, JV_CONSTANT_Utf8, false);
	check_tag (type_index, JV_CONSTANT_Utf8);
	prepare_pool_entry (type_index, JV_CONSTANT_Utf8);

	_Jv_storeIndexes (&pool_data[index], name_index, type_index);
	pool_tags[index] = JV_CONSTANT_NameAndType;
      }
      break;
	    
    case JV_CONSTANT_Float:
      {
	jfloat f = java::lang::Float::intBitsToFloat ((jint) get4 (this_data));
	_Jv_storeFloat (&pool_data[index], f);
	pool_tags[index] = JV_CONSTANT_Float;
      }
      break;
	    
    case JV_CONSTANT_Integer:
      {
	int i = get4 (this_data);
	_Jv_storeInt (&pool_data[index], i);
	pool_tags[index] = JV_CONSTANT_Integer;
      }
      break;
	    
    case JV_CONSTANT_Double:
      {
	jdouble d
	  = java::lang::Double::longBitsToDouble ((jlong) get8 (this_data));
	_Jv_storeDouble (&pool_data[index], d);
	pool_tags[index] = JV_CONSTANT_Double;
      }
      break;
	    
    case JV_CONSTANT_Long:
      {
	jlong i = get8 (this_data);
	_Jv_storeLong (&pool_data[index], i);
	pool_tags[index] = JV_CONSTANT_Long;
      }
      break;
	    
    default:
      throw_class_format_error ("erroneous constant pool tag");
    }
}


void
_Jv_ClassReader::handleClassBegin (int access_flags, int this_class, int super_class)
{
  using namespace java::lang::reflect;

  unsigned char *pool_tags = (unsigned char*) def->constants.tags;
  _Jv_word      *pool_data = def->constants.data;

  check_tag (this_class, JV_CONSTANT_Class);
  _Jv_Utf8Const *loadedName = pool_data[this_class].utf8;

  // was ClassLoader.defineClass called with an expected class name?
  if (def->name == 0)
    {
      jclass orig = def->loader->findLoadedClass(loadedName->toString());

      if (orig == 0)
	{
	  def->name = loadedName;
	}
      else
	{
	  jstring msg = JvNewStringUTF ("anonymous "
					"class data denotes "
					"existing class ");
	  msg = msg->concat (orig->getName ());

	  throw_no_class_def_found_error (msg);
	}
    }

  // assert that the loaded class has the expected name, 5.3.5
  else if (! _Jv_equalUtf8Consts (loadedName, def->name))
    {
      jstring msg = JvNewStringUTF ("loaded class ");
      msg = msg->concat (def->getName ());
      msg = msg->concat (_Jv_NewStringUTF (" was in fact named "));
      jstring klass_name = loadedName->toString();
      msg = msg->concat (klass_name);

      throw_no_class_def_found_error (msg);
    }

  def->accflags = access_flags | java::lang::reflect::Modifier::INTERPRETED;
  pool_data[this_class].clazz = def;
  pool_tags[this_class] = JV_CONSTANT_ResolvedClass;

  if (super_class == 0)
    {
      // Note that this is ok if we are defining java.lang.Object.
      // But there is no way to have this class be interpreted.
      throw_class_format_error ("no superclass reference");
    }

  def->state = JV_STATE_PRELOADING;

  // Register this class with its defining loader as well (despite the
  // name of the function we're calling), so that super class lookups
  // work properly.  If there is an error, our caller will unregister
  // this class from the class loader.  Also, we don't need to hold a
  // lock here, as our caller has acquired it.
  _Jv_RegisterInitiatingLoader (def, def->loader);

  // Note that we found a name so that unregistration can happen if
  // needed.
  *found_name = def->name;

  if (super_class != 0)
    {
      // Load the superclass.
      check_tag (super_class, JV_CONSTANT_Class);
      _Jv_Utf8Const* super_name = pool_data[super_class].utf8; 

      // Load the superclass using our defining loader.
      jclass the_super = _Jv_FindClass (super_name, def->loader);

      // This will establish that we are allowed to be a subclass,
      // and check for class circularity error.
      checkExtends (def, the_super);

      // Note: for an interface we will find Object as the
      // superclass.  We still check it above to ensure class file
      // validity, but we simply assign `null' to the actual field in
      // this case.
      def->superclass = (((access_flags & Modifier::INTERFACE))
			 ? NULL : the_super);
      pool_data[super_class].clazz = the_super;
      pool_tags[super_class] = JV_CONSTANT_ResolvedClass;
    }

  // Now we've come past the circularity problem, we can 
  // now say that we're loading.

  def->state = JV_STATE_LOADING;
  def->notifyAll ();
}

///// Implements the checks described in sect. 5.3.5.3
void
_Jv_ClassReader::checkExtends (jclass sub, jclass super)
{
  using namespace java::lang::reflect;

  _Jv_Linker::wait_for_state (super, JV_STATE_LOADING);

  // Having an interface or a final class as a superclass is no good.
  if ((super->accflags & (Modifier::INTERFACE | Modifier::FINAL)) != 0)
    {
      throw_incompatible_class_change_error (sub->getName ());
    }

  // If the super class is not public, we need to check some more.
  if ((super->accflags & Modifier::PUBLIC) == 0)
    {
      // With package scope, the classes must have the same class
      // loader.
      if (   sub->loader != super->loader
	  || !_Jv_ClassNameSamePackage (sub->name, super->name))
	{
	  throw_incompatible_class_change_error (sub->getName ());
	}
    } 

  for (; super != 0; super = super->getSuperclass ())
    {
      if (super == sub)
	throw_class_circularity_error (sub->getName ());
    }
}



void _Jv_ClassReader::handleInterfacesBegin (int count)
{
  def->interfaces = (jclass*) _Jv_AllocRawObj (count*sizeof (jclass));
  def->interface_count = count;
}

void _Jv_ClassReader::handleInterface (int if_number, int offset)
{
  _Jv_word       * pool_data = def->constants.data;
  unsigned char  * pool_tags = (unsigned char*) def->constants.tags;

  jclass the_interface;

  if (pool_tags[offset] == JV_CONSTANT_Class)
    {
      _Jv_Utf8Const* name = pool_data[offset].utf8;
      the_interface =  _Jv_FindClass (name, def->loader);
    }
  else if (pool_tags[offset] == JV_CONSTANT_ResolvedClass)
    {
      the_interface = pool_data[offset].clazz;
    }
  else
    {
      throw_no_class_def_found_error ("erroneous constant pool tag");
    }

  // checks the validity of the_interface, and that we are in fact
  // allowed to implement that interface.
  checkImplements (def, the_interface);
  
  pool_data[offset].clazz = the_interface;
  pool_tags[offset] = JV_CONSTANT_ResolvedClass;
  
  def->interfaces[if_number] = the_interface;
}

void
_Jv_ClassReader::checkImplements (jclass sub, jclass super)
{
  using namespace java::lang::reflect;

  // well, it *must* be an interface
  if ((super->accflags & Modifier::INTERFACE) == 0)
    {
      throw_incompatible_class_change_error (sub->getName ());
    }

  // if it has package scope, it must also be defined by the 
  // same loader.
  if ((super->accflags & Modifier::PUBLIC) == 0)
    {
      if (    sub->loader != super->loader
	  || !_Jv_ClassNameSamePackage (sub->name, super->name))
	{
	  throw_incompatible_class_change_error (sub->getName ());
	}
    } 

  // FIXME: add interface circularity check here
  if (sub == super)
    {
      throw_class_circularity_error (sub->getName ());
    }		
}

void _Jv_ClassReader::handleFieldsBegin (int count)
{
  def->fields = (_Jv_Field*) _Jv_AllocRawObj (count * sizeof (_Jv_Field));
  def->field_count = count;
  def_interp->field_initializers
    = (_Jv_ushort*) _Jv_AllocRawObj (count * sizeof (_Jv_ushort));
  for (int i = 0; i < count; i++)
    def_interp->field_initializers[i] = (_Jv_ushort) 0;
}

void _Jv_ClassReader::handleField (int field_no,
				   int flags,
				   int name,
				   int desc,
				   int *fieldmap)
{
  using namespace java::lang::reflect;

  _Jv_word *pool_data = def->constants.data;

  _Jv_Field *field = &def->fields[fieldmap[field_no]];
  _Jv_Utf8Const *field_name = pool_data[name].utf8;

  field->name      = field_name;

  // Ignore flags we don't know about.  
  field->flags = flags & (Field::FIELD_MODIFIERS
			  | Modifier::SYNTHETIC
			  | Modifier::ENUM);

  _Jv_Utf8Const* sig = pool_data[desc].utf8;

  if (verify)
    {
      verify_identifier (field_name);

      for (int i = 0; i < field_no; ++i)
	{
	  if (_Jv_equalUtf8Consts (field_name, def->fields[fieldmap[i]].name)
	      && _Jv_equalUtf8Consts (sig,
				      // We know the other fields are
				      // unresolved.
				      (_Jv_Utf8Const *) def->fields[i].type))
	    throw_class_format_error ("duplicate field name");
	}

      // At most one of PUBLIC, PRIVATE, or PROTECTED is allowed.
      if (1 < ( ((field->flags & Modifier::PUBLIC) ? 1 : 0)
		+((field->flags & Modifier::PRIVATE) ? 1 : 0)
		+((field->flags & Modifier::PROTECTED) ? 1 : 0)))
	throw_class_format_error ("erroneous field access flags");

      // FIXME: JVM spec S4.5: Verify ACC_FINAL and ACC_VOLATILE are not 
      // both set. Verify modifiers for interface fields.
      
    }

  if (verify)
    verify_field_signature (sig);

  // field->type is really a jclass, but while it is still
  // unresolved we keep an _Jv_Utf8Const* instead.
  field->type       = (jclass) sig;
  field->flags     |= _Jv_FIELD_UNRESOLVED_FLAG;
  field->u.boffset  = 0;
}


void _Jv_ClassReader::handleConstantValueAttribute (int field_index, 
						    int value,
						    bool *found_value)
{
  using namespace java::lang::reflect;

  _Jv_Field *field = &def->fields[field_index];

  if ((field->flags & (Modifier::STATIC
		       | Modifier::FINAL
		       | Modifier::PRIVATE)) == 0)
    {
      // Ignore, as per vmspec #4.7.2
      return;
    }

  // do not allow multiple constant fields!
  if (*found_value)
    throw_class_format_error ("field has multiple ConstantValue attributes");

  *found_value = true;
  def_interp->field_initializers[field_index] = value;

  /* type check the initializer */
  
  if (value <= 0 || value >= pool_count)
    throw_class_format_error ("erroneous ConstantValue attribute");

  /* FIXME: do the rest */
}

void
_Jv_ClassReader::handleMethodsBegin (int count)
{
  def->methods = (_Jv_Method *) _Jv_AllocRawObj (sizeof (_Jv_Method) * count);

  def_interp->interpreted_methods
    = (_Jv_MethodBase **) _Jv_AllocRawObj (sizeof (_Jv_MethodBase *)
					   * count);

  for (int i = 0; i < count; i++)
    {
      def_interp->interpreted_methods[i] = 0;
      def->methods[i].index = (_Jv_ushort) -1;
    }

  def->method_count = count;
}


void _Jv_ClassReader::handleMethod 
    (int mth_index, int accflags, int name, int desc)
{ 
  using namespace java::lang::reflect;

  _Jv_word *pool_data = def->constants.data;
  _Jv_Method *method = &def->methods[mth_index];

  check_tag (name, JV_CONSTANT_Utf8);
  prepare_pool_entry (name, JV_CONSTANT_Utf8, false);
  method->name = pool_data[name].utf8;

  check_tag (desc, JV_CONSTANT_Utf8);
  prepare_pool_entry (desc, JV_CONSTANT_Utf8);
  method->signature = pool_data[desc].utf8;

  // ignore unknown flags
  method->accflags = accflags & (Method::METHOD_MODIFIERS
				 | Modifier::BRIDGE
				 | Modifier::SYNTHETIC
				 | Modifier::VARARGS);

  // Initialize...
  method->ncode = 0;
  method->throws = NULL;
  
  if (verify)
    {
      if (_Jv_equalUtf8Consts (method->name, clinit_name)
	  || _Jv_equalUtf8Consts (method->name, init_name))
	/* ignore */;
      else
	verify_identifier (method->name);

      verify_method_signature (method->signature);

      for (int i = 0; i < mth_index; ++i)
	{
	  if (_Jv_equalUtf8Consts (method->name, def->methods[i].name)
	      && _Jv_equalUtf8Consts (method->signature,
				      def->methods[i].signature))
	    throw_class_format_error ("duplicate method");
	}

      // At most one of PUBLIC, PRIVATE, or PROTECTED is allowed.
      if (1 < ( ((method->accflags & Modifier::PUBLIC) ? 1 : 0)
		+((method->accflags & Modifier::PRIVATE) ? 1 : 0)
		+((method->accflags & Modifier::PROTECTED) ? 1 : 0)))
	throw_class_format_error ("erroneous method access flags");

      // FIXME: JVM spec S4.6: if ABSTRACT modifier is set, verify other 
      // flags are not set. Verify flags for interface methods.  Verify
      // modifiers for initializers. 
    }
}

void _Jv_ClassReader::handleCodeAttribute
  (int method_index, int max_stack, int max_locals, 
   int code_start, int code_length, int exc_table_length)
{
  int size = _Jv_InterpMethod::size (exc_table_length, code_length);
  _Jv_InterpMethod *method = 
    (_Jv_InterpMethod*) (_Jv_AllocRawObj (size));

  method->max_stack      = max_stack;
  method->max_locals     = max_locals;
  method->code_length    = code_length;
  method->exc_count      = exc_table_length;
  method->is_15          = is_15;
  method->defining_class = def;
  method->self           = &def->methods[method_index];
  method->prepared       = NULL;
  method->line_table_len = 0;
  method->line_table     = NULL;
#ifdef DIRECT_THREADED
  method->thread_count   = 0;
#endif

  // grab the byte code!
  memcpy ((void*) method->bytecode (),
	  (void*) (bytes+code_start),
	  code_length);

  def_interp->interpreted_methods[method_index] = method;

  if ((method->self->accflags & java::lang::reflect::Modifier::STATIC))
    {
      // Precompute the ncode field for a static method.  This lets us
      // call a static method of an interpreted class from precompiled
      // code without first resolving the class (that will happen
      // during class initialization instead).
      method->self->ncode = method->ncode (def);
    }
}

void _Jv_ClassReader::handleExceptionTableEntry
  (int method_index, int exc_index, 
   int start_pc, int end_pc, int handler_pc, int catch_type)
{
  _Jv_InterpMethod *method = reinterpret_cast<_Jv_InterpMethod *>
    (def_interp->interpreted_methods[method_index]);
  _Jv_InterpException *exc = method->exceptions ();

  exc[exc_index].start_pc.i     = start_pc;
  exc[exc_index].end_pc.i       = end_pc;
  exc[exc_index].handler_pc.i   = handler_pc;
  exc[exc_index].handler_type.i = catch_type;
}

void _Jv_ClassReader::handleMethodsEnd ()
{
  using namespace java::lang::reflect;

  for (int i = 0; i < def->method_count; i++)
    {
      _Jv_Method *method = &def->methods[i];
      if ((method->accflags & Modifier::NATIVE) != 0)
	{
	  if (def_interp->interpreted_methods[i] != 0)
	    throw_class_format_error ("code provided for native method");
	  else
	    {
	      _Jv_JNIMethod *m = (_Jv_JNIMethod *)
		_Jv_AllocRawObj (sizeof (_Jv_JNIMethod));
	      m->defining_class = def;
	      m->self = method;
	      m->function = NULL;
	      def_interp->interpreted_methods[i] = m;

	      if ((method->accflags & Modifier::STATIC))
		{
		  // Precompute the ncode field for a static method.
		  // This lets us call a static method of an
		  // interpreted class from precompiled code without
		  // first resolving the class (that will happen
		  // during class initialization instead).
		  method->ncode = m->ncode (def);
		}
	    }
	}
      else if ((method->accflags & Modifier::ABSTRACT) != 0)
	{
	  if (def_interp->interpreted_methods[i] != 0)
	    throw_class_format_error ("code provided for abstract method");
	  method->ncode = (void *) &_Jv_ThrowAbstractMethodError;
	}
      else
	{
	  if (def_interp->interpreted_methods[i] == 0)
	    throw_class_format_error ("method with no code");
	}
    }
}

void _Jv_ClassReader::throw_class_format_error (const char *msg)
{
  jstring str;
  if (def->name != NULL)
    {
      jsize mlen = strlen (msg);
      unsigned char* data = (unsigned char*) def->name->chars();
      int ulen = def->name->len();
      unsigned char* limit = data + ulen;
      jsize nlen = _Jv_strLengthUtf8 ((char *) data, ulen);
      jsize len = nlen + mlen + 3;
      str = JvAllocString(len);
      jchar *chrs = JvGetStringChars(str);
      while (data < limit)
	*chrs++ = UTF8_GET(data, limit);
      *chrs++ = ' ';
      *chrs++ = '(';
      for (;;)
	{
	  char c = *msg++;
	  if (c == 0)
	    break;
	  *chrs++ = c & 0xFFFF;
	}
      *chrs++ = ')';
    }
  else
    str = JvNewStringLatin1 (msg);
  ::throw_class_format_error (str);
}

/** Here we define the exceptions that can be thrown */

static void
throw_no_class_def_found_error (jstring msg)
{
  throw (msg
	 ? new java::lang::NoClassDefFoundError (msg)
	 : new java::lang::NoClassDefFoundError);
}

static void
throw_no_class_def_found_error (const char *msg)
{
  throw_no_class_def_found_error (JvNewStringLatin1 (msg));
}

static void
throw_class_format_error (jstring msg)
{
  throw (msg
	 ? new java::lang::ClassFormatError (msg)
	 : new java::lang::ClassFormatError);
}

static void
throw_internal_error (const char *msg)
{
  throw new java::lang::InternalError (JvNewStringLatin1 (msg));
}

static void
throw_incompatible_class_change_error (jstring msg)
{
  throw new java::lang::IncompatibleClassChangeError (msg);
}

static void
throw_class_circularity_error (jstring msg)
{
  throw new java::lang::ClassCircularityError (msg);
}

#endif /* INTERPRETER */



/** This section takes care of verifying integrity of identifiers,
    signatures, field ddescriptors, and class names */

#define UTF8_PEEK(PTR, LIMIT) \
  ({ unsigned char* xxkeep = (PTR); \
     int xxch = UTF8_GET(PTR,LIMIT); \
     PTR = xxkeep; xxch; })

/* Verify one element of a type descriptor or signature.  */
static unsigned char*
_Jv_VerifyOne (unsigned char* ptr, unsigned char* limit, bool void_ok)
{
  if (ptr >= limit)
    return 0;

  int ch = UTF8_GET (ptr, limit);

  switch (ch)
    {
    case 'V':
      if (! void_ok)
	return 0;

    case 'S': case 'B': case 'I': case 'J':
    case 'Z': case 'C': case 'F': case 'D': 
      break;

    case 'L':
      {
	unsigned char *start = ptr, *end;
	do
	  {
	    if (ptr > limit)
	      return 0;

	    end = ptr;

	    if ((ch = UTF8_GET (ptr, limit)) == -1)
	      return 0;

	  }
	while (ch != ';');
	if (! _Jv_VerifyClassName (start, (unsigned short) (end-start)))
	  return 0;
      }
      break;

    case '[':
      return _Jv_VerifyOne (ptr, limit, false);
      break;

    default:
      return 0;
    }

  return ptr;
}

/* Verification and loading procedures.  */
bool
_Jv_VerifyFieldSignature (_Jv_Utf8Const*sig)
{
  unsigned char* ptr = (unsigned char*) sig->chars();
  unsigned char* limit = ptr + sig->len();

  ptr = _Jv_VerifyOne (ptr, limit, false);

  return ptr == limit;
}

bool
_Jv_VerifyMethodSignature (_Jv_Utf8Const*sig)
{
  unsigned char* ptr = (unsigned char*) sig->chars();
  unsigned char* limit = ptr + sig->len();

  if (ptr == limit || UTF8_GET(ptr,limit) != '(')
    return false;

  while (ptr && UTF8_PEEK (ptr, limit) != ')')
    ptr = _Jv_VerifyOne (ptr, limit, false);

  if (! ptr || UTF8_GET (ptr, limit) != ')')
    return false;

  // get the return type
  ptr = _Jv_VerifyOne (ptr, limit, true);

  return ptr == limit;
}

/* We try to avoid calling the Character methods all the time, in
   fact, they will only be called for non-standard things. */
static __inline__ int 
is_identifier_start (int c)
{
  unsigned int ch = (unsigned)c;

  if ((ch - 0x41U) < 29U) 		/* A ... Z */
    return 1;
  if ((ch - 0x61U) < 29U) 		/* a ... z */
    return 1;
  if (ch == 0x5FU)       		/* _ */
    return 1;

  return java::lang::Character::isJavaIdentifierStart ((jchar) ch);
}

static __inline__ int 
is_identifier_part (int c)
{
  unsigned int ch = (unsigned)c;

  if ((ch - 0x41U) < 29U) 		/* A ... Z */
    return 1;
  if ((ch - 0x61U) < 29U) 		/* a ... z */
    return 1;
  if ((ch - 0x30) < 10U)       		/* 0 .. 9 */
    return 1;
  if (ch == 0x5FU || ch == 0x24U)       /* _ $ */
    return 1;

  return java::lang::Character::isJavaIdentifierStart ((jchar) ch);
}

bool
_Jv_VerifyIdentifier (_Jv_Utf8Const* name)
{
  unsigned char *ptr   = (unsigned char*) name->chars();
  unsigned char *limit = (unsigned char*) name->limit();
  int ch;

  if ((ch = UTF8_GET (ptr, limit))==-1
      || ! is_identifier_start (ch))
    return false;

  while (ptr != limit)
    {
      if ((ch = UTF8_GET (ptr, limit))==-1
	  || ! is_identifier_part (ch))
	return false;
    }
  return true;
}

bool
_Jv_VerifyClassName (unsigned char* ptr, _Jv_ushort length)
{
  unsigned char *limit = ptr+length;
  int ch;

  if ('[' == UTF8_PEEK (ptr, limit))
    {
      unsigned char *end = _Jv_VerifyOne (++ptr, limit, false);
      // _Jv_VerifyOne must leave us looking at the terminating nul
      // byte.
      if (! end || *end)
	return false;
      else
        return true;
    }

 next_level:
  for (;;) {
    if ((ch = UTF8_GET (ptr, limit))==-1)
      return false;
    if (! is_identifier_start (ch))
      return false;
    for (;;) {
      if (ptr == limit)
	return true;
      else if ((ch = UTF8_GET (ptr, limit))==-1)
	return false;
      else if (ch == '.')
	goto next_level;
      else if (! is_identifier_part (ch))
	return false;
    }
  }
}

bool
_Jv_VerifyClassName (_Jv_Utf8Const *name)
{
  return _Jv_VerifyClassName ((unsigned char*)name->chars(), name->len());
}

/* Returns true, if NAME1 and NAME2 represent classes in the same
   package.  Neither NAME2 nor NAME2 may name an array type.  */
bool
_Jv_ClassNameSamePackage (_Jv_Utf8Const *name1, _Jv_Utf8Const *name2)
{
  unsigned char* ptr1 = (unsigned char*) name1->chars();
  unsigned char* limit1 = (unsigned char*) name1->limit();

  unsigned char* last1 = ptr1;

  // scan name1, and find the last occurrence of '.'
  while (ptr1 < limit1) {
    int ch1 = UTF8_GET (ptr1, limit1);

    if (ch1 == '.')
      last1 = ptr1;

    else if (ch1 == -1)
      return false;
  }

  // Now the length of NAME1's package name is LEN.
  int len = last1 - (unsigned char*) name1->chars();

  // If this is longer than NAME2, then we're off.
  if (len > name2->len())
    return false;

  // Then compare the first len bytes for equality.
  if (memcmp ((void*) name1->chars(), (void*) name2->chars(), len) == 0)
    {
      // Check that there are no .'s after position LEN in NAME2.

      unsigned char* ptr2 = (unsigned char*) name2->chars() + len;
      unsigned char* limit2 = (unsigned char*) name2->limit();

      while (ptr2 < limit2)
	{
	  int ch2 = UTF8_GET (ptr2, limit2);
	  if (ch2 == -1 || ch2 == '.')
	    return false;
	}
      return true;
    }
  return false;
}
