// defineclass.cc - defining a class from .class format.

/* Copyright (C) 1999  Red Hat, Inc.

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

#ifdef INTERPRETER

#include <java-cpool.h>
#include <gcj/cni.h>

#include <java/lang/Class.h>
#include <java/lang/Float.h>
#include <java/lang/Double.h>
#include <java/lang/Character.h>
#include <java/lang/LinkageError.h>
#include <java/lang/InternalError.h>
#include <java/lang/ClassFormatError.h>
#include <java/lang/NoClassDefFoundError.h>
#include <java/lang/ClassCircularityError.h>
#include <java/lang/ClassNotFoundException.h>
#include <java/lang/IncompatibleClassChangeError.h>
#include <java/lang/reflect/Modifier.h>

#define ClassClass _CL_Q34java4lang5Class
extern java::lang::Class ClassClass;
#define StringClass _CL_Q34java4lang6String
extern java::lang::Class StringClass;
#define ClassObject _CL_Q34java4lang6Object
extern java::lang::Class ClassObject;

// we don't verify method names that match these.
static _Jv_Utf8Const *clinit_name = _Jv_makeUtf8Const ("<clinit>", 8);
static _Jv_Utf8Const *init_name = _Jv_makeUtf8Const ("<init>", 6);


// these go in some seperate functions, to avoid having _Jv_InitClass
// inserted all over the place.
static void throw_internal_error (char *msg)
	__attribute__ ((__noreturn__));
static void throw_no_class_def_found_error (jstring msg)
	__attribute__ ((__noreturn__));
static void throw_no_class_def_found_error (char *msg)
	__attribute__ ((__noreturn__));
static void throw_class_format_error (jstring msg)
	__attribute__ ((__noreturn__));
static void throw_class_format_error (char *msg)
	__attribute__ ((__noreturn__));
static void throw_incompatible_class_change_error (jstring msg)
	__attribute__ ((__noreturn__));
static void throw_class_circularity_error (jstring msg)
	__attribute__ ((__noreturn__));

static jdouble long_bits_to_double (jlong);
static jfloat int_bits_to_float (jint);

/**
 * We define class reading using a class.  It is practical, since then
 * the entire class-reader can be a friend of class Class (it needs to
 * write all it's different structures); but also because this makes it
 * easy to make class definition reentrant, and thus two threads can be
 * defining classes at the same time.   This class (_Jv_ClassReader) is
 * never exposed outside this file, so we don't have to worry about
 * public or private members here.
 */

struct _Jv_ClassReader {

  // do verification?  Currently, there is no option to disable this.
  // This flag just controls the verificaiton done by the class loader;
  // i.e., checking the integrity of the constant pool; and it is
  // allways on.  You always want this as far as I can see, but it also
  // controls weither identifiers and type descriptors/signatures are
  // verified as legal.  This could be somewhat more expensive since it
  // will call Characher.isJavaIdentifier{Start,Part} for each character
  // in any identifier (field name or method name) it comes by.  Thus,
  // it might be useful to turn off this verification for classes that
  // come from a trusted source.  However, for GCJ, trusted classes are
  // most likely to be linked in.

  bool verify;

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
  _Jv_InterpClass   *def;

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
  
  /* read an unsignend 1-byte unit */
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

  _Jv_ClassReader (jclass klass, jbyteArray data, jint offset, jint length)
  {
    if (klass == 0 || length < 0 || offset+length > data->length)
      throw_internal_error ("arguments to _Jv_DefineClass");

    verify = true;
    bytes  = (unsigned char*) (elements (data)+offset);
    len    = length;
    pos    = 0;
    def    = (_Jv_InterpClass*) klass;
  }

  /** and here goes the parser members defined out-of-line */
  void parse ();
  void read_constpool ();
  void prepare_pool_entry (int index, unsigned char tag);
  void read_fields ();
  void read_methods ();
  void read_one_class_attribute ();
  void read_one_method_attribute (int method);
  void read_one_code_attribute (int method);
  void read_one_field_attribute (int field);

  /** check an utf8 entry, without creating a Utf8Const object */
  bool is_attribute_name (int index, char *name);

  /** here goes the class-loader members defined out-of-line */
  void handleConstantPool ();
  void handleClassBegin (int, int, int);
  void handleInterfacesBegin (int);
  void handleInterface (int, int);
  void handleFieldsBegin (int);
  void handleField (int, int, int, int);
  void handleFieldsEnd ();
  void handleConstantValueAttribute (int,int);
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

/* This is used for the isJavaIdentifierStart & isJavaIdentifierPart
   methods, so we avoid doing _Jv_InitClass all the time */

static const java::lang::Character *character = 0;
static void prepare_character ();

void
_Jv_DefineClass (jclass klass, jbyteArray data, jint offset, jint length)
{
  if (character == 0)
    prepare_character ();

  _Jv_ClassReader reader (klass, data, offset, length);
  reader.parse();

  /* that's it! */
}

/** put it after _Jv_DefineClass, so it doesn't get inlined */
static void prepare_character ()
{
  character = new java::lang::Character ('!');
}


/** This section defines the parsing/scanning of the class data */

void
_Jv_ClassReader::parse ()
{
  int magic = read4 ();

  /* FIXME: Decide which range of version numbers to allow */

  /* int minor_version = */ read2u ();
  /* int major_verson  = */ read2u ();

  if (magic != (int) 0xCAFEBABE)
    throw_class_format_error ("bad magic number");

  pool_count = read2u ();

  read_constpool ();

  int access_flags = read2u ();
  int this_class = read2u ();
  int super_class = read2u ();

  check_tag (this_class, JV_CONSTANT_Class);
  if (super_class != 0) 
    check_tag (super_class, JV_CONSTANT_Class);

  handleClassBegin (access_flags, this_class, super_class);

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

  // tell everyone we're done.
  def->state = JV_STATE_LOADED;
  def->notifyAll ();

}

void _Jv_ClassReader::read_constpool ()
{
  tags    = (unsigned char*) _Jv_AllocBytesChecked (pool_count);
  offsets = (unsigned int *) _Jv_AllocBytesChecked (sizeof (int)
						    * pool_count) ;

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
      
      handleField (i, access_flags, name_index, descriptor_index);
      
      for (int j = 0; j < attributes_count; j++)
	{
	  read_one_field_attribute (i);
	}
    }

  handleFieldsEnd ();
}

bool
_Jv_ClassReader::is_attribute_name (int index, char *name)
{
  check_tag (index, JV_CONSTANT_Utf8);
  int len = get2u (bytes+offsets[index]);
  if (len != (int) strlen (name))
    return false;
  else
    return !memcmp (bytes+offsets[index]+2, name, len);
}

void _Jv_ClassReader::read_one_field_attribute (int field_index)
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
	    handleConstantValueAttribute (field_index, cv);
	  }
	else
	  {
	    throw_class_format_error ("erroneous ConstantValue attribute");
	  }

	if (length != 2) 
	  throw_class_format_error ("erroneous ConstantValue attribute");
      }

    else
      {
	skip (length);
      }
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
      prepare_pool_entry (descriptor_index, JV_CONSTANT_Utf8);

      check_tag (name_index, JV_CONSTANT_Utf8);
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
      /* we ignore this for now */
      skip (length);
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
	      || end_pc >= code_length
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

  else
    {
      /* ignore unknown attributes */
      skip (length);
    }
}

void _Jv_ClassReader::read_one_code_attribute (int /*method*/) 
{
  /* ignore for now, ... later we may want to pick up
     line number information, for debugging purposes;
     in fact, the whole debugger issue is open!  */

  /* int name = */ read2u ();
  int length = read4 ();
  skip (length);

}

void _Jv_ClassReader::read_one_class_attribute () 
{
  /* we also ignore the class attributes, ...
     some day we'll add inner-classes support. */

  /* int name = */ read2u ();
  int length = read4 ();
  skip (length);
}




/* this section defines the semantic actions of the parser */

void _Jv_ClassReader::handleConstantPool ()
{
  /** now, we actually define the class' constant pool */

  // the pool is scanned explicitly by the collector
  jbyte *pool_tags = (jbyte*) _Jv_AllocBytesChecked (pool_count);
  _Jv_word *pool_data
    = (_Jv_word*) _Jv_AllocBytesChecked (pool_count * sizeof (_Jv_word));
  
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
   Which is how we avoid initializing those entries which go unused. */
void
_Jv_ClassReader::prepare_pool_entry (int index, unsigned char this_tag)
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
	// If we came here, it is because some other tag needs this
	// utf8-entry for type information!  Thus, we translate /'s to .'s in
	// order to accomondate gcj's internal representation.

	int len = get2u (this_data);
	char *buffer = (char*) alloca (len);
	char *s = ((char*) this_data)+2;

	/* FIXME: avoid using a buffer here */
	for (int i = 0; i < len; i++)
	  {
	    if (s[i] == '/')
	      buffer[i] = '.';
	    else
	      buffer[i] = (char) s[i];
	  }
	
	pool_data[index].utf8 = _Jv_makeUtf8Const (buffer, len);
	pool_tags[index] = JV_CONSTANT_Utf8;
      }
      break;
	    
    case JV_CONSTANT_Class:      
      {
	int utf_index = get2u (this_data);
	check_tag (utf_index, JV_CONSTANT_Utf8);
	prepare_pool_entry (utf_index, JV_CONSTANT_Utf8);

	if (verify)
	  _Jv_VerifyClassName (pool_data[utf_index].utf8);
		
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
	    _Jv_VerifyFieldSignature (pool_data[type_index].utf8);
	  else
	    _Jv_VerifyMethodSignature (pool_data[type_index].utf8);

	  _Jv_Utf8Const* name = pool_data[name_index].utf8;

	  if (this_tag != JV_CONSTANT_Fieldref
	      && (   _Jv_equalUtf8Consts (name, clinit_name)
		  || _Jv_equalUtf8Consts (name, init_name)))
	    /* ignore */;
	  else
	    _Jv_VerifyIdentifier (pool_data[name_index].utf8);
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
	prepare_pool_entry (name_index, JV_CONSTANT_Utf8);	    

	check_tag (type_index, JV_CONSTANT_Utf8);
	prepare_pool_entry (type_index, JV_CONSTANT_Utf8);

	_Jv_storeIndexes (&pool_data[index], name_index, type_index);
	pool_tags[index] = JV_CONSTANT_NameAndType;
      }
      break;
	    
    case JV_CONSTANT_Float:
      {
	jfloat f = int_bits_to_float ((jint) get4 (this_data));
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
	jdouble d = long_bits_to_double ((jlong) get8 (this_data));
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
_Jv_ClassReader::handleClassBegin
  (int access_flags, int this_class, int super_class)
{
  using namespace java::lang::reflect;

  unsigned char *pool_tags = (unsigned char*) def->constants.tags;
  _Jv_word      *pool_data = def->constants.data;

  check_tag (this_class, JV_CONSTANT_Class);
  _Jv_Utf8Const *loadedName = pool_data[this_class].utf8;

  // was ClassLoader.defineClass called with an expected class name?
  if (def->name == 0)
    {
      jclass orig = _Jv_FindClassInCache (loadedName, def->loader);

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
      jstring klass_name = _Jv_NewStringUTF (loadedName->data);
      msg = msg->concat (klass_name);

      throw_no_class_def_found_error (msg);
    }

  def->accflags = access_flags;
  pool_data[this_class].clazz = def;
  pool_tags[this_class] = JV_CONSTANT_ResolvedClass;

  if (super_class == 0)
    {
      // interfaces have java.lang.Object as super.
      if (access_flags & Modifier::INTERFACE)
	{
	  def->superclass = (jclass)&ClassObject;
	}

      // FIXME: Consider this carefully!  
      else if (!_Jv_equalUtf8Consts (def->name, ClassObject.name))
	{
	  throw_no_class_def_found_error ("loading java.lang.Object");
	}
    }

  // In the pre-loading state, it can be looked up in the
  // cache only by this thread!  This allows the super-class
  // to include references to this class.

  def->state = JV_STATE_PRELOADING;
  _Jv_RegisterClass (def);

  if (super_class != 0)
    {
      // load the super class
      check_tag (super_class, JV_CONSTANT_Class);
      _Jv_Utf8Const* super_name = pool_data[super_class].utf8; 

      // load the super class using our defining loader
      jclass the_super = _Jv_FindClass (super_name,
					def->loader);

      // This will establish that we are allowed to be a subclass,
      // and check for class circularity error
      checkExtends (def, the_super);

      def->superclass = the_super;
      pool_data[super_class].clazz = the_super;
      pool_tags[super_class] = JV_CONSTANT_ResolvedClass;
    }
	    
  // now we've come past the circularity problem, we can 
  // now say that we're loading...

  def->state = JV_STATE_LOADING;
  def->notifyAll ();
}

///// implements the checks described in sect. 5.3.5.3
void
_Jv_ClassReader::checkExtends (jclass sub, jclass super)
{
  using namespace java::lang::reflect;

  // having an interface or a final class as a superclass is no good
  if ((super->accflags & (Modifier::INTERFACE | Modifier::FINAL)) != 0)
    {
      throw_incompatible_class_change_error (sub->getName ());
    }

  // if the super class is not public, we need to check some more
  if ((super->accflags & Modifier::PUBLIC) == 0)
    {
      // With package scope, the classes must have the same
      // class loader.
      if (   sub->loader != super->loader
	  || !_Jv_ClassNameSamePackage (sub->name, super->name))
	{
	  throw_incompatible_class_change_error (sub->getName ());
	}
    } 

  for (; super != 0; super = super->superclass)
    {
      if (super == sub)
	throw_class_circularity_error (sub->getName ());
    }
}



void _Jv_ClassReader::handleInterfacesBegin (int count)
{
  def->interfaces = (jclass*) _Jv_AllocBytesChecked (count*sizeof (jclass));
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
  def->fields = (_Jv_Field*) 
    _Jv_AllocBytesChecked (count * sizeof (_Jv_Field));
  def->field_count = count;
  def->field_initializers = (_Jv_ushort*)
    _Jv_AllocBytesChecked (count * sizeof (_Jv_ushort));
  for (int i = 0; i < count; i++)
    def->field_initializers[i] = (_Jv_ushort) 0;
}

void _Jv_ClassReader::handleField (int field_no,
				   int flags,
				   int name,
				   int desc)
{
  using namespace java::lang::reflect;

  _Jv_word *pool_data = def->constants.data;

  _Jv_Field *field = &def->fields[field_no];
  _Jv_Utf8Const *field_name = pool_data[name].utf8;

#ifndef COMPACT_FIELDS
  field->name      = field_name;
#else
  field->nameIndex = name;
#endif

  if (verify)
    _Jv_VerifyIdentifier (field_name);

  // ignore flags we don't know about.  
  field->flags = flags & Modifier::ALL_FLAGS;

  if (verify)
    {
      if (field->flags & (Modifier::SYNCHRONIZED
			  | Modifier::NATIVE
			  | Modifier::INTERFACE
			  | Modifier::ABSTRACT))
	throw_class_format_error ("erroneous field access flags");
      
      if (1 < ( ((field->flags & Modifier::PUBLIC) ? 1 : 0)
		+((field->flags & Modifier::PRIVATE) ? 1 : 0)
		+((field->flags & Modifier::PROTECTED) ? 1 : 0)))
	throw_class_format_error ("erroneous field access flags");
    }

  _Jv_Utf8Const* sig = pool_data[desc].utf8;

  if (verify)
    _Jv_VerifyFieldSignature (sig);

  // field->type is really a jclass, but while it is still
  // unresolved we keep an _Jv_Utf8Const* instead.
  field->type       = (jclass) sig;
  field->flags     |= _Jv_FIELD_UNRESOLVED_FLAG;
  field->u.boffset  = 0;
}


void _Jv_ClassReader::handleConstantValueAttribute (int field_index, 
						    int value)
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
  if (field->flags & _Jv_FIELD_CONSTANT_VALUE)
    throw_class_format_error ("field has multiple ConstantValue attributes");

  field->flags |= _Jv_FIELD_CONSTANT_VALUE;
  def->field_initializers[field_index] = value;

  /* type check the initializer */
  
  if (value <= 0 || value >= pool_count)
    throw_class_format_error ("erroneous ConstantValue attribute");

  /* FIXME: do the rest */
}

void _Jv_ClassReader::handleFieldsEnd ()
{
  using namespace java::lang::reflect;

  // We need to reorganize the fields so that the static ones are first,
  // to conform to GCJ class layout.

  int low            = 0;
  int high           = def->field_count-1;
  _Jv_Field  *fields = def->fields;
  _Jv_ushort *inits  = def->field_initializers;

  // this is kind of a raw version of quicksort.
  while (low < high)
    {
      // go forward on low, while it's a static
      while (low < high && (fields[low].flags & Modifier::STATIC) != 0)
	low++;
      
      // go backwards on high, while it's a non-static
      while (low < high && (fields[high].flags & Modifier::STATIC) == 0)
	high--;

      if (low==high)
	break;

      _Jv_Field  tmp  = fields[low];
      _Jv_ushort itmp = inits[low];
	  
      fields[low] = fields[high];
      inits[low]  = inits[high];
	  
      fields[high] = tmp;
      inits[high]  = itmp;
	  
      high -= 1;
      low  += 1;
    }
  
  if ((fields[low].flags & Modifier::STATIC) != 0) 
    low += 1;

  def->static_field_count = low;
}



void _Jv_ClassReader::handleMethodsBegin (int count)
{
  def->methods = (_Jv_Method*)
    _Jv_AllocBytesChecked (sizeof (_Jv_Method)*count);

  def->interpreted_methods = (_Jv_InterpMethod**)
    _Jv_AllocBytesChecked (sizeof (_Jv_InterpMethod*) * count);

  for (int i = 0; i < count; i++)
    def->interpreted_methods[i] = 0;

  def->method_count = count;
}


void _Jv_ClassReader::handleMethod 
    (int mth_index, int accflags, int name, int desc)
{ 
  using namespace java::lang::reflect;

  _Jv_word *pool_data = def->constants.data;
  _Jv_Method *method = &def->methods[mth_index];

  check_tag (name, JV_CONSTANT_Utf8);
  prepare_pool_entry (name, JV_CONSTANT_Utf8);
  method->name = pool_data[name].utf8;

  check_tag (desc, JV_CONSTANT_Utf8);
  prepare_pool_entry (desc, JV_CONSTANT_Utf8);
  method->signature = pool_data[desc].utf8;

  // ignore unknown flags
  method->accflags = accflags & Modifier::ALL_FLAGS;

  // intialize...
  method->ncode = 0;
  
  if (verify)
    {
      if (_Jv_equalUtf8Consts (method->name, clinit_name)
	  || _Jv_equalUtf8Consts (method->name, init_name))
	/* ignore */;
      else
	_Jv_VerifyIdentifier (method->name);

      _Jv_VerifyMethodSignature (method->signature);

      if (method->accflags & (Modifier::VOLATILE
			      | Modifier::TRANSIENT
			      | Modifier::INTERFACE))
	throw_class_format_error ("erroneous method access flags");
      
      if (1 < ( ((method->accflags & Modifier::PUBLIC) ? 1 : 0)
		+((method->accflags & Modifier::PRIVATE) ? 1 : 0)
		+((method->accflags & Modifier::PROTECTED) ? 1 : 0)))
	throw_class_format_error ("erroneous method access flags");
    }
}

void _Jv_ClassReader::handleCodeAttribute
  (int method_index, int max_stack, int max_locals, 
   int code_start, int code_length, int exc_table_length)
{
  int size = _Jv_InterpMethod::size (exc_table_length, code_length);
  _Jv_InterpMethod *method = 
    (_Jv_InterpMethod*) (_Jv_AllocBytesChecked (size));

  method->max_stack      = max_stack;
  method->max_locals     = max_locals;
  method->code_length    = code_length;
  method->exc_count      = exc_table_length;
  method->defining_class = def;
  method->self           = &def->methods[method_index];

  // grab the byte code!
  memcpy ((void*) method->bytecode (),
	  (void*) (bytes+code_start),
	  code_length);
  
  def->interpreted_methods[method_index] = method;

  /* that's all we do for now */
}

void _Jv_ClassReader::handleExceptionTableEntry 
  (int method_index, int exc_index, 
   int start_pc, int end_pc, int handler_pc, int catch_type)
{
  _Jv_InterpMethod *method = def->interpreted_methods[method_index];
  _Jv_InterpException *exc = method->exceptions ();

  exc[exc_index].start_pc     = start_pc;
  exc[exc_index].end_pc       = end_pc;
  exc[exc_index].handler_pc   = handler_pc;
  exc[exc_index].handler_type = catch_type;
}

void _Jv_ClassReader::handleMethodsEnd ()
{
  using namespace java::lang::reflect;

  for (int i = 0; i < def->method_count; i++)
    {
      _Jv_Method *method = &def->methods[i];
      if (method->accflags & (Modifier::NATIVE | Modifier::ABSTRACT))
	{
	  if (def->interpreted_methods[i] != 0)
	    throw_class_format_error ("code provided "
				      "for abstract or native method");
	}
      else
	{
	  if (def->interpreted_methods[i] == 0)
	    throw_class_format_error ("abstract or native method "
				      "with no code");
	}
    }

}


/** This section takes care of verifying integrity of identifiers,
    signatures, field ddescriptors, and class names */

#define UTF8_PEEK(PTR, LIMIT) \
  ({ unsigned char* xxkeep = (PTR); \
     int xxch = UTF8_GET(PTR,LIMIT); \
     PTR = xxkeep; xxch; })

/* verify one element of a type descriptor or signature */
static unsigned char*
_Jv_VerifyOne (unsigned char* ptr, unsigned char* limit, bool void_ok)
{
  if (ptr >= limit)
    return 0;

  int ch = UTF8_GET (ptr, limit);

  switch (ch)
    {
    case 'V':
      if (! void_ok) return 0;

    case 'S': case 'B': case 'I': case 'J':
    case 'Z': case 'C': case 'F': case 'D': 
      break;

    case 'L':
      {
	unsigned char *start = ptr, *end;
	do {
	  if (ptr > limit)
	    return 0;
		
	  end = ptr;
		
	  if ((ch = UTF8_GET (ptr, limit)) == -1)
	    return 0;
		
	} while (ch != ';');
	_Jv_VerifyClassName (start, (unsigned short) (end-start));
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


/** verification and loading procedures **/

void
_Jv_VerifyFieldSignature (_Jv_Utf8Const*sig)
{
  unsigned char* ptr = (unsigned char*) sig->data;
  unsigned char* limit = ptr + sig->length;

  ptr = _Jv_VerifyOne (ptr, limit, false);

  if (ptr != limit)
    throw_class_format_error ("erroneous type descriptor");
}

void
_Jv_VerifyMethodSignature (_Jv_Utf8Const*sig)
{
  unsigned char* ptr = (unsigned char*) sig->data;
  unsigned char* limit = ptr + sig->length;

  if (ptr == limit)
    throw_class_format_error ("erroneous type descriptor");

  if (UTF8_GET(ptr,limit) != '(')
    throw_class_format_error ("erroneous type descriptor");

  while (ptr && UTF8_PEEK (ptr, limit) != ')')
    ptr = _Jv_VerifyOne (ptr, limit, false);
    
  if (UTF8_GET (ptr, limit) != ')')
    throw_class_format_error ("erroneous type descriptor");

  // get the return type
  ptr = _Jv_VerifyOne (ptr, limit, true);

  if (ptr != limit)
    throw_class_format_error ("erroneous type descriptor");

  return;

}

/* we try to avoid calling the Character methods all the time, 
   in fact, they will only be called for non-standard things */

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

  return character->isJavaIdentifierStart ((jchar) ch);
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

  return character->isJavaIdentifierStart ((jchar) ch);
}

void 
_Jv_VerifyIdentifier (_Jv_Utf8Const* name)
{
  unsigned char *ptr   = (unsigned char*) name->data;
  unsigned char *limit = ptr + name->length;
  int ch;

  if ((ch = UTF8_GET (ptr, limit))==-1
      || ! is_identifier_start (ch))
    throw_class_format_error ("erroneous identifier");

  while (ptr != limit)
    {
      if ((ch = UTF8_GET (ptr, limit))==-1
	  || ! is_identifier_part (ch))
	throw_class_format_error ("erroneous identifier");
    }
}


void
_Jv_VerifyClassName (unsigned char* ptr, _Jv_ushort length)
{
  unsigned char *limit = ptr+length;
  int ch;

  if ('[' == UTF8_PEEK (ptr, limit))
    {
      if (! _Jv_VerifyOne (++ptr, limit, false))
        throw_class_format_error ("erroneous class name");
      else
        return;
    }

 next_level:
  do {
    if ((ch = UTF8_GET (ptr, limit))==-1)
      throw_class_format_error ("erroneous class name");
    if (! is_identifier_start (ch))
      throw_class_format_error ("erroneous class name");
    do {
      if (ptr == limit)
	return;
      else if ((ch = UTF8_GET (ptr, limit))==-1)
	throw_class_format_error ("erroneous class name");
      else if (ch == '.')
	goto next_level;
      else if (! is_identifier_part (ch))
	throw_class_format_error ("erroneous class name");
    } while (true);
  } while (true);

}

void
_Jv_VerifyClassName (_Jv_Utf8Const *name)
{
    _Jv_VerifyClassName ((unsigned char*)&name->data[0],
			 (_Jv_ushort) name->length);
}


/** returns true, if name1 and name2 represents classes in the same
    package. */
    
bool
_Jv_ClassNameSamePackage (_Jv_Utf8Const *name1, _Jv_Utf8Const *name2)
{
  unsigned char* ptr1 = (unsigned char*) name1->data;
  unsigned char* limit1 = ptr1 + name1->length;

  unsigned char* last1 = ptr1;

  // scan name1, and find the last occurrence of '.'
  while (ptr1 < limit1) {
    int ch1 = UTF8_GET (ptr1, limit1);

    if (ch1 == '.')
      last1 = ptr1;
	
    else if (ch1 == -1)
      return false;
  }

  // now the length of name1's package name is len
  int len = last1 - (unsigned char*) name1->data;

  // if this is longer than name2, then we're off
  if (len > name2->length)
    return false;

  // then compare the first len bytes for equality
  if (memcmp ((void*) name1->data, (void*) name2->data, len) == 0)
    {
      // check that there are no .'s after position len in name2

      unsigned char* ptr2 = (unsigned char*) name2->data + len;
      unsigned char* limit2 =
	(unsigned char*) name2->data + name2->length;

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



/** Here we define the exceptions that can be thrown */

static void
throw_no_class_def_found_error (jstring msg)
{
  if (msg == 0)
    JvThrow (new java::lang::NoClassDefFoundError);
  else
    JvThrow (new java::lang::NoClassDefFoundError (msg));
}

static void
throw_no_class_def_found_error (char *msg)
{
  throw_no_class_def_found_error (JvNewStringLatin1 (msg));
}

static void
throw_class_format_error (jstring msg)
{
  if (msg == 0)
    JvThrow (new java::lang::ClassFormatError);
  else
    JvThrow (new java::lang::ClassFormatError (msg));
}

static void
throw_class_format_error (char *msg)
{
  throw_class_format_error (JvNewStringLatin1 (msg));
}

static void
throw_internal_error (char *msg)
{
  JvThrow 
    (new java::lang::InternalError (JvNewStringLatin1 (msg)));
}

static jfloat int_bits_to_float (jint value)
{
  return java::lang::Float::intBitsToFloat (value);
}

static jdouble long_bits_to_double (jlong value)
{
  return java::lang::Double::longBitsToDouble (value);
}

static void throw_incompatible_class_change_error (jstring msg)
{
  JvThrow (new java::lang::IncompatibleClassChangeError (msg));
}

static void throw_class_circularity_error (jstring msg)
{
  JvThrow (new java::lang::ClassCircularityError (msg));
}

#endif /* INTERPRETER */

