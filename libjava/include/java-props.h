// java-props.h - Properties  -*- c++ -*-

#ifndef __JAVA_PROPS_H__
#define __JAVA_PROPS_H__

typedef struct
{
  char *key;
  size_t      key_length;
  char *value;
  size_t      value_length;
} property_pair;

#endif

