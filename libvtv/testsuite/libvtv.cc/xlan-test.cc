// { dg-do run }

#include <stdio.h>
#include <stdlib.h>

class XMemory
{
public:
  void * operator new (size_t size);
  void operator delete (void *p);

protected:
  XMemory () {}

  virtual ~XMemory() {}
};

class XSerializable
{
public:
  virtual ~XSerializable () {};

  virtual bool isSerializable() const = 0;
  virtual void serialize () = 0;

protected:
  XSerializable() {};

};

class Grammar: public XSerializable, public XMemory
{
public:
  enum GrammarType {
    DTDGrammarType,
    SchemaGrammarType,
    OtherGrammarType,
    Unknown
  };

  virtual ~Grammar() {}

  virtual GrammarType getGrammarType() const = 0;
  virtual bool getValidated() const = 0;

  virtual bool isSerializable() const;
  virtual void serialize ();

protected:
  Grammar() {};

};

class SchemaGrammar : public Grammar
{
public:

  SchemaGrammar () :  Grammar(), elemID(10) { fValidated = true; }

  virtual ~SchemaGrammar() {}

  virtual Grammar::GrammarType getGrammarType() const;
  virtual bool getValidated() const;

  virtual bool isSerializable () const;
  virtual void serialize ();

private:
  const unsigned int elemID;
  bool fValidated;

};

class OtherGrammar : public Grammar
{
public:

  OtherGrammar () :  Grammar(), elemID(10) { fValidated = true; }

  virtual ~OtherGrammar() {}

  virtual Grammar::GrammarType getGrammarType() const;
  virtual bool getValidated() const;

  virtual bool isSerializable () const;
  virtual void serialize ();

private:
  const unsigned int elemID;
  bool fValidated;

};

void
Grammar::serialize ()
{
  printf ("in Grammar::serialize\n");
}

bool
Grammar::isSerializable () const
{
  return true;
}

bool
SchemaGrammar::isSerializable () const
{
  return true;
}

void
SchemaGrammar::serialize ()
{
  printf ("in SchemaGrammar::serialize\n");
}

Grammar::GrammarType
SchemaGrammar::getGrammarType() const {
  return Grammar::SchemaGrammarType;
}

bool
SchemaGrammar::getValidated () const
{
  return fValidated;
}

void *
XMemory::operator new (size_t size)
{
  return malloc (size);
}

void
XMemory::operator delete (void *p)
{
}

bool
OtherGrammar::isSerializable () const
{
  return false;
}

void
OtherGrammar::serialize ()
{
  printf ("in OtherGrammar::serialize\n");
}

Grammar::GrammarType
OtherGrammar::getGrammarType() const {
  return Grammar::OtherGrammarType;
}

bool
OtherGrammar::getValidated () const
{
  return fValidated;
}

int
main (int argc, char **argv)
{
  SchemaGrammar sPtr;
  OtherGrammar oPtr;
  Grammar &sGrammar = sPtr;

  for (int i = 0; i < 2; ++i)
    {
      if (i == 0)
	sGrammar = oPtr;
      else
	sGrammar = sPtr;

      if (sGrammar.getGrammarType() != Grammar::SchemaGrammarType ||
	  sGrammar.getValidated ())
	printf ("if condition was true.\n");
      else
	printf ("if condition was false.\n");
    }

  return 0;
}
