// { dg-do run }

#include <stdlib.h>
#include <string>

class Literal;
class CallRuntime;

class AstNode {
public:

  enum Type {
    kLiteral, kCallRuntime,
    kInvalid = -1
  };

  AstNode() { }

  virtual ~AstNode() { }

  virtual Type node_type() const = 0;

  bool 
  IsLiteral() { return node_type() == AstNode::kLiteral; } 

  Literal *
  AsLiteral() { return IsLiteral() ? reinterpret_cast<Literal*>(this)
                                   : NULL; }

  bool
  IsCallRuntime() { return node_type() == AstNode::kCallRuntime; }

  CallRuntime *
  AsCallRuntime() { return IsCallRuntime() ? reinterpret_cast<CallRuntime*>(this)
                                           : NULL; }

};

class Expression: public AstNode {
public:
private:
  int id_;
};

class Literal: public Expression {
public:

  virtual AstNode::Type node_type() const { return AstNode::kLiteral; }

  private:
  std::string ToString();

};

class CallRuntime: public Expression {
public:

  virtual AstNode::Type node_type() const { return AstNode::kCallRuntime; }

  private:
  std::string name_;
};

Expression *
ExpressionCheck (bool *ok)
{
  if (*ok == true)
    return new CallRuntime();
  else
    return new Literal ();

  return NULL;
}

Expression *
GetExpression (bool *ok)
{
  Expression *expression = ExpressionCheck (ok);
  Expression *return_expr = NULL;

  if (expression != NULL && expression->AsLiteral() != NULL)
    return_expr = new Literal();
  else if (expression != NULL && expression->AsCallRuntime() != NULL)
    return_expr = expression;

  return return_expr;
}

int
main (int argc, char **argv)
{
  bool a_bool = true;

  AstNode *node = GetExpression (&a_bool);

  return 0;
}
