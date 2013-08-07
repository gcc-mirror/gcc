
extern "C" int printf(const char *, ...);

class Environment {
 public:
  virtual ~Environment();

  // Static factory method that returns the implementation that provide the
  // appropriate platform-specific instance.
  static Environment* Create();

  // Gets an environment variable's value and stores it in |result|.
  // Returns false if the key is unset.
  virtual bool GetVar(const char* variable_name, char* result) = 0;
};

class EnvironmentImpl : public Environment {
 public:
  virtual bool GetVar(const char* variable_name, char* result) {
      return true;
  }
};

Environment::~Environment() {}

// static
Environment* Environment::Create() {
  return new EnvironmentImpl();
}

int main()
{
  char * null = 0;
  Environment * env = Environment::Create();
  env->GetVar(0, null);
  printf("%p\n", env);
}
