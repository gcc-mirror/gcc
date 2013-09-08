#include <vector>

class ExtraParts;

class MainParts {
 public:
  MainParts ();
  virtual ~MainParts ();
  virtual void AddParts (ExtraParts *parts);

  virtual void PreEarlyInitialization ();

 protected:
  std::vector<ExtraParts *> main_extra_parts_;
};
