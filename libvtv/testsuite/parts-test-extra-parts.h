#ifndef _EXTRA_PARTS_H_
#define _EXTRA_PARTS_H_

class ExtraParts {
 public:
  ExtraParts ();
  virtual ~ExtraParts ();

  virtual void PreEarlyInitialization ();
  virtual void ToolkitInitialized ();
};

#endif /* _EXTRA_PARTS_H_ */
