
class EtherCtrl {
 protected:
  int ssap;

 public:
  EtherCtrl(void);
  ~EtherCtrl();
  virtual int getSsap(void) const;
  virtual void setSsap(int);
};

class EtherFrameWithLLC {
 protected:
  int ssap;

 public:
  EtherFrameWithLLC(const char *, int);
  ~EtherFrameWithLLC();
  virtual int getSsap(void) const;
  virtual void setSsap(int);
};


EtherCtrl::EtherCtrl()
{
  this->ssap = 0;
}

EtherCtrl::~EtherCtrl()
{
}

int EtherCtrl::getSsap() const
{
  return ssap;
}

void EtherCtrl::setSsap(int ssap)
{
  this->ssap = ssap;
}

EtherFrameWithLLC::EtherFrameWithLLC(const char *name, int kind)
{
  this->ssap = 0;
}

EtherFrameWithLLC::~EtherFrameWithLLC()
{
}

int EtherFrameWithLLC::getSsap() const
{
  return ssap;
}

void EtherFrameWithLLC::setSsap(int ssap)
{
  this->ssap = ssap;
}


int
main (int argc, char **argv)
{
  EtherCtrl *etherctrl = new EtherCtrl ();
  EtherFrameWithLLC *frame = new EtherFrameWithLLC ("test", 10);
  int my_value;

  etherctrl->setSsap(43);
  frame->setSsap(etherctrl->getSsap());
  my_value = frame->getSsap();

  return 0;
}
