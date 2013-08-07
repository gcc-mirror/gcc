class RefCountedBase {
protected:
  bool derefBase()
  {
    return true;
  }
};

template<typename T> class RefCounted : public RefCountedBase {
public:
    void deref()
    {
        if (derefBase())
            delete static_cast<T*>(this);
    }

protected:
  //    RefCounted() { }
    ~RefCounted()
    {
    }
};


class Event : public RefCounted<Event> {
 public:
        Event();
        virtual ~Event();
};
