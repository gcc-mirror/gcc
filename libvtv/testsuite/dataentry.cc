template<int patch_dim, int patch_space_dim>
class DataOutInterface
{
 public:
  virtual ~DataOutInterface() {}
};

template <int dof_handler_dim, int patch_dim, int patch_space_dim=patch_dim>
class DataOut_DoFData : public DataOutInterface<patch_dim,patch_space_dim>
{
 public:
  virtual ~DataOut_DoFData() {}

class DataEntryBase {
 public:
  virtual ~DataEntryBase () {}
};

template <typename T>
class DataEntry : public DataEntryBase
{
 public:
  virtual ~DataEntry() {}
};
};

template <typename T> void Destroy(T * p) __attribute__((noinline));
template <typename T> void Destroy(T * p)
{
  delete p;
}

int main()
{
  DataOut_DoFData<3,3>::DataEntryBase * p  = new DataOut_DoFData<3,3>::DataEntry<int>();
  Destroy(p);
}
