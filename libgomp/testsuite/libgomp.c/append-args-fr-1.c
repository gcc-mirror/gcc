/* { dg-do run } */

#include "append-args-fr.h"

enum { host_device, nvptx_device, gcn_device } used_device_type, used_device_type2;
static int used_device_num, used_device_num2;
static omp_interop_fr_t expected_fr, expected_fr2;
static _Bool is_targetsync, is_targetsync2;

void
check_interop (omp_interop_t obj)
{
  if (used_device_type == host_device)
    check_host (obj);
  else if (used_device_type == nvptx_device)
    check_nvptx (obj, used_device_num, expected_fr, is_targetsync);
  else if (used_device_type == gcn_device)
    check_gcn (obj, used_device_num, expected_fr, is_targetsync);
  else
    __builtin_unreachable ();

  #pragma omp interop use(obj)
}

void
check_interop2 (omp_interop_t obj, omp_interop_t obj2)
{
  check_interop (obj);

  #pragma omp interop use(obj2)

  if (used_device_type2 == host_device)
    check_host (obj2);
  else if (used_device_type2 == nvptx_device)
    check_nvptx (obj2, used_device_num2, expected_fr2, is_targetsync2);
  else if (used_device_type2 == gcn_device)
    check_gcn (obj2, used_device_num2, expected_fr2, is_targetsync2);
  else
    __builtin_unreachable ();
}


/* Check no args + one interop arg - and no prefer_type.  */

int f0_1_tg_ (omp_interop_t obj) { check_interop (obj); return 4242; }
#pragma omp declare variant(f0_1_tg_)     match(construct={dispatch}) append_args(interop(target))
int f0_1_tg () { assert (false); return 42; }

void f0_1_tgsy_ (omp_interop_t obj) { check_interop (obj); }
#pragma omp declare variant(f0_1_tgsy_)   match(construct={dispatch}) append_args(interop(targetsync))
void f0_1_tgsy () { assert (false); }

int f0_1_tgtgsy_ (omp_interop_t obj) { check_interop (obj); return 3333; }
#pragma omp declare variant(f0_1_tgtgsy_) match(construct={dispatch}) append_args(interop(targetsync,target))
int f0_1_tgtgsy () { assert (false); return 33; }


/* And with PREFER_TYPE.  */

// nv: cuda, gcn: -, -, hip
void f0_1_tgsy_c_cd_hi_hs_ (omp_interop_t obj) { check_interop (obj); }
#pragma omp declare variant(f0_1_tgsy_c_cd_hi_hs_)   match(construct={dispatch}) \
            append_args(interop(targetsync, prefer_type("cuda","cuda_driver", "hip", "hsa")))
void f0_1_tgsy_c_cd_hi_hs () { assert (false); }

// nv: -, cuda_driver, gcn: hsa
void f0_1_tgsy_hs_cd_c_hi_ (omp_interop_t obj) { check_interop (obj); }
#pragma omp declare variant(f0_1_tgsy_hs_cd_c_hi_)   match(construct={dispatch}) \
            append_args(interop(targetsync, prefer_type({attr("ompx_foo")}, {fr("hsa")}, {attr("ompx_bar"), fr("cuda_driver"), attr("ompx_foobar")},{fr("cuda")}, {fr("hip")})))
void f0_1_tgsy_hs_cd_c_hi () { assert (false); }

// nv: -, hip, gcn: hsa
void f0_1_tgsy_hs_hi_cd_c_ (omp_interop_t obj) { check_interop (obj); }
#pragma omp declare variant(f0_1_tgsy_hs_hi_cd_c_)   match(construct={dispatch}) \
            append_args(interop(targetsync, prefer_type("hsa", "hip", "cuda_driver", "cuda")))
void f0_1_tgsy_hs_hi_cd_c () { assert (false); }


void
check_f0 ()
{
  if (used_device_type == nvptx_device)
    expected_fr = omp_ifr_cuda;
  else if (used_device_type == gcn_device)
    expected_fr = omp_ifr_hip;
  else  /* host; variable shall not be accessed  */
    expected_fr = omp_ifr_level_zero;

  int i;
  if (used_device_num == DEFAULT_DEVICE)
    {
      is_targetsync = 0;
      #pragma omp dispatch
        i = f0_1_tg ();
      assert (i == 4242);

      is_targetsync = 1;
      #pragma omp dispatch
        f0_1_tgsy ();

      #pragma omp dispatch
        i = f0_1_tgtgsy ();
      assert (i == 3333);


      if (used_device_type == nvptx_device)
	expected_fr = omp_ifr_cuda;
      else if (used_device_type == gcn_device)
	expected_fr = omp_ifr_hip;
      #pragma omp dispatch
	 f0_1_tgsy_c_cd_hi_hs ();

      if (used_device_type == nvptx_device)
	expected_fr = omp_ifr_cuda_driver;
      else if (used_device_type == gcn_device)
	expected_fr = omp_ifr_hsa;
      #pragma omp dispatch
	f0_1_tgsy_hs_cd_c_hi ();
  
      if (used_device_type == nvptx_device)
	expected_fr = omp_ifr_hip;
      else if (used_device_type == gcn_device)
	expected_fr = omp_ifr_hsa;
      #pragma omp dispatch
	f0_1_tgsy_hs_hi_cd_c ();
    }
  else
    {
      is_targetsync = 0;
      #pragma omp dispatch device(used_device_num)
        i = f0_1_tg ();
      assert (i == 4242);

      is_targetsync = 1;
      #pragma omp dispatch device(used_device_num)
        f0_1_tgsy ();

      #pragma omp dispatch device(used_device_num)
        i = f0_1_tgtgsy ();
      assert (i == 3333);


      if (used_device_type == nvptx_device)
	expected_fr = omp_ifr_cuda;
      else if (used_device_type == gcn_device)
	expected_fr = omp_ifr_hip;
      #pragma omp dispatch device(used_device_num)
	 f0_1_tgsy_c_cd_hi_hs ();

      if (used_device_type == nvptx_device)
	expected_fr = omp_ifr_cuda_driver;
      else if (used_device_type == gcn_device)
	expected_fr = omp_ifr_hsa;
      #pragma omp dispatch device(used_device_num)
	f0_1_tgsy_hs_cd_c_hi ();
  
      if (used_device_type == nvptx_device)
	expected_fr = omp_ifr_hip;
      else if (used_device_type == gcn_device)
	expected_fr = omp_ifr_hsa;
      #pragma omp dispatch device(used_device_num)
	f0_1_tgsy_hs_hi_cd_c ();
    }
}



void
do_check (int dev)
{
  int num_dev = omp_get_num_devices ();
  const char *dev_type;
  if (dev != DEFAULT_DEVICE)
    omp_set_default_device (dev);
  int is_nvptx = on_device_arch_nvptx ();
  int is_gcn = on_device_arch_gcn ();
  int is_host;
 
  if (dev != DEFAULT_DEVICE)
    is_host = dev == -1 || dev == num_dev;
  else
    {
      int def_dev = omp_get_default_device ();
      is_host = def_dev == -1 || def_dev == num_dev;
    }

  assert (is_nvptx + is_gcn + is_host == 1);

  if (num_dev > 0 && dev != DEFAULT_DEVICE)
    {
      if (is_host)
	omp_set_default_device (0);
      else
	omp_set_default_device (-1);
    }

  used_device_num = dev;
  if (is_host)
    {
      dev_type = "host";
      used_device_type = host_device;
    }
  else if (is_nvptx)
    {
      dev_type = "nvptx";
      used_device_type = nvptx_device;
    }
  else if (is_gcn)
    {
      dev_type = "gcn";
      used_device_type = gcn_device;
    }

  printf ("Running on the %s device (%d)\n", dev_type, dev);
  check_f0 ();
}



int
main ()
{
  do_check (DEFAULT_DEVICE);
  int ndev = omp_get_num_devices ();
  for (int dev = -1; dev < ndev; dev++)
    do_check (dev);
  for (int dev = -1; dev < ndev; dev++)
    {
      omp_set_default_device (dev);
      do_check (DEFAULT_DEVICE);
    }
}
