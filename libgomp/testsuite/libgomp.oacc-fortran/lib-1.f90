use openacc

if (acc_get_num_devices (acc_device_host) .ne. 1) STOP 1
call acc_set_device_type (acc_device_host)
if (acc_get_device_type () .ne. acc_device_host) STOP 2
call acc_set_device_num (0, acc_device_host)
if (acc_get_device_num (acc_device_host) .ne. 0) STOP 3
call acc_shutdown (acc_device_host)

call acc_init (acc_device_host)
call acc_shutdown (acc_device_host)

end
