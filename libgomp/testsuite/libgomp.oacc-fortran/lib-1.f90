use openacc

if (acc_get_num_devices (acc_device_host) .ne. 1) call abort
call acc_set_device_type (acc_device_host)
if (acc_get_device_type () .ne. acc_device_host) call abort
call acc_set_device_num (0, acc_device_host)
if (acc_get_device_num (acc_device_host) .ne. 0) call abort
call acc_shutdown (acc_device_host)

call acc_init (acc_device_host)
call acc_shutdown (acc_device_host)

end
