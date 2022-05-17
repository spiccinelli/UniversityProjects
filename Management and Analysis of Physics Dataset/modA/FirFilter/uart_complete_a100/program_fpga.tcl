set_param general.maxBackupLogs 0
open_hw
connect_hw_server
open_hw_target 
current_hw_device [get_hw_devices ]
refresh_hw_device -update_hw_probes false [lindex [get_hw_devices ] 0]
set_property PROBES.FILE {} [get_hw_devices]
set_property FULL_PROBES.FILE {} [get_hw_devices ]
set_property PROGRAM.FILE {top.bit} [get_hw_devices ]
program_hw_devices [get_hw_devices ]
exit
