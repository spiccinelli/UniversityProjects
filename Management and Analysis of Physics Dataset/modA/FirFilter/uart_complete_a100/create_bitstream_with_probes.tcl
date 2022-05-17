open_checkpoint post_synth.dcp
#Implement Design
opt_design
place_design
route_design
#Generate bitstream
set_property BITSTREAM.GENERAL.COMPRESS TRUE [current_design]
write_bitstream -force  top.bit
write_debug_probes -force top.ltx
foreach path [glob *webtalk*] {
    file delete -force -- $path
 }
#
exit
