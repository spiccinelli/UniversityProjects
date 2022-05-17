## Simple "Non Project Mode" Vivado Project template

1. Create the Xilinx environment
```console
source  /tools/Xilinx/Vivado/2018.3/settings64.sh
```

1. Put your source file in the *src* subdir,  save them with *.vhd* extension

1.  build your bitstream 
```console
make bitstream
```

1. Program FPGA

```console
make program_fpga
```

**A simple UART Trasmitter is already implemented**

( transmit 'a' character one time per second)

