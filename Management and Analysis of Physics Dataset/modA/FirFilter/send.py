import serial

ser = serial.Serial('/dev/ttyUSB17', baudrate=115200) # open serial port
print(ser.name) # check which port was really used

def to_2(num):
    if num <= 127:
        return num
    elif num > 127:
        return num - 256

with open("input_vectors.txt") as f, open("output_vectors.txt", "w") as out:
    signal = [int(line.rstrip()) for line in f]

    for sig in signal:
        ser.write(chr(sig))

        d = ser.read()
        ordinal = ord(d)
        print(ordinal))
        result = to_2(ordinal)
        out.write(result)

    f.close()
    out.close()
#ser.close() # close port
