ca65 bank_01.asm
ca65 bank_09.asm
ca65 bank_0B.asm
ca65 bank_0C.asm
ca65 bank_0D.asm
ca65 bank_0E.asm
ca65 bank_0F.asm

ld65 -C nes.cfg bank_01.o bank_09.o bank_0B.o bank_0C.o bank_0D.o bank_0E.o bank_0F.o

copy /B nesheader.bin+bank_00.dat+bank_01.bin+bank_02.dat+bank_03.dat+bank_04.dat+bank_05.dat+bank_06.dat+bank_07.dat+bank_08.dat+bank_09.bin+bank_0A.dat+bank_0B.bin+bank_0C.bin+bank_0D.bin+bank_0E.bin+bank_0F.bin FinalFantasy.nes

del *.o
del bank_*.bin

pause