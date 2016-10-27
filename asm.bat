@echo off
as11 %1.asm -l s > %1.lst
type %1.lst
