.MODEL tiny
.STACK 100h

.DATA

hello db "It's temp.exe!", 10, 13, '$'


.CODE
jmp start

init macro
  mov ax, @data
  mov ds, ax
endm

exit macro
  mov ax, 4c00h
  int 21h
endm

terminate macro
  int 20h
endm

log proc
  push bp
  mov bp, sp
  mov ax, 0900h
  mov dx, ss:[bp+4]
  int 21h
  pop bp
  ret
endp

call_log macro value
  push offset value
  call log
  pop dx
endm

start:
  init
  call_log hello
  exit
end start