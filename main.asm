.286
.MODEL tiny
.STACK 100h

; place is important (program start)
empty_segment SEGMENT    
empty_segment ENDS

.DATA

dta_buffer db 128 dup(0)            ;dta local copy

find_end db 0                       ; find_end flag                           
number dw 0
endl db  10, 13, '$'
space db " ", '$'
temp_buffer db 14 dup ('$')
error_parsing db "Failed to parse command line args", '$'
allocate_error_message db "Unable to alocate memory", '$'
load_and_run_error_message db "Unable to load and run programm", '$'
error_status_code_message db "Error status code ->", '$'
file_error db "File Error", '$'
; folder db "./BUILD", 0
folder db 15 dup (0)
find_pattern db "/*.exe", 0
filename db 20 dup (0)

; find_temp db "./build/*.exe", 0

path         db 25 dup ('$')
command_line db 0
epb          dw 0         ; 2 bytes - seg_env (enviroment segement address (if 0 = copy from parent))
cmd_off      dw ? 
cmd_seg      dw ?
fcb1         dd ?         ; address of FCB structure (37 bytes for file description)
fcb2         dd ?         ; address of FCB structure (37 bytes for file description)

.CODE
jmp start

save_dta dw 0     ; for restoring
save_ss dw 0      ; for restoring
save_sp dw 0      ; for restoring
save_es dw 0      ; for restoring
save_ds dw 0      ; for restoring

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

;first param is output buffer
;second param is number address (offset from `ds`)
itoa:
  push bp
  mov bp, sp
  push di
  push si
  push bx
  push ax
  push dx
  
  mov di, ss:[bp + 6] ;output
  mov si, ss:[bp + 4] ;number address (number stored as word) 
  mov ax, word ptr [si] ;actual number
  mov bx, 10
  
  ;check if number is negative
  cmp word ptr [si], 0h
  jge positive_true
  
  mov byte ptr [di], '-'
  inc di
  neg ax
  positive_true:
  mov si, di
  
  itoa_iterate:
    mov dx, 00h
    cmp ax, 10
    jl itoa_end
    
    div bx
    add dx, 48
    mov byte ptr [di], dl
    inc di
    
    jmp itoa_iterate
  
  itoa_end:
  add ax, 48
  mov byte ptr [di], al
  
  push si
  push di
  call reverse
  pop di
  pop si
  
  inc di
  mov byte ptr[di], '$'
  
  pop dx
  pop ax
  pop bx
  pop si
  pop di
  pop bp
ret

reverse:
    push bp
    mov bp, sp
    push si
    push di
    push ax
    push bx
    push dx
    
    mov si, ss:[bp + 6] ;start address
    mov bx, ss:[bp + 4] ;end address
    mov di, bx
    
    cmp si, bx
    je reverse_end
    
    mov ah, 00h
    reverse_iterate:
        lodsb ; from memory with [si] address to `al` register
        dec si
        mov dl, byte ptr [di]
        mov byte ptr ds:[si], dl
        stosb ;from `al` register to memory with [di] address 
        inc si
        sub di, 2
        
        cmp si, di
        je reverse_end
        
        dec si
        cmp si, di
        je reverse_end
        inc si
        jne reverse_iterate
    
    
    reverse_end:    
    
    pop dx
    pop bx
    pop ax
    pop di
    pop si    
    pop bp
ret

call_itoa macro
  push dx
  push offset temp_buffer
  push offset number
  call itoa
  pop dx
  pop dx
  pop dx
endm

log proc
  push bp
  mov bp, sp
  push ax
  mov ax, 0900h
  mov dx, ss:[bp+4]
  int 21h
  pop ax
  pop bp
  ret
endp

call_log macro value
  push offset value
  call log
  pop dx
endm

allocate_memory proc
  push ax
  push bx

  mov ax, es                      ; program start segment address (in paragraphs)
  mov bx, empty_segment           ; program end segment address (in paragraphs)
  sub bx, ax                      ; BX contain minimum required memory size for programm

  mov ah, 4Ah
  int 21h
  jc allocate_memory_error
  jmp allocate_memory_end

  allocate_memory_error:
    mov word ptr ds:[number], ax ;error code
    call_log allocate_error_message
    call_log space
    call_itoa
    call_log temp_buffer
    call_log endl
    mov dx, 1

  allocate_memory_end:
  pop bx
  pop ax
  ret
endp

load_and_run proc
  push bp
  mov bp, sp
  push ax
  push bx
  push dx

  mov ah, 2fh
  int 21h

  mov word ptr cs:[save_ss], ss
  mov word ptr cs:[save_sp], sp
  mov word ptr cs:[save_ds], ds
  
  mov bx, offset command_line     ; cli address (first byte is length, next is cli itself)
  mov cmd_off, bx
  mov cmd_seg, ds

  mov bx, offset epb              ; ES:BX contain EPB address block
  mov dx, ss:[bp + 4]             ; DS:DX contain path address

  mov ah, 4Bh
  mov al, 00h                     ;0 function code -> load and execute
  int 21h
  jc load_and_run_error

  mov ss, word ptr cs:[save_ss]
  mov sp, word ptr cs:[save_sp]
  mov ds, word ptr cs:[save_ds]

  jmp load_and_run_end

  load_and_run_error:
    mov ss, word ptr cs:[save_ss]
    mov sp, word ptr cs:[save_sp]
    mov ds, word ptr cs:[save_ds]
    mov word ptr ds:[number], ax
    call_log load_and_run_error_message
    call_log endl
    call_log error_status_code_message
    call_log space
    call_itoa
    call_log temp_buffer
    call_log endl
    mov si, 1                       ; signal error

  load_and_run_end:
  pop dx
  pop bx
  pop ax
  pop bp
  ret
endp

; bundle folder + found file_name into full path
prepare_for_load_and_run proc
  push bx
  push di
  mov bx, offset folder
  mov di, offset path

  prepare_for_load_and_run_folder:
    mov al, byte ptr ds:[bx]
    cmp al, 0
    je exit_prepare_for_load_and_run_folder
    mov byte ptr ds:[di], al
    inc bx
    inc di
  jmp prepare_for_load_and_run_folder
  exit_prepare_for_load_and_run_folder:

  mov byte ptr ds:[di], '/'
  inc di
  mov bx, offset filename

  prepare_for_load_and_run_filename:
    mov al, byte ptr ds:[bx]
    mov byte ptr ds:[di], al
    cmp al, 0
    je exit_prepare_for_load_and_run_filename
    inc bx
    inc di
  jmp prepare_for_load_and_run_filename
  exit_prepare_for_load_and_run_filename:

  ; call_log path

  pop di
  pop bx
  ret
endp

call_load_and_run macro
  call prepare_for_load_and_run
  push ax
  push bx

  ; mov ax, es:[bx]
  ; mov cs:[save_dta], ax
  mov word ptr cs:[save_es], es
  mov bx, @data
  mov es, bx
  push offset path
  call load_and_run
  pop dx

  mov es, word ptr cs:[save_es]
  mov ah, 1ah
  mov dx, offset dta_buffer
  int 21h

  pop bx
  pop ax
endm

find_file proc
  push ax
  push cx
  push di
  push si
  push bx

  mov bx, si
  cmp bx, 1                    ; if it's first call
  jne next_call

  mov si, offset folder
  mov di, offset path
  
  find_file_create_pattern_folder:
    mov al, byte ptr ds:[si]
    cmp al, 0
    je exit_find_file_create_pattern_folder
    mov byte ptr ds:[di], al
    inc si
    inc di
  jmp find_file_create_pattern_folder
  exit_find_file_create_pattern_folder:
  
  mov si, offset find_pattern
  find_file_create_pattern_extension:
    mov al, byte ptr ds:[si]
    mov byte ptr ds:[di], al
    cmp al, 0
    je exit_find_file_create_pattern_extension
    inc si
    inc di
  jmp find_file_create_pattern_extension
  exit_find_file_create_pattern_extension:

  mov cx, 0
  mov dx, offset path
  mov ah, 4eh
  int 21h
  jc find_file_error
  jmp find_file_prepare

  next_call: 
  mov dx, offset dta_buffer
  mov ah, 4fh
  int 21h
  jc find_file_error
  
  find_file_prepare:
  mov si, offset dta_buffer + 1Eh           ; DTA begin + search result (filename in ASCIIZ format)
  mov di, offset filename  ; buffer for copy file name
  mov cx, 13                    ; buffer_size
  jmp find_file_success

  find_file_error:
    mov word ptr ds:[number], ax;
    call_log file_error
    call_log endl
    call_log error_status_code_message
    call_log space
    call_itoa
    call_log temp_buffer
    call_log endl
    mov ds:[find_end], 1
  jmp find_file_end

  find_file_success:
    mov al, byte ptr ds:[si]
    mov byte ptr ds:[di], al
    cmp al, 0
    je find_file_copy_end
    inc si
    inc di
  loop find_file_success

  find_file_copy_end:

  ; call_log filename
  ; call_log endl
  find_file_end:
  pop bx
  pop si
  pop di
  pop cx
  pop ax
  ret
endp

; `first` param should be 1, if it's first call, and 0 in another case 
call_find_file macro first
  push si 
  mov si, first
  call find_file
  pop si
endm

parse_command_line proc
  push di
  push bx
  push ax
  push cx
  ; `es` contain `PSP` segment address
  ; store file name in `file_name`

  xor bx, bx
  mov bl, byte ptr es:[80h] ; cli args length
  cmp bl, 0
  jbe bad_args
  jmp parse_command_line_continue
  
  bad_args:
  call_log error_parsing
  ; сall_log endl
  exit

  parse_command_line_continue:
  mov di, 81h
  mov al, ' '
  rep scasb ; skip all spaces
  ; dec di
  dec di

  xor cx, cx
  mov cl, byte ptr es:[80h]
  dec cl

  mov bx, offset folder
  copy_folder_name:
    mov al, byte ptr es:[di]
    mov byte ptr ds:[bx], al
    inc di
    inc bx
  loop copy_folder_name
  ; mov byte ptr ds:[bx], 0
  
  parse_command_line_end:
  pop cx
  pop ax
  pop bx
  pop di
  ret
endp

start:
  call allocate_memory
  cmp dx, 1
  jne allocate_memory_success
  exit

  allocate_memory_success:
  init
  call parse_command_line

  mov ah, 1ah
  mov dx, offset dta_buffer
  int 21h

  call_find_file 1
  ; call_log path
  cmp ds:[find_end], 1
  je start_end

  call_load_and_run
  cmp si, 1
  je start_end

  app_loop:
    call_find_file 0
    cmp ds:[find_end], 1
    je start_end

    call_load_and_run
    cmp si, 1
    je start_end
  jmp app_loop

  start_end:
  exit
end start