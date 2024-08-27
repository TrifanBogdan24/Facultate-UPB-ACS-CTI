%include "../include/io.mac"

struc proc
  .pid: resw 1
  .prio: resb 1
  .time: resw 1
endstruc

section .text
  global sort_procs
  extern printf

sort_procs:
  ;; DO NOT MODIFY
  enter 0,0
  pusha

  mov edx, [ebp + 8]      ; processes
  mov eax, [ebp + 12]     ; length
  ;; DO NOT MODIFY

  ;; Your code starts here

  mov ecx, eax     ; ecx = num_elements

  ; Initialize pointer to first element of vector
  mov esi, edx     ; esi = pointer to vector
  mov ax, [esi + proc.pid]   ; ax = pid
  mov bl, [esi + proc.prio]  ; bl = prio
  mov cx, [esi + proc.time]  ; cx = time
  add esi, 5
  mov ax, [esi + proc.pid]   ; ax = pid

  jmp .loop_end
.loop_start:
  ; Check if loop counter is zero, exit if so
  cmp ecx, 0
  je .loop_end

  ; Load current struct proc into registers

  ; TODO: Process current struct proc here

  ; Increment pointer to next struct proc
  add esi, 6       ; esi += sizeof(struct proc)

  ; Decrement loop counter
  dec ecx

  ; Continue looping
  jmp .loop_start

.loop_end:
  ;; Your code ends here
  
  ;; DO NOT MODIFY
  popa
  leave
  ret
  ;; DO NOT MODIFY
