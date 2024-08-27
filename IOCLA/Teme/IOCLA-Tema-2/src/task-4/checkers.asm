%include "../include/io.mac"

section .data

section .text
	global checkers
    extern printf

checkers:
    ;; DO NOT MODIFY
    push ebp
    mov ebp, esp
    pusha

    mov eax, [ebp + 8]	; x
    mov ebx, [ebp + 12]	; y
    mov ecx, [ebp + 16] ; table

    ;; DO NOT MODIFY
    ;; FREESTYLE STARTS HERE
    ; PRINTF32 `x = %u ; y = %u\n\x0`, eax, ebx

    ; if (x > 0 && y > 0)
    cmp eax, 0
    jle .if_statement_1 ; sari la daca x <= 0
    cmp ebx, 0
    jle .if_statement_1 ; sar la daca y <= 0

    ; PRINTF32 `Exista vecin stanga sus (\x0`
    ; PRINTF32 `x = %u > 0 si \x0`, eax
    ; PRINTF32 `y = %u > 0)\n\x0`, ebx

    ; vecinul stanga sus
    ; table[x - 1][y - 1] = 1

    ; aflam elementul de pe pozitia (x - 1, y - 1)
    ; folosim stiva pentru a salva valorile lui x si y
    ; intrucat vom modifica valorile registrelor eax si ebx
    ; iar cand extragem din stiva, vom retribui valorile initiale
    
    push eax
    push ebx
    
    dec eax
    dec ebx

    mov edx, 8
    mul edx
    add eax, ebx
    mov edx, eax ; edx va retine (x - 2) * 8 + (y - 1), x, y - initiale
    mov byte [ecx + edx], 1 ; table[x - 1][y - 1] = 1
    
    pop ebx
    pop eax


.if_statement_1:
    ; if (x > 0 && y < 7)
    cmp eax, 0 
    jle .if_statement_2 ; if x <= 0, atunci facem salt la if_statement_2
    cmp ebx, 7 ;
    jge .if_statement_2 ; if y >= 7, atunci facem salt la if_statement_2
    
    ; code for x > 0 && y < 7 goes here
    
    ; PRINTF32 `Exista vecin dreapta sus (\x0`
    ; PRINTF32 `x = %u > 0 si \x0`, eax
    ; PRINTF32 `y = %u < 7)\n\x0`, ebx

    ; vecinul dreapta sus
    ; table[x - 1][y + 1] = 1

    ; aflam elementul de pe pozitia (x - 1, y + 1)
    ; folosim stiva pentru a salva valorile lui x si y
    ; intrucat vom modifica valorile registrelor eax si ebx
    ; iar cand extragem din stiva, vom retribui valorile initiale

    push eax
    push ebx

    dec eax
    inc ebx

    mov edx, 8
    mul edx
    add eax, ebx
    mov edx, eax ; edx va retine (x - 2) * 8 + (y + 1), x, y - initiale
    mov byte [ecx + edx], 1 ; table[x - 1][y + 1] = 1

    pop ebx
    pop eax


.if_statement_2:
    ; if (x < 7 && y > 0)
    cmp eax, 7 
    jge .if_statement_3 ; if x >= 7, atunci facem salt la if_statement_3
    cmp ebx, 0
    jle .if_statement_3 ; if y <= 0, atunic facem salt la if_statement_3

    ; code for x < 7 && y > 0 goes here
    
    ; PRINTF32 `Exista vecin stanga jos (\x0`
    ; PRINTF32 `x = %u < 7 si \x0`, eax
    ; PRINTF32 `y = %u > 0)\n\x0`, ebx

    ; vecinul stanga jos
    ; table[x + 1][y - 1] = 1
    ; aflam elementul de pe pozitia (x + 1, y + 1)
    ; folosim stiva pentru a salva valorile lui x si y
    ; intrucat vom modifica valorile registrelor eax si ebx
    ; iar cand extragem din stiva, vom retribui valorile initiale
    
    push eax
    push ebx
    
    inc eax
    dec ebx
    
    mov edx, 8
    mul edx
    add eax, ebx
    mov edx, eax ; edx va retine x * 8 + (y +1) , x, y - initiale
    mov byte [ecx + edx], 1 ; table[x + 1][y + 1] = 1

    pop ebx
    pop eax

.if_statement_3:
    ; if (x < 7 && y < 7)
    cmp eax, 7
    jge .nu_fac_nimic ; if x >= 7, atunci facem salt la .nu_fac_nimic
    cmp ebx, 7
    jge .nu_fac_nimic ; if y >= 7, atunci facem salt la .nu_fac_nimic

    ; code for x < 7 && y < 7 goes here

    ; PRINTF32 `Exista vecin dreapta jos (\x0`
    ; PRINTF32 `x = %u < 7 si \x0`, eax
    ; PRINTF32 `y = %u < 7)\n\x0`, ebx

    ; vecinul dreapta jos
    ; table[x + 1][y + 1] = 1
    ; aflam elementul de pe pozitia (x + 1, y + 1)
    ; folosim stiva pentru a salva valorile lui x si y
    ; intrucat vom modifica valorile registrelor eax si ebx
    ; iar cand extragem din stiva, vom retribui valorile initiale

    push eax
    push ebx

    inc eax
    inc ebx

    mov edx, 8
    mul edx
    add eax, ebx
    mov edx, eax ; edx va retine x * 8 + (y + 1), x, y - initiale
    mov byte [ecx + edx], 1 ; table[x + 1][y + 1] = 1

    pop ebx
    pop eax

.nu_fac_nimic:
    ; aici nu se intampla nimic
    ; este else - ul care nu face nimi
    ; cand cazul nu este unul favorabil, ajungem aici
    

    ;; FREESTYLE ENDS HERE
    ;; DO NOT MODIFY
    popa
    leave
    ret
    ;; DO NOT MODIFY

