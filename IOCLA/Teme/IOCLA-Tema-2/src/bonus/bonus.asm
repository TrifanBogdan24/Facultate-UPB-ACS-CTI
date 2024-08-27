%include "../include/io.mac"

section .data

section .text
    global bonus
    extern printf

bonus:
    ;; DO NOT MODIFY
    push ebp
    mov ebp, esp
    pusha

    mov eax, [ebp + 8]	; x
    mov ebx, [ebp + 12]	; y
    mov ecx, [ebp + 16] ; board
    
    ;; DO NOT MODIFY
    ;; FREESTYLE STARTS HERE

    ; PRINTF32 `x = %u ; y = %u.\n\x0`, eax, ebx
    ; PRINTF32 `\n\n\x0`
    push eax
    push ebx

    mov edx, 8
    mul edx
    add edx, eax
    add edx, ebx
    ; edx va retine (x - 1)* 8 + y; x, y - initiale

    ; PRINTF32 `Elementul are indexul = %u.\n\n\x0`, edx
    pop ebx
    pop eax
    ; if (x > 0 && y > 0)
    cmp eax, 0
    jle .if_statement_1 ; sari la .if_statement_1 daca x <= 0
    cmp ebx, 0
    jle .if_statement_1 ; sari la .if_statement_1 daca y <= 0
    
    ; cazul pentru vecin stanga-sus : x > 0 && y > 0

    ; PRINTF32 `Exista vecin stanga sus (\x0`
    ; PRINTF32 `x = %u > 0 si \x0`, eax
    ; PRINTF32 `y = %u > 0)\n\x0`, ebx

    ; vecinul stanga-sus
    ; table[x - 1][y - 1] = 1

    ; aflam elementul de pe pozitia (x - 1, y - 1)
    ; folosim stiva pentru a salva valorile lui x si y
    ; intrucat vom modifica valorile registrelor eax si ebx
    ; iar cand extragem din stiva, vom retribui valorile initiale
    push eax
    push ebx

    dec eax
    dec ebx

    ; PRINTF32 `Vecinul stanga-sus (%u - %u). \x0`, eax, ebx
    mov edx, 8
    mul edx
    add edx, eax
    add edx, ebx
    ; edx va retine (x -1) * 8 + (y - 1), x, y - initiale

    ; setare bit

    cmp edx, 32
    jle .setare_bit_al_doilea_numar_1   ; sari la setare_bit_al_doilea_numar if edx <= 32

    ; PRINTF32 ` este al %u-lea element si va seta\x0`, edx
    sub edx, 32
    ; PRINTF32 ` bitul %u din primul numar cu 1.\n\0`, edx

    ; PRINTF32 `2 ^ %u = \x0`, edx
    mov eax, 1

    ; iteram de edx ori si facem eax ^ 2 (eax va fi mereu putere a lui 2)
.pow_1:
    cmp edx, 0   
    jle .2_la_edx_indice_1      ; ne oprim cand puterea este 0
    add eax, eax                ; inmultim cu doi - dublam valoarea
    dec edx                     ; scadem puterea
    jmp .pow_1                  ; iteram din nou

.2_la_edx_indice_1:
    ; eax = 2 ^ edx_initial
    ; PRINTF32 `%u = \n\x0`, eax

    ; vrem sa adaugam 2 ^ edx la primul numar
    add [ecx], eax
    jmp .end_1

.setare_bit_al_doilea_numar_1:
    ; PRINTF32 `Vecinul stanga-sus va seta bit-ul %u din al doilea numar cu 1.\n\x0`, edx
    
    ; PRINTF32 `2 ^ %u = \x0`, edx
    mov eax, 1

    ; calculam 2 ^ edx
    ; iteram de edx ori si facem eax ^ 2 (eax va fi mereu putere a lui 2)

.pow_2:
    cmp edx, 0   
    jle .2_la_edx_indice_2      ; ne orpim cand puterea este 0
    add eax, eax                ; inmultim cu doi - dublam valoarea
    dec edx                     ; scadem puterea
    jmp .pow_2                  ; iteram din nou

.2_la_edx_indice_2:
    ; ecx = 2 ^ edx_initial
    ; PRINTF32 `%u\n\x0`, eax
    ; vrem sa adaugam 2 ^ edx la al doilea numar
    add [ecx + 4], eax

.end_1:
    ; restauram valorile lui eax = x si ebx = y
    pop ebx
    pop eax
    ; byte is setare_bit


.if_statement_1:
    ; if (x > 0 && y < 7)
    cmp eax, 0 
    jle .if_statement_2 ; if x <= 0, atunci facem salt la .if_statement_2
    cmp ebx, 7 ;
    jge .if_statement_2 ; if y >= 7, atunci facem salt la .if_statement_2
    
    ; cazul pentru vecin dreapta-sus : x > 0 && y < 7
    
    ; PRINTF32 `Exista vecin dreapta sus (\x0`
    ; PRINTF32 `x = %u > 0 si \x0`, eax
    ; PRINTF32 `y = %u < 7)\n\x0`, ebx

    ; vecinul drapta-sus
    ; table[x - 1][y + 1] = 1

    ; aflam elementul de pe pozitia (x - 1, y + 1)
    ; folosim stiva pentru a salva valorile lui x si y
    ; intrucat vom modifica valorile registrelor eax si ebxsecond
    ; iar cand extragem din stiva, vom retribui valorile initiale

    push eax
    push ebx

    dec eax
    inc ebx
    ; PRINTF32 `Vecinul dreapta-sus (%u - %u). \x0`, eax, ebx

    mov edx, 8
    mul edx
    add edx, eax
    add edx, ebx
    ; edx va retine (x - 1) * 8 + (y + 1), x, y - initiale

    ; setare bit

    cmp edx, 32
    jle .setare_bit_al_doilea_numar_2   ; sari la .setare_bit_al_doilea_numar if edx <= 32

    ; PRINTF32 `Vecinul dreapta-sus este al %u-lea element si va seta\x0`, edx
    sub edx, 32
    ; PRINTF32 ` bitul %u din al primul numar cu 1.\n\0`, edx

    ; PRINTF32 `2 ^ %u = \x0`, edx
    mov eax, 1

    ; calculam 2 ^ edx
    ; iteram de edx ori si facem eax ^ 2 (eax va fi mereu putere a lui 2)

.pow_3:
    cmp edx, 0
    jle .2_la_edx_indice_3      ; ne orpim cand puterea este 0
    add eax, eax                ; inmultim cu doi - dublam valoarea
    dec edx                     ; scadem puterea
    jmp .pow_3                  ; iteram din noua

.2_la_edx_indice_3:
    ; ecx = 2 ^ edx_initial
    ; PRINTF32 `%u\n\x0`, eax
    ; vrem sa adaugam 2 ^ edx la primul numar
    add [ecx], eax
    jmp .end_2

.setare_bit_al_doilea_numar_2:
    ; PRINTF32 `Vecinul dreapta-sus va seta bit-ul %u din al doilea numar cu 1.\n\x0`, edx
    
    ; PRINTF32 `2 ^ %u = \x0`, edx
    mov eax, 1

    ; calculam 2 ^ edx
    ; iteram de edx ori si facem eax ^ 2 (eax va fi mereu putere a lui 2)

.pow_4:
    cmp edx, 0   ;
    jle .2_la_edx_indice_4      ; ne oprim cand puterea este 0
    add eax, eax                ; inmultim cu doi - dublam valoarea
    dec edx                     ; scadem puterea
    jmp .pow_4                  ; iteram din nou

.2_la_edx_indice_4:
    ; ecx = 2 ^ edx_initial
    ; PRINTF32 `%u\n\x0`, eax
    ; vrem sa adaugam 2 ^ edx la al doilea numar
    add [ecx + 4], eax


.end_2:
     ; restauram valorile lui eax = x si ebx = y
    pop ebx
    pop eax
    ; bit-ul este setat


.if_statement_2:
    ; if (x < 7 && y > 0)
    cmp eax, 7
    jge .if_statement_3 ; if x >= 7, atunci facem salt la .if_statement_3
    cmp ebx, 0
    jle .if_statement_3 ; if y <= 0, atunic facem salt la .if_statement_3

    ; cazul pentru vecin stanga jos : x < 7 && y > 0
    
    ; PRINTF32 `Exista vecin stanga jos (\x0`
    ; PRINTF32 `x = %u < 7 si \x0`, eax
    ; PRINTF32 `y = %u > 0)\n\x0`, ebx

    ; vecinul stanga-jos
    ; table[x + 1][y - 1] = 1
    ; aflam elementul de pe pozitia (x + 1, y + 1)
    ; folosim stiva pentru a salva valorile lui x si y
    ; intrucat vom modifica valorile registrelor eax si ebx
    ; iar cand extragem din stiva, vom retribui valorile initiale

    push eax
    push ebx

    inc eax
    dec ebx
    ; PRINTF32 `Vecinul stanga-jos (%u - %u). \x0`, eax, ebx

    mov edx, 8
    mul edx
    add edx, eax
    add edx, ebx
    ; edx va retine (x + 1) * 8 + (y - 1) , x, y - initiale

    ; setare bit

    cmp edx, 32
    jle .setare_bit_al_doilea_numar_3   ; sari la .setare_bit_al_doilea_numar if edx <= 32

    ; PRINTF32 `Vecinul stanga-jos este al %u-lea element si va seta\x0`, edx
    sub edx, 32
    ; PRINTF32 ` bitul %u din primul numar cu 1.\n\0`, edx

    ; PRINTF32 `2 ^ %u = \x0`, edx
    mov eax, 1

    ; calculam 2 ^ edx
    ; iteram de edx ori si facem eax ^ 2 (eax va fi mereu putere a lui 2)
.pow_5:
    cmp edx, 0
    jle .2_la_edx_indice_5      ; ne oprima cand puterea este 0
    add eax, eax                ; inmultim cu doi - dublam valoarea
    dec edx                     ; scadem putrea
    jmp .pow_5                  ; iteram din nou

.2_la_edx_indice_5:
    ; ecx = 2 ^ edx_initial
    ; PRINTF32 `%u\n \x0`, eax
    ; vrem sa adaugam 2 ^ edx la primul numar
    add [ecx], eax
    jmp .end_3

.setare_bit_al_doilea_numar_3:
    ; PRINTF32 `2 ^ %u = \x0`, edx
    mov eax, 1

    ; calculam 2 ^ edx
    ; iteram de edx ori si facem eax ^ 2 (eax va fi mereu putere a lui 2)

.pow_6:
    cmp edx, 0
    jle .2_la_edx_indice_6      ; ne oprim cand puterea este 0
    add eax, eax                ; ridcam la patrat
    dec edx                     ; scadem puterea
    jmp .pow_6                  ; iteram din nou

.2_la_edx_indice_6:    
    ; PRINTF32 `%u\n \x0`, eax
    ; vrem sa adaugam 2 ^ edx la al doilea numar
    add [ecx + 4], eax
    ; PRINTF32 `Vecinul stanga-jos va seta bit-ul %u din al doilea numar cu 1.\n\x0`, edx

.end_3:
    ; restauram valorile : eax = x si ebx =x
    pop ebx
    pop eax
    ; bit-ul este setat
    
.if_statement_3:
    ; if (x < 7 && y < 7)
    cmp eax, 7
    jge .nu_fac_nimic ; if x >= 7, atunci facem salt la .nu_fac_nimic
    cmp ebx, 7
    jge .nu_fac_nimic ; if y >= 7, atunci facem salt la .nu_fac_nimic

    ; cazul pentru vecin drapta-jos :  x < 7 && y < 7
    
    ; PRINTF32 `Exista vecin dreapta jos (\x0`
    ; PRINTF32 `x = %u < 7 si \x0`, eax
    ; PRINTF32 `y = %u < 7)\n\x0`, ebx

    ; vecinul dreapta sus
    ; table[x + 1][y + 1] = 1
    ; aflam elementul de pe pozitia (x + 1, y + 1)
    ; folosim stiva pentru a salva valorile lui x si y
    ; intrucat vom modifica valorile registrelor eax si ebx
    ; iar cand extragem din stiva, vom retribui valorile initiale

    push eax
    push ebx

    inc eax
    inc ebx
    ; PRINTF32 `Vecinul dreapta-jos (%u - %u).\x0`, eax, ebx

    mov edx, 8
    mul edx
    add edx, eax
    add edx, ebx
    ; edx va retine (x + 1) * 8 + (y + 1), x, y - initiale

    ; setare bit

    cmp edx, 32
    jle .setare_bit_al_doilea_numar_4   ; sari la .setare_bit_al_doilea_numar if edx <= 32


    ; PRINTF32 `Vecinul dreapta-jos este al %u-lea element si va seta\x0`, edx
    sub edx, 32
    ; PRINTF32 ` bitul %u din primul numar cu 1.\n\0`, edx
    
    ; PRINTF32 `2 ^ %u = \x0`, edx
    mov eax, 1

    ; calculam 2 ^ edx
    ; iteram de edx ori si facem eax ^ 2 (eax va fi mereu putere a lui 2)

.pow_7:
    cmp edx, 0
    jle .2_la_edx_indice_7      ; ne oprim cand puterea este 0
    add eax, eax                ; inmultim cu doi - dublam valoarea
    dec edx                     ; scadem puterea
    jmp .pow_7                  ; iteram din nou

.2_la_edx_indice_7:
    ; ecx = 2 ^ edx_initial
    ; PRINTF32 `%u\n \x0`, eax
    ; vrem sa adaugam 2 ^ edx la al primul numar
    add [ecx], eax
    jmp .end_4

.setare_bit_al_doilea_numar_4:
    ; PRINTF32 `Vecinul dreapta-sus va seta bit-ul %u din al doilea numar cu 1.\n\x0`, edx
    mov eax, 1
    ; PRINTF32 `2 ^ %u = \x0`, edx

    ; calculam 2 ^ edx
    ; iteram de edx ori si facem eax ^ 2 (eax va fi mereu putere a lui 2)

.pow_8:
    cmp edx, 0
    jle .2_la_edx_indice_8      ; ne oprima cand puterea este 0
    add eax, eax                ; ridcam la patrat
    dec edx                     ; scadem puterea
    jmp .pow_8                  ; iteram din nou

.2_la_edx_indice_8:
    ; ecx = 2 ^ edx_initial
    ; PRINTF32 `2 ^ %u = \x0`, eax

    ; vrem sa adaugam 2 ^ edx  la al doilea numar
    add [ecx + 4], eax

.end_4:
    ; restauram valorile : eax = x si ebx = y
    pop ebx
    pop eax
    ; bit-ul este setat

.nu_fac_nimic:
    ; aici nu se intampla nimic
    ; este else - ul care nu face nimic
    ; cand cazul nu este unul favorabil, ajungem aici
    

    ;; FREESTYLE ENDS HERE
    ;; DO NOT MODIFY
    popa
    leave
    ret
    ;; DO NOT MODIFY
