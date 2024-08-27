%include "../include/io.mac"

section .text
    global simple
    extern printf

simple:
    ;; DO NOT MODIFY
    push    ebp
    mov     ebp, esp
    pusha

    mov     ecx, [ebp + 8]  ; len
    mov     esi, [ebp + 12] ; plain
    mov     edi, [ebp + 16] ; enc_string
    mov     edx, [ebp + 20] ; step

    ;; DO NOT MODIFY
   
    ;; Your code starts here


    mov ecx, [ebp + 8]  ; valoarea lui ecx este n (n -> ecx)
    mov esi, [ebp + 12] ; valoarea lui esi este sirul de char-uri plain
    mov edi, [ebp + 16] ; valoare lui ebp este sirul de char-uri enc_string
    mov edx, [ebp + 20] ; valoearea lui edx este step (step -> edx)


iteratie_for:
    ; vom itera sirul de la drapta la stanga (de la n la 1)
    mov bl, [esi + ecx - 1] ; bl retine valoarea char-ului de la indexul curent din plain
    add bl, dl ; daplasam 'bl' cu numarul de pasi
    cmp bl, 'Z' ; daca 'bl', este mai mare decat 'Z'
    jle atribuire_caracter_codat ; daca intram in 'atribuire_caracter_codat'
    sub bl, 'Z' - 'A' + 1 ; daca da, corectam char - ul a.i sa fie litera mare
    ; de vreme ce am ajuns aici, programul nu a gasit niciun jump
    ; si va trece automat la 'atribuire_caracter_codat'

atribuire_caracter_codat:
    
    mov [edi + ecx - 1], bl ; setam char-ul codat la indexul curent
    loop iteratie_for ; decrementeaza iteratorul
    ;; Your code ends here
    
    ;; DO NOT MODIFY

    popa
    leave
    ret
    
    ;; DO NOT MODIFY
