section .data
	; declare global vars here

section .text
	global reverse_vowels

;;	void reverse_vowels(char *string)
;	Cauta toate vocalele din string-ul `string` si afiseaza-le
;	in ordine inversa. Consoanele raman nemodificate.
;	Modificare se va face in-place
reverse_vowels:
    push ebp
    push esp
    pop ebp
    pusha

    push dword [ebp + 8]
    pop ebx
    push ebx
    pop ecx

    xor eax, eax 
    ; eax = 0 (o folosim drept contor :
    ; in prima iteratie numaram caracterele,
    ; iar in a doua scadem acest numar si ne oprim cand ajungem la 0)
    

; inceputul primei iteteratii a string - ului (de la dreapta la stanga)

iter_back:
    ; verificare capat :
    cmp ebx, 0
    jle iter_fwd
    inc eax;

    ; verificare vocala:
    cmp dword [ebx], 'a'
    je push_vowel
    
    cmp dword [ebx], 'e'
    je push_vowel
    
    cmp dword [ebx], 'i'
    je push_vowel

    cmp dword [ebx], 'o'
    je push_vowel

    cmp dword [ebx], 'u'
    je push_vowel

    jmp iter_back ; continuare iteratie

push_vowel:
    push ebx
	dec ebx
    jmp iter_back ; continuare iteratie
; sfarsitul primei iteratii (de la dreapta la stanga)



; inceputul celei de a doua secvente (de la stanga la dreapta)


iter_fwd:
    ; verificare capat :
    cmp eax, 0
    jle end
    dec eax

    ; verifcare vocala :
    cmp dword [ebx], 'a'
    je pop_vowel

    cmp dword [ebx], 'e'
    je pop_vowel

    cmp dword [ebx], 'i'
    je pop_vowel

    cmp dword [ebx], 'o'
    je pop_vowel

    cmp dword [ebx], 'u'
    je pop_vowel

    jmp iter_fwd ; continuare iteratie

pop_vowel:
    pop ebx         
    inc ebx
    jmp iter_fwd

end:

; sfarsitul celei de a doua iteratii
    popa
    push ebp
    pop esp
    pop ebp
	ret
