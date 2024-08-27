## TRIFAN BOGDAN-CRISTIAN, 312 CD
## TEMA 2 IOCLA

# TASK 1
    Primul task consta in parcurgerea sirului de caractere
    (eu am ales de la dreapta la stanga, pentru ca este mai usor
    sa il decrementam pe n decat sa luam alt registru in care sa momeram indexul).
    
    Extragem elementul din sirul plain si adaugam numarul de pasi.
    Daca charul depaseste codul ASCII al lui Z, atunci scadem 'Z', scadem 1 si adunam 'A'.
    Pentru implementare, am creat doua label-uri : 
        -> iteratie_for
        -> atribuire_caracter_codat

    Rolul primului label este de a extrage caracterul din sirul plain
    intr-un registru nou numit 'bl', la care adun numarul de pasi.
    
    Daca caractul nu are codul ASCII mai mare decat 'Z', 
    atunci intram in cel de al doilea label,
    care construieste sirul 'enc_string' si decrementeaza contorul.
    Aceste operatii se vor efectua chiar si daca bl > 'Z',
    dar nu inainte de a modifica valoarea retinuta in registrul bl
    prin urmatoarea ecuatie : bl = bl - 'Z' - 1 + 'A'.

# TASK 4
    Cea de a patra cerinta a acestei teme ne cere sa setam cu 1
    caracterele vecine pe diagonala cu piesa de joc.

    Pentru a afla daca fiecare vecin este pe o pozitie valida (nu iese din matrice)
    Am implementat echivalentul a patru if - uri
    in care facem cate doua comparatii.

    Stim ca piesa de joc este de coordonate (x ,y) (x -> eax ; y -> ebx), iar cei 4 vecini ai ei ocupa pozitiile :
        -> stanga-sus   : (x - 1, y - 1)
        -> dreapta-sus  : (x - 1, y + 1)
        -> stanga-jos   : (x + 1, y - 1)
        -> dreapta-jos  : (x + 1, y + 1)
    
    Pentru ca un vecin sa existe, atunci el trebuie sa fie in interiorul matricii,
    cu alte cuvinte, atat valorile memorate in registrele eax si ebx (x si y)
    sa apartina intervalului inchis [0, 7]
        
        Exista stanga-sus <=>
            0 <= x - 1 <= 7
            0 <= y - 1 <= 7

        Exista dreapta-sus <=>
            0 <= x - 1 <= 7
            0 <= y + 1 <= 7

        Exista stanga-jos <=>
            0 <= x + 1 <= 7
            0 <= y - 1 <= 7

        Exista dreapta-jos <=>
            0 <= x + 1 <= 7
            0 <= y + 1 <= 7

    Totusi, cand scadem 1 din una dintre aceste valorile, care sunt by default <= 7, nu mai avem de ce sa comparam x - 1 sau y - 1 cu 7.
    In aceeasi ordine de idei, comparatiile 0 <= x + 1 si 0 <= y + 1 devin redundante.

    In concluzie, avem 4 mari cazuri si 8 'if' - uri de facut

        Exista stanga-sus   <=> (0 <= x - 1)  && (0 <= y - 1)
        Exista dreapta-sus  <=> (0 <= x - 1)  && (y + 1 <= 7)
        Exista stanga-jos   <=> (x + 1 <= 7)  && (0 <= y - 1)
        Exista dreapta-jos  <=> (x + 1  <= 7) && (y + 1 <= 7)

    Deoarece nu doresc sa folosesc alte registre pentru a calcula coordonatele
    de mai sus, voi face over-riding pe registrele eax si ebx, dar nu inainte
    de a le pune pe stiva, ca mai apoi, la finalul fiecarui caz in parte,
    sa le resturez.

    Pentru fiecare caz in parte, trebuie sa calculam al catelea element
    din matrice reprezinta vecinul piesei, dupa formuala
        if (x_vecin > 0)
            index_vecin = (x_vecin - 1) * 8 + y_vecin
        else
            index_vecin = y_vecin
    , valoare pe care urmeaza sa o memorez in registrul edx.
    
    Spre exemplu, daca vecinul se afla pe linia 1, coloana 2, ne referim la al 11 - lea element (edx = 10)

    Daca vecinul se afla pe linia 0, coloana 7, ne refrim la al 8-lea element (edx = 7)
    
    Prin urmare, valoarea pe care registrul edx o memoreaza
    reprezinta al catelea bit din vectorul plain trebuie setat cu 1.

    Iar pentru a set al n - ulea bit din matricea (prin n ma refer la val. edx),
    accesam elementul din matrice astfel : [ecx + edx].
        ecx -> registrul matricii
        edx -> indexul al carui bit il setam cu 1

    Operatia prin care facem aceasta atribuire este : mov byte [ecx + edx], 1

    Acum, ca am terminat verificarea apartenentei unui vecin la matrice
    si setarea bitului de la indexul sau in cazul afirmativ,
    restauram valorile celor doua registre care memoreaza coordonatele piesei
    (eax si ebx), pe care la extragem din stiva.
    

# BONUS
    Preluam de la TASK-4 structura de verificare a existentei a fiecarui vecin in parte al piesei.
    
    Vom folosi stiva pentru a pune coordonatele damei pe table de joc (x si y),
    pe care, pentru a nu folosi alte variabile (registre), le voi modifca pentru
    a calcula pozitia fiecarui vecin si indice la care apare in matrice,
    indice memorat in edx si calculat dupa formula : x * 8 + y.
    La finalul operatiilor pe care urmeaza sa le explic, voi restaura
    x si y (registrele eax si ebx) cu valorile din stiva.
    
    Daca valoarea memorata in edx este mai mica sau egala decat 32, atunci suntem in partea de jos a matricii,
    care reprezinta al doilea numar (board[1]), 
    numar pentru caruia ii vom adauga 2 la puterea numarului memorat in edx.
    
    Altfel, suntem in jumatatea superioara a tabloului bidimensional, 
    adica ne referim la primul numar (board[0]).
    In acest caz, trebuie sa scadem 32 din valoarea lui edx inainte
    de a adauga 2 la puterea memorata in el la board[0].    

    Pentru efectuarea operatiei 2 ^ val_edx, am ales o implementare iterativa :
    Pornesc cu variabila (in registrul eax) initializata cu 1 si
    cat timp puterea la care ridic (in registrul edx) este nenula,
    atunci inmultim valoarea lui eax cu 2 si decrementam puterea.
    La finalul 'while'-ului, valoarea dorita va fi memorata in registrul eax,
    si o putem aduna la numarul dorit : registrul [ecx] daca ne referim primul numar
    sau [ecx + 4], daca vecinul este in partea de sus a matricii.
    Adunam 4 la ecx, deoarece tipul int se reprezinta pe 4 octeti, iar
    noi sarim peste board[0] (care este de tipul int si ocupa 4 baiti)
    pentru a-l accesa pe board[1].


