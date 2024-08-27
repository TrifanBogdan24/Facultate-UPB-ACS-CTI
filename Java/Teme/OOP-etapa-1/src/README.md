# TRIFAN BOGDAN-CRISTIAN, 322 CD
# ETAPA 1

## Comenzile de intrare

Pentru a citi comenzile si pentru a le stoca intr-un **ArrayList**,
am definit clasa **InCmd** care include toate field-urile gasite in toate
fisierele folder-ului **input/library/**.


Astfel, pentru citirea in mod corect a fiserelor **JSON** din **input/library/**
este necesar pentru clasa **InCmd** sa respecte urmatoarele conditii :
- numele field-ului din clasa sa coincida cu numele de dinainte de **:** din **JSON**
- tipul de date al field-ului sa respecte ce se afla dupa **:** in **JSON**

| JSON file              | InCmd class     |
|------------------------|-----------------|
| "command" : "search"   | String command  |
| "username" : "alice22" | String username |
| "timestamp" : 15       | int timestamp   |

Pentru filtrele de search trebuie definita a subclasa care se respecte aceasta logica.


> pentru ajutorul meu, am folosit urmatorul site :
> https://json2csharp.com/code-converters/json-to-pojo



## Comenzile de iesire

Pentru a afisa comenzile in folderul **result/**, am creat clasa 'OutCmd',
care respecta structura tuturor fisierelor din **ref/**.
Pentru constructia clasei 'OutCmd', am pastrat aceleasi principii ca si pentru 'InCmd'
(schimbate pentru fisiere JSON diferite) :
- numele field-ului din clasa sa coincida cu numele de dinainte de **:** din **JSON**
- tipul de date al field-ului sa respecte ce se afla dupa **:** in **JSON**


| OutCmd class   | JSON file            |
|----------------|----------------------|
| String command | "command" : "select" |
| String user    | "user" : "alice22"   |
| int timestamp  | "timestamp" : 15     |



Observam insa ca pentru comenzile  **showPrefferedPlaylists**, **showPreferredSongs**,
**getTop5Songs** si **getTop5Playlists**, tipul de data afisat difera, dar numele din **ref**
este acelasi, drept pentru care declar un **ArrayList<Object>**.

Tipul de date difera, doarece, in timp primele trei au nevoie de un array de string-uri
Comanda **showPlaylists** impune crearea unei clase seperate.

Este important ca in clasele care implementeaza aceste comenzii, la final,
sa se faca casting la tipul de date **Arraylist<Object>**

> pentru ajutorul meu, am folosit urmatorul site :
> https://json2csharp.com/code-converters/json-to-pojo


## Singleton class

Informatiile generale referitoare la starea player-ului 
vor fi tinute in clasa **SingletonInformation**, si acestea includ :

- rezultatul ultimului search
  - lista cu numele gasite
  - meldoiile gasite
  - playlist-urile gasite
  - podcast-urile gasite
  > un singur field din utlimele 3 poate fi nenul
- rezultatul ultimului select
  - melodiea selectata
  - playlist-ul selectat
  - podcast-ul selectat
  > un singur field poate fi nenul
- rezultatul ultimei incarcari (LOAD)
  - numele sursei 
  - melodia incarcata
  - playlist-ul incarcat
  - podcast-ul incarcat
  > un singur field din utlimele 3 poate fi nenul


Optez penru folosirea unui design pattern de tip **singleton**,
intrucat exista un singur player, deci informatiile acestuia trebuiesc
stocate intr-o instanta **unica**.



## Interface class

Interfata **FunctionHeader** contine functia abstracta **func**.

Pentru fiecare tip de comanda in parte, am creat cate o clasa care
interpreteaza comanda de input si seteaza field-urile unei comenzi de output,
implementand interfata mai sus mentionata.

Fiecare implementare a functiei din interfata 'rezolva' cate o comanda.

## Clasele de input

Pentru usurinta mea, am adaugat urmatoarele field-uri claselor din **fileio.input** :

| class       | new field                                     |
|-------------|-----------------------------------------------|
| SongInput   | private ArrayList<UserInput> usersWhoLikedIt; |
| UserInput   | private ArrayList<Playlist> playlists;        |
| UserInput   | private ArrayList<SongInput> likedSongs;      |



## Playlist

Creez o clasa seperata de cele de la input care contine toate informatiile unui playlist.
Un playlist poate avea melodii si utilizatorii care il au la **follow**
dar nu poate exista fara un utilzator (owner).



## Clasele pentru comenzi

Pentru fiecare comanda definesc o clasa care ii poarta numele si care aduce
o implementare diferita (in functie de comanda si context) a interfetei **FunctionHeader**

Functia implementata care rezolva problema sa va numeste **func**, dar
pentru a nu fi nevoie de a crea o instanta de a o apela,
am definite o functie statica intermediara **solve**, la a carui apel,
creaza o instanta a clasei respective si apeleaza functia **func**.

| **solve**                               | **func**                                         |
|-----------------------------------------|--------------------------------------------------|
| functie de tip helper                   | functie pentru interpretarea efectiva a comenzii |
| public static void solve(....)          | public void func(...)                            |
| nu are nevoie de o instanta a clasie    | are nevoie de o instanta a clasei                |

## Comenzi

In main voi face pattern matching (switch) pe numele comenzii de input,
iar in functie de acesta, voi epla functia **sovle** din clasa care poarta numele comenzii


"name" : "search" -> Search.solve -> se creaza o instanta a clasei Search si se apeleaza func
