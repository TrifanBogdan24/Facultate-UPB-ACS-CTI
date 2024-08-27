# TRIFAN BOGDAN-CRISTIAN, 322 CD
# ETAPA 2

> Pentru rezolvarea etapei 2, am folosit solutia oficiala de la etapa precedenta.
> Am reformatat ulterior acest cod.

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

Pentru a afisa comenzile in folderul **result/**, am respectat principiile din
implementarea primei etape, folosind `ObjectMapper` si `ObjectNode`
```angular2html
        ObjectMapper objectMapper = new ObjectMapper();
        ObjectNode objectNode = objectMapper.createObjectNode();

        objectNode.put("command", commandInput.getCommand());
        objectNode.put("user", commandInput.getUsername());
        objectNode.put("timestamp", commandInput.getTimestamp());
        objectNode.put("message", message);

        // in cazul in care field-ul contine un ArrayList
        objectNode.put("result", objectMapper.valueToTree(result));
```

Pentru field-urile care au asociate o colectie de obiecte, se va folosi
**objectMapper.valueToTree(collection)**


## Singleton Design Pattern

> Class SingletonAdmin

Am modification clasa initial denumita **Admin**, facand-o Singleton.
Pentru a respecta acest Design Pattern care presupunere existenta unei unice
instante a clasei (de tip Singleton). folosim un constructor privat, care este
folosit doar in cadrul clasei si care este apelat numai atunci cand
unica instanta este nula (inca nu a fost creata instanta clasei) 

Proprietatile (field-urile clasei) retin informatii referitoare la
- toti utilizatorii din sistem
- toate melodiile din sistem
- toate podcast-urile din sistem
- toate evenenimentele din sistem
- toate albumele din sistem
- toate merch-urile din sistem
- toate anunturile din sistem


Optez penru folosirea unui design pattern de tip **Singleton**,
intrucat exista un singur admin, care are acces la toata 'baza de date a aplicatiei',
deci informatiile acestuia trebuiesc stocate intr-o instanta **unica**.


## Command Design Pattern

Initial, fisierul `CommandRunner` facea refire la o clasa care contine metode
la a caror apel, se genera o comanda de iesire, metode ce poarta denumirea
comenzii pe care o interpreteaza.

Primul meu pas a fost sa distribui aceste metode din fisierul initial in `Clase`
numite dupa aceste metode, care sa le contina. Fiecare clasa va contine si metodele
folosite pentru generarea campurilor `message` si `result`, pentru a crea un cod
cat mai intuitiv si mai usor de depanat.

Mai apoi, am redenumit metodele care construiesc comanda de output cu numele `execute`.
Astfel, acesta a fost primul pas pentru a crea `Design Pattern-ul Command`,
Design Pattern care cuprinde in clasa mea

- `Interface CommandRunner`
  1. contine metoda abstracta `ObjectNode execute(CommandInput commandInput);`
  1. contine metoda `createCommand`, in care a fost mutata intreaga logica din `Main`
     pentru alegerea comenzii corespunzatoare, in functie de numele acesteia

- `public final class <cmdname> implements CommandRunner`
  (in loc de <cmdname> va fi numele comenzii: Search, Select, Load ...)
  1. implementeaza interfata `CommandRunner`
  1. `@Override public ObjectNode execute(final CommandInput commandInput) { /* code to generate the output command */]`
  implementeaza metoda abstracta din interfata `CommandRunner`
- `public class CommandInvoker`
  1. contine o singura metoda `invoke`, menita sa apeleze metoda `execute` a unei comenzi data ca parametru
- consideram `Main`-ul ca fiind **receiver-ul comandatului**, iar la fiecare comanda de input,
  vom obtine o instanta a clasei ce poarta numele comenzii, pe care o vom folosi (aluturi de comanda de intrare)
  pentru a invoca metoda `execute` a acesteia, si pentru a obtine rezultatul interpretarii ei.

In aceasta implementare a **Design Pattern-ului Command**, nu este nevoie stocarea
istoricului de comenzi sau pentru a rula o comanda mai veche.

In proiect, fieacere comanda este interpretata imediat cum este citita si nu se va mai
putea reveni la aceasta doar daca este citita din nou.


### Clase noi

> Noi clase care `extind` clasa `User`: `Artist` si `Host`

Artist is an User    => mostenire
Host is an User      => mostenire

> Noi clase care sunt in relatie de `compunere` cu `Artist`: `Album`, `Event`, `Merch`

Artist has a/an Album/Event/Merch   => compunere

> Noi clase care este in relatie de `compunere` cu `Host`: `Announcement`.

Artist has an announcement          => compunere

Clasa `Podcast` devine in relatie de compunere cu `Host` din acelasi motiv

Orice modificare (adaugare / eliminare) a claselor cu care utilizatorii
sunt in relatie de compunere vor aduce modificari in baza de date a aplicatiei,
retinuta de catre unica instanta a clasei **SingletonAdmin**.

Comenziile de tip `Add*` si `Remove*` vor updata atat campul corespunzator din clasa
utilizatorului, cat si informatiile retine in **SingletonAdmin**.

Pentru a retine toate aceste noi entitati in baza de date
fiecare va avea un field destinat numului utilizatorului (host / artist)
care le-a creat.

> Clasa Album extinde (mostenteste) proprietatile si metodele clasii AudioCollection
> care are (by default) field-ul pentru utilizatorul care detine colectia.