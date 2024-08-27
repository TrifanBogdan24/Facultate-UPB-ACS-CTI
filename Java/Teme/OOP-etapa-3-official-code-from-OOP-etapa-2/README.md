# TRIFAN BOGDAN-CRISTIAN, 322 CD
# ETAPA 3

> Pentru rezolvarea etapei 3, am folosit solutia oficiala de la etapa precedenta.
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
```java
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
Clasele care implementeaza acest Design Pattern:
> class SingletonAdmin
> class CommandRunner

Am modification clasa initial denumita **Admin**, redunimind-o **SingletonAdmin**.

Clasa **SingletonAdmin** a fost creata create de voi.
Eu am creat in aceasi idee clasa de tip Singleton `CommandRunner`,
care, alaturi de alte clase, intra in structura unui alt Design Patter (`Command`).


Creare unei clase care respecta acest Design Pattern,
presupunere existenta unei unice instante a clasei (de tip Singleton).
Folosim un constructor privat, care este folosit doar in cadrul clasei si care este
apelat numai atunci cand unica instanta este nula (inca nu a fost creata instanta clasei)

Proprietatile (field-urile clasei) clasei `SingletonAdmin` retin informatii referitoare la
- toti utilizatorii din sistem
- toate melodiile din sistem
- toate podcast-urile din sistem
- toate evenenimentele din sistem
- toate albumele din sistem
- toate merch-urile din sistem
- toate anunturile din sistem


La solotia oficiala, s-a optat penru folosirea unui design pattern de tip **Singleton**,
intrucat exista un singur admin, care are acces la toata 'baza de date a aplicatiei',
deci informatiile acestuia trebuiesc stocate intr-o instanta **unica**.


In aceeasi idee, nu este logic sa cream mai multe instante ale clasei `CommandRunner`,
intrucat clasa are un singur rol: sa execute o comanda, fara alte prelucrari de date.



## Command Design Pattern

Initial, fisierul `CommandRunner` facea refire la o clasa care contine metode
la a caror apel, se genera o comanda de iesire, metode ce poarta denumirea
comenzii pe care o interpreteaza.

Primul meu pas a fost sa distribui aceste metode din fisierul initial in `Clase`
numite dupa aceste metode, care sa le contina.

Mai apoi, am redenumit metodele care construiesc comanda de output cu numele `execute`.
Astfel, acesta a fost primul pas pentru a crea `Design Pattern-ul Command`,
Design Pattern care cuprinde in clasa mea

- `Interface Command`
  1. contine metoda abstracta `ObjectNode execute(CommandInput commandInput);`
  2. este o interfata functionala, intrucat contine o unica metoda neimplementata

- `public final class <cmdname> implements Command`
  (in loc de <cmdname> va fi numele comenzii: Search, Select, Load ...)
  1. implementeaza interfata `Command`
  2. `@Override public ObjectNode execute(final CommandInput commandInput) { /* code to generate the output command */]`
     implementeaza metoda abstracta din interfata `Command`

- `public class CommandInvoker`
  1. contine o singura metoda `invoke`, menita sa apeleze metoda `execute` a unei comenzi data ca parametru
- consideram `Main`-ul ca fiind **receiver-ul comandatului**, iar la fiecare comanda de input,
  vom obtine o instanta a clasei ce poarta numele comenzii, pe care o vom folosi (aluturi de comanda de intrare)
  pentru a invoca metoda `execute` a acesteia, si pentru a obtine rezultatul interpretarii ei.



In aceasta implementare a **Design Pattern-ului Command**, nu este nevoie stocarea
istoricului de comenzi sau pentru a rula o comanda mai veche.


In proiect, fieacere comanda este interpretata imediat cum este citita si nu se va mai
putea reveni la aceasta doar daca este citita din nou.




## Singleton & Factory Design Patterns

Petnru a gestiona exectuia propriu-zisa a comenzilor citite din fisierele din `input/*.json`,
am folosit 


- `interface FactoryCommand`
 1. contine metoda abstracta `Command createCommand(CommandInput commandInput);` care va returna comanda de executat
 2. este o interfata functionala 


- `public final class <cmdname> implements FactoryCommand`
  (in loc de <cmdname> va fi numele comenzii: Search, Select, Load ...)
  1. implementeaza interfata `FactoryCommand`
  2. `@Override public Command createCommand(CommandInput commandInput) { /* code to generate the output command */]`
     implementeaza metoda abstracta din interfata `FactoryCommand`
  3. vor creea o instanta `Command`

- `class CommandRunner`
  1. toata logica din `main` pentru aflarea comenzii curente si executarea ei a fost mutata
  in aceasta clasa
  2. este de tip `Singleton`
  3. putem observa **polimorfismul**, un concept de baza OOP, pe care l-am implementat cu ajutorul
  instantierii claselor care implementeaza aceasi interfata (interfata functionala in cazul meu).
  4. la runtime, va decide ce comanda sa execute, in functie de numele acesteia
  5. este `Singleton`, intrucat nu este logic sa avem doua instante ale acestei clase (instele nu vor diferi nu nimi,
  maxim poate vor intoarce alta comanda si vor executa altceva)


Strucutura acestor Design Pattern-uri (**Singleton Factory**)
este folosita in stransa legatura cu cea a Desing Pattern-ului **Command**.
