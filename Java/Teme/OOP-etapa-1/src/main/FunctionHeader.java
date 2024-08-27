package main;

import fileio.input.LibraryInput;
import main.cmd.InCmd;
import main.cmd.OutCmd;

public interface FunctionHeader {

    /**
     * pentru fiecare tip de comanda de input
     * va exista o implementare a acestei interfete (cu o singura functie)
     *
     * orice implementare trateaza si implementeaza o unica comanda
     *
     * @param library primeste toata arhiva cu melodii, cantece si utilizatori
     *                playlist-urile apartin utilizatorilor
     * @param inputcmd primeste comanda curenta de interpretat
     * @param outputcmd modifica comanda de iesire, care va afisata in fisierele din result/
     */
    void func(LibraryInput library, InCmd inputcmd, OutCmd outputcmd);
}
