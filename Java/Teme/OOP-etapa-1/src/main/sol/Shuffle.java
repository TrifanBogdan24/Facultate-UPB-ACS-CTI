package main.sol;

import fileio.input.LibraryInput;
import main.FunctionHeader;
import main.SingletonInformation;
import main.cmd.InCmd;
import main.cmd.OutCmd;

public final class Shuffle implements FunctionHeader {

    private Shuffle() {

    }

    /**
     * functie 'helper' prin intermediul careia se apeleaza
     * functia implementata din interfata 'FunctionHeader'
     * fara a mai fi nevoie de se crea o instanta a clasei
     * (lucru realizat deja in interiorul functiei)
     *
     * @param library primeste toata arhiva cu melodii, cantece si utilizatori
     * playlist-urile apartin utilizatorilor
     * @param inputcmd primeste comanda curenta de interpretat
     * @param outputcmd modifica comanda de iesire, care va afisata in fisierele din result/

     */
    public static void solve(final LibraryInput library, final InCmd inputcmd,
                             final OutCmd outputcmd) {
        Shuffle obj = new Shuffle();
        obj.func(library, inputcmd, outputcmd);
    }


    /**
     * functie implementata din interfata 'FunctionHeader'
     * are ca scop implementarea si executarea comenzii
     *
     * shuffle
     *
     * @param library primeste toata arhiva cu melodii, cantece si utilizatori
     *                playlist-urile apartin utilizatorilor
     * @param inputcmd primeste comanda curenta de interpretat
     * @param outputcmd modifica comanda de iesire, care va afisata in fisierele din result/
     */
    public void func(final LibraryInput library, final InCmd inputcmd,
                     final OutCmd outputcmd) {

        SingletonInformation info = SingletonInformation.getInstance();

        if (!info.isLoaded()) {
            outputcmd.setMessage("Please load a source before using the shuffle function.");
            return;
        }

        if (info.getLoadedPlaylist() == null) {
            outputcmd.setMessage("The loaded source is not a playlist.");
            return;
        }

        if (!info.isShuffle()) {
            info.setShuffle(true);
            outputcmd.setMessage("Shuffle function activated successfully.");
        } else {
            info.setShuffle(false);
            outputcmd.setMessage("Shuffle function deactivated successfully.");
        }

    }
}
