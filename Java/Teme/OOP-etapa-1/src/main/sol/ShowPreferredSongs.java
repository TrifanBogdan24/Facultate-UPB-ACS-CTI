package main.sol;

import fileio.input.LibraryInput;
import fileio.input.SongInput;
import fileio.input.UserInput;
import main.FunctionHeader;
import main.cmd.InCmd;
import main.cmd.OutCmd;

import java.util.ArrayList;

public final class ShowPreferredSongs implements FunctionHeader {

    private ShowPreferredSongs() {

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
        ShowPreferredSongs obj = new ShowPreferredSongs();
        obj.func(library, inputcmd, outputcmd);
    }

    /**
     * functie implementata din interfata 'FunctionHeader'
     * are ca scop implementarea si executarea comenzii
     *
     * showPrefferedSongs
     *
     * @param library primeste toata arhiva cu melodii, cantece si utilizatori
     * playlist-urile apartin utilizatorilor
     * @param inputcmd primeste comanda curenta de interpretat
     * @param outputcmd modifica comanda de iesire, care va afisata in fisierele din result/
     */
    public void func(final LibraryInput library, final InCmd inputcmd, final OutCmd outputcmd) {
        if (library == null) {
            return;
        }

        String userName = inputcmd.getUsername();
        UserInput user = UserInput.getUserByName(library, userName);

        if (user == null) {
            outputcmd.setMessage("User does not exist");
            return;
        }

        ArrayList<SongInput> likedSongs = user.getLikedSongs();
        ArrayList<String> result = new ArrayList<String>();


        for (SongInput song : likedSongs) {
            result.add(song.getName());
        }

        ArrayList<Object> castedResult = new ArrayList<>(result);
        outputcmd.setResult(castedResult);
    }
}
