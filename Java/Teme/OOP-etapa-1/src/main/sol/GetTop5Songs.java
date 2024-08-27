package main.sol;

import fileio.input.LibraryInput;
import fileio.input.SongInput;
import main.cmd.InCmd;
import main.cmd.OutCmd;

import java.util.ArrayList;
import java.util.Comparator;

public final class GetTop5Songs {

    private final int five = 5;

    private GetTop5Songs() {

    }

    /**
     * functie 'helper' prin intermediul careia se apeleaza
     * functia implementata din interfata 'FunctionHeader'
     * fara a mai fi nevoie de se crea o instanta a clasei
     * (lucru realizat deja in interiorul functiei)
     *
     * @param library primeste toata arhiva cu melodii, cantece si utilizatori
     *                playlist-urile apartin utilizatorilor
     * @param inputcmd primeste comanda curenta de interpretat
     * @param outputcmd modifica comanda de iesire, care va afisata in fisierele din result/

     */
    public static void solve(final LibraryInput library, final InCmd inputcmd,
                             final OutCmd outputcmd) {
        GetTop5Songs obj = new GetTop5Songs();
        obj.func(library, inputcmd, outputcmd);
    }



    /**
     * functie implementata din interfata 'FunctionHeader'
     * are ca scop implementarea si executarea comenzii
     *
     * getTop5Songs
     *
     * @param library primeste toata arhiva cu melodii, cantece si utilizatori
     * playlist-urile apartin utilizatorilor
     * @param inputcmd primeste comanda curenta de interpretat
     * @param outputcmd modifica comanda de iesire, care va afisata in fisierele din result/
     */
    public void func(final LibraryInput library,
                     final InCmd inputcmd, final OutCmd outputcmd) {

        ArrayList<String> result = new ArrayList<String>();

        ArrayList<SongInput> allSongs = library.getSongs();

        // ordonam allSongs dupa numarul de like-uri
        allSongs.sort(new Comparator<SongInput>() {
            @Override
            public int compare(final SongInput s1, final SongInput s2) {
                return (s2.getUsersWhoLikedIt().size() - s1.getUsersWhoLikedIt().size());
            }
        });


        for (int i = 0; i < five; i++) {
            if (i + 1 <= allSongs.size()) {
                result.add(allSongs.get(i).getName());
            }
        }

        ArrayList<Object> castedResult = new ArrayList<>(result);
        outputcmd.setResult(castedResult);
    }

}
