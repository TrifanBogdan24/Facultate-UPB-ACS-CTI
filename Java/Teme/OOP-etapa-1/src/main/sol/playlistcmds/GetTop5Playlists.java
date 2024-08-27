package main.sol.playlistcmds;

import fileio.input.LibraryInput;
import fileio.input.UserInput;
import main.FunctionHeader;
import main.Playlist;
import main.cmd.InCmd;
import main.cmd.OutCmd;

import java.util.ArrayList;
import java.util.Comparator;

public final class GetTop5Playlists implements FunctionHeader {

    private final int five = 5;

    private GetTop5Playlists() {

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
        GetTop5Playlists obj = new GetTop5Playlists();
        obj.func(library, inputcmd, outputcmd);
    }

    /**
     * functie implementata din interfata 'FunctionHeader'
     * are ca scop implementarea si executarea comenzii
     *
     * getTop5Playlists
     *
     * @param library primeste toata arhiva cu melodii, cantece si utilizatori
     * playlist-urile apartin utilizatorilor
     * @param inputcmd primeste comanda curenta de interpretat
     * @param outputcmd modifica comanda de iesire, care va afisata in fisierele din result/
     */
    public void func(final LibraryInput library, final InCmd inputcmd,
                     final OutCmd outputcmd) {

        ArrayList<Playlist> allPlaylists = new ArrayList<Playlist>();

        for (UserInput user: library.getUsers()) {
            for (Playlist playlist : user.getPlaylists()) {
                allPlaylists.add(playlist);
            }
        }

        // in acest moment avem toate playlisturile publice
        // trebuie sa le ordonam descrescator dupa numarul de follower
        // la acelasi numar de follower  : crescator dupa timpul de creare


        allPlaylists.sort(new Comparator<Playlist>() {
            @Override
            public int compare(final Playlist p1, final Playlist p2) {
                int nrFollowers1 = p1.getFollowers().size();
                int nrFollowers2 = p2.getFollowers().size();


                if (nrFollowers1 == nrFollowers2) {
                    // crescator dupa timpul de creare
                    int cTime1 = p1.getCreateTime();
                    int cTime2 = p2.getCreateTime();
                    return  (cTime1 - cTime2);
                }

                // descrescator dupa numarul de followers
                return (nrFollowers2 - nrFollowers1);
            }
        });

        ArrayList<String> result = new ArrayList<String>();

        for (int i = 0; i < five; i++) {
            if (i + 1 <= allPlaylists.size()) {
                result.add(allPlaylists.get(i).getName());
            }
        }

        ArrayList<Object> castedResult = new ArrayList<>(result);
        outputcmd.setResult(castedResult);
    }
}
