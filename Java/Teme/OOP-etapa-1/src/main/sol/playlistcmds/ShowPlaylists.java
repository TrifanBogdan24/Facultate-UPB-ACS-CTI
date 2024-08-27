package main.sol.playlistcmds;

import fileio.input.LibraryInput;
import fileio.input.SongInput;
import fileio.input.UserInput;
import main.FunctionHeader;
import main.Playlist;
import main.cmd.InCmd;
import main.cmd.OutCmd;

import java.util.ArrayList;

public final class ShowPlaylists implements FunctionHeader {

    private ShowPlaylists() {

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
        ShowPlaylists obj = new ShowPlaylists();
        obj.func(library, inputcmd, outputcmd);
    }



    /**
     * implementeaza cerinta pentru afisarea playlist-urilor unui user
     * metoda va transforma o instanta de OutCmd
     * sa afiseze informatiile despre toate playlist-urile unui utilizator
     *
     * @param library primeste toata arhiva cu melodii, cantece si utilizatori
     *                (playlist-urile apartin utilizatorilor)
     * @param inputcmd primeste comanda curenta de interpretat
     * @param outputcmd modifica comanda de iesire, care va afisata in fisierele din result/
     */
    public void func(final LibraryInput library, final InCmd inputcmd, final OutCmd outputcmd) {

        UserInput user = UserInput.getUserByName(library, inputcmd.getUsername());

        if (user == null) {
            outputcmd.setMessage("User does not exists");
            return;
        }


        ArrayList<OutCmd.ResultPlaylists> results = new ArrayList<OutCmd.ResultPlaylists>();

        for (Playlist playlist: user.getPlaylists()) {
            String name = playlist.getName();

            ArrayList<SongInput> songs = playlist.getSongs();
            ArrayList<String> songNames = new ArrayList<>();

            for (SongInput song : songs) {
                songNames.add(song.getName());
            }

            String visibilty = playlist.getVisibility();
            int followers = 0;

            if (playlist.getFollowers() != null) {
                followers = playlist.getFollowers().size();
            }

            OutCmd.ResultPlaylists itemResult = new OutCmd.ResultPlaylists(name, songNames,
                    visibilty, followers);
            results.add(itemResult);
        }

        ArrayList<Object> castedResult = new ArrayList<>(results);
        outputcmd.setResult(castedResult);
    }

}
