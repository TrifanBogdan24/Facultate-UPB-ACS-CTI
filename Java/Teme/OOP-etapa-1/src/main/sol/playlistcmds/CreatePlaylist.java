package main.sol.playlistcmds;

import fileio.input.LibraryInput;
import fileio.input.UserInput;
import main.FunctionHeader;
import main.Playlist;
import main.cmd.InCmd;
import main.cmd.OutCmd;

import java.util.ArrayList;

public final class CreatePlaylist  implements FunctionHeader {

    private CreatePlaylist() {

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
        CreatePlaylist obj = new CreatePlaylist();
        obj.func(library, inputcmd, outputcmd);
    }

    /**
     * implementeaza cerinta pentru crearea playlist-ului
     *
     * @param library primeste toata arhiva cu melodii, cantece si utilizatori
     *                (playlist-urile apartin utilizatorilor)
     * @param inputcmd primeste comanda curenta de interpretat
     * @param outputcmd modifica comanda de iesire, care va afisata in fisierele din result/
     */
    public void func(final LibraryInput library, final InCmd inputcmd,
                     final OutCmd outputcmd) {


        String ownerNane = new String(inputcmd.getUsername());
        String playlistName = new String(inputcmd.getPlaylistName());

        UserInput owner = UserInput.getUserByName(library, ownerNane);

        if (owner == null) {
            outputcmd.setMessage("User does not exists");
            return;
        }

        ArrayList<Playlist> allPlaylists = owner.getPlaylists();
        Playlist searchedPlaylist = Playlist.getPlaylistByUserAndName(library, ownerNane,
                playlistName);

        if (searchedPlaylist != null) {
            outputcmd.setMessage("A playlist with the same name already exists.");
            return;
        }

        // adaugarea unui nou playlist de catre un utilizator
        outputcmd.setMessage("Playlist created successfully.");

        Playlist playlist = new Playlist();
        playlist.setOwner(ownerNane);
        playlist.setName(playlistName);
        playlist.setPlaylistId(allPlaylists.size() + 1);
        playlist.setCreateTime(inputcmd.getTimestamp());

        allPlaylists.add(playlist);
    }

}
