package main.sol.playlistcmds;

import fileio.input.LibraryInput;
import fileio.input.SongInput;
import fileio.input.UserInput;
import main.FunctionHeader;
import main.Playlist;
import main.SingletonInformation;
import main.cmd.InCmd;
import main.cmd.OutCmd;

import java.util.ArrayList;

public final class AddRemoveInPlaylist implements FunctionHeader {

    private AddRemoveInPlaylist() {

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
        AddRemoveInPlaylist obj = new AddRemoveInPlaylist();
        obj.func(library, inputcmd, outputcmd);
    }



    /**
     * implementeaza cerinta pentru adaugarea / eliminarea melodiei curente intr-un playlist
     *
     * @param library primeste toata arhiva cu melodii, cantece si utilizatori
     * playlist-urile apartin utilizatorilor
     * @param inputcmd primeste comanda curenta de interpretat
     * @param outputcmd modifica comanda de iesire, care va afisata in fisierele din result/
     */
    public void func(final LibraryInput library, final InCmd inputcmd,
                     final OutCmd outputcmd) {

        SingletonInformation info = SingletonInformation.getInstance();


        if (!info.isLoaded()) {
            outputcmd.setMessage("Please load a source before adding to "
                    + "or removing from the playlist.");
            return;
        }

        if (info.getLoadedSong() == null) {
            outputcmd.setMessage("The loaded source is not a song.");
            return;
        }

        SongInput loaededSong = info.getLoadedSong();
        String loadedSongName = loaededSong.getName();
        int playlistId = inputcmd.getPlaylistId();
        String owner = inputcmd.getUsername();
        UserInput user = UserInput.getUserByName(library, owner);

        if (user == null) {
            outputcmd.setMessage("User does not exist");
            return;
        }

        ArrayList<Playlist> playlists = user.getPlaylists();

        if (playlistId > playlists.size()) {
            outputcmd.setMessage("The specified playlist does not exist.");
            return;
        }


        ArrayList<SongInput> playlistSongs = playlists.get(playlistId - 1).getSongs();

        // stergerea / adaugarea unei melodi din playlist

        boolean isInPlaylist = false;
        for (SongInput song : playlistSongs) {
            if (song.getName().equals(loadedSongName)) {
                isInPlaylist = true;
                break;
            }
        }

        if (isInPlaylist) {
            outputcmd.setMessage("Successfully removed from playlist.");
            playlistSongs.remove(loaededSong);

        } else {
            outputcmd.setMessage("Successfully added to playlist.");
            playlistSongs.add(loaededSong);
        }

        info.setResultSelect(null);
        playlists.get(playlistId - 1).setSongs(playlistSongs);
    }
}
