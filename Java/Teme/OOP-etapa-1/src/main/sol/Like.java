package main.sol;

import main.FunctionHeader;
import main.SingletonInformation;
import main.cmd.InCmd;
import main.cmd.OutCmd;
import fileio.input.LibraryInput;
import fileio.input.SongInput;
import fileio.input.UserInput;

import main.Playlist;
import java.util.ArrayList;

public final class Like implements FunctionHeader {


    private Like() {

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
        Like obj = new Like();
        obj.func(library, inputcmd, outputcmd);
    }


    /**
     * functie implementata din interfata 'FunctionHeader'
     * are ca scop implementarea si executarea comenzii
     *
     * like
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
            outputcmd.setMessage("Please load a source before liking or unliking.");
            return;
        }


        if (info.getLoadedSong() == null && info.getLoadedPlaylist() == null) {
            outputcmd.setMessage("Loaded source is not a song.");
            return;
        }

        String userName = inputcmd.getUsername();
        UserInput user = UserInput.getUserByName(library, userName);

        SongInput loadedSong = null;

        if (info.getLoadedSong() != null) {
            loadedSong = info.getLoadedSong();
        }

        if (info.getLoadedPlaylist() != null) {
            Playlist playlist = info.getLoadedPlaylist();
            int idx = info.getIdxCurrentPlaylistSong();
            loadedSong = playlist.getSongs().get(idx);
        }


        String songName = loadedSong.getName();

        if (userName == null) {
            outputcmd.setMessage("The user does not exists");
            return;
        }


        ArrayList<SongInput> likedSongs = user.getLikedSongs();
        ArrayList<UserInput> usersWhoLikedIt = loadedSong.getUsersWhoLikedIt();

        boolean isLiked = false;

        for (SongInput song : likedSongs) {
            if (song.getName().equals(songName)) {
                isLiked = true;
            }
        }

        if (isLiked) {
            likedSongs.remove(loadedSong);
            usersWhoLikedIt.remove(user);
            outputcmd.setMessage("Unlike registered successfully.");
        } else {
            likedSongs.add(loadedSong);
            usersWhoLikedIt.add(user);
            outputcmd.setMessage("Like registered successfully.");
        }

        user.setLikedSongs(likedSongs);
        loadedSong.setUsersWhoLikedIt(usersWhoLikedIt);
    }
}
