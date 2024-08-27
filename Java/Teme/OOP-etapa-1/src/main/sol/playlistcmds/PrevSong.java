package main.sol.playlistcmds;

import fileio.input.LibraryInput;
import fileio.input.SongInput;
import main.FunctionHeader;
import main.Playlist;
import main.SingletonInformation;
import main.cmd.InCmd;
import main.cmd.OutCmd;
import main.stats.PlaylistStatus;

import java.util.ArrayList;

public final class PrevSong implements FunctionHeader {

    private PrevSong() {

    }

    /**
     * functie 'helper' prin intermediul careia se apeleaza
     * functia implementata din interfata 'FunctionHeader'
     * fara a mai fi nevoie de se crea o instanta a clasei
     * (lucru realizat deja in interiorul functiei)
     *
     * @param library   primeste toata arhiva cu melodii, cantece si utilizatori
     *                  playlist-urile apartin utilizatorilor
     * @param inputcmd  primeste comanda curenta de interpretat
     * @param outputcmd modifica comanda de iesire, care va afisata in fisierele din result/
     */
    public static void solve(final LibraryInput library, final InCmd inputcmd,
                             final OutCmd outputcmd) {
        PrevSong obj = new PrevSong();
        obj.func(library, inputcmd, outputcmd);
    }


    /**
     * functie implementata din interfata 'FunctionHeader'
     * are ca scop implementarea si executarea comenzii
     *
     * prev (pentru playlist)
     *
     * @param library   primeste toata arhiva cu melodii, cantece si utilizatori
     *                  playlist-urile apartin utilizatorilor
     * @param inputcmd  primeste comanda curenta de interpretat
     * @param outputcmd modifica comanda de iesire, care va afisata in fisierele din result/
     */
    public void func(final LibraryInput library, final InCmd inputcmd,
                     final OutCmd outputcmd) {

        /* repeat type
        0 = No Repeat
        1 = Repeat All
        2 = Repeat Current Song
         */

        SingletonInformation info = SingletonInformation.getInstance();

        Playlist loadedPlaylist = info.getLoadedPlaylist();
        ArrayList<SongInput> playlistSongs = loadedPlaylist.getSongs();


        int timeSong = info.getTimeCurrentPlaylistSong();
        int idxSong = info.getIdxCurrentPlaylistSong();

        SongInput currentSong;

        if (timeSong > 0) {
            timeSong = 0;   // redam melodia de la inceput
            currentSong = playlistSongs.get(idxSong);
            PlaylistStatus.setPlaylistTime(idxSong, timeSong, currentSong.getDuration());
            outputcmd.setMessage("Returned to previous track successfully. The current track is "
                    + currentSong.getName() + ".");
            return;
        }

        idxSong--;
        timeSong = 0;       // redam melodia de la inceput


        if (idxSong < 0) {
            idxSong = 0;
            currentSong = playlistSongs.get(idxSong);
            PlaylistStatus.setPlaylistTime(idxSong, timeSong, currentSong.getDuration());
            outputcmd.setMessage("Returned to previous track successfully. The current track is "
                    + currentSong.getName() + ".");
            return;
        }

        currentSong = playlistSongs.get(idxSong);
        PlaylistStatus.setPlaylistTime(idxSong, timeSong, currentSong.getDuration());
        outputcmd.setMessage("Returned to previous track successfully. The current track is "
                + currentSong.getName() + ".");
    }
}
